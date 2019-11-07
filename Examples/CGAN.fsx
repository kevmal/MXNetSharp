
// Conditional Generative Adversarial Nets (https://arxiv.org/abs/1411.1784) Mehdi Mirza, Simon Osindero
// Adapted from https://github.com/eriklindernoren/Keras-GAN/tree/master/cgan


#load "load.fsx"
open MXNetSharp
open MXNetSharp.IO
open System
open System.Net
open System.IO
open System.Collections.Generic
open System.IO.Compression

open MXNetSharp.PrimitiveOperators

let batchSize = 128
let nLatent = 100
let nClasses = 10
    
let context = GPU(0)

let ensure (url : string) (file : string) = 
    if not(File.Exists file) then
        use wc = new WebClient()
        if url.ToLower().EndsWith ".gz" && not (file.EndsWith ".gz") then 
            let tmpFile = Path.GetTempFileName()
            wc.DownloadFile(url, tmpFile)
            do 
                use gfile = File.OpenRead(tmpFile)
                use g = new GZipStream(gfile,CompressionMode.Decompress)
                use f = File.OpenWrite(file)
                g.CopyTo(f)
            File.Delete tmpFile
        else
            wc.DownloadFile(url, file)

ensure "http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz" "train-images-idx3-ubyte"
ensure "http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz" "train-labels-idx1-ubyte"
ensure "http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz" "t10k-images-idx3-ubyte"
ensure "http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz" "t10k-labels-idx1-ubyte"

let trainIter = new MNISTIter(@"train-images-idx3-ubyte", 
                              @"train-labels-idx1-ubyte",
                              batchSize = batchSize,
                              flat = false)

let valIter   = new MNISTIter(@"t10k-images-idx3-ubyte", 
                              @"t10k-labels-idx1-ubyte",
                              batchSize = batchSize,
                              flat = false)

let inline (.>>) (x : Symbol) (y : 'a) : 'a = 
    let _f() = y :> SymbolOperator
    y.ComposedWith(x) |> box |> unbox

let inline (.|>) (x : ^a) (f : ^b) = 
    let _f() = f :> SymbolOperator
    f.Initialize()
    let fcopy = f.Copy() :?> SymbolOperator
    fcopy.ComposedWith(x)

let withName (name) (x : #Symbol) = x.Name <- name; x

let composable f = 
    let v = Variable()
    SymbolComposable(v,f v)

let noise = Variable "noise"
let label = Variable "label"
let generator = composable (fun noise -> 
        let labelEmbedding = Embedding(data = label, inputDim = nClasses, outputDim = nLatent)
        let layer n = 
            composable(fun x -> 
                x
                .>> FullyConnected(numHidden = n) 
                .>> LeakyReLU(actType = LeakyReLUType.Leaky, slope = 0.2)
                .>> BatchNorm(momentum = 0.8)
            )
        (noise*labelEmbedding)
        .>> layer 256
        .>> layer 512
        .>> layer 1024
        .>> FullyConnected(numHidden = 28*28)
        .>> Activation(ActType.Tanh)
    )

let discriminator = composable (fun image -> 
        let labelEmbedding = Embedding(data = label, inputDim = nClasses, outputDim = 28*28)
        let dense n = 
            composable(fun x -> 
                x
                .>> FullyConnected(numHidden = n) 
                .>> LeakyReLU(actType = LeakyReLUType.Leaky, slope = 0.2)
            )
        (Reshape(image, [batchSize; 1; 28*28])*labelEmbedding)
        .>> dense 512
        .>> dense 512
        .>> Dropout(p = 0.4, mode = DropoutMode.Training)
        .>> dense 512
        .>> Dropout(p = 0.4, mode = DropoutMode.Training)
        .>> FullyConnected(1) 
        .>> Activation(ActType.Sigmoid)
    )


let inputImage = Variable "inputImage"
let scaledImage = inputImage + 0.0 //(inputImage - 127.5) / 127.5

//let valid = Ones(shape = [batchSize])    
//let fake = Zeros(shape = [batchSize])    


let sampledLabels = RandomRandint(0L, 10L, [batchSize])

let ep = 0.000001
let onActual = -Log(ep + (scaledImage .|> discriminator)) .>> Mean() .>> MakeLoss()
let onFake = -Log(ep + 1.0 - ((* freeze *) generator .|> discriminator)) .>> Mean() .>> MakeLoss()
let gen = -Log(ep + (generator .|> (* freeze *) discriminator)) .>> Mean() .>> MakeLoss()


let randNormal = RandomNormal(shape = [batchSize; 1; nLatent]) 
let genNoise = 
    randNormal.Bind(context, BindMap())


let inputs = 
    [
        Binding.Arg(inputImage.Name, shape = [batchSize; 1; 28; 28], opReqType = OpReqType.NullOp, dataType = DataType.Float32)
        Binding.Arg(label.Name, shape = [batchSize; 1], opReqType = OpReqType.NullOp, dataType = DataType.Float32)
        Binding.Arg(noise.Name, shape = [batchSize; 1; nLatent] , opReqType = OpReqType.NullOp, dataType = DataType.Float32, ndarray = genNoise.Outputs.[0])
    ]





let q = 
    inputs
    |> BindMap.ofSeq
    |> BindMap.inferShapes onActual
    |> BindMap.inferShapes onFake
    |> BindMap.inferShapes gen
    |> BindMap.map   
        (fun a ->
            match a.Shape with 
            | Some (shape) -> 
                //Operators.ZerosNDArray(shape, ctx = context.ToString()).[0]
                Operators.RandomUniformNDArray(low = -0.1, high = 0.1, shape = shape, ctx = context.ToString())
                |> a.WithNDArray
            | None -> a
        )
    |> BindMap.mapArg 
        (fun a ->
            match a.OpReqType with 
            | None -> {a with OpReqType = Some OpReqType.WriteTo}
            | _ -> a
        )
    |> BindMap.mapArg 
        (fun a ->
            match a.OpReqType with 
            | Some NullOp -> {a with Grad = Some(new NDArray())}
            | _ -> {a with Grad = Some(Operators.ZerosLike(a.NDArray.Value))}
        )

let actualLoss = onActual.Bind(context, q)

let bindOnFake = q |> BindMap.freezeGraph randNormal |> BindMap.freezeGraph generator  
let fakeLoss = onFake.Bind(context, bindOnFake)

let bindGenLoss = q |> BindMap.freezeGraph discriminator
let genLoss = gen.Bind(context, bindGenLoss)


let optUpdate (e : Executor) = 

    let lu = 
        let d = Dictionary<string, NDArray*NDArray>()
        fun (s : String) (a : NDArray) ->
            let scc,v = d.TryGetValue(s)
            if scc then 
                v
            else
                let v = Operators.ZerosLike(a),Operators.ZerosLike(a)
                d.[s] <- v
                v
    fun () -> 
        e.BindMap
        |> Seq.iter
            (fun a ->
                match a with 
                | ArgBinding ({Name = name; OpReqType = Some WriteTo; Grad = Some grad; NDArray = Some weight}) -> 
                    let m,v = lu name grad
                    Operators.AdamUpdate([weight], weight, grad, m, v, 0.0002, 0.5)
                | _ -> ()
            )

let optAct = optUpdate actualLoss
let optFake = optUpdate fakeLoss
let optGen = optUpdate genLoss

let xa = 
    match q.TryGetValue(inputImage.Name) with 
    | _,ArgBinding({NDArray = Some a}) -> a
    | _ -> failwith ""
let la = 
    match q.TryGetValue(label.Name) with 
    | _,ArgBinding({NDArray = Some a}) -> a
    | _ -> failwith ""

genLoss.BindMap
genLoss.BindMap
|> Seq.iter (fun x -> printfn "%s" x.Name)

let generated = generator.Bind(context, q)
generated.Outputs.[0].Shape



let singleBmp pixs = 
    let bitmap = System.Drawing.Bitmap(28*16,28*8)
    let mutable col = 0
    let mutable row = 0
    pixs 
    |> Seq.chunkBySize (28*28)
    |> Seq.iter 
        (fun s ->
            s 
            |> Seq.chunkBySize 28
            |> Seq.iteri
                (fun i xs ->
                    xs 
                    |> Seq.iteri 
                        (fun j x ->
                            let xx = (1.f - x)*255.f |> min 255.f |> max 0.f |> round |> int |> max 0 |> min 255
                            bitmap.SetPixel(col*27 + j,row*27 + i,Drawing.Color.FromArgb(xx, xx, xx))
                        )
                )
            row <- row + 1
            if row = 8 then 
                row <- 0 
                col <- col + 1
    )
    bitmap


let f = Windows.Forms.Form(Visible = true)
let p = Windows.Forms.PictureBox(Dock = Windows.Forms.DockStyle.Fill)
f.Controls.Add p
p.SizeMode <- Windows.Forms.PictureBoxSizeMode.Zoom

valIter.Reset()
valIter.Next() |> ignore

let update epoch mb = 
    let loss1 : float32 = actualLoss.Outputs.[0].ToArray().[0]
    let loss2 : float32 = fakeLoss.Outputs.[0].ToArray().[0]
    let loss3 : float32 = genLoss.Outputs.[0].ToArray().[0]
    generated.Forward(false)
    let out : float32 [] = generated.Outputs.[0].ToArray()
    p.Invoke(Action(fun() ->
        p.Image <- singleBmp out
        f.Text <- sprintf "Epoch % 4d  Mb % 7d  Loss: %A" epoch mb (loss1,loss2,loss3) 
    )) |> ignore



let trainTask = 
    async {
        let mutable epoch = 0
        let mutable mb = 0
        while true do
            trainIter.Reset()
            while (trainIter.Next()) do 
                trainIter.GetData().CopyTo(xa)
                trainIter.GetLabel().CopyTo(la)
                actualLoss.Forward(true)
                actualLoss.Backward()
                optAct()

                genNoise.Forward(false)

                fakeLoss.Forward(true)
                fakeLoss.Backward()
                optFake()

                genLoss.Forward(true)
                genLoss.Backward()
                optGen()

                //let loss1 : float32 = actualLoss.Outputs.[0].ToArray().[0]
                //let loss2 : float32 = fakeLoss.Outputs.[0].ToArray().[0]
                //let loss3 : float32 = genLoss.Outputs.[0].ToArray().[0]
                //printfn "Epoch % 4d  Mb % 7d  Loss: %A" epoch mb (loss1,loss2,loss3) 
                mb <- mb + 1
                if mb % 10 = 0 then 
                    update epoch mb
                //printfn "%f" (eoutput.ToArray() : float32[]).[0]
            epoch <- epoch + 1
            update epoch mb
        }
    |> Async.StartAsTask

        





