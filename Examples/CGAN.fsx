// Conditional Generative Adversarial Nets (https://arxiv.org/abs/1411.1784) Mehdi Mirza, Simon Osindero
// Adapted from https://github.com/eriklindernoren/Keras-GAN/tree/master/cgan


#load "loadui.fsx"
open Loadui

open MXNetSharp
open MXNetSharp.IO
open System
open System.Net
open System.IO
open System.Collections.Generic
open System.IO.Compression
open Microsoft.FSharp.NativeInterop

open MXNetSharp.PrimitiveOperators
open MXNetSharp.SymbolOperators

let batchSize = 128
let nLatent = 100
let nClasses = 10
    
let context = CPU(0)

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
    randNormal.Bind(context, Bindings())


let inputs = 
    [
        Bind.Arg(inputImage.Name, shape = [batchSize; 1; 28; 28], opReqType = OpReqType.NullOp, dataType = DataType.Float32)
        Bind.Arg(label.Name, shape = [batchSize; 1], opReqType = OpReqType.NullOp, dataType = DataType.Float32)
        Bind.Arg(noise.Name, shape = [batchSize; 1; nLatent] , opReqType = OpReqType.NullOp, dataType = DataType.Float32, ndarray = genNoise.Outputs.[0])
    ]





let q = 
    inputs
    |> Bindings.ofSeq
    |> Bindings.inferShapes onActual
    |> Bindings.inferShapes onFake
    |> Bindings.inferShapes gen
    |> Bindings.map   
        (fun a ->
            match a.Shape with 
            | Some (shape) -> 
                //Operators.ZerosNDArray(shape, ctx = context.ToString()).[0]
                MX.RandomUniformNDArray(low = -0.1, high = 0.1, shape = shape, ctx = context.ToString())
                |> a.WithNDArray
            | None -> a
        )
    |> Bindings.mapArg 
        (fun a ->
            match a.OpReqType with 
            | None -> {a with OpReqType = Some OpReqType.WriteTo}
            | _ -> a
        )
    |> Bindings.mapArg 
        (fun a ->
            match a.OpReqType with 
            | Some NullOp -> {a with Grad = Some(new NDArray())}
            | _ -> {a with Grad = Some(MX.ZerosLike(a.NDArray.Value))}
        )

let actualLoss = onActual.Bind(context, q)

let bindOnFake = q |> Bindings.freezeGraph randNormal |> Bindings.freezeGraph generator  
let fakeLoss = onFake.Bind(context, bindOnFake)

let bindGenLoss = q |> Bindings.freezeGraph discriminator
let genLoss = gen.Bind(context, bindGenLoss)


let optUpdate (e : Executor) = 

    let lu = 
        let d = Dictionary<string, NDArray*NDArray>()
        fun (s : String) (a : NDArray) ->
            let scc,v = d.TryGetValue(s)
            if scc then 
                v
            else
                let v = MX.ZerosLike(a),MX.ZerosLike(a)
                d.[s] <- v
                v
    fun () -> 
        e.BindMap
        |> Seq.iter
            (fun a ->
                match a with 
                | ArgBinding ({Name = name; OpReqType = Some WriteTo; Grad = Some grad; NDArray = Some weight}) -> 
                    let m,v = lu name grad
                    MX.AdamUpdate([weight], weight, grad, m, v, 0.0002, 0.5)
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



open Avalonia
open Avalonia.Controls
open Avalonia.Media.Imaging

let singleBmp (pixs : float32 []) (bitmap : WriteableBitmap) = 
    let mutable col = 0
    let mutable row = 0
    use fb = bitmap.Lock()
    let ptr =  fb.Address |> NativePtr.ofNativeInt<uint32>
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
                            let xx = (1.f - x)*255.f |> min 255.f |> max 0.f |> round |> uint32
                            //bitmap.SetPixel(col*27 + j,row*27 + i,Drawing.Color.FromArgb(xx, xx, xx))
                            let c = col*27 + j
                            let r = (row*27 + i)*(fb.RowBytes / 4)
                            let ix = r + c
                            let pixel = (xx <<< 16) ||| (xx <<< 8) ||| xx  ||| (0xFFu <<< 24)
                            NativePtr.set ptr ix pixel
                        )
                )
            row <- row + 1
            if row = 8 then 
                row <- 0 
                col <- col + 1
    )

let bmp1 = new WriteableBitmap(PixelSize(28*16,28*8),Vector(90.,90.), Nullable(Platform.PixelFormat.Bgra8888))

let wnd = 
    UI.ui (fun () ->
        let f = Window()
        let p = Image()
        p.Source <- bmp1
        f.Content <- p
        f.Show()
        {|
            Window = f
            Image = p
        |}
    )


valIter.Reset()
valIter.Next() |> ignore

let update epoch mb = 
    let loss1 : float32 = actualLoss.Outputs.[0].ToArray().[0]
    let loss2 : float32 = fakeLoss.Outputs.[0].ToArray().[0]
    let loss3 : float32 = genLoss.Outputs.[0].ToArray().[0]
    generated.Forward(false)
    let out : float32 [] = generated.Outputs.[0].ToArray()
    singleBmp out bmp1
    UI.uido(fun() ->
        wnd.Image.InvalidateVisual()
        wnd.Window.Title <- sprintf "Epoch % 4d  Mb % 7d  Loss: %A" epoch mb (loss1,loss2,loss3) 
    )



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

        





