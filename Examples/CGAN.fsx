// Conditional Generative Adversarial Nets (https://arxiv.org/abs/1411.1784) Mehdi Mirza, Simon Osindero
// Adapted from https://github.com/eriklindernoren/Keras-GAN/tree/master/cgan

#nowarn "9" // Bitmap updates for displaying
#load "loadui.fsx"
open Loadui

open MXNetSharp
open MXNetSharp.Interop
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
    
let context = if MXLib.getGpuCount() > 0 then GPU 0 else CPU 0

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

let label = Input("label", [0; 1])
let generator = 
    let labelEmbedding = Embedding(data = label, inputDim = nClasses, outputDim = nLatent)
    let layer n = 
        Hole()
        .>> FullyConnected(numHidden = n) 
        .>> LeakyReLU(actType = LeakyReLUType.Leaky, slope = 0.2)
        .>> BatchNorm(momentum = 0.8)
    (Hole()*labelEmbedding)
    .>> layer 256
    .>> layer 512
    .>> layer 1024
    .>> FullyConnected(numHidden = 28*28)
    .>> Activation(ActType.Tanh)

let discriminator = 
    let labelEmbedding = Embedding(data = label, inputDim = nClasses, outputDim = 28*28)
    let dense n = 
        Hole()
        .>> FullyConnected(numHidden = n) 
        .>> LeakyReLU(actType = LeakyReLUType.Leaky, slope = 0.2)
    (Reshape(Hole(), [0; 1; 28*28])*labelEmbedding)
    .>> dense 512
    .>> dense 512
    .>> Dropout(p = 0.4, mode = DropoutMode.Training)
    .>> dense 512
    .>> Dropout(p = 0.4, mode = DropoutMode.Training)
    .>> FullyConnected(1) 
    .>> Activation(ActType.Sigmoid)


let inputImage = Input("inputImage", [0; 1; 28; 28])

let ep = 0.000001
let onActual = -Log(ep + (inputImage .|> discriminator)) .>> Mean() .>> MakeLoss()
let onFake = -Log(ep + 1.0 - ((* freeze *) generator .|> discriminator)) .>> Mean() .>> MakeLoss()
let gen = -Log(ep + (generator .|> (* freeze *) discriminator)) .>> Mean() .>> MakeLoss()
let inputs = Bindings.inputs [ inputImage; label ]


let bindings = 
    inputs
    |> Bindings.ofSeq
    |> Bindings.inferShapes onActual
    |> Bindings.inferShapes onFake
    |> Bindings.inferShapes gen


let randNormal = RandomNormal(shape = [128; 1; nLatent]) 
let genNoise = randNormal.Bind(context)
let trainBindings = 
    bindings 
    |> Bindings.batchSize 128
    |> Bindings.setContext context
    |> Bindings.initWith (Initializer.Xavier())


let actualLoss = onActual.Bind(context, trainBindings)

let bindOnFake = trainBindings |> Bindings.freezeGraph randNormal |> Bindings.freezeGraph generator  
let fakeLoss = onFake.Bind(context, bindOnFake)

let bindGenLoss = trainBindings |> Bindings.freezeGraph discriminator
let genLoss = gen.Bind(context, bindGenLoss)


let optUpdate (e : Executor) = 
    let lr = 0.0002
    let beta1 = 0.5
    let beta2 = 0.999
    let mutable updateCount = 0
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
        updateCount <- updateCount + 1
        let t = double updateCount
        let lr = lr*sqrt(1.0 - Math.Pow(beta2,t)) / (1.0 - Math.Pow(beta1,t))
        e.Bindings
        |> Seq.iter
            (fun a ->
                match a with 
                | {Name = name; BindType = ArgBind(Some WriteTo, Some grad); NDArray = Some weight} -> 
                    let m,v = lu name grad
                    MX.AdamUpdate([weight], weight, grad, m, v, lr, beta1, beta2)
                | _ -> ()
            )

let optAct = optUpdate actualLoss
let optFake = optUpdate fakeLoss
let optGen = optUpdate genLoss

let xa = trainBindings.NDArray(inputImage)
let la = trainBindings.NDArray(label) 

let generated = generator.Bind(context, trainBindings)


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
    let loss1 : float32 = actualLoss.Outputs.[0].ToArray<_>().[0]
    let loss2 : float32 = fakeLoss.Outputs.[0].ToArray<_>().[0]
    let loss3 : float32 = genLoss.Outputs.[0].ToArray<_>().[0]
    generated.Forward(false)
    let out : float32 [] = generated.Outputs.[0].ToArray<_>()
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
                trainIter.GetLabel().Reshape(la.Shape).CopyTo(la)
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

        





