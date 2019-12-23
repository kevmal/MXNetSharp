
#load "loadui.fsx"
open Loadui

open MXNetSharp
open MXNetSharp.Interop
open MXNetSharp.SymbolOperators
open MXNetSharp.IO
open System
open System.Net
open System.IO
open System.IO.Compression
open Microsoft.FSharp.NativeInterop

open MXNetSharp.PrimitiveOperators

let batchSize = 64

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

let xIn = new Variable("X")
let scaledInput = xIn + 0.0 //1.0 - xIn/255.0
let dropProb = 0.2
let decInChannels = 1
let nLatent = 10

let reshapedDim = [batchSize;decInChannels;7;7]
let inputsDecoder = 49*decInChannels / 2
let conv name kernelSize strides outFeatureMapCount x = 
    let y = new Convolution(data = x, numFilter = outFeatureMapCount, kernel=[kernelSize;kernelSize], stride = [strides;strides], Name = name + "_conv")
    new LeakyReLU(y, Name = name + "_lrelu")


let convTranspose name kernelSize (strides : int) outFeatureMapCount x = 
    let y = new Deconvolution(data = x, numFilter = outFeatureMapCount, kernel = [kernelSize; kernelSize], stride = [strides; strides], Name = name + "_dconv")
    new Relu(y,Name = name + "_relu")
    
let encoder x dropProb = 
    let layer name s x = 
        let c1 = conv name 4 s 64 x
        new Dropout(c1, dropProb, mode = DropoutMode.Training, Name = name + "_dropout")
    let c1 = layer "eL1" 2 x
    let c2 = layer "eL2" 1 c1
    let c3 = layer "eL3" 1 c2
    let mn = new FullyConnected(data = c3, numHidden = nLatent, noBias = false, flatten = true, Name = "e_mn")
    let sd = 0.5*(new FullyConnected(data = c3, numHidden = nLatent, noBias = false, flatten = true, Name = "e_sd"))
    let epsilon = new RandomNormalLike(mn, Name = "e_ep")
    let z = mn + epsilon * (exp sd)
    z.Name <- "ez"
    z, mn, sd

let decoder (sample : Symbol) dropProb =    
    let d1 = new FullyConnected(data = sample, numHidden = inputsDecoder, noBias = false, Name = "d_fc1")
    let d1 = new LeakyReLU(d1, Name = "d_relu1")
    let d2 = new FullyConnected(data = d1, numHidden = inputsDecoder*2 + 1, noBias = false, Name = "d_fc2")
    let d2 = new LeakyReLU(d2, Name = "d_relu2")
    let x = new Reshape(d2, reshapedDim, Name = "d_dx")
    let layer name s x = 
        let c1 = convTranspose name 4 s 64 x
        new Dropout(c1, dropProb, mode = DropoutMode.Training, Name = name + "_dropout")
    let c1 = layer "d_L1" 2 x
    let c2 = layer "d_L2" 1 c1
    let c3 = convTranspose "d_L3" 4 1 64 c2
    let z = 
        let fc = new FullyConnected(data = c3, numHidden = 28*28, Name = "d_fc3")
        new Activation(fc, ActType.Sigmoid, Name = "d_z")
    let dec = new Reshape(z, [batchSize; 1; 28; 28])
    dec.Name <- "decoder"
    dec


let sampled, mn, sd = encoder scaledInput dropProb

let dec = decoder sampled dropProb
let unscaledOut = dec //((0.0-dec) + 1.0)*255.0 // 1.0 - xIn/255.0

let imgLoss = new Sum(new Square(dec - scaledInput))
let latentLoss = -0.5*(new Sum(1.0 + 2.0*sd - new Square(mn) - exp(2.0*sd)))
let loss = new MakeLoss(new Mean(imgLoss + latentLoss))

let inputs = loss.InputSymbols


let dataShape = [batchSize;1;28;28]
let (k,i,d) = MXSymbol.keyShapeToCsrForm uint32 [| "X", Array.ofSeq dataShape |]
let inferResult = MXSymbol.inferShape loss.UnsafeHandle k i d
loss.ArgumentNames |> Seq.length

inferResult.InputShapes |> Seq.length
loss.InputSymbols |> Seq.length
loss.InputSymbols
|> Seq.iter 
    (fun x ->
        printfn "%s" x.Name
    )

type Train = 
    | NoTrain of NDArray
    | TrainParameters of NDArray*NDArray*NDArray*NDArray

let ps = 
    inputs
    |> Array.zip inferResult.InputShapes
    |> Array.map
        (fun (s,x) ->
            if x.Name = "X" then NoTrain(new NDArray(shape = dataShape, context = context))
            else
                let s = s |> Seq.map int
                TrainParameters(
                    MX.RandomUniformNDArray(context, -0.1, 0.1, s), 
                        MX.ZerosNDArray(context, s),
                        MX.ZerosNDArray(context, s),
                        MX.ZerosNDArray(context, s))
        )
    
let lr = 0.0005

let exe, texe = 
    let inArgs = 
        ps 
        |> Array.map (function
            | TrainParameters(a,_,_,_)
            | NoTrain a -> a)
    let grads = 
        ps 
        |> Array.map (function
            | TrainParameters(_,g,_,_) -> g
            | NoTrain a -> new NDArray())
    let g = 
        ps 
        |> Array.map (function
            | TrainParameters(_,g,_,_) -> OpReqType.WriteTo
            | NoTrain a -> OpReqType.NullOp)
    let e1 = new Executor(loss, context, inArgs, grads, g, [])
    let e2 = new Executor(unscaledOut, context, inArgs, grads, g, [])
    e1,e2

let xa = ps |> Seq.pick (function NoTrain a -> Some a | _ -> None)



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

let bmp1 = new WriteableBitmap(PixelSize(28*8,28*8),Vector(90.,90.), Nullable(Platform.PixelFormat.Bgra8888))
let bmp2 = new WriteableBitmap(PixelSize(28*8,28*8),Vector(90.,90.), Nullable(Platform.PixelFormat.Bgra8888))

let wnd = 
    UI.ui (fun () ->
        let f = Window()
        let p = Image()
        let p2 = Image()
        p.Source <- bmp1
        p2.Source <- bmp2
        let split = Grid()
        split.ColumnDefinitions.Add(ColumnDefinition(GridLength.Parse "*"))
        split.ColumnDefinitions.Add(ColumnDefinition(GridLength.Parse "5"))
        split.ColumnDefinitions.Add(ColumnDefinition(GridLength.Parse "*"))
        //split.RowDefinitions.Add(RowDefinition())
        //p.SetValue(Grid.RowProperty, 0)
        p.SetValue(Grid.ColumnProperty, 0)
        //p2.SetValue(Grid.RowProperty, 0)
        p2.SetValue(Grid.ColumnProperty, 2)
        split.Children.Add(p)
        let ss = GridSplitter()
        ss.SetValue(Grid.ColumnProperty, 1)
        ss.SetValue(Grid.WidthProperty, 5)
        //ss.SetValue(Grid.HorizontalAlignmentProperty, "Stretch")
        split.Children.Add(ss)
        split.Children.Add(p2)
        f.Content <- split
        f.Show()
        f.KeyUp
        |> Observable.add
            (fun k ->
                if k.Key = Input.Key.Space then 
                    printfn  "Next test batch"
                    if not(valIter.Next()) then 
                        valIter.Reset() 
                        valIter.Next() |> ignore
            )
        {|
            Window = f
            Image1 = p
            Image2 = p2
        |}
    )


valIter.Reset()
valIter.Next() |> ignore

let update epoch mb = 
    valIter.GetData().CopyTo(xa)
    exe.Forward(false)
    let loss : float32 = exe.Outputs.[0].ToArray().[0]
    texe.Forward(false)
    let imgs = texe.Outputs
    singleBmp (xa.ToArray()) bmp1
    singleBmp (imgs.[0].ToArray()) bmp2
    UI.uido (fun() ->
        wnd.Window.Title <- sprintf "Epoch % 4d  Mb % 7d  Loss: %f" epoch mb loss
        wnd.Image1.InvalidateVisual()
        wnd.Image2.InvalidateVisual()
    )




let trainTask = 
    async {
        let mutable epoch = 0
        let mutable mb = 0
        let beta1 = 0.9
        let beta2 = 0.999
        let mutable updateCount = 0
        while true do
            trainIter.Reset()
            while (trainIter.Next()) do 
                trainIter.GetData().CopyTo(xa)
                exe.Forward(true)
                exe.Backward()
                updateCount <- updateCount + 1
                let t = double updateCount
                let lr = lr*sqrt(1.0 - Math.Pow(beta2,t)) / (1.0 - Math.Pow(beta1,t))
                ps
                |> Array.iter 
                    (function 
                     | NoTrain _ -> ()
                     | TrainParameters(w,g,mu,sd) -> MX.AdamUpdate([w],w,g,mu,sd,lr, beta1, beta2)
                     )
                mb <- mb + 1
                if mb % 10 = 0 then 
                    update epoch mb
                //printfn "%f" (eoutput.ToArray() : float32[]).[0]
            epoch <- epoch + 1
            update epoch mb
    } |> Async.StartAsTask        









    


