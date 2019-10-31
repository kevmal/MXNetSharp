open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Imaging

(*
# Neural Style Transfer
Ported from https://github.com/apache/incubator-mxnet/tree/225f71f744ac5e7bd29868b6d3ba0e4fe2527c43/example/neural-style
*)

#load "load.fsx"
open MXNetSharp
open MXNetSharp.Interop
open System
open System.Net
open System.IO

let context = GPU(0)
let ctxStr = context.ToString()

let vggParamsUrl = "https://github.com/dmlc/web-data/raw/master/mxnet/neural-style/model/vgg19.params"
let vggParamsFile = "vgg19.params"

let styleUrl = "https://github.com/dmlc/web-data/raw/master/mxnet/neural-style/input/starry_night.jpg"
let styleFile = "starry_night.jpg"
let contentUrl = "https://github.com/dmlc/web-data/raw/master/mxnet/neural-style/input/IMG_4343.jpg"
let contentFile = "IMG_4343.jpg"

let ensure (url : string) file = 
        use wc = new WebClient()
        wc.DownloadFile(url, file)

ensure vggParamsUrl vggParamsFile
ensure styleUrl styleFile
ensure contentUrl contentFile

let styleImage = Image.FromFile(styleFile)
let contentImage = Image.FromFile(contentFile)

let loadImage (image : Image) = 
    let bmp = new Bitmap(image)
    let dat =
        [|
            for y = 0 to image.Height - 1 do
                for x = 0 to image.Width - 1 do
                    let p = bmp.GetPixel(x,y)
                    yield float32 p.R - 123.68f
                    yield float32 p.G - 116.779f
                    yield float32 p.B - 103.939f
        |]
    let im = new NDArray(dat |> Array.map float32, [image.Height; image.Width; 3], context)
    let resized = Operators.ImageResize(im, [224;224]).[0]
    resized.SwapAxis(0,2).SwapAxis(1,2).Reshape([1;3;224;224])


let vggParams = NDArray.Load vggParamsFile    

let data = new Variable("data")
let conv1_1 = new Convolution(data = data, numFilter = 64, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv1_1")
let relu1_1 = new Relu(conv1_1, Name = "relu1_2")
let conv1_2 = new Convolution(data = relu1_1, numFilter = 64, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv1_2")
let relu1_2 = new Relu(conv1_2, Name = "relu1_2")
let pool1 = new Pooling(relu1_2, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = PoolType.Avg, Name = "pool1")

let conv2_1 = new Convolution(data = pool1, numFilter = 128, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv2_1")
let relu2_1 = new Relu(conv2_1, Name = "relu2_1")
let conv2_2 = new Convolution(data = relu2_1, numFilter = 128, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv2_2")
let relu2_2 = new Relu(conv2_2, Name = "relu2_2")
let pool2 = new Pooling(relu2_2, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = PoolType.Avg, Name = "pool2")

let conv3_1 = new Convolution(data = pool2, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_1")
let relu3_1 = new Relu(conv3_1, Name = "relu3_1")
let conv3_2 = new Convolution(data = relu3_1, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_2")
let relu3_2 = new Relu(conv3_2, Name = "relu3_2")
let conv3_3 = new Convolution(data = relu3_2, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_3")
let relu3_3 = new Relu(conv3_3, Name = "relu3_3")
let conv3_4 = new Convolution(data = relu3_3, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_4")
let relu3_4 = new Relu(conv3_4, Name = "relu3_4")
let pool3 = new Pooling(relu3_4, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = PoolType.Avg, Name = "pool3")

let conv4_1 = new Convolution(data = pool3, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_1")
let relu4_1 = new Relu(conv4_1, Name = "relu4_1")
let conv4_2 = new Convolution(data = relu4_1, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_2")
let relu4_2 = new Relu(conv4_2, Name = "relu4_2")
let conv4_3 = new Convolution(data = relu4_2, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_3")
let relu4_3 = new Relu(conv4_3, Name = "relu4_3")
let conv4_4 = new Convolution(data = relu4_3, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_4")
let relu4_4 = new Relu(conv4_4, Name = "relu4_4")
let pool4 = new Pooling(relu4_4, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = PoolType.Avg, Name = "pool4")

let conv5_1 = new Convolution(data = pool4, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv5_1")
let relu5_1 = new Relu(conv5_1, Name = "relu5_1")

let style = new SymbolGroup<unit>((), [|relu1_1; relu2_1; relu3_1; relu4_1; relu5_1|])
let content = new SymbolGroup<unit>((), [| relu4_2 |])

let makeExecutor style content (inputSize : int seq) = 
    let inputSize = inputSize |> Seq.toArray
    let out = new SymbolGroup<unit>((), [| style; content |])
    //let inputSize = [|224;224|]
    let dataShape = [1;3;inputSize.[0];inputSize.[1]]
    let (k,i,d) = MXSymbol.keyShapeToCsrForm uint32 [| "data", Array.ofSeq dataShape |]
    let inferResult = MXSymbol.inferShape out.UnsafeHandle k i d
    let args = 
        out.ArgumentNames
        |> Array.mapi 
            (fun i n -> 
                let s = inferResult.InputShapes.[i] |> Array.map int
                let a = Operators.ZerosNDArray(shape = s, ctx = ctxStr) |> Array.head
                if n = "data" then 
                    n, a
                else
                    let scc,b = vggParams.TryGetValue ("arg:" + n)
                    if scc then 
                        b.CopyTo(a)
                        n, a
                    else    
                        printfn "Skip argument %s" n
                        n, a
            )
        |> dict
    let inArgs = out.ArgumentNames |> Array.map (fun n -> args.[n])
    let gradReqType, argGrad = 
        out.ArgumentNames
        |> Array.map 
            (fun n ->
                if n = "data" then  
                    OpReqType.WriteTo, Operators.ZerosLike(args.[n]).[0]
                else
                    OpReqType.NullOp, new NDArray()
            )
        |> Array.unzip
    {| 
        ArgGrad = Array.zip out.ArgumentNames argGrad |> dict
        Args = args
        Executor = new Executor(out, context, inArgs, argGrad, gradReqType, Array.empty)
    |}

let loss (gram: SymbolGroup<'a>) content = 
    let gramLoss = 
        MXSymbol.listOutputs gram.UnsafeHandle
        |> Array.mapi 
            (fun i oname ->
                let gvar = new Variable(sprintf "target_gram_%d" i)
                new Sum(new Square(gvar - gram.SymbolArray.[i]))
            )
    let cvar = new Variable("target_content")
    let contentLoss = new Sum(new Square(cvar - content))
    new SymbolGroup<unit>((), gramLoss |> Array.map (fun x -> upcast x)), contentLoss


let tvWeight = Some 1e-2    
let stopThreshold = 0.0005
let maxSize = 600
let contentWeight = 10.0
let styleWeight = 1.0
let learningRate = 0.001

let lrScheduleDelay = 50
let lrScheduleFactor = 0.6
let saveEpochs = 5
let maxNumEpochs = 1000

let removeNoise = 0.02

let contentNoise = 0.02


let contentIn = loadImage contentImage
let styleIn = loadImage styleImage

let styleInferResult = 
    let (k,i,d) = MXSymbol.keyShapeToCsrForm uint32 [| "data", [|1; 3; 224; 224|]|]
    MXSymbol.inferShape style.UnsafeHandle k i d


let gramList, gradScale = 
    MXSymbol.listOutputs style.UnsafeHandle
    |> Array.mapi 
        (fun i outName ->
            let shape = styleInferResult.OutputShapes.[i] |> Array.map int
            let target = [shape.[1]; shape.[2 ..] |> Array.reduce (*)] 
            let x = new Reshape(style.SymbolArray.[i], shape = target) //TODO: deprecate targetShape
            let gram = new FullyConnected(data = x, weight = x, noBias = true, numHidden = shape.[1])
            gram, (shape.[1..] |> Array.reduce (*)) * shape.[1]
        )
    |> Array.unzip

let gram = new SymbolGroup<unit>((), gramList |> Array.map (fun x -> x :> Symbol))

let modelExe = makeExecutor gram content [|224; 224|]



styleIn.CopyTo(modelExe.Args.["data"])
let outputs = modelExe.Executor.Forward(true) //REVIEW: training true?

let styleArray = 
    outputs.[0 .. outputs.Length - 2] 
    |> Array.map 
        (fun x ->
            x.CopyTo(context)
        )


contentIn.CopyTo(modelExe.Args.["data"])
modelExe.Executor.Forward(true) //REVIEW: training true?

let contentArray = outputs.[outputs.Length - 1].CopyTo(context)


// we could get rid of modelExe here

let styleLoss, contentLoss = loss gram content

let executor = makeExecutor styleLoss contentLoss [|224; 224|]

let gradArray = 
    [|
        yield! 
            styleArray
            |> Array.mapi 
                (fun i a ->
                    a.CopyTo(executor.Args.[sprintf "target_gram_%d" i])
                    //TODO: handle ctx parameters in op gen
                    let w = Operators.OnesNDArray(shape = [1], ctx = ctxStr)
                    Operators.MulScalar(w, w.[0], styleWeight / double gradScale.[i])
                    w.[0]
                )
        let w = 
            let w = Operators.OnesNDArray(shape = [1], ctx = ctxStr)
            Operators.MulScalar(w, w.[0], contentWeight) |> ignore
            w.[0]
        w
    |]


contentArray.CopyTo(executor.Args.["target_content"])

let makeTvGradExecutor (img : NDArray) tvWeight = 
    match tvWeight with 
    | Some w ->
        let nchannel = img.Shape.[1]
        let simg = new Variable("img")
        let skernel = new Variable("kernel")
        let channels = new SliceChannel(simg, nchannel)
        let convs : Symbol [] = 
            channels.Outputs
            |> Array.map
                (fun c ->
                    new Convolution(data = c, weight = skernel, numFilter = 1, kernel = [3;3], pad = [1;1], noBias = true, stride = [1;1]) :> Symbol
                )
        let out = new Concat(convs)
        let kernel = [ 0; -1;  0;
                      -1;  4; -1;
                       0; -1;  0]
                     |> List.map (fun x -> float32 x / 8.f)
                     |> (fun x -> new NDArray(x, [1;1;3;3], context))
        let out = new MulScalar(out, w)
        let inArgs,argGrad,grapReqType = 
            out.ArgumentNames
            |> Array.map 
                (function 
                 | "img" -> img, new NDArray(), OpReqType.NullOp
                 | "kernel" -> kernel, new NDArray(), OpReqType.NullOp
                 | v -> failwithf "Unhandled arg %s" v)
            |> Array.unzip3
        {|
            Executor = new Executor(out, context, inArgs, argGrad, grapReqType, Array.empty)
            //KeepAlive = ([box simg; skernel; channels; convs; out; kernel] : obj list)
            //KeepAlive = ([channels] : obj list) //TODO: we should not need to ref this
        |}
        |> Some
    | None -> None

// Train


let img = Operators.RandomUniformNDArray(-0.1, 0.1, contentIn.Shape, ctx = ctxStr).[0]
let mutable oldImg = img.CopyTo(context)
let clipNorm = 1.f * (img.Shape |> Array.reduce (*) |> float32)
let tvGradExe = makeTvGradExecutor img tvWeight


let momentum = Operators.ZerosLike(img).[0]
let mutable lr = learningRate
let opt w g = Operators.NagMomUpdate([w],w,g,momentum, lr, momentum = 0.95, wd = 0.0001 )

// https://stackoverflow.com/questions/1922040/how-to-resize-an-image-c-sharp
let resizeImage (image : Image) w h = 
    let w,h = 
        let longEdge = max w h
        if longEdge > maxSize then 
            let r = double maxSize / double longEdge
            let scale x = double x * r |> round |> int
            scale w, scale h
        else
            w,h
    let destRect = Rectangle(0,0,w,h)
    let destImage = new Bitmap(w,h)
    destImage.SetResolution(image.HorizontalResolution, image.VerticalResolution)
    use g = Graphics.FromImage destImage
    g.CompositingMode <- CompositingMode.SourceCopy
    g.CompositingQuality <- CompositingQuality.HighQuality
    g.InterpolationMode  <- InterpolationMode.HighQualityBicubic
    g.SmoothingMode <- SmoothingMode.HighQuality
    g.PixelOffsetMode <- PixelOffsetMode.HighQuality
    use wrapMode = new ImageAttributes()
    wrapMode.SetWrapMode(WrapMode.TileFlipXY)
    g.DrawImage(image,destRect,0,0,image.Width,image.Height,GraphicsUnit.Pixel,wrapMode)
    destImage

let save (filename : string) (img : NDArray) = 
    printfn "Saving %s" filename
    let img = Operators.Reshape(img, shape = [3; 224; 224]).[0]
    let img = Operators.SwapAxis(img,1,2).[0]
    let img = Operators.SwapAxis(img,0,2).[0]
    let h = 224
    let w = 224
    //let img = Operators.ImageResize(img, contentImage.Height, contentImage.Width).[0]
    let a : float32 [] = img.ToArray()
    use bmp = new Bitmap(w,h,PixelFormat.Format32bppRgb)
    let mutable i = 0
    for y = 0 to h - 1 do
        for x = 0 to w - 1 do
            let r = a.[i] + 123.68f |> min 255.f |> max 0.f |> int
            i <- i + 1
            let g = a.[i] + 116.779f |> min 255.f |> max 0.f |> int
            i <- i + 1
            let b = a.[i] + 103.939f |> min 255.f |> max 0.f |> int
            i <- i + 1
            let c = Color.FromArgb(1,r,g,b)
            bmp.SetPixel(x,y,c)
    let resizedImage = resizeImage bmp contentImage.Width contentImage.Height
    resizedImage.Save(filename, Imaging.ImageFormat.Jpeg)

save "nstyle_start.jpg" img
save "nstyle_content.jpg" contentIn

let rec trainLoop epoch =
    if epoch >= maxNumEpochs then 
        printf "Max epoch hit"
        save "nstyle_maxepoch_final.jpg" img
    else
        //for i = 1 to  maxNumEpochs do 
        img.CopyTo(executor.Args.["data"])
        let outs = executor.Executor.Forward(true)
        executor.Executor.Backward(gradArray)
        let gnorm : float32 = Operators.Norm(executor.ArgGrad.["data"]).[0].ToArray().[0]
        if gnorm > clipNorm then 
            Operators.MulScalar([executor.ArgGrad.["data"]], executor.ArgGrad.["data"], double(clipNorm / gnorm))

        let optResult = 
            match tvGradExe with 
            | Some e -> 
                let outs = e.Executor.Forward(true)
                //opti
                let g = Operators.ElemwiseAdd(executor.ArgGrad.["data"], outs.[0]).[0]
                opt img g
            | None -> 
                opt img executor.ArgGrad.["data"]

        //let newImg = optResult.[0]
        let diff = Operators.ElemwiseSub(oldImg, img).[0]
        let eps : float32 = Operators.ElemwiseDiv(Operators.Norm(diff).[0], Operators.Norm(img).[0]).[0].ToArray().[0]
        oldImg <- img.CopyTo(context)
        printfn "%5d : %f" epoch eps
        if (epoch + 1) % lrScheduleDelay = 0 then 
            let olr = lr
            lr <- lr * lrScheduleFactor
            printfn "Learning rate %f -> %f" olr lr
        if double eps < stopThreshold then 
            save "nstyle_eps_final.jpg" img
        elif (epoch + 1) % saveEpochs = 0 then 
            let filename = sprintf "nstyle_e_%d.jpg" (epoch + 1)
            save filename img
            GC.Collect()
            trainLoop (epoch + 1)
        else
            trainLoop (epoch + 1)
 
 


trainLoop 0


//TODO: don't just save to default directory
//TODO: show images















    


