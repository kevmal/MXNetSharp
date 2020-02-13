// Neural Style Transfer
// Ported from https://github.com/apache/incubator-mxnet/tree/225f71f744ac5e7bd29868b6d3ba0e4fe2527c43/example/neural-style


#load "load.fsx"
open MXNetSharp
open MXNetSharp.SymbolOperators
open MXNetSharp.Interop
open System
open System.Net
open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Imaging


// Images will be saved here
let outputDirectory = IO.Path.Combine(__SOURCE_DIRECTORY__, "Output")
IO.Directory.CreateDirectory outputDirectory
IO.Directory.SetCurrentDirectory outputDirectory

let context = if MXLib.getGpuCount() > 0 then GPU 0 else CPU 0

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
    use bmp = new Bitmap(image)
    let dat =
        [|
            for y = 0 to image.Height - 1 do
                for x = 0 to image.Width - 1 do
                    let p = bmp.GetPixel(x,y)
                    yield float32 p.R - 123.68f
                    yield float32 p.G - 116.779f
                    yield float32 p.B - 103.939f
        |]
    let im = context.CopyFrom(dat, [image.Height; image.Width; 3])
    let resized = MX.ImageResize(im, [224;224])
    resized.SwapAxis(0,2).SwapAxis(1,2).Reshape([1;3;224;224])


let vggParams = NDArray.Load vggParamsFile    

let data = Variable("data")
let conv1_1 = Convolution(data = data, numFilter = 64, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv1_1")
let relu1_1 = Relu(conv1_1, Name = "relu1_2")
let conv1_2 = Convolution(data = relu1_1, numFilter = 64, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv1_2")
let relu1_2 = Relu(conv1_2, Name = "relu1_2")
let pool1 = Pooling(relu1_2, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = PoolType.Avg, Name = "pool1")

let conv2_1 = Convolution(data = pool1, numFilter = 128, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv2_1")
let relu2_1 = Relu(conv2_1, Name = "relu2_1")
let conv2_2 = Convolution(data = relu2_1, numFilter = 128, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv2_2")
let relu2_2 = Relu(conv2_2, Name = "relu2_2")
let pool2 = Pooling(relu2_2, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = PoolType.Avg, Name = "pool2")

let conv3_1 = Convolution(data = pool2, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_1")
let relu3_1 = Relu(conv3_1, Name = "relu3_1")
let conv3_2 = Convolution(data = relu3_1, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_2")
let relu3_2 = Relu(conv3_2, Name = "relu3_2")
let conv3_3 = Convolution(data = relu3_2, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_3")
let relu3_3 = Relu(conv3_3, Name = "relu3_3")
let conv3_4 = Convolution(data = relu3_3, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_4")
let relu3_4 = Relu(conv3_4, Name = "relu3_4")
let pool3 = Pooling(relu3_4, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = PoolType.Avg, Name = "pool3")

let conv4_1 = Convolution(data = pool3, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_1")
let relu4_1 = Relu(conv4_1, Name = "relu4_1")
let conv4_2 = Convolution(data = relu4_1, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_2")
let relu4_2 = Relu(conv4_2, Name = "relu4_2")
let conv4_3 = Convolution(data = relu4_2, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_3")
let relu4_3 = Relu(conv4_3, Name = "relu4_3")
let conv4_4 = Convolution(data = relu4_3, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_4")
let relu4_4 = Relu(conv4_4, Name = "relu4_4")
let pool4 = Pooling(relu4_4, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = PoolType.Avg, Name = "pool4")

let conv5_1 = Convolution(data = pool4, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv5_1")
let relu5_1 = Relu(conv5_1, Name = "relu5_1")

let style = SymbolGroup([|relu1_1 :> Symbol; upcast relu2_1; upcast relu3_1; upcast relu4_1; upcast relu5_1|])
let content = SymbolGroup([| relu4_2 :> Symbol |])

let makeExecutor style content (inputSize : int seq) = 
    let inputSize = inputSize |> Seq.toArray
    let out = SymbolGroup([| style; content |])
    //let inputSize = [|224;224|]
    let dataShape = [1;3;inputSize.[0];inputSize.[1]]
    let (k,i,d) = MXSymbol.keyShapeToCsrForm uint32 [| "data", Array.ofSeq dataShape |]
    let inferResult = MXSymbol.inferShape out.UnsafeHandle k i d
    let args = 
        out.ArgumentNames
        |> Array.mapi 
            (fun i n -> 
                let s = inferResult.InputShapes.[i] |> Array.map int
                let a = MX.ZerosNDArray(context, shape = s)
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
                    OpReqType.WriteTo, MX.ZerosLike(args.[n])
                else
                    OpReqType.NullOp, new NDArray()
            )
        |> Array.unzip
    {| 
        ArgGrad = Array.zip out.ArgumentNames argGrad |> dict
        Args = args
        Executor = new Executor(out, context, inArgs, argGrad, gradReqType, Array.empty)
    |}

let loss (gram: SymbolGroup) content = 
    let gramLoss = 
        MXSymbol.listOutputs gram.UnsafeHandle
        |> Array.mapi 
            (fun i oname ->
                let gvar = Variable(sprintf "target_gram_%d" i)
                Sum(Square(gvar - gram.[i]))
            )
    let cvar = Variable("target_content")
    let contentLoss = Sum(Square(cvar - content))
    SymbolGroup(gramLoss |> Array.map (fun x -> x :> Symbol)), contentLoss


let tvWeight = Some 1e-2    
let stopThreshold = 0.0005
let maxSize = 600
let contentWeight = 10.0
let styleWeight = 1.0
let learningRate = 0.001

let lrScheduleDelay = 50
let lrScheduleFactor = 0.6
let saveEpochs = 50
let maxNumEpochs = 1000

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
            let x = Reshape(style.[i], shape = target) //TODO: deprecate targetShape
            let gram = FullyConnected(data = x, weight = x, noBias = true, numHidden = shape.[1])
            gram, (shape.[1..] |> Array.reduce (*)) * shape.[1]
        )
    |> Array.unzip

let gram = SymbolGroup(gramList |> Array.map (fun x -> x :> Symbol))

let styleArray, contentArray = 
    let modelExe = makeExecutor gram content [|224; 224|]
    styleIn.CopyTo(modelExe.Args.["data"])
    modelExe.Executor.Forward(false)
    let outputs = modelExe.Executor.Outputs
    let styleArray = 
        outputs.[0 .. outputs.Length - 2] 
        |> Array.map 
            (fun x ->
                x.CopyTo(context)
            )
    contentIn.CopyTo(modelExe.Args.["data"])
    modelExe.Executor.Forward(false)
    let contentArray = outputs.[outputs.Length - 1].CopyTo(context)
    modelExe.Executor.Dispose()
    styleArray, contentArray


let styleLoss, contentLoss = loss gram content

let executor = makeExecutor styleLoss contentLoss [|224; 224|]

let gradArray = 
    [|
        yield! 
            styleArray
            |> Array.mapi 
                (fun i a ->
                    a.CopyTo(executor.Args.[sprintf "target_gram_%d" i])
                    let w = MX.OnesNDArray(context, shape = [1])
                    MX.MulScalar([|w|], w, styleWeight / double gradScale.[i])
                    w
                )
        let w = 
            let w = MX.OnesNDArray(context, shape = [1])
            MX.MulScalar([|w|], w, contentWeight) |> ignore
            w
        w
    |]


contentArray.CopyTo(executor.Args.["target_content"])

let makeTvGradExecutor (img : NDArray) tvWeight = 
    match tvWeight with 
    | Some w ->
        let nchannel = img.Shape.[1]
        let simg = Variable("img")
        let skernel = Variable("kernel")
        let channels = SliceChannel(simg, nchannel)
        let convs : Symbol [] = 
            channels.Outputs
            |> Array.map
                (fun c ->
                    Convolution(data = c, weight = skernel, numFilter = 1, kernel = [3;3], pad = [1;1], noBias = true, stride = [1;1]) :> Symbol
                )
        let out = Concat(convs)
        let kernel = [ 0; -1;  0;
                      -1;  4; -1;
                       0; -1;  0]
                     |> List.map (fun x -> float32 x / 8.f)
                     |> (fun x -> context.CopyFrom(x, [1;1;3;3]))
        let out = MulScalar(out, w)
        let inArgs,argGrad,grapReqType = 
            out.ArgumentNames
            |> Array.map 
                (function 
                 | "img" -> img, new NDArray(), OpReqType.NullOp
                 | "kernel" -> kernel, new NDArray(), OpReqType.NullOp
                 | v -> failwithf "Unhandled arg %s" v)
            |> Array.unzip3
        Some(new Executor(out, context, inArgs, argGrad, grapReqType, Array.empty))
    | None -> None

// Train


let img = MX.RandomUniformNDArray(context, -0.1, 0.1, contentIn.Shape)
let mutable oldImg = img.CopyTo(context)
let clipNorm = 1.f * (img.Shape |> Array.reduce (*) |> float32)
let tvGradExe = makeTvGradExecutor img tvWeight


let momentum = MX.ZerosLike(img)
let mutable lr = learningRate
let opt w g = MX.NagMomUpdate([w],w,g,momentum, lr, momentum = 0.95, wd = 0.0001 )

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
    let img = MX.Reshape(img, shape = [3; 224; 224])
    let img = MX.SwapAxis(img,1,2)
    let img = MX.SwapAxis(img,0,2)
    let h = 224
    let w = 224
    //let img = Operators.ImageResize(img, contentImage.Height, contentImage.Width).[0]
    let a : float32 [] = img.ToArray<_>()
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
    use resizedImage = resizeImage bmp contentImage.Width contentImage.Height
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
        executor.Executor.Forward(true)
        executor.Executor.Backward(gradArray)
        let gnorm : float32 = MX.Norm(executor.ArgGrad.["data"]).ToArray<_>().[0]
        if gnorm > clipNorm then 
            MX.MulScalar([executor.ArgGrad.["data"]], executor.ArgGrad.["data"], double(clipNorm / gnorm))

        let optResult = 
            match tvGradExe with 
            | Some e -> 
                e.Forward(true)
                let outs = e.Outputs
                //opti
                let g = MX.ElemwiseAdd(executor.ArgGrad.["data"], outs.[0])
                opt img g
            | None -> 
                opt img executor.ArgGrad.["data"]

        //let newImg = optResult.[0]
        let diff = MX.ElemwiseSub(oldImg, img)
        let eps : float32 = MX.ElemwiseDiv(MX.Norm(diff), MX.Norm(img)).ToArray<_>().[0]
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




