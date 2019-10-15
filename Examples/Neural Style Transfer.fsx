open System.Drawing

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



let context = 
    {
        DeviceType = DeviceType.CPU
        DeviceId = 1
    }

let vggParamsUrl = "https://github.com/dmlc/web-data/raw/master/mxnet/neural-style/model/vgg19.params"
let vggParamsFile = "vgg19.params"

let styleUrl = "https://github.com/dmlc/web-data/raw/master/mxnet/neural-style/input/starry_night.jpg"
let styleFile = "starry_night.jpg"
let contentUrl = "https://github.com/dmlc/web-data/raw/master/mxnet/neural-style/input/IMG_4343.jpg"
let contentFile = "IMG_4343.jpg"

let ensure (url : string) file = 
    if not(File.Exists file) then 
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
    let resized = Operators.ImageResize(im,224,224).[0]
    let s1 = Operators.SwapAxis(resized,0,2).[0]
    let s3 = Operators.SwapAxis(s1,1,2).[0]
    s3

let vggParams = NDArray.Load vggParamsFile    


let data = new Variable("data")
let conv1_1 = Operators.Convolution(data, Symbol.Empty, Symbol.Empty, numFilter = 64, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv1_1")
let relu1_1 = Operators.Activation(conv1_1, actType = Relu, Name = "relu1_1")
let conv1_2 = Operators.Convolution(relu1_1, Symbol.Empty, Symbol.Empty, numFilter = 64, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv1_2")
let relu1_2 = Operators.Activation(conv1_2, actType = Relu, Name = "relu1_2")
let pool1 = Operators.Pooling(relu1_2, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = Avg, Name = "pool1")

let conv2_1 = Operators.Convolution(pool1, Symbol.Empty, Symbol.Empty, numFilter = 128, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv2_1")
let relu2_1 = Operators.Activation(conv2_1, actType = Relu, Name = "relu2_1")
let conv2_2 = Operators.Convolution(relu2_1, Symbol.Empty, Symbol.Empty, numFilter = 128, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv2_2")
let relu2_2 = Operators.Activation(conv2_2, actType = Relu, Name = "relu2_2")
let pool2 = Operators.Pooling(relu2_2, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = Avg, Name = "pool2")

let conv3_1 = Operators.Convolution(pool2, Symbol.Empty, Symbol.Empty, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_1")
let relu3_1 = Operators.Activation(conv3_1, actType = Relu, Name = "relu3_1")
let conv3_2 = Operators.Convolution(relu3_1, Symbol.Empty, Symbol.Empty, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_2")
let relu3_2 = Operators.Activation(conv3_2, actType = Relu, Name = "relu3_2")
let conv3_3 = Operators.Convolution(relu3_2, Symbol.Empty, Symbol.Empty, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_3")
let relu3_3 = Operators.Activation(conv3_3, actType = Relu, Name = "relu3_3")
let conv3_4 = Operators.Convolution(relu3_3, Symbol.Empty, Symbol.Empty, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_4")
let relu3_4 = Operators.Activation(conv3_4, actType = Relu, Name = "relu3_4")
let pool3 = Operators.Pooling(relu3_4, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = Avg, Name = "pool3")

let conv4_1 = Operators.Convolution(pool3, Symbol.Empty, Symbol.Empty, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_1")
let relu4_1 = Operators.Activation(conv4_1, actType = Relu, Name = "relu4_1")
let conv4_2 = Operators.Convolution(relu4_1, Symbol.Empty, Symbol.Empty, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_2")
let relu4_2 = Operators.Activation(conv4_2, actType = Relu, Name = "relu4_2")
let conv4_3 = Operators.Convolution(relu4_2, Symbol.Empty, Symbol.Empty, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_3")
let relu4_3 = Operators.Activation(conv4_3, actType = Relu, Name = "relu4_3")
let conv4_4 = Operators.Convolution(relu4_3, Symbol.Empty, Symbol.Empty, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_4")
let relu4_4 = Operators.Activation(conv4_4, actType = Relu, Name = "relu4_4")
let pool4 = Operators.Pooling(relu4_4, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = Avg, Name = "pool4")

let conv5_1 = Operators.Convolution(pool4, Symbol.Empty, Symbol.Empty, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv5_1")
let relu5_1 = Operators.Activation(conv5_1, actType = Relu, Name = "relu5_1")

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
                let a = Operators.ZerosNDArray(shape = s, ctx = "cpu(1)") |> Array.head
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
        //TODO: We have to keep references alive here to prevent crashing. Need to create a test and then make sure Executor keeps handles alive
        Out = out
        ArgGrad = Array.zip out.ArgumentNames argGrad |> dict
        Args = args
        Executor = Executor(out, context, inArgs, argGrad, gradReqType, Array.empty)
    |}

let loss (gram: SymbolGroup<'a>) content = 
    let gramLoss = 
        MXSymbol.listOutputs gram.UnsafeHandle
        |> Array.mapi 
            (fun i oname ->
                let gvar = new Variable(sprintf "target_gram_%d" i)
                Operators.Sum(Operators.Square(Operators.ElemwiseSub(gvar, gram.Symbols.[i])))
            )
    let cvar = new Variable("target_content")
    let contentLoss = Operators.Sum(Operators.Square(Operators.ElemwiseSub(cvar, content)))
    new SymbolGroup<unit>((), gramLoss |> Array.map (fun x -> upcast x)), contentLoss


let tvWeight = Some 1e-2    
let stopThreshold = 0.005
let maxSize = 600
let contentWeight = 10.0
let styleWeight = 1.0
let learningRate = 0.001

let lrScheduleDelay = 50
let trScheduleFactor = 0.6
let saveEpochs = 50
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
            let x = Operators.Reshape(style.Symbols.[i],  shape = target) //TODO: deprecate targetShape
            let gram = Operators.FullyConnected(x,x,Symbol.Empty,noBias = true, numHidden = shape.[1])
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
                    let w = Operators.OnesNDArray(shape = [1], ctx = "cpu(1)")
                    Operators.MulScalar(w, w.[0], styleWeight)
                    w.[0]
                )
        let w = 
            let w = Operators.OnesNDArray(shape = [1], ctx = "cpu(1)")
            Operators.MulScalar(w, w.[0], contentWeight) |> ignore
            w.[0]
        w
    |]


contentArray.CopyTo(executor.Args.["target_content"])

let makeTvGradExecutor (img : NDArray) tvWeight = 
    match tvWeight with 
    | Some w ->
        let nchannel = img.Shape.[0]
        let simg = new Variable("img")
        let skernel = new Variable("kernel")
        let channels = Operators.SliceChannel(simg, numOutputs = nchannel)
        let convs : BaseSymbol [] = 
            channels.Outputs
            |> Array.map
                (fun c ->
                    upcast Operators.Convolution(c, skernel, Symbol.Empty, numFilter = 1, kernel = [3;3], pad = [1;1], noBias = true, stride = [1;1])
                )
        let out = Operators.Concat(data = convs)
        let kernel = [ 0; -1;  0;
                      -1;  4; -1;
                       0; -1;  0]
                     |> List.map (fun x -> float x / 8.0)
                     |> (fun x -> new NDArray(x, [1;1;3;3], context))
        let out = Operators.MulScalar(out :> BaseSymbol, w)
        let inArgs,argGrad,grapReqType = 
            out.ArgumentNames
            |> Array.map 
                (function 
                 | "img" -> img, NDArray(), OpReqType.NullOp
                 | "kernel" -> kernel, NDArray(), OpReqType.NullOp
                 | v -> failwithf "Unhandled arg %s" v)
            |> Array.unzip3
        Executor(out, context, inArgs, argGrad, grapReqType, Array.empty)
        |> Some
    | None -> None

// Train


let img = Operators.RandomUniformNDArray(-0.1, 0.1, contentIn.Shape, ctx = "cpu(1)").[0]
let oldImg = img.CopyTo(context)
let clipNorm = 1.f * (img.Shape |> Array.reduce (*) |> float32)
let tvGradExe = makeTvGradExecutor img tvWeight


let momentum = Operators.ZerosLike(img).[0]
let opt w g = Operators.NagMomUpdate(w,g,momentum, learningRate, momentum = 0.95, wd = 0.0001 )

//for i = 1 to  maxNumEpochs do 
img.CopyTo(executor.Args.["data"])
let outs = executor.Executor.Forward(true)
executor.Executor.Backward(gradArray)
let gnorm : float32 = Operators.Norm(executor.ArgGrad.["data"]).[0].ToArray().[0]
if gnorm > clipNorm then 
    Operators.MulScalar([executor.ArgGrad.["data"]], executor.ArgGrad.["data"], double(clipNorm / gnorm))

match tvGradExe with 
| Some e -> 
    let outs = e.Forward(true)
    //opti
    let g = Operators.ElemwiseAdd(executor.ArgGrad.["data"], outs.[0]).[0]
    opt img g
| None -> 
    opt img executor.ArgGrad.["data"]

let diff = Operators.ElemwiseSub(oldImg, img).[0]
let eps : float32 = Operators.ElemwiseDiv(Operators.Norm(diff).[0], Operators.Norm(img).[0]).[0].ToArray().[0]

Operators.Norm()

NNVM.getOpHandle "concatenate"
|> NNVM.getOpInfo
let s = AtomicSymbolCreator.FromName "Concat"

let sym = MXSymbol.createAtomicSymbol s.AtomicSymbolCreatorHandle [|"num_args"|] [|"2"|]

let s1 = Variable("s1")
let s2 = Variable("s2")

MXSymbol.compose sym "concat" null [|s1.UnsafeHandle;s2.UnsafeHandle|]
MXSymbol.compose sym "concat" [|"concat_arg0"; "concat_arg1"|] [|s1.UnsafeHandle;s2.UnsafeHandle|]
MXSymbol.compose sym "concat" null null

MXSymbol.getInputSymbols sym 
MXSymbol.saveToJSON sym

MXSymbol.listArguments sym

MXSymbol.listArguments sym
MXSymbol.saveToJSON sym

CApi.MXSymbolCompose(sym, "concat", 2u, [|"[0]oncat_arg0"; "[0]arg1"|], [|s1.UnsafeHandle;s2.UnsafeHandle|]) |> throwOnError "MXSymbolCompose"


/// <summary>Compose the symbol on other symbols.
///
/// This function will change the sym hanlde.
/// To achieve function apply behavior, copy the symbol first
/// before apply.</summary>
/// <param name="sym">the symbol to apply</param>
/// <param name="name">the name of symbol</param>
/// <param name="num_args">number of arguments</param>
/// <param name="keys">the key of keyword args (optional)</param>
/// <param name="args">arguments to sym</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<System.Runtime.InteropServices.DllImport("libmxnet", CallingConvention = System.Runtime.InteropServices.CallingConvention.Cdecl, CharSet = System.Runtime.InteropServices.CharSet.Ansi)>]
extern int MXSymbolCompose(IntPtr sym, string name, uint32 num_args, IntPtr[] keys, IntPtr[] args)

let keys = [|s.Info.KeyVarNumArgs; s.Info.KeyVarNumArgs|]
MXSymbolCompose(sym, "concat", ulength keys, keys, args) |> throwOnError "MXSymbolCompose"





    











    


