(********************************************************************************************************
 * F# implementation of "Sequential MNIST" from
 * "An Empirical Evaluation of Generic Convolutional and Recurrent Networks for Sequence Modeling"
 * (Shaojie Bai and J. Zico Kolter and Vladlen Koltun)
 * arXiv:1803.01271
 * https://arxiv.org/abs/1803.01271
 * PyTorch implementation: https://github.com/locuslab/TCN
 ********************************************************************************************************)

#load "../load.fsx"
open Load

open System
open System.Net
open System.IO
open System.IO.Compression
open System.Collections.Generic
open System.Diagnostics

open MXNetSharp
open MXNetSharp.IO
open MXNetSharp.SymbolOperators
open MXNetSharp.Interop
open MXNetSharp.PrimitiveOperators


// ******************************************** Parameters **********************************************
let seed = 393939
let batchSize = 64
let dropout = 0.05
let seqLength = 784
let epochs = 20
let lr = 2e-3
let ksize = 7
let levels = 8
let nhidden = 25
let nclasses = 10
    
let context = if MXLib.getGpuCount() > 0 then GPU 0 else CPU 0
////

MXLib.randomSeed seed
let withName name (symbol : #Symbol) = symbol.Name <- name; symbol

// ******************************************** Model **********************************************
let weightNormalization name =
    let g = Variable(name + "_g_weight")
    let v = Variable(name + "_v_weight")
    let n = Norm(v, 2, [1;2], keepdims = true)
    let f = BroadcastLike(g / n, v)
    let w = f * v |> withName (name + "_w")
    w :> Symbol    

let temporalBlock name inCount outputCount kernelSize stride dilation padding dropout (x : Symbol) = 
    let conv1 = Convolution(x, 
                            weightNormalization (name + "_conv1"),
                            Variable("_conv1_bias"),
                            [kernelSize],  
                            outputCount, 
                            stride = [stride], 
                            dilate = [dilation], 
                            pad = [padding],  
                            noBias = false,
                            Name = name + "_conv1")
    let conv1Sliced = conv1.[*,*,.. -padding] 
    let relu1 = Relu(conv1Sliced, Name = name + "_relu1")
    let dropout1 = Dropout(relu1, dropout, DropoutMode.Training)
    let conv2 = Convolution(dropout1,
                            weightNormalization (name + "_conv2"),
                            Variable(name + "_conv2_bias"),
                            [kernelSize],  
                            outputCount, 
                            stride = [stride], 
                            dilate = [dilation], 
                            pad = [padding],  
                            noBias = false,
                            Name = name + "_conv2")
    let conv2Sliced = conv2.[*,*,.. -padding] 
    let relu2 = Relu(conv2Sliced, Name = name + "_relu2")
    let dropout2 = Dropout(relu2, dropout, DropoutMode.Training)
    let final = dropout2 :> Symbol
    let res = 
        if inCount <> outputCount then 
            Convolution(data = x, numFilter = outputCount, kernel = [1], Name = name + "_res_downsample") :> Symbol
        else 
            x        
    let relu = Relu(final + res, Name = name + "_relu")
    relu :> Symbol


let make numInputs outCount numChannels kernelSize dropout x = 
    (((0,numInputs),x :> Symbol),numChannels)
    ||> Seq.fold
        (fun ((i, lastN), last : Symbol) outCount ->
            let d = pown 2 i
            let padding = (kernelSize - 1) * d
            let y = temporalBlock (sprintf "L%d" i) lastN outCount kernelSize 1 d padding dropout last
            (i + 1, outCount), y
        )
    |> snd
    .>> SwapAxis(dim1 = 1, dim2 = 2)
    .>> FullyConnected(numHidden = outCount, Name = "final_fc", flatten = false)
    .>> SwapAxis(dim1 = 1, dim2 = 2)

let x = Input("xIn", shape = [0; 1; seqLength])
let tcn = make 1 nclasses (Array.create levels nhidden) ksize dropout x
let label = Input("y", shape = [0; 1])

let z = 
    let tcn = SwapAxis(tcn,0,2)
    let tcn = SwapAxis(tcn,1,2)
    SequenceLast(tcn)

let correct = Argmax(z, axis = 1) .= Reshape(label, [-1]) .>> Sum()
let loss = -Mean(Pick(LogSoftmax(z, axis=1), Reshape(label, [-1]))) .>> MakeLoss()
let outp = SymbolGroup(loss, correct)


// ******************************************** Optimizer **********************************************
type AdamOptimizer(e : Executor, lr, ?beta1, ?beta2) = 
    let beta1 = defaultArg beta1 0.9
    let beta2 = defaultArg beta2 0.999
    let epsilon = 1e-8
    let mutable count = 1
    let d = Dictionary<string, NDArray*NDArray>()
    let lu (s : String) (a : NDArray) =
        let scc,v = d.TryGetValue(s)
        if scc then 
            v
        else
            let v = MX.ZerosLike(a),MX.ZerosLike(a)
            d.[s] <- v
            v
    member x.Update() =
        let t = count 
        let coef1 = 1.0 - Math.Pow(beta1, double t)
        let coef2 = 1.0 - Math.Pow(beta2, double t)
        let lr = lr*(sqrt(coef2)/coef1)
        count <- count + 1
        e.Bindings
        |> Seq.iter
            (fun a ->
                match a with 
                | {Name = name; BindType = ArgBind(Some WriteTo, Some grad); NDArray = Some weight} -> 
                    let m,v = lu name grad
                    MX.AdamUpdate([weight], weight, grad, m, v, lr, beta1, beta2, epsilon)
                | _ -> ()
            )


// ******************************************** Initilization **********************************************
let bindings = 
    loss.Bindings
    |> Bindings.batchSize batchSize 
    |> Bindings.inferShapes loss
    |> Bindings.setContext context
    |> Bindings.initWithFunc 
        (fun binding ndarray ->  
            match binding with 
            | Factor FactorType.In fanin ->
                let alpha = sqrt 5.0
                let gain = sqrt(2.0 / (1.0 + alpha*alpha))
                let stdv = sqrt(3.0) * (gain) / sqrt(fanin)
                MX.RandomUniform([ndarray], ndarray.Context, -stdv, stdv)
                true 
            | _ -> false
        )

// ******************************************** Datasets **********************************************

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



// ******************************************** Training **********************************************

let exe = outp.Bind(context, bindings)
let opt = AdamOptimizer(exe,lr)

let testEpoch () = 
    let losses, correct = 
        [|
            valIter.Reset()
            while (valIter.Next()) do 
                let inp = valIter.GetData().Reshape(batchSize,1,seqLength)
                let inp = (inp - 0.1307) / 0.3081
                inp.Reshape(batchSize,1,seqLength).CopyTo(bindings.NDArray(x))
                valIter.GetLabel().Reshape(batchSize,1).CopyTo(bindings.NDArray(label))
                exe.Forward(false)
                exe.Outputs.[0].ToFloat32Scalar(), exe.Outputs.[1].ToDoubleScalar()
        |] |> Array.unzip
    let avgLoss = losses |> Array.average
    let acc = (Array.sum correct) / (double losses.Length * double batchSize)
    avgLoss, acc


let trainEpoch () = 
    seq {
        trainIter.Reset()
        while (trainIter.Next()) do 
            let inp = trainIter.GetData().Reshape(batchSize,1,seqLength)
            let inp = (inp - 0.1307) / 0.3081
            inp.CopyTo(bindings.NDArray(x))
            trainIter.GetLabel().Reshape(batchSize,1).CopyTo(bindings.NDArray(label))
            exe.Forward(true)
            exe.Backward()
            opt.Update()
            exe.Outputs.[0].ToFloat32Scalar()
    } |> Seq.average



let sw = Stopwatch()
sw.Start()
for i = 1 to epochs do 
    let s = sw.Elapsed
    let l = trainEpoch()
    let l2,acc = testEpoch()
    let e = sw.Elapsed
    printfn "%-10d train:%-15f test loss:%-15f test acc:%-15f elapsed:%-15O" i l l2 acc (e - s)
