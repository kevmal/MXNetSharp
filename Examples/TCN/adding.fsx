(********************************************************************************************************
 * F# implementation of "The Adding Problem" from
 * "An Empirical Evaluation of Generic Convolutional and Recurrent Networks for Sequence Modeling"
 * (Shaojie Bai and J. Zico Kolter and Vladlen Koltun)
 * arXiv:1803.01271
 * https://arxiv.org/abs/1803.01271
 * PyTorch implementation: https://github.com/locuslab/TCN
 ********************************************************************************************************)

#load "../load.fsx"
open Load

open System
open System.Collections.Generic
open System.Diagnostics

open MXNetSharp
open MXNetSharp.SymbolOperators
open MXNetSharp.Interop
open MXNetSharp.PrimitiveOperators


// ******************************************** Parameters **********************************************
let seed = 393939
let batchSize = 32
let seqLength = 400
let epochs = 10
let lr = 4e-3
let ksize = 7
let levels = 8
let nhidden = 30
let inputChannels = 2
let nclasses = 1
    
let context = if MXLib.getGpuCount() > 0 then GPU 0 else CPU 0

MXLib.randomSeed seed
let rng = Random(seed)
let withName name (symbol : #Symbol) = symbol.Name <- name; symbol

// ******************************************** Model **********************************************
let weightNormalization name =
    let g = Variable(name + "_g")
    let v = Variable(name + "_v")
    let n = Norm(v, 2, [1;2], keepdims = true)
    let f = BroadcastLike(g / n, v)
    let w = f * v |> withName (name + "_w")
    w :> Symbol    

let temporalBlock name inCount outputCount kernelSize stride dilation padding dropout (x : Symbol) = 
    let conv1 = Convolution(x, 
                            weightNormalization (name + "_conv1_weight"),
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
                            weightNormalization (name + "_conv2_weight"),
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

let x = Input("xIn", shape = [0; inputChannels; seqLength])
let tcn = make inputChannels nclasses (Array.create levels nhidden) ksize 0.0 x
let label = Input("y", shape = [0; 1])

let z = 
    let tcn = SwapAxis(tcn,0,2)
    let tcn = SwapAxis(tcn,1,2)
    SequenceLast(tcn)

let loss = (label - z) .>> Square() .>> Mean() .>> MakeLoss()

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
    |> Bindings.init 
        (fun a shape ->  
            let fanin = 
                if shape.Length = 3 then    
                    double a.Shape.Value.[1]*double a.Shape.Value.[2]
                elif shape.Length = 1 then 
                    double a.Shape.Value.[0]
                else
                    double a.Shape.Value.[1]
            let alpha = sqrt 5.0
            let gain = sqrt(2.0 / (1.0 + alpha*alpha))
            let stdv = sqrt(3.0) * (gain) / sqrt(fanin)
            MX.RandomUniformNDArray(context, -stdv, stdv, a.Shape.Value)
        )

// ******************************************** Data gen **********************************************
let genSample count = 
    let x = Array.CreateInstance(typeof<float32>, count, 2, seqLength)
    let ra = ResizeArray()
    for sample = 0 to count - 1 do 
        for i = 0 to seqLength - 1 do 
            x.SetValue(rng.NextDouble() |> float32, sample, 0, i)
        let i1 = rng.Next(seqLength)
        let rec loop() = 
            let i2 = rng.Next(seqLength)
            if i2 = i1 then loop() else i2
        let i2 = loop()
        let mutable sum = 0.f
        for i = 0 to seqLength - 1 do
            let v = 
                if i = i1 || i = i2 then 
                    sum <- (x.GetValue(sample, 0, i) :?> _) + sum
                    1.f
                else 
                    0.f
            x.SetValue(v, sample, 1, i)
        ra.Add sum
    CPU(0).CopyFrom(x), CPU(0).CopyFrom(ra.ToArray(), [count; 1])


printfn "Generate train set"
let trainSet = genSample 50000
printfn "Generate test set"
let testSet = genSample 1000


// ******************************************** Training **********************************************

let exe = loss.Bind(context, bindings)
let opt = AdamOptimizer(exe,lr)

let testBatch (ix : int []) = 
    let allX,allY = testSet
    let ix2 = NDArray.CopyFrom(ix, allX.Context)
    let xs = MX.Take(allX, ix2, axis = 0)
    let ys = MX.Take(allY, ix2, axis = 0)
    xs.CopyTo(bindings.NDArray(x))
    ys.CopyTo(bindings.NDArray(label))
    exe.Forward(false)
    exe.Outputs.[0].ToFloat32Scalar()


module Array = 
    let mutateShuffle (rand : Random) (x:'a[]) =
        let random upper = rand.Next(upper)
        let n = x.Length
        for i = (n - 1) downto 1 do
            let j = random (i+1)
            let temp = x.[j]
            x.[j] <- x.[i]
            x.[i] <- temp
        x
    let shuffle (rand : Random) (x:'a[]) = mutateShuffle rand (Array.copy x)


let testEpoch () = 
    let length = (fst testSet).Shape.[0]
    let shuffled = Array.init length id |> Array.shuffle rng |> Array.chunkBySize batchSize |> Array.filter (fun x -> x.Length = batchSize)
    shuffled
    |> Array.map testBatch 
    |> Array.average

let trainBatch (ix : int []) = 
    let allX,allY = trainSet
    let ix2 = NDArray.CopyFrom(ix, allX.Context)
    let xs = MX.Take(allX, ix2, axis = 0)
    let ys = MX.Take(allY, ix2, axis = 0)
    xs.CopyTo(bindings.NDArray(x))
    ys.CopyTo(bindings.NDArray(label))
    exe.Forward(true)
    exe.Backward()
    opt.Update()
    exe.Outputs.[0].ToFloat32Scalar()

let trainEpoch () = 
    let length = (fst trainSet).Shape.[0]
    let shuffled = Array.init length id |> Array.shuffle rng |> Array.chunkBySize batchSize |> Array.filter (fun x -> x.Length = batchSize)
    shuffled
    |> Array.map trainBatch 
    |> Array.average


printfn "Training for %d epochs..." epochs
let sw = Stopwatch()
sw.Start()
for i = 1 to epochs do 
    let s = sw.Elapsed
    let l = trainEpoch()
    let l2 = testEpoch()
    let e = sw.Elapsed
    printfn "%-10d train:%-15f test:%-15f elapsed:%-15O" i l l2 (e - s)


