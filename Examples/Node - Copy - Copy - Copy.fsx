// ********************** Incomplete "Example" *****************************

// Adapted from https://github.com/locuslab/TCN

open System.Collections.Generic
open System.Runtime.InteropServices

#load "load.fsx"
open MXNetSharp
open MXNetSharp.Operator
open MXNetSharp.SymbolOperators
open MXNetSharp.IO
open System
open System.Net
open System.IO
open System.IO.Compression

open MXNetSharp.PrimitiveOperators

let printShapes (b : Bindings) = 
    b.Bindings.Values
    |> Seq.iter 
        (fun b ->
            printfn "%-20s %A" b.Name b.Shape
        )
    b

let thresholdSupport (s : int []) (x : Symbol) dim = 
    let sorted = MX.Topk(x, k = 0, axis = dim, isAscend = false,retTyp = RetTyp.Value) //TODO fix Sort on NDArray
    let rho = MX.ContribArangeLike(x, start = 1.0, axis = dim) .>> Reshape(shape = [-1;1;1])
    let mean = MX.NpCumsum(sorted, axis = dim) ./ rho
    let meanSq = MX.NpCumsum(MX.Square(sorted), axis = dim) ./ rho
    let ss = rho.*(meanSq - MX.Square(mean))
    let delta = (1.0 - ss) ./ rho
    let deltaNz = MX.Relu(delta)
    let tau = mean - sqrt deltaNz
    let supportSize = MX.Sum(tau .<= sorted, keepdims=true, axis = [dim])
    let xs = 
        [|
            for a = 0 to s.Length - 1 do 
                if a = dim then 
                    supportSize - 1.0 :> Symbol
                else
                    let x = MX.Arange(start = 0.0, stop = double s.[a]) .>> Reshape(shape = (s |> Array.mapi (fun i x -> if i = a then x else 1)))
                    MX.Tile(x, s |> Array.mapi (fun i x -> if i <> a then x else 1) ) :> Symbol
        |]
    let ix = MX.Stack(xs,0)
    let tauStar = MX.GatherNd(tau, ix)
    tauStar, supportSize

let entmax15 (s : int []) (x : Symbol) dim = 
    let x2 = (x .- MX.Max(x, axis = [dim], keepdims=true)) / 2.0
    let tauStar,_ = thresholdSupport s x2 dim
    MX.MaximumScalar(x2 .- tauStar, 0.0) ** 2.0

let entmoid15 (x : Symbol) =
    let isPos = x .>= 0.0
    let input = Abs(x)
    let poo = (input ** 2.0)
    let tau = 
        let tau = (input + sqrt(MX.Relu(8.0 - poo))) / 2.0
        let c = tau .<= input
        tau * (1.0 - c) + 2.0*c //TODO: Beter way to do mask assign
    let yNeg = 0.25*((MX.Relu(tau - input)**2.0) :> Symbol)
    MX.Where(isPos, 1.0 - yNeg, yNeg)

let ctx = CPU 0
let odst inFeatures numTrees treeDim depth flatten choicef binf (x : Symbol) = 
    //let response = Parameter("response", shape = [numTrees; treeDim; pown 2 depth]) //init normal 0.0 1.0
    let response = Parameter("response", ndarray = ctx.RandomNormal([numTrees; treeDim; pown 2 depth])) //init normal 0.0 1.0
    //let featureSelectionLogits = Parameter("featureSelectionLogits", shape = [inFeatures; numTrees; depth])
    let featureSelectionLogits = Parameter("featureSelectionLogits", ndarray = ctx.RandomUniform([inFeatures; numTrees; depth]))
    //let featureThresholds = Parameter("featureThresholds", shape = [numTrees; depth])
    let featureThresholds = Parameter("featureThresholds", ndarray = ctx.RandomNormal([numTrees; depth]))
    //let logTemperatures = Parameter("logTemperatures", shape = [numTrees; depth])
    let logTemperatures = Parameter("logTemperatures", ndarray = ctx.RandomNormal([numTrees; depth]))
    let binCodesOneHot = 
        let ctx = CPU 0
        let indices = ctx.Arange(start = 0.0, stop = double(pown 2 depth))
        let offsets = 2.0 ** ctx.Arange(start = 0.0, stop = double depth)
        let binCodes = 
            let x = indices.AsType(DataType.Int32).Reshape(1,-1) ./ offsets.AsType(DataType.Int32).Reshape(-1,1)
            (x % 2.0).AsType(DataType.Float32)
        let binCodesOneHot = MX.Stack(binCodes, 1.0 - binCodes, axis = -1)
        Constant(binCodesOneHot, "binCodesOneHot")


    let featureSelectors = choicef [|inFeatures; numTrees; depth|] featureSelectionLogits 0
    let featureValues = Dot(x, featureSelectors)

    let thresholdLogits = 
        let tl = (featureValues .- ExpandDims(featureThresholds, 0)) .* ExpandDims(exp(-logTemperatures), 0)
        Stack([-tl; tl], -1)


    let bins = binf thresholdLogits

    let binMatches = NpiEinsum([bins :> Symbol; binCodesOneHot :> Symbol], "btds,dcs->btdc")
    let responseWeights = Prod(binMatches, [-2])
    let output = NpiEinsum([responseWeights :> Symbol; response :> Symbol], "bnd,ncd->bnc")
    if flatten then Flatten(output) :> Symbol else output :> Symbol

let trainSize = 463715
let testSize = 51630

let allData = 
    File.ReadAllLines("D:\Data\yeaddataset\YearPredictionMSD.txt")
    |> Array.map 
        (fun line -> 
            let fields = line.Split(',')
            let year = fields.[0] |> int
            let features = fields.[1 ..] |> Array.map float32
            year,features
        )
let trainSet, testSet = allData |> Array.splitAt 463715

#I @"E:\profile\fsi\moneyshot"
#r @"MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.dll"

open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
let flength = (snd trainSet.[0]).Length

let norm = Normal()
let transforms = 
    let samples = 
        Array.init flength (fun _ -> norm.Samples() |> Seq.take trainSet.Length |> Seq.toArray)
    Array.Parallel.init flength
        (fun i -> 
            let c = trainSet |> Array.map (fun (_,x) -> double x.[i]) 
            let qnoise = 1e-3
            let std = ArrayStatistics.StandardDeviation(c)
            let nstd = qnoise / (max std qnoise)
            for j = 0 to c.Length - 1 do 
                c.[j] <- c.[j] + nstd*samples.[i].[j]
            let cdf = Statistics.empiricalCDFFunc c
            (fun x -> norm.InverseCumulativeDistribution(cdf x))
        )

let trainSet2 = 
    let a = trainSet |> Array.Parallel.map (fun (year,fs) -> year, Array.copy fs)
    a
    |> Array.Parallel.iter 
        (fun (_year,fs) ->
            for i = 0 to fs.Length - 1 do 
                fs.[i] <- transforms.[i] (double fs.[i]) |> float32
        )
    a
let testSet2 = 
    let a = testSet |> Array.Parallel.map (fun (year,fs) -> year, Array.copy fs)
    a
    |> Array.Parallel.iter 
        (fun (_year,fs) ->
            for i = 0 to fs.Length - 1 do 
                fs.[i] <- transforms.[i] (double fs.[i]) |> float32
        )
    a

let tymu,tystd = ArrayStatistics.MeanStandardDeviation(trainSet2 |> Array.map fst)

let yTrain = trainSet2 |> Array.map fst |> Array.map (fun x -> (double x - tymu) / tystd |> float32)
let yTest = testSet2 |> Array.map fst |> Array.map (fun x -> (double x - tymu) / tystd |> float32)


let inp = Input("x", [0; flength])
let l1 = odst flength 2048 3 6 false entmax15 entmoid15 inp
let otp = l1.[*,*,0] .>> Reshape(shape = [0;0]) .>> Mean(axis = [-1])


let label = Input("label", [0])
let loss = label - otp .>> Square() .>> Mean() .>> MakeLoss()


//otp.Bindings |> Bindings.batchSize 1000 |> Bindings.inferShapes otp |> printShapes |> ignore
//loss.Bindings |> Bindings.batchSize 1000 |> Bindings.inferShapes loss |> printShapes |> ignore


type AdamOptimizer(e : Executor, ?beta1, ?beta2) =
    let beta1 = defaultArg beta1 0.9
    let beta2 = defaultArg beta2 0.999
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
    member x.Update(learningRate) = 
        updateCount <- updateCount + 1
        let t = double updateCount
        let lr = learningRate*sqrt(1.0 - Math.Pow(beta2,t)) / (1.0 - Math.Pow(beta1,t))
        e.Bindings
        |> Seq.iter
            (fun a ->
                match a with 
                | ArgBinding ({Name = name; OpReqType = Some WriteTo; Grad = Some grad; NDArray = Some weight}) -> 
                    let m,v = lu name grad
                    MX.AdamUpdate([weight], weight, grad, m, v, lr, beta1, beta2)
                | _ -> ()
            )


let bm = loss.Bindings |> Bindings.batchSize 512 |> Bindings.inferShapes loss |> Bindings.init (fun _ s -> ctx.Zeros(s))
let exe = loss.Bind(ctx,bm)
bm |> printShapes |> ignore
let opt = AdamOptimizer(exe)

let epoch = MX.Shuffle(ctx.Arange(0.0, double trainSet.Length)).ToIntArray() |> Array.chunkBySize 512 |> Array.filter (fun x -> x.Length = 512)


let bi = epoch.[0]

let y,x = bi |> Array.map (fun i -> trainSet.[i]) |> Array.unzip

exe.[label].CopyFrom(y)
exe.[inp].CopyFrom(x |> Array.concat)

exe.Forward(false)



exe.Backward()

NDArray.WaitAll()








