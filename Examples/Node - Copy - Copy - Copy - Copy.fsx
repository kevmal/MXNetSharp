// ********************** Incomplete "Example" *****************************

// Adapted from https://github.com/locuslab/TCN

open System.Collections.Generic
open System.Runtime.InteropServices

#I @"../MXNetSharp/bin/Debug/netstandard2.0/"
#I @"bin/Debug/netstandard2.0/"
#r @"MXNetSharp.dll"
#r @"MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.dll"

open MXNetSharp
open MXNetSharp.Operator
open MXNetSharp.SymbolOperators
open MXNetSharp.IO
open System
open System.Net
open System.IO
open System.IO.Compression

open MXNetSharp.PrimitiveOperators


open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics



let printShapes (b : Bindings) = 
    b.Bindings.Values
    |> Seq.iter 
        (fun b ->
            printfn "%-20s %A" b.Name b.Shape
        )
    b

let thresholdSupport (ctx : Context) (x : NDArray) dim = 
    let s = x.Shape
    let sorted = MX.Sort(x, axis = dim, isAscend = false)
    let rho = MX.ContribArangeLike(x, ctx = ctx, start = 1.0, axis = dim).Reshape(-1, 1, 1)
    let mean = MX.NpCumsum(sorted, axis = dim) ./ rho
    let meanSq = MX.NpCumsum(MX.Square(sorted), axis = dim) ./ rho
    let ss = rho.*(meanSq - MX.Square(mean))
    let delta = (1.0 - ss) ./ rho
    let deltaNz = MX.Relu(delta)
    let tau = mean - sqrt deltaNz
    let supportSize = MX.Sum(tau .<= sorted, keepdims=true, axis = [dim])
    let xs = 
        let s = supportSize.Shape
        [|
            for a = 0 to s.Length - 1 do 
                if a = dim then 
                    supportSize - 1.0 
                else
                    let x = ctx.Arange(start = 0.0, stop = double s.[a]).Reshape((s |> Array.mapi (fun i x -> if i = a then x else 1)))
                    MX.Tile(x, s |> Array.mapi (fun i x -> if i <> a then x else 1) ) 
        |]
    let ix = MX.Stack(xs,0)
    let tauStar = MX.GatherNd(tau, ix)
    tauStar, supportSize

let entmax15 (ctx : Context) (x : NDArray) dim = 
    let x2 = (x .- MX.Max(x, axis = [dim], keepdims=true)) / 2.0
    let tauStar,_ = thresholdSupport ctx x2 dim
    MX.MaximumScalar(x2 .- tauStar, 0.0) ** 2.0

let entmoid15 (ctx : Context) (x : NDArray) =
    let isPos = x .>= 0.0
    let input = abs x
    let poo = (input ** 2.0)
    let tau = 
        let tau = (input + sqrt(MX.Relu(8.0 - poo))) / 2.0
        let c = tau .<= input
        tau * (1.0 - c) + 2.0*c //TODO: Beter way to do mask assign
    let yNeg = 0.25*((MX.Relu(tau - input)**2.0) : NDArray)
    MX.Where(isPos, 1.0 - yNeg, yNeg)

CustomOp.register "entmax15"
    (fun ins ->
        {new CustomOperation() with
             member this.CreateOperator(ctx: Context, inShapes: int [] [], inDataTypes: TypeFlag []): ICustomOperation = 
                 printfn "%A" ins
                 let dim = 
                     let scc,v = ins.TryGetValue("dim")
                     if scc then 
                        int v
                     else
                        printfn "default dim....."
                        0
                 {new ICustomOperation with
                      member this.Backward(req: OpReqType [], inData: NDArray [], outData: NDArray [], inGrad: NDArray [], outGrad: NDArray [], auxData: NDArray []): unit = 
                          if req.[0] = OpReqType.NullOp then 
                              () 
                          else
                              let gppr = sqrt outData.[0]
                              let dx = inGrad.[0] * gppr
                              let q = MX.Sum(dx, axis = [dim], keepdims = true) / MX.Sum(gppr, axis = [dim], keepdims = true)
                              match req.[0] with 
                              | OpReqType.AddTo -> (inGrad.[0] - (q.*gppr)).CopyTo(outGrad.[0])
                              | OpReqType.WriteTo -> (-(q.*gppr)).CopyTo(outGrad.[0])
                              | q -> failwithf "Entmax15 unexpected OpReqType %A" q
                      member this.Forward(isTrain: bool, req: OpReqType [], inData: NDArray [], outData: NDArray [], auxData: NDArray []): unit = 
                          (entmax15 ctx inData.[0] dim).CopyTo(outData.[0])
                 }
             //member this.DeclareBackwardDependency(outGrad: int [], inData: int [], outData: int []): int [] = 
             //    raise (System.NotImplementedException())
             //member this.InferBackwardStorageType(storageTypes: BackwardStorageTypes): unit = 
             //    raise (System.NotImplementedException())
             member this.InferShape(inShape: int [] []): int [] [] * int [] [] * int [] [] = 
                 let dataShape = inShape.[0]
                 let outputShape = inShape.[0]
                 [|dataShape|], [|outputShape|], Array.empty
             //member this.InferStorageType(inputStorageTypes: StorageType []): StorageType [] * StorageType [] * StorageType [] = 
             //    let outType = this.ListOutputs() |> Array.map (fun x -> StorageType.Default)
             //    let auxType = this.ListAuxiliaryStates() |> Array.map (fun x -> StorageType.Default)
             //    inputStorageTypes, outType, auxType
             //member this.InferType(inType: TypeFlag []): TypeFlag [] * TypeFlag [] * TypeFlag [] = 
             //    let outType = this.ListOutputs() |> Array.map (fun x -> inType.[0])
             //    let auxType = this.ListAuxiliaryStates() |> Array.map (fun x -> inType.[0])
             //    inType, outType, auxType
             //member this.ListArguments(): string [] = [|"data"; "label"|]
             //member this.ListAuxiliaryStates(): string [] = Array.empty
             //member this.ListOutputs(): string [] = [|"output"|]
        } :> _
    )

CustomOp.register "entmoid15"
    (fun ins ->
        {new CustomOperation() with
             member this.CreateOperator(ctx: Context, inShapes: int [] [], inDataTypes: TypeFlag []): ICustomOperation = 
                 {new ICustomOperation with
                      member this.Backward(req: OpReqType [], inData: NDArray [], outData: NDArray [], inGrad: NDArray [], outGrad: NDArray [], auxData: NDArray []): unit = 
                          if req.[0] = OpReqType.NullOp then 
                              () 
                          else
                              let gppr0 = sqrt outData.[0]
                              let gppr1 = sqrt (1.0 - outData.[0])
                              let gi = inGrad.[0]*gppr0
                              let q = gi / (gppr0 + gppr1)
                              match req.[0] with 
                              | OpReqType.AddTo -> (inGrad.[0] - (q*gppr0)).CopyTo(outGrad.[0])
                              | OpReqType.WriteTo -> (-(q*gppr0)).CopyTo(outGrad.[0])
                              | q -> failwithf "Entmax15 unexpected OpReqType %A" q
                      member this.Forward(isTrain: bool, req: OpReqType [], inData: NDArray [], outData: NDArray [], auxData: NDArray []): unit = 
                          (entmoid15 ctx inData.[0]).CopyTo(outData.[0])
                 }
             //member this.DeclareBackwardDependency(outGrad: int [], inData: int [], outData: int []): int [] = 
             //    raise (System.NotImplementedException())
             //member this.InferBackwardStorageType(storageTypes: BackwardStorageTypes): unit = 
             //    raise (System.NotImplementedException())
             member this.InferShape(inShape: int [] []): int [] [] * int [] [] * int [] [] = 
                 let dataShape = inShape.[0]
                 let outputShape = inShape.[0]
                 [|dataShape|], [|outputShape|], Array.empty
             //member this.InferStorageType(inputStorageTypes: StorageType []): StorageType [] * StorageType [] * StorageType [] = 
             //    let outType = this.ListOutputs() |> Array.map (fun x -> StorageType.Default)
             //    let auxType = this.ListAuxiliaryStates() |> Array.map (fun x -> StorageType.Default)
             //    inputStorageTypes, outType, auxType
             //member this.InferType(inType: TypeFlag []): TypeFlag [] * TypeFlag [] * TypeFlag [] = 
             //    let outType = this.ListOutputs() |> Array.map (fun x -> inType.[0])
             //    let auxType = this.ListAuxiliaryStates() |> Array.map (fun x -> inType.[0])
             //    inType, outType, auxType
             //member this.ListArguments(): string [] = [|"data"; "label"|]
             //member this.ListAuxiliaryStates(): string [] = Array.empty
             //member this.ListOutputs(): string [] = [|"output"|]
        } :> _
    )

[<AutoOpen>]
module MyOps = 
    open MXNetSharp.SymbolArgument
    type Entmax15 private (operatorArguments) = 
        inherit SymbolOperator("Custom", operatorArguments)
        static member CreateFromArguments(args : Arguments<Symbol>) = new Entmax15(args)
        override this.WithArguments(args : Arguments<Symbol>) = new Entmax15(this.OperatorArguments.AddReplace(args)) :> Symbol
        new(?data : Symbol, ?dim : int) = 
            let data = defaultArg data (new ImplicitVariable() :> Symbol)
            let dim = defaultArg dim 0
            let operatorArguments = 
                [
                    "data", Input data
                    "op_type", Parameter(Some("entmax15" :> obj))
                    "dim", Parameter(Some(box dim))
                ]
            new Entmax15(Arguments<Symbol>(operatorArguments))
        member __.Data = operatorArguments.GetInput "data"
    type Entmoid15 private (operatorArguments) = 
        inherit SymbolOperator("Custom", operatorArguments)
        static member CreateFromArguments(args : Arguments<Symbol>) = new Entmoid15(args)
        override this.WithArguments(args : Arguments<Symbol>) = new Entmoid15(this.OperatorArguments.AddReplace(args)) :> Symbol
        new(?data : Symbol) = 
            let data = defaultArg data (new ImplicitVariable() :> Symbol)
            let operatorArguments = 
                [
                    "data", Input data
                    "op_type", Parameter(Some("entmoid15" :> obj))
                ]
            new Entmoid15(Arguments<Symbol>(operatorArguments))
        member __.Data = operatorArguments.GetInput "data"





let q = Statistics.quantileFunc [|1.0 .. 100.0|]
q 1.0

let rng = Random(3423)

let ctx = CPU 0
let odst (dat : NDArray option) inFeatures numTrees treeDim depth flatten (x : Symbol) = 
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


    let featureSelectors = Entmax15(featureSelectionLogits, 0)

    let featureValues = Dot(x, featureSelectors)
    
    match dat with 
    | Some nd ->
        let bm = 
            featureValues.Bindings
            |> Bindings.map
                (fun a -> 
                    if a.Name = x.Name then 
                        Bind.Arg(name = x.Name, ndarray = nd, grad = new NDArray(), opReqType = OpReqType.NullOp, shape = nd.Shape)
                    else a
                )
            |> Bindings.inferShapes featureValues
            |> Bindings.init (fun a b -> printfn "---> %A" a.Name; ctx.Zeros(b))
        let output = featureValues.Eval(ctx,bm).Outputs.[0]
        printfn "vals %A" output
        let q = output.ToDoubleArray() |> Statistics.quantileFunc
        let initV = Sample.betaSeq 1.0 1.0 rng |> Seq.take (numTrees * depth) |> Seq.map q |> Seq.toArray
        printfn "initV %A" initV
        featureThresholds.NDArray.Value.CopyFrom(initV)
        let q2 = (abs(output .- MX.ExpandDims(featureThresholds.NDArray.Value,0))).ToDoubleArray() |> Statistics.quantileFunc
        let temp = q2 1.0
        printfn "temp %A" temp
        //let temp = temp / 1.0
        let initTemp = (log temp + 1e-6)
        printfn "inittemp %A" initTemp
        logTemperatures.NDArray.Value.MutFull(initTemp) |> ignore
    | None -> ()

    let thresholdLogits = 
        let tl = (featureValues .- ExpandDims(featureThresholds, 0)) .* ExpandDims(exp(-logTemperatures), 0)
        Stack([-tl; tl], -1)


    let bins = Entmoid15(thresholdLogits)

    let binMatches = NpiEinsum([bins :> Symbol; binCodesOneHot :> Symbol], "btds,dcs->btdc")
    let responseWeights = Prod(binMatches, [-2])
    let output = NpiEinsum([responseWeights :> Symbol; response :> Symbol], "bnd,ncd->bnc")
    if flatten then Flatten(output) :> Symbol else output :> Symbol

let trainSize = 463715
let testSize = 51630

let allData = 
    File.ReadAllLines("D:\Data\yeaddataset\YearPredictionMSD.txt")
    //File.ReadAllLines("YearPredictionMSD.txt")
    |> Array.map 
        (fun line -> 
            let fields = line.Split(',')
            let year = fields.[0] |> int
            let features = fields.[1 ..] |> Array.map float32
            year,features
        )
let trainSet, testSet = allData |> Array.splitAt 463715


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

let initBatch = 
    let ix = MX.Shuffle(ctx.Arange(0.0, double trainSet.Length)).ToIntArray() |> Array.truncate 1000 
    let y,x = ix |> Array.map (fun i -> trainSet.[i]) |> Array.unzip
    ctx.CopyFrom(x |> Array.concat, shape = [1000;flength])



let inp = Input("x", [0; flength])


let l1 = odst (Some initBatch) flength 2048 3 6 false inp


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
                    MX.AdamUpdate([weight], weight, MX.Clip(grad,-0.1,0.1) , m, v, lr, beta1, beta2)
                | _ -> ()
            )


let bm = loss.Bindings |> Bindings.batchSize 512 |> Bindings.inferShapes loss |> Bindings.init (fun _ s -> ctx.Zeros(s))
let exe = loss.Bind(ctx,bm)
bm |> printShapes |> ignore
let opt = AdamOptimizer(exe)

let epoch = MX.Shuffle(ctx.Arange(0.0, double trainSet.Length)).ToIntArray() |> Array.chunkBySize 512 |> Array.filter (fun x -> x.Length = 512)
epoch.Length


for i = 0 to epoch.Length - 1 do
    printfn "%d" i
    GC.Collect()
    let bi = epoch.[i]
    let x = bi |> Array.map (fun i -> trainSet2.[i] |> snd)
    let y = bi |> Array.map (fun i -> yTrain.[i])
    exe.[label].CopyFrom(y)
    exe.[inp].CopyFrom(x |> Array.concat)
    exe.Forward(true)
    exe.Backward()
    opt.Update(0.001)
    printfn "%f" (exe.Outputs.[0].ToFloat32Scalar())

exe.Bindings
|> Seq.iter 
    (fun x ->
        match x.Grad with 
        | Some g -> printfn "%A" (x.Name, g.ToFloat32Array() )
        | _ -> ()
    )

