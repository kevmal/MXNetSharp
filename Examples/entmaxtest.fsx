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
open MXNetSharp.Interop
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
    let dim = if dim < 0 then dim + s.Length else dim
    let sorted = MX.Sort(x, axis = dim, isAscend = false) //TODO fix Sort on NDArray
    //NDArray.WaitAll()
    let rho = 
        let s = s |> Array.mapi (fun i d -> if i = dim then -1 else 1)
        printfn "%A" s
        MX.ContribArangeLike(x, ctx, start = 1.0, axis = dim).Reshape(dims = s)
    //NDArray.WaitAll()
    let mean = MX.NpCumsum(sorted, axis = dim) ./ rho
    //NDArray.WaitAll()
    let meanSq = MX.NpCumsum(MX.Square(sorted), axis = dim) ./ rho
    //NDArray.WaitAll()
    let ss = rho.*(meanSq - MX.Square(mean))
    //NDArray.WaitAll()
    let delta = (1.0 - ss) ./ rho
    //NDArray.WaitAll()
    let deltaNz = MX.Relu(delta)
    //NDArray.WaitAll()
    let tau = mean - sqrt deltaNz
    //NDArray.WaitAll()
    let supportSize = MX.Sum(tau .<= sorted, keepdims=true, axis = [dim])
    //NDArray.WaitAll()
    let xs = 
        [|
            for a = 0 to s.Length - 1 do 
                if a = dim then 
                    MX.BroadcastTo(supportSize - 1.0, s)
                else
                    let x = MX.ArangeNDArray(ctx = ctx,start = 0.0, stop = double s.[a]).Reshape(dims = (s |> Array.mapi (fun i x -> if i = a then x else 1)))
                    MX.Tile(x, s |> Array.mapi (fun i x -> if i <> a then x else 1) )
        |]
    //NDArray.WaitAll()
    printfn "------"
    xs |> Array.iter (fun x -> printfn "%A" x.Shape)
    printfn "------"
    let ix = MX.Stack(xs,0)
    //NDArray.WaitAll()
    let tauStar = MX.GatherNd(tau, ix)
    //NDArray.WaitAll()
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
                 printfn "%A" ins.Count
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
                          printfn "ddddl"
                          if req.[0] = OpReqType.NullOp then 
                              () 
                          else
                              printfn "ghell"
                              let pp (x : NDArray) = 
                                    x.ToArray<float32>()
                                    |> Array.chunkBySize 4
                                    |> Array.iter 
                                        (fun x ->
                                            x |> Array.map (sprintf "%0.4f") |> String.concat ", " |> printfn "%s"
                                        )
                              pp outData.[0]
                              let gppr = sqrt outData.[0]
                              printfn "InGrad"
                              pp outGrad.[0]
                              printfn "OGrad"
                              pp inGrad.[0]
                              let dx = outGrad.[0] * gppr
                              printfn "dx"
                              pp dx
                              let q = MX.Sum(dx, axis = [dim], keepdims = true) / MX.Sum(gppr, axis = [dim], keepdims = true)
                              printfn "q %A" q.Shape 
                              q.ToArray<float32>() |> printfn "%A"
                              let o = dx - q.*gppr
                              match req.[0] with 
                              | OpReqType.AddTo -> (inGrad.[0] - o).CopyTo(inGrad.[0])
                              | OpReqType.WriteTo -> o.CopyTo(inGrad.[0])
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
                          printfn "vvv"
                          if req.[0] = OpReqType.NullOp then 
                              () 
                          else
                              printfn "ghell"
                              let gppr0 = sqrt outData.[0]
                              let gppr1 = sqrt (1.0 - outData.[0])
                              let gi = outGrad.[0]*gppr0
                              let q = gi / (gppr0 + gppr1)
                              let o = gi - q*gppr0
                              match req.[0] with 
                              | OpReqType.AddTo -> (inGrad.[0] - o).CopyTo(inGrad.[0])
                              | OpReqType.WriteTo -> o.CopyTo(inGrad.[0])
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
    type Entmax15(dim : int, data : Symbol option) = 
        inherit Custom("entmax15", [|"dim", Some(dim :> obj)|], [|data|])
        new(?data, ?dim) = Entmax15(defaultArg dim -1, data)
        member x.Data = x.OperatorArguments.GetInput "data" 
        member x.Dim = dim
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



let c = CPU 0


let m = c.Arange(0.0, double (6*5*4)).Reshape(6,5,4)
(*
m.AttachGradient()

let yGrad = Autograd.record (fun () -> entmax15 c m -1)

yGrad.Backward()

m.Grad.Value.ToArray<float32>()
|> Array.chunkBySize 4
|> Array.iter 
    (fun x ->
        x |> Array.map (sprintf "%0.4f") |> String.concat ", " |> printfn "%s"
    )


let r = entmax15 c m -1
NDArray.WaitAll()

r.ToArray<float32>()
|> Array.chunkBySize 4
|> Array.iter 
    (fun x ->
        x |> Array.map (sprintf "%0.4f") |> String.concat ", " |> printfn "%s"
    )
*)

let x = Parameter("x", ndarray = m,grad = MX.ZerosLike(m), opReqType = OpReqType.WriteTo)
let s = Entmax15(x, -1)

let r = s.Bind(c)

r.Forward(true)
//r.Backward([MX.OnesLike(x.NDArray.Value)])
m.UnsafeHandle
r.Backward([m])


NDArray.WaitAll()

r.Bindings.[x].Shape
r.Bindings.[x].Grad.Value.ToArray<float32>()
|> Array.chunkBySize 4
|> Array.iter 
    (fun x ->
        x |> Array.map (sprintf "%0.4f") |> String.concat ", " |> printfn "%s"
    )



r.Outputs.[0].Shape
r.Outputs.[0].DataType
r.Outputs.[0].ToArray()
r.Outputs.[0].ToArray<float32>()
|> Array.chunkBySize 4
|> Array.iter 
    (fun x ->
        x |> Array.map (sprintf "%0.4f") |> String.concat ", " |> printfn "%s"
    )



