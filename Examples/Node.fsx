// ********************** Incomplete "Example" *****************************

// Adapted from https://github.com/locuslab/TCN

open System.Collections.Generic
open System.Runtime.InteropServices

#load "load.fsx"
open MXNetSharp
open MXNetSharp.SymbolOperators
open MXNetSharp.IO
open System
open System.Net
open System.IO
open System.IO.Compression

open MXNetSharp.PrimitiveOperators

// entmax15
(*
let x = Input("x", ndarray = CPU(0).CopyFrom([|-2.f; 0.f; 0.f|]))
let k = 3
let dim = 1

let sorted = Topk(x, axis = dim, k = k, retTyp = RetTyp.Value)
let rho = Arange(1.0)
let mean = NpCumsum(sorted) / rho
let meanSq = NpCumsum(Square(sorted)) / rho
let ss = rho*(meanSq - Square(mean))
let delta = (1.0 - ss) / rho
let deltaNz = Relu(delta)
let tau = mean - sqrt deltaNz
let supportSize = Sum(tau .<= sorted, keepdims=true)
*)
  

let alpha = Variable("alpha")
let z = Variable("z")
let d = 3.0
let T = 50

let z0 = (alpha - 1.0).*z
let maxZ = Max(z0)
let tauMin = maxZ - 1.0
let tauMax = maxZ - RpowerScalar(1.0 - alpha, d)

let rec loop t (Z : Symbol) (tauMin : Symbol) (tauMax : Symbol) = 
    let z = z0
    if t = T then 
        Z, tauMin, tauMax 
    else
        let tau = (tauMin + tauMax) / 2.0
        let Z = Sum(BroadcastPower(Relu(z .- tau), (1.0 / (alpha - 1.0))))
        let tauMin,tauMax = 
            let cond = Z .< 1.0
            Where(cond, tauMin, tau) :> Symbol, 
                Where(cond, tau, tauMax) :> Symbol
        loop (t + 1) Z tauMin tauMax

let Z,tau = 
    let Z, tauMin, tauMax = loop 0 (z0 :> Symbol) tauMin tauMax
    Z,(tauMin + tauMax) / 2.0

let pt = BroadcastPower(Relu(z0 .- tau), (1.0 / (alpha - 1.0)))
let r = pt ./ Z

pt.ArgumentNames


let inputs = 
    [
        Bind.Arg(alpha.Name, shape = [1], opReqType = OpReqType.NullOp, dataType = DataType.Float32, ndarray = NDArray.CopyFrom([|1.0001f|], [1], CPU 0))
        Bind.Arg(z.Name, shape = [3], opReqType = OpReqType.NullOp, dataType = DataType.Float32, ndarray = NDArray.CopyFrom([|-2.f; 0.f; 0.5f|], [3], CPU 0))
    ]



let q = 
    inputs
    |> Bindings.ofSeq
    |> Bindings.mapArg 
        (fun a ->
            match a.OpReqType with 
            | Some NullOp -> {a with Grad = Some(new NDArray())}
            | _ -> {a with Grad = Some(MX.ZerosLike(a.NDArray.Value))}
        )
    |> Bindings.inferShapes r



let printShapes (b : Bindings) = 
    b.Bindings.Values
    |> Seq.iter 
        (fun b ->
            printfn "%-20s %A" b.Name b.Shape
        )
    b

q |> printShapes |> ignore

let f = r.Bind(CPU 0, q)

f.Forward(false)

f.Backward()

let aa : float32[] = f.Outputs.[0].ToArray() 

let input = Input("x")

let inFeatures = 10
let numTrees = 10
let treeDim = 2
let depth = 5
let flatten = true
let choicef t d = Softmax(t,axis=d,useLength=false)
let binf t = t


let response = Parameter(shape = [numTrees; treeDim; pown 2 depth])

// init response

let featureSelectionLogits = Parameter(ndarray = CPU(0).Zeros([inFeatures; numTrees; depth]))
let featureThresholds = Parameter(shape = [numTrees; depth])
let logTemperatures = Parameter(shape = [numTrees; depth])
pown 2 0
let ctx = CPU 0
let indices = ctx.Arange(start = 0.0, stop = double(pown 2 depth))
let offsets = 2.0 ** ctx.Arange(start = 0.0, stop = double depth)


let binCodes = indices.Reshape(1,-1) ./ offsets.Reshape(-1,1)
binCodes.ToFloat32Array()
offsets.ToFloat32Array()
ctx.Arange(start = 0.0, stop = double depth).ToFloat32Array()


let featureSelectors = choicef featureSelectionLogits 0
let featureValues = Dot(input, featureSelectors)

let thresholdLogits = 
    let tl = (featureValues - featureThresholds) * exp(-logTemperatures)
    Stack([-tl; tl], -1)


let bins = binf thresholdLogits

let binMatches = NpiEinsum([bins :> Symbol; binCodesOneHot :> Symbol], "btds,dcs->btdc")


let f = (CPU 0).Arange(0.0, 24.0).Reshape(3,2,4)
let i = (CPU 0).CopyFrom([|1.f;0.f;1.f|]).Reshape(1,3)



let q1 = f.SwapAxis(0,2)
let q = q1.SwapAxis(0,1)


open MXNetSharp.Interop
type Crap() =
    static member FullyConnected(data : NDArray, 
                                 weight : NDArray, 
                                 numHidden : int, 
                                 [<Optional; DefaultParameterValue(true)>] flatten : bool) =
        let creator = AtomicSymbolCreator.FromName "FullyConnected"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.UnsafeHandle; weight.UnsafeHandle|]
                                                 [|"num_hidden"; "no_bias"; "flatten"|]
                                                 [|string numHidden; "true"; string flatten|]
        (new NDArray(SafeNDArrayHandle(outputs.[0], true)))


let a = MX.Dot(i,f)

NDArray.WaitAll()


a.ToFloat32Array()


let poo = Crap.FullyConnected(q,i,1,flatten=false)

poo

poo.ToFloat32Array()





f .* i

MX.LinalgGemm2(i,f)

MX.expa

MX.einsu
