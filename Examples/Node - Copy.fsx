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

let printShapes (b : Bindings) = 
    b.Bindings.Values
    |> Seq.iter 
        (fun b ->
            printfn "%-20s %A" b.Name b.Shape
        )
    b
(1).ValueString()

let mutable way1 = [|0|]
let mutable way2 = [|0|]
(way1,way2) <- ([|1|],[|2|])


let odst inFeatures numTrees treeDim depth flatten choicef binf (x : Symbol) = 

    let response = Parameter("response", shape = [numTrees; treeDim; pown 2 depth]) //init normal 0.0 1.0

    let featureSelectionLogits = Parameter("featureSelectionLogits", shape = [inFeatures; numTrees; depth])
    let featureThresholds = Parameter("featureThresholds", shape = [numTrees; depth])
    let logTemperatures = Parameter("logTemperatures", shape = [numTrees; depth])
    let binCodesOneHot = 
        let ctx = CPU 0
        let indices = ctx.Arange(start = 0.0, stop = double(pown 2 depth))
        let offsets = 2.0 ** ctx.Arange(start = 0.0, stop = double depth)
        let binCodes = 
            let x = indices.AsType(DataType.Int32).Reshape(1,-1) ./ offsets.AsType(DataType.Int32).Reshape(-1,1)
            (x % 2.0).AsType(DataType.Float32)
        let binCodesOneHot = MX.Stack(binCodes, 1.0 - binCodes, axis = -1)
        Constant(binCodesOneHot, "binCodesOneHot")


    let featureSelectors = choicef featureSelectionLogits 0
    let featureValues = Dot(x, featureSelectors)

    let thresholdLogits = 
        let tl = (featureValues .- ExpandDims(featureThresholds, 0)) .* ExpandDims(exp(-logTemperatures), 0)
        Stack([-tl; tl], -1)


    let bins = binf thresholdLogits

    let binMatches = NpiEinsum([bins :> Symbol; binCodesOneHot :> Symbol], "btds,dcs->btdc")
    let responseWeights = Prod(binMatches, [-2])
    let output = NpiEinsum([responseWeights :> Symbol; response :> Symbol], "bnd,ncd->bnc")
    if flatten then Flatten(output) :> Symbol else output :> Symbol




let inFeatures = 10
let numTrees = 10
let treeDim = 2
let depth = 5
let flatten = true
let choicef t d = Softmax(t,axis=d,useLength=false)
let binf (x : Symbol) = Clip(0.5*x + 0.5, 0.0, 1.0)
let input = Input("x", shape= [1000;inFeatures])
let x = input

let o = odst inFeatures numTrees treeDim depth flatten choicef binf input
o.Bindings |> Bindings.inferShapes o |> printShapes |> ignore



r.Bindings |> Bindings.inferShapes r |> printShapes |> ignore
bins.Bindings |> Bindings.inferShapes bins |> printShapes |> ignore
featureValues.Bindings |> Bindings.inferShapes featureValues |> printShapes |> ignore
logTemperatures.Bindings |> Bindings.inferShapes logTemperatures |> printShapes |> ignore
featureThresholds.Bindings |> Bindings.inferShapes featureThresholds |> printShapes |> ignore

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
