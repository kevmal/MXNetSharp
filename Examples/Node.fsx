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

let json = MXNetSharp.Interop.MXSymbol.saveToJSON r.UnsafeHandle

File.WriteAllText("dmfkmsdfkmsdfkmsdkfmsd.json", json)

let aa : float32[] = f.Outputs.[0].ToArray() 
