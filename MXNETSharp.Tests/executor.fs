﻿namespace MXNETSharp.Tests.Executor


open MXNetSharp.Interop
open Xunit
open MXNetSharp
open MXNetSharp.SymbolOperators
open MXNetSharp.PrimitiveOperators

module Basic =
    // See issue #12
    [<Fact>]
    let ``check outputs get updated``() =
        let a = Variable "a"
        let b = Variable "b"
        let outputs = 
            Cond(a .< b, 
                 a + b,
                 -(a + b),
                 [a :> Symbol;b :> Symbol],
                 1, [0L;1L], [0L; 1L], [0L; 1L])
        let q = 
            [
                Bind.Arg(a.Name, shape = [1], opReqType = OpReqType.NullOp, dataType = DataType.Float32, ndarray = NDArray.CopyFrom([|4.f|], [1], CPU 0), grad = new NDArray())
                Bind.Arg(b.Name, shape = [1], opReqType = OpReqType.NullOp, dataType = DataType.Float32, ndarray = NDArray.CopyFrom([|5.f|], [1], CPU 0), grad = new NDArray())
            ]
            |> Bindings.ofSeq
        let exe = outputs.Bind(CPU 0, q)
        exe.Forward(false)
        Assert.Equal(9.f, exe.Outputs.[0].ToFloat32Scalar())

    // See issue #28
    [<Fact>]
    let ``Symbol.Bind timing``() = 
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
        
        let f = 
            async {
                return r.Bind(CPU 0, q)
            } |> Async.StartAsTask
        let mutable waitTime = 10*1000
        while f.Status <> System.Threading.Tasks.TaskStatus.RanToCompletion && waitTime > 0 do 
            Async.Sleep 100 |> Async.RunSynchronously
            waitTime <- waitTime - 100
        Assert.Equal(System.Threading.Tasks.TaskStatus.RanToCompletion , f.Status)
        
        
        