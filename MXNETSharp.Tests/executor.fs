namespace MXNETSharp.Tests.Executor


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


        