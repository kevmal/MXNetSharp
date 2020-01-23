namespace MXNETSharp.Tests.Symbol


open MXNetSharp.Interop
open Xunit
open MXNetSharp
open MXNetSharp.SymbolOperators

module Composition =
    [<Fact>]
    let ``compose output``() = 
        let f n = 
           let l = Hole() .>> FullyConnected(numHidden = n, noBias = true) 
           l.Outputs.[0]
        let x = Input("x", [1], NDArray.CopyFrom([|1.4f|], [-1], CPU 0))
        let model = x .>> f 1 .>> f 1
        let bm = 
            Bindings.inputs [x] 
            |> Bindings.inferShapes model
            |> Bindings.map 
                (fun x ->
                    printfn "%A" x
                    x
                )
            |> Bindings.defaultInitializer    
                (Init.create (fun _ a ->
                    a.MutFull(2.0) |> ignore
                    true
                ))
            |> Bindings.init
        let exe = model.Bind(CPU 0, bm)
        exe.Forward(false)
        Assert.Equal(4.0*1.4*10.0 |> round, exe.Outputs.[0].ToDoubleScalar()*10.0 |> round)

        
