namespace MXNETSharp.Tests.NDArray


open MXNetSharp.Interop
open Xunit
open MXNetSharp

module BasicBinaryOps =
    let nd (x : float32 seq) = 
        let data = x |> Seq.toArray
        new NDArray(data, shape = [data.Length], context = CPU(0))

    let testBinaryOp1 ((+) : float32 -> float32 -> float32) 
                             ((++) : _ -> _ -> NDArray) 
                             ((|++) : (_ -> _ -> NDArray))
                             ((++|) : (_ -> _ -> NDArray))
                             ((.++) : (_ -> _ -> NDArray))
                             hasElemWise =
        let d1 = [155.01; 1.0] |> List.map float32
        let d2 = [63.0; 1.0] |> List.map float32
        let d3 = [232.0; 564.0; 2.0; 376.345] |> List.map float32
        let scalar = 72.23
        let a = nd d1
        let b = nd d2
        let c = (nd d3).Reshape([2;2])
        do // a + b
            let expected = (d1,d2) ||> List.map2 (+) |> Seq.toArray
            let actual : float32 [] = (a ++ b).ToArray()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        do // b + a
            let expected = (d2,d1) ||> List.map2 (+) |> Seq.toArray
            let actual : float32 [] = (b ++ a).ToArray()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        do // a + scalar
            let expected = d1 |> List.map (fun x -> x + float32 scalar)
            let actual : float32 [] = (a ++| scalar).ToArray()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        do // scalar + a
            let expected = d1 |> List.map (fun x -> float32 scalar + x)
            let actual : float32 [] = (scalar |++ a).ToArray()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        if hasElemWise then // a + c -> dimension mismatch
            Assert.Throws<MXNetSharp.Interop.MXNetException>(fun _ -> a ++ c |> ignore) |> ignore
        do // a .+ c 
            let d1 = d1 @ d1
            let expected = (d1,d3) ||> List.map2 (+) |> Seq.toArray
            let actual : float32 [] = (a .++ c).ToArray()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)


 

    [<Fact>]
    let ``add``() = testBinaryOp1 (+) (+) (+) (+) (.+) true
    [<Fact>]
    let ``sub``() = testBinaryOp1 (-) (-) (-) (-) (.-) true
    [<Fact>]
    let ``div``() = testBinaryOp1 (/) (/) (/) (/) (./) true
    [<Fact>]
    let ``mul``() = testBinaryOp1 ( * ) ( * ) ( * ) ( * ) (.*) true
    //[<Fact>] // TODO: power op
    //let ``pow``() = testBinaryOp1 ( ** ) ( ** ) ( ** ) ( ** ) (.**)
    [<Fact>]
    let ``mod``() = testBinaryOp1 (%) (%) (%) (%) (%) false
    let boolValue (==) x y = if x == y then 1.f else 0.f
    [<Fact>]
    let eq() = testBinaryOp1 (boolValue(=)) (.=) (.=) (.=) (.=) false
    [<Fact>]
    let neq() = testBinaryOp1 (boolValue(<>)) (.<>) (.<>) (.<>) (.<>) false
    [<Fact>]
    let greater() = testBinaryOp1 (boolValue(>)) (.>) (.>) (.>) (.>) false
    [<Fact>]
    let lesser() = testBinaryOp1 (boolValue(<)) (.<) (.<) (.<) (.<) false
    [<Fact>]
    let greaterEq() = testBinaryOp1 (boolValue(>=)) (.>=) (.>=) (.>=) (.>=) false
    [<Fact>]
    let lesserEq() = testBinaryOp1 (boolValue(<=)) (.<=) (.<=) (.<=) (.<=) false
    let logicalValue (==) x y = if (x <> 0.f) == (y <> 0.f) then 1.f else 0.f
    [<Fact>]
    let logicalAnd() = testBinaryOp1 (logicalValue(&&)) (.&&) (.&&) (.&&) (..&&) true
    [<Fact>]
    let logicalOr() = testBinaryOp1 (logicalValue(||)) (.||) (.||) (.||) (..||) true
    [<Fact>]
    let logicalXor() = testBinaryOp1 (logicalValue(<>))  (.^^) (.^^) (.^^) (..^^) true


module BasicUnaryOps =    
    open MXNetSharp.PrimitiveOperators
    let nd (x : float32 seq) = 
        let data = x |> Seq.toArray
        new NDArray(data, shape = [data.Length], context = CPU(0))
    let uop f (f2 : _ -> NDArray) = 
        let d1 = [155.01; 1.0] |> List.map float32
        let a = nd d1
        let expected = d1 |> List.map f |> Seq.toArray
        let actual : float32 [] = (f2 a).ToArray()
        Assert.Equal(expected.Length, actual.Length)
        for i = 0 to expected.Length - 1 do    
            Assert.Equal(double expected.[i], double actual.[i], 6)
    [<Fact>]
    let exp() = uop exp exp
    [<Fact>]
    let log() = uop log log
    [<Fact>]
    let abs() = uop abs abs
    [<Fact>]
    let atan() = uop atan atan
    [<Fact>]
    let acos() = uop acos acos
    [<Fact>]
    let asin() = uop asin asin
    [<Fact>]
    let ceil() = uop ceil ceil
    [<Fact>]
    let floor() = uop floor floor
    [<Fact>]
    let truncate() = uop truncate truncate
    [<Fact>]
    let round() = uop round round
    [<Fact>]
    let log10() = uop log10 log10
    [<Fact>]
    let sqrt() = uop sqrt sqrt
    [<Fact>]
    let cos() = uop cos cos
    [<Fact>]
    let cosh() = uop cosh cosh
    [<Fact>]
    let sin() = uop sin sin
    [<Fact>]
    let sinh() = uop sinh sinh
    [<Fact>]
    let tan() = uop tan tan
    [<Fact>]
    let tanh() = uop tanh tanh