namespace MXNETSharp.Tests.NDArray


open MXNetSharp.Interop
open Xunit
open MXNetSharp
//TODO: test mutation operators
//TODO: add and test unary mutation operators

module BasicBinaryOps =
    let nd (x : float32 seq) = 
        let data = x |> Seq.toArray
        NDArray.CopyFrom(data,[data.Length],CPU(0))

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


 
    open MXNetSharp.PrimitiveOperators

    [<Fact>]
    let ``add``() = testBinaryOp1 (+) (+) (+) (+) (.+) true
    [<Fact>]
    let ``sub``() = testBinaryOp1 (-) (-) (-) (-) (.-) true
    [<Fact>]
    let ``div``() = testBinaryOp1 (/) (/) (/) (/) (./) true
    [<Fact>]
    let ``mul``() = testBinaryOp1 ( * ) ( * ) ( * ) ( * ) (.*) true
    [<Fact>] 
    let ``pow``() = testBinaryOp1 ( ** ) ( ** ) ( ** ) ( ** ) (.**)
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
        NDArray.CopyFrom(data, [data.Length], CPU(0))
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


module Slicing =  
    let nd (x : float32 seq) = 
        let data = x |> Seq.toArray
        NDArray.CopyFrom(data, [data.Length], CPU(0))
    [<Fact>]
    let ``simple 1d``() = 
        let d1 = [0.0 .. 20.0] |> List.map float32
        let a = nd d1
        let expected = d1.[3 .. 5]
        let actual : float32 [] = (a.[3 .. 5]).ToArray()
        Assert.Equal(expected.Length, actual.Length)
        for i = 0 to expected.Length - 1 do    
            Assert.Equal(double expected.[i], double actual.[i], 6)

    [<Fact>]
    let ``1d no start``() = 
        let d1 = [0.0 .. 20.0] |> List.map float32
        let a = nd d1
        let expected = d1.[.. 5]
        let actual : float32 [] = (a.[.. 5]).ToArray()
        Assert.Equal(expected.Length, actual.Length)
        for i = 0 to expected.Length - 1 do    
            Assert.Equal(double expected.[i], double actual.[i], 6)

    [<Fact>]
    let ``1d no end``() = 
        let d1 = [0.0 .. 20.0] |> List.map float32
        let a = nd d1
        let expected = d1.[2 .. ]
        let actual : float32 [] = (a.[2 ..]).ToArray()
        Assert.Equal(expected.Length, actual.Length)
        for i = 0 to expected.Length - 1 do    
            Assert.Equal(double expected.[i], double actual.[i], 6)

    [<Fact>]
    let ``getItem``() = 
        let d1 = [|0.0 .. 9.0|] |> Array.map float32
        let a = (nd d1).Reshape([2; 5])
        do 
            let actual : float32[] = a.[0].ToArray()
            let expected = d1.[0 .. 4]
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        do 
            let actual : float32[] = a.[1].ToArray()
            let expected = d1.[5 .. ]
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        do 
            let actual : float32[] = a.[0, 0].ToArray()
            let expected = [|d1.[0]|]
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        do 
            let actual : float32[] = a.[1, 1].ToArray()
            let expected = [|d1.[6]|]
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)


    [<Fact>]
    let ``python doc examples``() = 
        let d1 = [|0.0 .. 5.0|] |> Array.map float32
        let a = (nd d1).Reshape([2; 3])
        let check expected (a : NDArray) = 
            let expected = Seq.toArray expected
            let actual : float32[] = a.ToArray()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        check [0.0 .. 2.0] a.[0]
        check [0.0 .. 2.0] a.[0,*]
        check [0.0 .. 1.0] a.[0,..1]
        check [0.0 ;1.0 ;3.0; 4.0] a.[*, .. -1]

        check [3.0 .. 5.0] a.[1..]
        check [|0.0 .. 5.0|] a.[NewAxis,*,*]
        Assert.Equal(3,a.[NewAxis,*,*].Shape.Length)
        Assert.Equal(5,a.[NewAxis,NewAxis,*,*,NewAxis].Shape.Length)

        let d2 = [|0.0 .. 15.0|] |> Array.map float32
        let a2 = (nd d2).Reshape([2; 2; 2; 2])
        // check [1.0; 3.0; 5.0; 7.0] a2.[0, ..., 1] //TODO: Omit range?
        check [1.0; 3.0; 5.0; 7.0] a2.[0, *, *, 1]



    [<Fact>]
    let ``simple stepping``() = 
        let d1 = [|0.0 .. 10.0|] |> Array.map float32
        let a = nd d1
        let check expected (a : NDArray) = 
            let expected = Seq.toArray expected
            let actual : float32[] = a.ToArray()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)

        check ({0 .. 2 .. 10} |> Seq.map (fun i -> d1.[i])) a.[SliceRange(0L,10L,2L)]


    //[<Fact>]
    //let sliceAssign() = 
    //    let a = new NDArray([1.f .. 100.f], shape = [50; 2], context  = CPU 0)
    //    let bb : float32 [] = a.ToArray()
    //    a.[30 .. 40, *] <- 10000.f
    //    a.[30 .. 40, *] <- a.[0 .. 10, *] 
    //    Assert.Equal(1,2)

module Basic = 
    [<Fact>]
    let ``AsType int -> double``() = 
        let nd = NDArray.CopyFrom([|0 .. 10|], CPU(0))
        let nd2 = nd.AsType(DataType.Float64)
        Assert.Equal(Some(DataType.Float64), nd2.DataType)
        Assert.Equal(Some(DataType.Int32), nd.DataType)
        Assert.True(nd2.ToArray() = [|0.0 .. 10.0|])
    [<Fact>]
    let ``CopyFrom Array``() = 
        let a1 = System.Array.CreateInstance(typeof<float32>, 2, 2)
        a1.SetValue(23.f, 1, 1)
        a1.SetValue(50.f, 1, 0)
        let b1 = NDArray.CopyFrom(a1, CPU 1)
        Assert.True(b1.Shape = [|2;2|])
        Assert.Equal(23.f, b1.[1,1].ToFloat32Scalar())
        Assert.Equal(50.f, b1.[1,0].ToFloat32Scalar())
        let a2 = System.Array.CreateInstance(typeof<float32>, 2, 2, 4, 2, 5)
        a2.SetValue(23.f, 1, 1, 2, 1, 4)
        a2.SetValue(50.f, 0, 1, 0, 0, 3)
        a2.SetValue(110.f, 0, 1, 3, 0, 2)
        let b2 = NDArray.CopyFrom(a2, CPU 1)
        Assert.True(b2.Shape = [|2;2;4;2;5|])
        Assert.Equal(23.f, b2.[1, 1, 2, 1, 4].ToFloat32Scalar())
        Assert.Equal(50.f, b2.[0, 1, 0, 0, 3].ToFloat32Scalar())
        Assert.Equal(110.f, b2.[0, 1, 3, 0, 2].ToFloat32Scalar())
    [<Fact>]
    let ``Float16``() = 
        let a1 = System.Array.CreateInstance(typeof<float32>, 2, 2)
        a1.SetValue(23.f, 1, 1)
        a1.SetValue(50.f, 1, 0)
        let b1 = new NDArray([2;2], CPU 0, DataType.Float16)
        b1.CopyFrom(a1)
        Assert.Equal(Some(DataType.Float16), b1.DataType)
        Assert.Equal(23.f, b1.[1,1].ToFloat32Scalar())
        Assert.Equal(50.f, b1.[1,0].ToFloat32Scalar())
    [<Fact>]
    let ``MutFull tests``() = 
        let a = NDArray.CopyFrom([|0.0 .. 9.0|], [-1], CPU 0)
        a.MutFull(0.0) |> ignore
        Assert.True(a.ToDoubleArray() = Array.zeroCreate 10)
        a.MutFull(0.f) |> ignore
        Assert.True(a.ToDoubleArray() = Array.zeroCreate 10)
        a.MutFull(0) |> ignore
        Assert.True(a.ToDoubleArray() = Array.zeroCreate 10)

module Main = 
    [<EntryPoint>]
    let main argv = 
        0 