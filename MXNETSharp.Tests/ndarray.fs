namespace MXNETSharp.Tests.NDArray


open MXNetSharp.Interop
open Xunit
open MXNetSharp
open MXNetSharp

module OpHelp = 

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
            let actual : float32 [] = (a ++ b).ToArray<_>()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        do // b + a
            let expected = (d2,d1) ||> List.map2 (+) |> Seq.toArray
            let actual : float32 [] = (b ++ a).ToArray<_>()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        do // a + scalar
            let expected = d1 |> List.map (fun x -> x + float32 scalar)
            let actual : float32 [] = (a ++| scalar).ToArray<_>()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        do // scalar + a
            let expected = d1 |> List.map (fun x -> float32 scalar + x)
            let actual : float32 [] = (scalar |++ a).ToArray<_>()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        if hasElemWise then // a + c -> dimension mismatch
            Assert.Throws<MXNetSharp.Interop.MXNetException>(fun _ -> a ++ c |> ignore) |> ignore
        do // a .+ c 
            let d1 = d1 @ d1
            let expected = (d1,d3) ||> List.map2 (+) |> Seq.toArray
            let actual : float32 [] = (a .++ c).ToArray<_>()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)



module BasicBinaryOps =
    open OpHelp
    [<Fact>]
    let ``add``() = testBinaryOp1 (+) (+) (+) (+) (.+) true
    [<Fact>]
    let ``sub``() = testBinaryOp1 (-) (-) (-) (-) (.-) true
    [<Fact>]
    let ``div``() = testBinaryOp1 (/) (/) (/) (/) (./) true
    [<Fact>]
    let ``mul``() = testBinaryOp1 ( * ) ( * ) ( * ) ( * ) (.*) true
    [<Fact>] 
    let ``pow``() = testBinaryOp1 ( ** ) (fun a b -> NDArray.Pow(a,b)) (fun a b -> NDArray.ApplyPow(a,b)) (fun a b -> NDArray.Pow(a,b)) (.**) true
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

module BasicBinaryOpsWithPrim =
    open OpHelp
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
    let ``pow``() = testBinaryOp1 ( ** ) ( ** ) ( ** ) ( ** ) (.**) true
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

    [<Fact>]
    let ``power op test 1``() = 
        let ctx = CPU 0
        let r = (2.0 ** ctx.Arange(0.0, 6.0)).ToDoubleArray() |> Array.map (round >> int)
        let actual = [|0 .. 5|] |> Array.map (fun x -> pown 2 x)
        Assert.True((actual = r))


module BasicUnaryOps =    
    open MXNetSharp.PrimitiveOperators
    let nd (x : float32 seq) = 
        let data = x |> Seq.toArray
        NDArray.CopyFrom(data, [data.Length], CPU(0))
    let uop f (f2 : _ -> NDArray) = 
        let d1 = [155.01; 1.0] |> List.map float32
        let a = nd d1
        let expected = d1 |> List.map f |> Seq.toArray
        let actual : float32 [] = (f2 a).ToArray<_>()
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

module MutatingOps = 
    open MXNetSharp.PrimitiveOperators
    let checkUni mop op = 
        MXLib.randomSeed 123423
        let c = CPU 0
        let a = c.RandomNormal([10;10])
        let b = a.CopyTo(CPU 0)
        mop b |> ignore
        let expected = (op a : NDArray).ToFloat32Array()
        let actual = b.ToFloat32Array()
        Assert.Equal(expected.Length, actual.Length)
        for i = 0 to expected.Length - 1 do    
            Assert.Equal(double expected.[i], double actual.[i], 6)

    let check (scalar : double) (a1 : NDArray) (a2 : NDArray) (b1 : NDArray) (+@) (+) (+@.) (+.) (+@|) (+|) = 
        let c = CPU 0
        do 
            let expected = ((a1 + a2) : NDArray).ToFloat32Array()
            let actual = 
                let a = c.CopyFrom(a1)
                a +@ a2 |> ignore
                a.ToFloat32Array()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        do 
            let expected = ((a1 +. scalar) : NDArray).ToFloat32Array()
            let actual = 
                let a = c.CopyFrom(a1)
                a +@. scalar |> ignore
                a.ToFloat32Array()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        do 
            let expected = ((a1 +| b1) : NDArray).ToFloat32Array()
            let actual = 
                let a = c.CopyFrom(a1)
                a +@| b1 |> ignore
                a.ToFloat32Array()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
    let checkr (a1 : NDArray) a2 op1 op2 = 
        let a = a1.CopyTo(CPU 0)
        op1 a a2 |> ignore
        let expectednd : NDArray = op2 a2 a1
        let actual = a.ToFloat32Array()
        let expected = expectednd.ToFloat32Array()
        Assert.Equal(expected.Length, actual.Length)
        for i = 0 to expected.Length - 1 do    
            Assert.Equal(double expected.[i], double actual.[i], 6)

    let checkr1 = 
        MXLib.randomSeed 123423
        let c = CPU 0
        let a1 = c.RandomNormal([10;10])
        let a2 = c.RandomNormal([10;10])
        checkr a1 a2
    let checkrs1 = 
        MXLib.randomSeed 123423
        let c = CPU 0
        let a1 = c.RandomNormal([10;10])
        let a2 = 22.3423
        checkr a1 a2
    let checkrb1 = 
        MXLib.randomSeed 123423
        let c = CPU 0
        let a1 = c.RandomNormal([10;10])
        let a2 = c.RandomNormal([1;10])
        checkr a1 a2
        
    let check1 = 
        MXLib.randomSeed 123423
        let c = CPU 0
        let a1 = c.RandomNormal([10;10])
        let a2 = c.RandomNormal([10;10])
        let b1 = c.RandomNormal([1;10])
        check 23.23234 a1 a2 b1
    let check2 = 
        MXLib.randomSeed 123423
        let c = CPU 0
        let a1 = MX.Cast(c.RandomRandint([10;10],0L, 5L), DataType.Float32)
        let a2 = MX.Cast(c.RandomRandint([10;10],0L, 5L), DataType.Float32)
        let b1 = MX.Cast(c.RandomRandint([1;10],0L, 5L), DataType.Float32)
        check 2.0 a1 a2 b1


    [<Fact>]
    let ``add``() = check1 (fun (x : NDArray) (y : NDArray) -> x.MutPlus(y)) (+) 
                           (fun (x : NDArray) (y : double) -> x.MutPlus(y)) (+) 
                           (fun (x : NDArray) (y : NDArray) -> x.MutPlusBroadcast(y)) (.+) 
    [<Fact>]
    let ``sub``() = 
        check1 (fun (x : NDArray) (y : NDArray) -> x.MutSubstract(y)) (-) 
               (fun (x : NDArray) (y : double) -> x.MutSubstract(y)) (-) 
               (fun (x : NDArray) (y : NDArray) -> x.MutSubstractBroadcast(y)) (.-) 
        checkr1 (fun (x : NDArray) (y : NDArray) -> x.MutSubstractFrom(y)) (-)
        checkrs1 (fun (x : NDArray) (y : double) -> x.MutSubstractFrom(y)) (-)
        checkrb1 (fun (x : NDArray) (y : NDArray) -> x.MutSubstractBroadcastFrom(y)) (.-)

    [<Fact>]
    let ``div``() = 
        check1 (fun (x : NDArray) (y : NDArray) -> x.MutDividedBy(y)) (/) 
               (fun (x : NDArray) (y : double) -> x.MutDividedBy(y)) (/) 
               (fun (x : NDArray) (y : NDArray) -> x.MutDividedBroadcastBy(y)) (./) 
        checkr1 (fun (x : NDArray) (y : NDArray) -> x.MutDividedInto(y)) (/)
        checkrs1 (fun (x : NDArray) (y : double) -> x.MutDividedInto(y)) (/)
        checkrb1 (fun (x : NDArray) (y : NDArray) -> x.MutDividedBroadcastInto(y)) (./)
    [<Fact>]
    let ``mul``() = check1 (fun (x : NDArray) (y : NDArray) -> x.MutMultiply(y)) ( * ) 
                           (fun (x : NDArray) (y : double) -> x.MutMultiply(y)) ( * ) 
                           (fun (x : NDArray) (y : NDArray) -> x.MutMultiplyBroadcast(y)) ( .* ) 
    [<Fact>] 
    let ``pow``() = 
        check1 (fun (x : NDArray) (y : NDArray) -> x.MutPower(y)) (fun (x : NDArray) (y : NDArray) -> x ** y)
               (fun (x : NDArray) (y : double) -> x.MutPower(y)) (fun (x : NDArray) (y : double) -> x ** y)
               (fun (x : NDArray) (y : NDArray) -> x.MutPowerBroadcast(y)) (fun (x : NDArray) (y : NDArray) -> x .** y)
        checkr1 (fun (x : NDArray) (y : NDArray) -> x.MutPowerBaseOf(y)) (fun (x : NDArray) (y : NDArray) -> x ** y)
        checkrs1 (fun (x : NDArray) (y : double) -> x.MutPowerBaseOf(y)) (fun (x : double) (y : NDArray) -> (x ** y) : NDArray)
        checkrb1 (fun (x : NDArray) (y : NDArray) -> x.MutPowerBaseOfBroadcast(y)) 
    [<Fact>]
    let ``mod``() = 
        check1 (fun (x : NDArray) (y : NDArray) -> x.MutMod(y)) (%) 
               (fun (x : NDArray) (y : double) -> x.MutMod(y)) (%) 
               (fun (x : NDArray) (y : NDArray) -> x.MutMod(y)) (%) 
        checkr1 (fun (x : NDArray) (y : NDArray) -> x.MutModOf(y)) (%)
        checkrs1 (fun (x : NDArray) (y : double) -> x.MutModOf(y)) (%)


    [<Fact>]
    let eq() = check2 (fun (x : NDArray) (y : NDArray) -> x.MutEqual(y)) (.=) 
                      (fun (x : NDArray) (y : double) -> x.MutEqual(y)) (.=) 
                      (fun (x : NDArray) (y : NDArray) -> x.MutEqual(y)) (.=) 
    [<Fact>]
    let neq() = check2 (fun (x : NDArray) (y : NDArray) -> x.MutNotEqual(y)) (.<>) 
                       (fun (x : NDArray) (y : double) -> x.MutNotEqual(y)) (.<>) 
                       (fun (x : NDArray) (y : NDArray) -> x.MutNotEqual(y)) (.<>) 
    [<Fact>]
    let greater() = check2 (fun (x : NDArray) (y : NDArray) -> x.MutGreater(y)) (.>) 
                              (fun (x : NDArray) (y : double) -> x.MutGreater(y)) (.>) 
                              (fun (x : NDArray) (y : NDArray) -> x.MutGreater(y)) (.>) 
    [<Fact>]
    let lesser() = check2 (fun (x : NDArray) (y : NDArray) -> x.MutLesser(y)) (.<) 
                              (fun (x : NDArray) (y : double) -> x.MutLesser(y)) (.<) 
                              (fun (x : NDArray) (y : NDArray) -> x.MutLesser(y)) (.<) 
    [<Fact>]
    let greaterEq() = check2 (fun (x : NDArray) (y : NDArray) -> x.MutGreaterOrEqual(y)) (.>=) 
                              (fun (x : NDArray) (y : double) -> x.MutGreaterOrEqual(y)) (.>=) 
                              (fun (x : NDArray) (y : NDArray) -> x.MutGreaterOrEqual(y)) (.>=)
    [<Fact>]
    let lesserEq() = check2 (fun (x : NDArray) (y : NDArray) -> x.MutLesserOrEqual(y)) (.<=) 
                              (fun (x : NDArray) (y : double) -> x.MutLesserOrEqual(y)) (.<=) 
                              (fun (x : NDArray) (y : NDArray) -> x.MutLesserOrEqual(y)) (.<=) 
    [<Fact>]
    let logicalAnd() = check2 (fun (x : NDArray) (y : NDArray) -> x.MutLogicalAnd(y)) (.&&) 
                              (fun (x : NDArray) (y : double) -> x.MutLogicalAnd(y)) (.&&) 
                              (fun (x : NDArray) (y : NDArray) -> x.MutLogicalAndBroadcast(y)) (..&&) 
    [<Fact>]
    let logicalOr() = check2 (fun (x : NDArray) (y : NDArray) -> x.MutLogicalOr(y)) (.||) 
                              (fun (x : NDArray) (y : double) -> x.MutLogicalOr(y)) (.||) 
                              (fun (x : NDArray) (y : NDArray) -> x.MutLogicalOrBroadcast(y)) (..||) 
    [<Fact>]
    let logicalXor() = check2 (fun (x : NDArray) (y : NDArray) -> x.MutLogicalXor(y)) (.^^) 
                              (fun (x : NDArray) (y : double) -> x.MutLogicalXor(y)) (.^^) 
                              (fun (x : NDArray) (y : NDArray) -> x.MutLogicalXorBroadcast(y)) (..^^) 

    [<Fact>]
    let exp() = checkUni (fun x -> x.MutExp()) exp
    [<Fact>]
    let log() = checkUni (fun x -> x.MutLog()) log
    [<Fact>]
    let abs() = checkUni (fun x -> x.MutAbs()) abs
    [<Fact>]
    let atan() = checkUni (fun x -> x.MutAtan()) atan
    [<Fact>]
    let acos() = checkUni (fun x -> x.MutAcos()) acos
    [<Fact>]
    let asin() = checkUni (fun x -> x.MutAsin()) asin
    [<Fact>]
    let ceil() = checkUni (fun x -> x.MutCeiling()) ceil
    [<Fact>]
    let floor() = checkUni (fun x -> x.MutFloor()) floor
    [<Fact>]
    let truncate() = checkUni (fun x -> x.MutTruncate()) truncate
    [<Fact>]
    let round() = checkUni (fun x -> x.MutRound()) round
    [<Fact>]
    let log10() = checkUni (fun x -> x.MutLog10()) log10
    [<Fact>]
    let sqrt() = checkUni (fun x -> x.MutSqrt()) sqrt
    [<Fact>]
    let cos() = checkUni (fun x -> x.MutCos()) cos
    [<Fact>]
    let cosh() = checkUni (fun x -> x.MutCosh()) cosh
    [<Fact>]
    let sin() = checkUni (fun x -> x.MutSin()) sin
    [<Fact>]
    let sinh() = checkUni (fun x -> x.MutSinh()) sinh
    [<Fact>]
    let tan() = checkUni (fun x -> x.MutTan()) tan
    [<Fact>]
    let tanh() = checkUni (fun x -> x.MutTanh()) tanh

module Slicing =  
    let nd (x : float32 seq) = 
        let data = x |> Seq.toArray
        NDArray.CopyFrom(data, [data.Length], CPU(0))
    [<Fact>]
    let ``simple 1d``() = 
        let d1 = [0.0 .. 20.0] |> List.map float32
        let a = nd d1
        let expected = d1.[3 .. 5]
        let actual : float32 [] = (a.[3 .. 5]).ToArray<_>()
        Assert.Equal(expected.Length, actual.Length)
        for i = 0 to expected.Length - 1 do    
            Assert.Equal(double expected.[i], double actual.[i], 6)

    [<Fact>]
    let ``1d no start``() = 
        let d1 = [0.0 .. 20.0] |> List.map float32
        let a = nd d1
        let expected = d1.[.. 5]
        let actual : float32 [] = (a.[.. 5]).ToArray<_>()
        Assert.Equal(expected.Length, actual.Length)
        for i = 0 to expected.Length - 1 do    
            Assert.Equal(double expected.[i], double actual.[i], 6)

    [<Fact>]
    let ``1d no end``() = 
        let d1 = [0.0 .. 20.0] |> List.map float32
        let a = nd d1
        let expected = d1.[2 .. ]
        let actual : float32 [] = (a.[2 ..]).ToArray<_>()
        Assert.Equal(expected.Length, actual.Length)
        for i = 0 to expected.Length - 1 do    
            Assert.Equal(double expected.[i], double actual.[i], 6)

    [<Fact>]
    let ``getItem``() = 
        let d1 = [|0.0 .. 9.0|] |> Array.map float32
        let a = (nd d1).Reshape([2; 5])
        do 
            let actual : float32[] = a.[0].ToArray<_>()
            let expected = d1.[0 .. 4]
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        do 
            let actual : float32[] = a.[1].ToArray<_>()
            let expected = d1.[5 .. ]
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        do 
            let actual : float32[] = a.[0, 0].ToArray<_>()
            let expected = [|d1.[0]|]
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)
        do 
            let actual : float32[] = a.[1, 1].ToArray<_>()
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
            let actual : float32[] = a.ToArray<_>()
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
            let actual : float32[] = a.ToArray<_>()
            Assert.Equal(expected.Length, actual.Length)
            for i = 0 to expected.Length - 1 do    
                Assert.Equal(double expected.[i], double actual.[i], 6)

        check ({0 .. 2 .. 10} |> Seq.map (fun i -> d1.[i])) a.[SliceRange(0L,10L,2L)]


    //[<Fact>]
    //let sliceAssign() = 
    //    let a = new NDArray([1.f .. 100.f], shape = [50; 2], context  = CPU 0)
    //    let bb : float32 [] = a.ToArray<_>()
    //    a.[30 .. 40, *] <- 10000.f
    //    a.[30 .. 40, *] <- a.[0 .. 10, *] 
    //    Assert.Equal(1,2)

module Basic = 

    // issue #19 - empty ndarray tostring causes exception
    [<Fact>]
    let ``NDArray.ToString``() = 
        let nd = NDArray.CopyFrom([|0 .. 10|], CPU(0))
        Assert.Equal("NDArray[11] @cpu(0)", nd.ToString())
        let nd2 = new NDArray()
        Assert.Equal("NDArray[EMPTY]", nd2.ToString())

    [<Fact>]
    let ``AsType int -> double``() = 
        let nd = NDArray.CopyFrom([|0 .. 10|], CPU(0))
        let nd2 = nd.AsType(DataType.Float64)
        Assert.Equal(Some(DataType.Float64), nd2.DataType)
        Assert.Equal(Some(DataType.Int32), nd.DataType)
        Assert.True(nd2.ToArray<_>() = [|0.0 .. 10.0|])
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
        a.MutFull(123.0) |> ignore
        Assert.True(a.ToDoubleArray() = Array.create 10 123.0)

    //https://github.com/kevmal/MXNetSharp/issues/37
    [<Fact>]
    let ``optional NDArray args``() = 
        let a = CPU(0).Arange(0.0,10.0).Reshape(1,-1)
        let b = CPU(0).Zeros [10]
        let w = CPU(0).RandomUniform([10;10], -1.0,1.0)
        let r1 = MX.FullyConnected(a,w,b,numHidden = 10)
        let r2 = MX.FullyConnected(a,w,MX.NoArg(),numHidden = 10, noBias = true)
        let s1 = r1.ToFloat32Array() |> Array.sum
        let s2 = r2.ToFloat32Array() |> Array.sum
        Assert.Equal(s1,s2)

module Main = 
    [<EntryPoint>]
    let main argv = 
        0 