namespace MXNETSharp.Tests.Interop

#nowarn "9"
#nowarn "988"

open MXNetSharp.Interop
open Xunit
open MXNetSharp

module MXLib =
    [<Fact>]
    let ``basic MXLib calls``() =
        // Mainly looking for no exceptions
        Assert.NotEqual(0, MXLib.getVersion())
        Assert.NotEqual(-1, MXLib.getGpuCount())
        let features = MXLib.infoFeatures()
        Assert.NotEqual(0, features.Length)
        MXLib.randomSeed(32432)
        let opNames = MXLib.listAllOpNames()
        Assert.NotEmpty opNames

    [<Fact>]
    let ``functions``() = 
        let fs = MXLib.listFunctions()
        Assert.NotEmpty(fs)
        for h in fs do 
            let info = MXLib.funcGetInfo h
            Assert.NotEqual(0, info.Name.Length)
            for arg in info.Arguments do 
                Assert.NotEqual(0, arg.Name.Length)
                Assert.NotEqual(0, arg.TypeInfo.Length)

module MXSymbol =            
    [<Fact>]
    let ``atomic symbols``() = 
        let xs = MXSymbol.listAtomicSymbolCreators()
        Assert.NotEmpty(xs)
        for h in xs do 
            let name = MXSymbol.getAtomicSymbolName h
            let info = MXSymbol.getAtomicSymbolInfo h
            Assert.Equal(info.Name, name)
            for arg in info.Arguments do 
                Assert.NotEqual(0, arg.Name.Length)
                Assert.NotEqual(0, arg.TypeInfo.Length)

    [<Fact>]
    let ``simple create symbol``() = 
        let h = MXNDArray.createEx [|100;10|] DeviceTypeEnum.CPU 0 false TypeFlag.Float32
        MXNDArray.syncCopyFromCPU h [|1.f .. 1000.f|]
        //let h2 = MXNDArray.createEx [|1|] DeviceType.CPU 0 false TypeFlag.Float32
        //MXNDArray.syncCopyFromCPU h [|1.f|]
        let creator = MXSymbol.listAtomicSymbolCreators() |> Seq.find (fun x -> MXSymbol.getAtomicSymbolName x = "_plus_scalar")
        let mySymbol = MXSymbol.createAtomicSymbol creator [|"scalar"|] [|"1.0"|]
        Assert.True(MXSymbol.listAttr mySymbol = [|"$scalar"|])
        Assert.Equal(Some("1.0"), MXSymbol.getAttr mySymbol "scalar")
        Assert.Empty(MXSymbol.getInputSymbols mySymbol)
        let inputVariable = MXSymbol.createVariable "myInput"
        MXSymbol.compose mySymbol "mySymbol" null [|inputVariable|]
        let inputSymbols = MXSymbol.getInputSymbols mySymbol
        Assert.Equal(1, inputSymbols.Length)
        Assert.Equal(Some("myInput"), MXSymbol.getName inputSymbols.[0])
        MXSymbol.free mySymbol


module MXNDArray = 
    [<Fact>]
    let none() = 
        let h1 = MXNDArray.createNone()
        Assert.Equal(TypeFlag.None, MXNDArray.getDType h1)
        MXNDArray.free h1

    [<Fact>]
    let ``many none``() = 
        Array.init 1000 (fun _ -> MXNDArray.createNone()) |> Array.iter MXNDArray.free

    [<Fact>]
    let ``create getShape and free 10x10``() =
        let h = MXNDArray.create [|10; 10|] DeviceTypeEnum.CPU 0 false 
        Assert.True(MXNDArray.getShape h = [|10;10|])
        Assert.Equal(TypeFlag.Float32, MXNDArray.getDType h)
        MXNDArray.free h
    
    [<Fact>]
    let ``create getShape and free 10000x111``() =
        let h = MXNDArray.create [|10000; 111|] DeviceTypeEnum.CPU 0 false 
        Assert.True(MXNDArray.getShape h = [|10000;111|])
        Assert.Equal(TypeFlag.Float32, MXNDArray.getDType h)
        MXNDArray.free h

    [<Fact>]
    let ``createEx float32 getShape and free 10x10``() =
        let h = MXNDArray.createEx [|10; 10|] DeviceTypeEnum.CPU 0 false TypeFlag.Float32
        Assert.True(MXNDArray.getShape h = [|10;10|])
        Assert.Equal(TypeFlag.Float32, MXNDArray.getDType h)
        MXNDArray.free h

    [<Fact>]
    let ``createEx float32 getShape and free 10000x111``() =
        let h = MXNDArray.createEx [|10000; 111|] DeviceTypeEnum.CPU 0 false TypeFlag.Float32
        Assert.True(MXNDArray.getShape h = [|10000;111|])
        Assert.Equal(TypeFlag.Float32, MXNDArray.getDType h)
        MXNDArray.free h


    [<Fact>]
    let ``createEx float32 getShape and free 5x5x5x5x5x5``() =
        let h = MXNDArray.createEx [|5;5;5;5;5;5|] DeviceTypeEnum.CPU 0 false TypeFlag.Float32
        Assert.True(MXNDArray.getShape h = [|5;5;5;5;5;5|])
        Assert.Equal(TypeFlag.Float32, MXNDArray.getDType h)
        MXNDArray.free h


    [<Fact>]
    let ``createEx float64 getShape and free 10x10``() =
        let h = MXNDArray.createEx [|10; 10|] DeviceTypeEnum.CPU 0 false TypeFlag.Float64
        Assert.True(MXNDArray.getShape h = [|10;10|])
        Assert.Equal(TypeFlag.Float64, MXNDArray.getDType h)
        MXNDArray.free h                
            
    [<Fact>]
    let ``createEx float16 getShape and free 10x10``() =
        let h = MXNDArray.createEx [|10; 10|] DeviceTypeEnum.CPU 0 false TypeFlag.Float16
        Assert.True(MXNDArray.getShape h = [|10;10|])
        Assert.Equal(TypeFlag.Float16, MXNDArray.getDType h)
        MXNDArray.free h                

    [<Fact>]
    let ``createEx int32 getShape and free 10x10``() =
        let h = MXNDArray.createEx [|10; 10|] DeviceTypeEnum.CPU 0 false TypeFlag.Int32
        Assert.True(MXNDArray.getShape h = [|10;10|])
        Assert.Equal(TypeFlag.Int32, MXNDArray.getDType h)
        MXNDArray.free h                
    
    [<Fact>]
    let ``createEx int64 getShape and free 10x10``() =
        let h = MXNDArray.createEx [|10; 10|] DeviceTypeEnum.CPU 0 false TypeFlag.Int64
        Assert.True(MXNDArray.getShape h = [|10;10|])
        Assert.Equal(TypeFlag.Int64, MXNDArray.getDType h)
        MXNDArray.free h                

    [<Fact>]
    let ``createEx int8 getShape and free 10x10``() =
        let h = MXNDArray.createEx [|10; 10|] DeviceTypeEnum.CPU 0 false TypeFlag.Int8
        Assert.True(MXNDArray.getShape h = [|10;10|])
        Assert.Equal(TypeFlag.Int8, MXNDArray.getDType h)
        MXNDArray.free h                

    [<Fact>]
    let ``createEx uint8 getShape and free 10x10``() =
        let h = MXNDArray.createEx [|10; 10|] DeviceTypeEnum.CPU 0 false TypeFlag.Uint8
        Assert.True(MXNDArray.getShape h = [|10;10|])
        Assert.Equal(TypeFlag.Uint8, MXNDArray.getDType h)
        MXNDArray.free h                

    [<Fact>]
    let ``init NDArray from float32 array``() = 
        let h = MXNDArray.createEx [|100;10|] DeviceTypeEnum.CPU 0 false TypeFlag.Float32
        MXNDArray.syncCopyFromCPU h [|1.f .. 1000.f|]
        Assert.True(MXNDArray.getShape h = [|100;10|])
        Assert.Equal(TypeFlag.Float32, MXNDArray.getDType h)
        MXNDArray.free h

    [<Fact>]
    let ``get context``() = 
        let h = MXNDArray.createEx [|100;10|] DeviceTypeEnum.CPU 0 false TypeFlag.Float32
        let struct(t,id) = MXNDArray.getContext h
        Assert.Equal(DeviceTypeEnum.CPU,t)
        Assert.Equal(0,id)
        MXNDArray.free h

    [<Fact>]
    let getData() = 
        let h = MXNDArray.createEx [|100;10|] DeviceTypeEnum.CPU 0 false TypeFlag.Float32
        let srcArray = [|1.f .. 1000.f|]
        MXNDArray.syncCopyFromCPU h srcArray
        let dataptr = MXNDArray.getData h |> NativeInterop.NativePtr.ofNativeInt
        let a : float32 [] = Array.zeroCreate 1000
        for i = 0 to a.Length - 1 do
            a.[i] <- NativeInterop.NativePtr.get dataptr i
        Assert.True((a = srcArray))
        MXNDArray.free h


    [<Fact>]
    let ``reshape``() = 
        let h = MXNDArray.createEx [|100;10|] DeviceTypeEnum.CPU 0 false TypeFlag.Float32
        let srcArray = [|1.f .. 1000.f|]
        MXNDArray.syncCopyFromCPU h srcArray
        Assert.True(MXNDArray.getShape h = [|100;10|])
        Assert.Equal(TypeFlag.Float32, MXNDArray.getDType h)
        let h2 = MXNDArray.reshape h [|1000|]
        Assert.True(MXNDArray.getShape h2 = [|1000|])
        Assert.Equal(TypeFlag.Float32, MXNDArray.getDType h)
        let dataptr = MXNDArray.getData h2 |> NativeInterop.NativePtr.ofNativeInt
        let a : float32 [] = Array.zeroCreate 1000
        for i = 0 to a.Length - 1 do
            a.[i] <- NativeInterop.NativePtr.get dataptr i
        Assert.True((a = srcArray))
        MXNDArray.free h
        MXNDArray.free h2



    [<Fact>]
    let ``simple MXImperativeInvoke``() = 
        let h = MXNDArray.createEx [|100;10|] DeviceTypeEnum.CPU 0 false TypeFlag.Float32
        MXNDArray.syncCopyFromCPU h [|1.f .. 1000.f|]
        MXNDArray.waitAll()
        //let h2 = MXNDArray.createEx [|1|] DeviceType.CPU 0 false TypeFlag.Float32
        //MXNDArray.syncCopyFromCPU h [|1.f|]
        let creator = MXSymbol.listAtomicSymbolCreators() |> Seq.find (fun x -> MXSymbol.getAtomicSymbolName x = "_plus_scalar")
        let outputs = MXNDArray.imperativeInvoke creator [|h|] [|"scalar"|] [|"1.0"|]
        Assert.Equal(1, outputs.Length)
        let o = outputs.[0]
        MXNDArray.waitAll()
        let dataptr = MXNDArray.getData o |> NativeInterop.NativePtr.ofNativeInt
        let a : float32 [] = Array.zeroCreate 1000
        for i = 0 to a.Length - 1 do
            a.[i] <- NativeInterop.NativePtr.get dataptr i
        Assert.True((a |> Array.map (round >> int) = [|2 .. 1001|]))
        MXNDArray.free h 
        MXNDArray.free o

