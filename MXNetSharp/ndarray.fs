namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop



module internal Helper = 
    let inline (<--) x y = x, Util.valueString y

open Helper

type NDArray(handle : SafeNDArrayHandle) = 
    let mutable disposed = false
    static let invoke opName inputs parameters =
        let creator = AtomicSymbolCreator.FromName opName
        let inputs = inputs |> Array.map (fun (x : NDArray) -> (x.NDArrayHandle : SafeNDArrayHandle).UnsafeHandle)
        let pkeys,pvals = parameters |> Array.unzip
        MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle inputs pkeys pvals
        |> Array.map (fun x -> new NDArray(x))
    static let invoke1 opName inputs parameters = invoke opName inputs parameters |> Array.head
    static let mutInvoke (out : NDArray) opName inputs parameters =
        let creator = AtomicSymbolCreator.FromName opName
        let inputs = inputs |> Array.map (fun (x : NDArray) -> (x.NDArrayHandle : SafeNDArrayHandle).UnsafeHandle)
        let pkeys,pvals = parameters |> Array.unzip
        let outcount = MXNDArray.imperativeInvokeInto creator.AtomicSymbolCreatorHandle [|(out.NDArrayHandle : SafeNDArrayHandle).UnsafeHandle|] inputs pkeys pvals
        assert(outcount = 1)
        out
    internal new(h : CApi.NDArrayHandle) = new NDArray(new SafeNDArrayHandle(h, true))
    new() = 
        let h1 = MXNDArray.createNone()
        new NDArray(h1)
    new(shape : int seq, context : Context, ?dtype, ?delayAlloc) = 
        let dtype = defaultArg dtype TypeFlag.Float32
        let delayAlloc = defaultArg delayAlloc true
        let shape = shape |> Seq.toArray
        new NDArray(MXNDArray.createEx shape context.DeviceType context.DeviceId delayAlloc dtype)
    new(data : float[], shape, context : Context) = 
        let dtype = TypeFlag.Float64
        let delayAlloc = false
        let shape = shape |> Seq.toArray
        let handle = MXNDArray.createEx shape context.DeviceType context.DeviceId delayAlloc dtype
        MXNDArray.syncCopyFromCPU handle data
        new NDArray(handle)
    new(data : float seq, shape, context : Context) = new NDArray(data |> Seq.toArray, shape, context)
    new(data : float32[], shape, context : Context) = 
        let dtype = TypeFlag.Float32
        let delayAlloc = false
        let shape = shape |> Seq.toArray
        let handle = MXNDArray.createEx shape context.DeviceType context.DeviceId delayAlloc dtype
        MXNDArray.syncCopyFromCPU handle data
        new NDArray(handle)
    new(data : float32 seq, shape, context) = new NDArray(data |> Seq.toArray, shape, context)
    new(data : float[]) =
        let h1 = MXNDArray.createNone()
        MXNDArray.syncCopyFromCPU h1 data
        new NDArray(h1)
    new(data : byte[], shape, context : Context) = 
        let dtype = TypeFlag.Uint8
        let delayAlloc = false
        let shape = shape |> Seq.toArray
        let handle = MXNDArray.createEx shape context.DeviceType context.DeviceId delayAlloc dtype
        MXNDArray.syncCopyFromCPU handle data
        new NDArray(handle)
        
    member x.NDArrayHandle = handle
    member x.UnsafeHandle = x.NDArrayHandle.UnsafeHandle

    member x.Shape = MXNDArray.getShape handle.UnsafeHandle
    member x.Size = x.Shape |> Array.reduce (*)
    member x.Context = MXNDArray.getContext handle.UnsafeHandle

    member x.CopyTo(destination : NDArray) = 
        MXNDArray.syncCopyFromNDArray destination.NDArrayHandle.UnsafeHandle x.NDArrayHandle.UnsafeHandle -1

    member x.CopyTo(deviceContext : Context) = 
        let destination = new NDArray(x.Shape, deviceContext, delayAlloc = true)
        x.CopyTo(destination)
        destination

    member x.SyncCopyFromCPU(data : float32 []) = MXNDArray.syncCopyFromCPU handle.UnsafeHandle data

    member x.Set(value : float32) =
        let setValue = AtomicSymbolCreator.FromName "_set_value"
        MXNDArray.imperativeInvokeInto setValue.AtomicSymbolCreatorHandle null [|handle.UnsafeHandle|] [|"src"|] [|value.ToString()|]
        |> ignore
    static member Load(file : string) = 
        let names,handles = MXNDArray.load file
        let arrs = handles |> Array.map (fun h -> new NDArray(h))
        Array.zip names arrs |> dict
    static member WaitAll() = MXNDArray.waitAll()
    static member ( * )(x : NDArray, y : float) = invoke1 "_mul_scalar" [|x|] [|"scalar" <-- y|]
    static member ( * )(y : float, x : NDArray) = invoke1 "_mul_scalar" [|x|] [|"scalar" <-- y|]
    static member ( .* )(x : NDArray, y : NDArray) = invoke1 "broadcast_mul" [|x; y|] Array.empty 
    member x.MutMultiply(y : float) = mutInvoke x "_mul_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutMultiply(y : NDArray) = mutInvoke x "elemwise_mul" [|x; y|]
    member x.MutMultiplyBroadcast(y : NDArray) = mutInvoke x "broadcast_mul" [|x; y|] 

       
    static member (~-)(x : NDArray) = x * -1.0
    member x.MutNegate() = x.MutMultiply(-1.0)

    static member (-)(x : NDArray, y : float) = invoke1 "_minus_scalar" [|x|] [|"scalar" <-- y|]
    static member (-)(y : float, x : NDArray) = invoke1 "_rminus_scalar" [|x|] [|"scalar" <-- y|]
    static member (-)(x : NDArray, y : NDArray) = invoke1 "elemwise_sub" [|x; y|] Array.empty 
    static member (.-)(x : NDArray, y : NDArray) = invoke1 "broadcast_sub" [|x; y|] Array.empty 
    member x.MutSubstract(y : float) = mutInvoke x "_sub_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutSubstract(y : NDArray) = mutInvoke x "elemwise_sub" [|x; y|]
    member x.MutSubstractFrom(y : float) = mutInvoke x "_rminus_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutSubstractFrom(y : NDArray) = mutInvoke x "elemwise_sub" [|y; x|]
    member x.MutSubstractBroadcast(y : NDArray) = mutInvoke x "broadcast_sub" [|x; y|] 
    member x.MutSubstractBroadcastFrom(y : NDArray) = mutInvoke x "broadcast_sub" [|y; x|] 

    
    static member (+)(x : NDArray, y : float) = invoke1 "_plus_scalar" [|x|] [|"scalar" <-- y|]
    static member (+)(y : float, x : NDArray) = invoke1 "_plus_scalar" [|x|] [|"scalar" <-- y|]
    static member (+)(x : NDArray, y : NDArray) = invoke1 "elemwise_add" [|x; y|] Array.empty 
    static member (.+)(x : NDArray, y : NDArray) = invoke1 "broadcast_plus" [|x; y|] Array.empty 
    member x.MutPlus(y : float) = mutInvoke x "_plus_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutPlus(y : NDArray) = mutInvoke x "elemwise_add" [|x; y|]
    member x.MutPlusBroadcast(y : NDArray) = mutInvoke x "broadcast_add" [|x; y|] 

    
    static member (/)(x : NDArray, y : float) = invoke1 "_div_scalar" [|x|] [|"scalar" <-- y|]
    static member (/)(y : float, x : NDArray) = invoke1 "_rdiv_scalar" [|x|] [|"scalar" <-- y|]
    static member (/)(x : NDArray, y : NDArray) = invoke1 "elemwise_div" [|x; y|] Array.empty 
    static member (./)(x : NDArray, y : NDArray) = invoke1 "broadcast_div" [|x; y|] Array.empty 
    member x.MutDividedBy(y : float) = mutInvoke x "_div_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutDividedBy(y : NDArray) = mutInvoke x "elemwise_div" [|x; y|]
    member x.MutDividedInto(y : float) = mutInvoke x "_rdiv_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutDividedInto(y : NDArray) = mutInvoke x "elemwise_div" [|y; x|]
    member x.MutDividedBroadcastBy(y : NDArray) = mutInvoke x "broadcast_div" [|x; y|] 
    member x.MutDividedBroadcastInto(y : NDArray) = mutInvoke x "broadcast_div" [|y; x|] 

    member x.SwapAxis(dim1 : int, dim2 : int) = invoke1 "SwapAxis" [|x|] [|"dim1" <-- dim1; "dim2" <-- dim2|]
    member x.Reshape([<ParamArray>] dims : int []) = invoke1 "Reshape" [|x|] [|"shape" <-- dims|]
    member x.Reshape(dims : int seq) = invoke1 "Reshape" [|x|] [|"shape" <-- dims|]
    
    member x.ToArray() : 'a [] = 
        let a = Array.zeroCreate x.Size
        MXNDArray.syncCopyToCPU handle.UnsafeHandle a
        a
    override x.ToString() = sprintf "NDArray[%s]" (x.Shape |> Array.map string |> String.concat ",")
    member x.Dispose(disposing) = 
        if not disposed then 
            if disposing then 
                handle.Dispose()
        disposed <- true
    member x.Dispose() = 
        x.Dispose(true)
        GC.SuppressFinalize(x)
    interface IDisposable with  
        member x.Dispose() = x.Dispose()



