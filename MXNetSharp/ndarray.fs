namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop


type SafeNDArrayHandle(owner) = 
    inherit SafeHandle(0n, true)
    new() = new SafeNDArrayHandle(true)
    new(ptr,owner) as this = new SafeNDArrayHandle(owner) then this.SetHandle(ptr)
    override x.IsInvalid = x.handle <= 0n
    override x.ReleaseHandle() = CApi.MXNDArrayFree x.handle = 0
    member internal x.UnsafeHandle = 
        if not x.IsClosed then
            x.handle
        else
            ObjectDisposedException("SafeNDArrayHandle", "NDArray handle has been closed") |> raise


// From https://github.com/apache/incubator-mxnet/blob/225f71f744ac5e7bd29868b6d3ba0e4fe2527c43/cpp-package/include/mxnet-cpp/base.h#L39
type OpReqType =
    | NullOp = 0
    | WriteTo = 1
    | WriteInplace = 2
    | AddTo = 3

type NDArray(handle : SafeNDArrayHandle) = 
    let mutable disposed = false
    internal new(h : CApi.NDArrayHandle) = new NDArray(new SafeNDArrayHandle(h, true))
    new() = 
        let h1 = MXNDArray.createNone()
        new NDArray(h1)
    new(shape : int seq, context : Context, ?dtype, ?delayAlloc) = 
        let dtype = defaultArg dtype TypeFlag.Float32
        let delayAlloc = defaultArg delayAlloc true
        let shape = shape |> Seq.toArray
        new NDArray(MXNDArray.createEx shape context.DeviceType context.DeviceId delayAlloc dtype)
    new(data : float[], shape, context) = 
        let dtype = TypeFlag.Float64
        let delayAlloc = false
        let shape = shape |> Seq.toArray
        let handle = MXNDArray.createEx shape context.DeviceType context.DeviceId delayAlloc dtype
        MXNDArray.syncCopyFromCPU handle data
        new NDArray(handle)
    new(data : float seq, shape, context) = new NDArray(data |> Seq.toArray, shape, context)
    new(data : float32[], shape, context) = 
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
    new(data : byte[], shape, context) = 
        let dtype = TypeFlag.Uint8
        let delayAlloc = false
        let shape = shape |> Seq.toArray
        let handle = MXNDArray.createEx shape context.DeviceType context.DeviceId delayAlloc dtype
        MXNDArray.syncCopyFromCPU handle data
        new NDArray(handle)
        
    member x.NDArrayHandle = handle

    member x.Shape = MXNDArray.getShape handle.UnsafeHandle
    member x.Size = x.Shape |> Array.reduce (*)
    member x.Context = MXNDArray.getContext handle.UnsafeHandle

    member x.CopyTo(destination : NDArray) = 
        MXNDArray.syncCopyFromNDArray destination.NDArrayHandle.UnsafeHandle x.NDArrayHandle.UnsafeHandle -1

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
    static member ( * )(x : NDArray, y : float32) = 
        let setValue = AtomicSymbolCreator.FromName "_mul_scalar"
        MXNDArray.imperativeInvoke setValue.AtomicSymbolCreatorHandle [|x.NDArrayHandle.UnsafeHandle|] [|"scalar"|] [|string y|] 
        |> Array.head
        |> (fun x -> new NDArray(x))
    member x.Substract(y : NDArray) = 
        let setValue = AtomicSymbolCreator.FromName "elemwise_sub"
        let nout = MXNDArray.imperativeInvokeInto setValue.AtomicSymbolCreatorHandle [|x.NDArrayHandle.UnsafeHandle; y.NDArrayHandle.UnsafeHandle|] [|x.NDArrayHandle.UnsafeHandle|] null null
        assert(nout = 1)
        ()
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


    
