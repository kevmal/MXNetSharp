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

//TODO: NDArray Use safe handle and add IDisposable
type NDArray(handle : CApi.NDArrayHandle) = 
    new() = 
        let h1 = MXNDArray.createNone()
        NDArray(h1)
    new(shape : int seq, context : Context, ?dtype, ?delayAlloc) = 
        let dtype = defaultArg dtype TypeFlag.Float32
        let delayAlloc = defaultArg delayAlloc true
        let shape = shape |> Seq.toArray
        NDArray(MXNDArray.createEx shape context.DeviceType context.DeviceId delayAlloc dtype)
    new(data : float[], shape, context) = 
        let dtype = TypeFlag.Float64
        let delayAlloc = false
        let shape = shape |> Seq.toArray
        let handle = MXNDArray.createEx shape context.DeviceType context.DeviceId delayAlloc dtype
        MXNDArray.syncCopyFromCPU handle data
        NDArray(handle)
    new(data : float seq, shape, context) = NDArray(data |> Seq.toArray, shape, context)
    new(data : float32[], shape, context) = 
        let dtype = TypeFlag.Float32
        let delayAlloc = false
        let shape = shape |> Seq.toArray
        let handle = MXNDArray.createEx shape context.DeviceType context.DeviceId delayAlloc dtype
        MXNDArray.syncCopyFromCPU handle data
        NDArray(handle)
    new(data : float32 seq, shape, context) = NDArray(data |> Seq.toArray, shape, context)
    new(data : float[]) =
        let h1 = MXNDArray.createNone()
        MXNDArray.syncCopyFromCPU h1 data
        NDArray h1
        
    member x.NDArrayHandle : CApi.NDArrayHandle = handle

    member x.Shape = MXNDArray.getShape handle
    member x.Size = x.Shape |> Array.reduce (*)
    member x.Context = MXNDArray.getContext handle

    member x.SyncCopyFromCPU(data : float32 []) = MXNDArray.syncCopyFromCPU handle data

    member x.Set(value : float32) =
        let setValue = AtomicSymbolCreator.FromName "_set_value"
        MXNDArray.imperativeInvokeInto setValue.AtomicSymbolCreatorHandle null [|handle|] [|"src"|] [|value.ToString()|]
        |> ignore

    static member WaitAll() = MXNDArray.waitAll()
    static member ( * )(x : NDArray, y : float32) = 
        let setValue = AtomicSymbolCreator.FromName "_mul_scalar"
        MXNDArray.imperativeInvoke setValue.AtomicSymbolCreatorHandle [|x.NDArrayHandle|] [|"scalar"|] [|string y|] 
        |> Array.head
        |> NDArray
    member x.Substract(y : NDArray) = 
        let setValue = AtomicSymbolCreator.FromName "elemwise_sub"
        let nout = MXNDArray.imperativeInvokeInto setValue.AtomicSymbolCreatorHandle [|x.NDArrayHandle; y.NDArrayHandle|] [|x.NDArrayHandle|] null null
        assert(nout = 1)
        ()
    member x.ToArray() : 'a [] = 
        let a = Array.zeroCreate x.Size
        MXNDArray.syncCopyToCPU handle a
        a
        
