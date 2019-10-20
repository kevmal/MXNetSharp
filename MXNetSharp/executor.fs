namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop
open System.Collections.Generic
 

type SafeExecutorHandle(owner) = 
    inherit SafeHandle(0n, true)
    new() = new SafeExecutorHandle(true)
    new(ptr,owner) as this = new SafeExecutorHandle(owner) then this.SetHandle(ptr)
    override x.IsInvalid = x.handle <= 0n
    override x.ReleaseHandle() = CApi.MXNDArrayFree x.handle = 0
    member internal x.UnsafeHandle = 
        if not x.IsClosed then
            x.handle
        else
            ObjectDisposedException("SafeExecutorHandle", "Executor handle has been closed") |> raise

 
type Executor(handle : SafeExecutorHandle, ?symbol, ?context, ?contextMap, ?inArgs, ?argGrad, ?gradReqType, ?auxStates) =   
    let mutable disposed = false
    new(symbol : Symbol, context : Context, contextMap : IDictionary<string,Context> option, inArgs, argGrad, gradReqType, auxStates, sharedExecutor : Executor option) = 
        let inArgs = inArgs |> Seq.toArray
        let argGrad = argGrad |> Seq.toArray
        let gradReqType = gradReqType |> Seq.toArray
        let auxStates = auxStates |> Seq.toArray
        let inArgsHandles = inArgs |> Array.map (fun (x : NDArray) -> x.NDArrayHandle.UnsafeHandle)
        let argGradHandles = argGrad |> Array.map (fun (x : NDArray) -> x.NDArrayHandle.UnsafeHandle)
        let gradReqTypeHandles = gradReqType |> Array.map (fun (x : OpReqType) -> uint32 x.OpReqTypeInt)
        let auxStatesHandles = auxStates |> Array.map (fun (x : NDArray) -> x.NDArrayHandle.UnsafeHandle)
        let mapKeys,mapDevTypes,mapDevIds = 
            match contextMap with 
            | None -> null,null,null
            | Some(d) -> 
                d 
                |> Seq.map 
                    (fun kvp ->
                        kvp.Key, int kvp.Value.DeviceType, kvp.Value.DeviceId
                    )
                |> Seq.toArray
                |> Array.unzip3
        let sharedExecutorHandle = 
            match sharedExecutor with 
            | Some x -> x.UnsafeHandle
            | None -> 0n
        let h = MXExecutor.bindEX symbol.UnsafeHandle (int context.DeviceType) context.DeviceId mapKeys mapDevTypes mapDevIds inArgsHandles argGradHandles gradReqTypeHandles auxStatesHandles sharedExecutorHandle
        let safeHandle = new SafeExecutorHandle(h, true)
        // NOTE: We need to make sure all references get stored to prevent handles from being freed.
        new Executor(safeHandle,symbol,context,?contextMap = contextMap,inArgs = inArgs,argGrad = argGrad,gradReqType = gradReqType,auxStates = auxStates)
    new(symbol : Symbol, context, inArgs, argGrad, gradReqType, auxStates) = 
        new Executor(symbol, context, None, inArgs,argGrad,gradReqType,auxStates,None)
    member x.Symbol = symbol
    member internal x.UnsafeHandle = handle.UnsafeHandle
    member x.Forward(isTraining : bool) = 
        let isTrain = if isTraining then 1 else 0
        MXExecutor.forward handle.UnsafeHandle isTrain
        //REVIEW: Can we own these handles?
        MXExecutor.outputs handle.UnsafeHandle
        |> Array.map NDArray
    member x.Backward() = 
        MXExecutor.backward handle.UnsafeHandle null
    member x.Backward(grads) = 
        grads
        |> Seq.map (fun (x : NDArray) -> x.NDArrayHandle.UnsafeHandle) 
        |> Seq.toArray
        |> MXExecutor.backward handle.UnsafeHandle
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



