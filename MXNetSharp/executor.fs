namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop
open System.Collections.Generic
 
//[<NoComparison>]
//type GradArray = 
//    | NullOp
//    | WriteTo of NDArray
//    | WriteInplace of NDArray
//    | AddTo of NDArray

[<NoComparison; NoEquality>]
type BindType = 
    | NoGrad of NDArray
    | Delayed of (int [] -> BindType)
    | GradWriteTo of NDArray*NDArray
    | GradWriteInplace of NDArray*NDArray
    | GradAddTo of NDArray*NDArray

type SafeExecutorHandle(owner) = 
    inherit SafeHandle(0n, true)
    new() = new SafeExecutorHandle(true)
    new(ptr,owner) as this = new SafeExecutorHandle(owner) then this.SetHandle(ptr)
    override x.IsInvalid = x.handle <= 0n
    override x.ReleaseHandle() = CApi.MXExecutorFree x.handle = 0
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
    member x.InputBindings = 
        let symbol = match symbol with Some s -> s | _ -> failwith  "Executor symbol is null"
        let names = symbol.ArgumentNames
        let inArgs = match inArgs with Some s -> s | _ -> failwith  "Executor inArgs is null"
        let argGrad = match argGrad with Some s -> s | _ -> failwith  "Executor argGrad is null"
        let gradReqType = match gradReqType with Some s -> s | _ -> failwith  "Executor gradReqType is null"
        assert(names.Length = inArgs.Length)
        assert(names.Length = argGrad.Length)
        assert(names.Length = gradReqType.Length)
        [|
            for i = 0 to names.Length - 1 do
                let name = names.[i]
                let a = inArgs.[i]
                let g = argGrad.[i]
                let t = gradReqType.[i]
                name, 
                    match t with 
                    | NullOp -> NoGrad a
                    | WriteTo -> GradWriteTo(a,g)
                    | WriteInplace -> GradWriteInplace(a,g)
                    | AddTo -> GradAddTo(a,g)
        |]
    member x.Symbol = symbol
    member internal x.UnsafeHandle = handle.UnsafeHandle
    member x.Forward(isTraining : bool) = 
        let isTrain = if isTraining then 1 else 0
        MXExecutor.forward handle.UnsafeHandle isTrain
        MXExecutor.outputs handle.UnsafeHandle |> Array.map (fun h -> new NDArray(h))
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

[<AutoOpen>]
module SymbolExtension =
    type Symbol with 
        member x.Bindings(f) = 
            let inputs = x.InputSymbols
            let bindings = inputs |> Array.map f
            let shapes = 
                (inputs,bindings)
                ||> Array.map2
                    (fun i x ->
                        match x with
                        | NoGrad a
                        | GradWriteTo(a,_)
                        | GradWriteInplace(a,_)
                        | GradAddTo(a,_) -> Some(i.Name, a.Shape)
                        | Delayed _ -> None
                    )
                |> Array.choose id
            let (k,i,d) = MXSymbol.keyShapeToCsrForm uint32 shapes
            let inferResult = MXSymbol.inferShape x.UnsafeHandle k i d
            (inputs, inferResult.InputShapes, bindings)
            |||> Array.map3
                (fun i shape b ->
                    match b with 
                    | Delayed f -> i.Name, shape |> Array.map int |> f
                    | x -> i.Name, x
                )
        member x.Bind(context, f) = 
            let inputs = x.InputSymbols
            let bindings = inputs |> Array.map f
            let shapes = 
                (inputs,bindings)
                ||> Array.map2
                    (fun i x ->
                        match x with
                        | NoGrad a
                        | GradWriteTo(a,_)
                        | GradWriteInplace(a,_)
                        | GradAddTo(a,_) -> Some(i.Name, a.Shape)
                        | Delayed _ -> None
                    )
                |> Array.choose id
            let (k,i,d) = MXSymbol.keyShapeToCsrForm uint32 shapes
            let inferResult = MXSymbol.inferShape x.UnsafeHandle k i d
            let auxStates = 
                inferResult.AuxShapes
            let inArgs, grads, opReqType = 
                (inputs, inferResult.InputShapes, bindings)
                |||> Array.map3
                    (fun i shape b ->
                        let expand x = 
                            match x with
                            | NoGrad a -> a, new NDArray(), NullOp
                            | GradWriteTo(a,g) -> a, g, WriteTo
                            | GradWriteInplace(a,g) -> a, g, WriteInplace
                            | GradAddTo(a,g) -> a, g, AddTo
                            | Delayed _ -> failwith "Unexpected Delayed" //TODO: make ex
                        match b with 
                        | Delayed f -> shape |> Array.map int |> f |> expand
                        | x -> expand x
                    )
                |> Array.unzip3
            new Executor(x,context,inArgs, grads, opReqType, Seq.empty)
