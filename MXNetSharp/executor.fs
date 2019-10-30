namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop
open System.Collections.Generic
 

[<NoComparison>]
type AuxBinding = 
    {
        Name : string
        NDArray : NDArray option
        Shape : int [] option
        DataType : DataType option
        StorageType : StorageType option
    }
    static member Named(name) =     
        {
            Name = name 
            NDArray = None 
            Shape = None 
            DataType = None 
            StorageType = None
        }

[<NoComparison>]
type ArgBinding = 
    {
        Name : string
        NDArray : NDArray option
        Grad : NDArray option
        OpReqType : OpReqType option
        Shape : int [] option
        DataType : DataType option
        StorageType : StorageType option
    }
    static member Named(name) =     
        {
            Name = name 
            NDArray = None 
            Grad = None 
            OpReqType = None 
            Shape = None 
            DataType = None 
            StorageType = None
        }

[<NoComparison>]
type Binding = 
    | AuxBinding of AuxBinding
    | ArgBinding of ArgBinding
    member x.Name = 
        match x with 
        | AuxBinding r -> r.Name
        | ArgBinding r -> r.Name
    member x.Shape = 
        match x with 
        | AuxBinding {Shape = s}
        | ArgBinding {Shape = s} -> s
    member x.DataType = 
        match x with 
        | AuxBinding {DataType = s}
        | ArgBinding {DataType = s} -> s
    member x.WithNDArray ndarray = 
        match x with 
        | AuxBinding(a) -> AuxBinding{a with NDArray = Some ndarray}
        | ArgBinding(a) -> ArgBinding{a with NDArray = Some ndarray}
    static member Arg(name, ?ndarray : NDArray, ?grad : NDArray, ?opReqType : OpReqType, ?shape : int seq, ?dataType : DataType, ?storageType : StorageType) = 
        ArgBinding 
            {ArgBinding.Named name with 
                Name = name
                NDArray = ndarray
                Grad = grad 
                OpReqType = opReqType
                Shape = shape |> Option.map Seq.toArray
                DataType = dataType 
                StorageType = storageType
            }
    static member Aux(name, ?ndarray : NDArray, ?shape : int [], ?dataType : DataType, ?storageType : StorageType) = 
        AuxBinding 
            {AuxBinding.Named name with 
                Name = name
                NDArray = ndarray
                Shape = shape |> Option.map Seq.toArray
                DataType = dataType 
                StorageType = storageType
            }

type BindMap(bindings : IDictionary<string, Binding>) = 
    new() = BindMap(Map.empty)
    member x.TryGetValue(name : string, [<Out>] value : Binding byref) = 
        let scc,v = bindings.TryGetValue(name)
        value <- v
        scc

    member x.WithBindings(newBindings : Binding seq) = 
        let d = Dictionary(bindings)
        newBindings |> Seq.iter (fun b -> d.[b.Name] <- b)
        BindMap d
    member x.InferShapes(symbol : Symbol) =    
        let argNames = symbol.ArgumentNames
        let result = 
            argNames
            |> Array.choose 
                (fun name -> 
                    match bindings.TryGetValue(name) with 
                    | true, ArgBinding {Shape = Some s} -> Some(name, s)
                    | _ -> None)
            |> MXSymbol.keyShapeToCsrForm uint32 
            |||> MXSymbol.inferShapePartial symbol.UnsafeHandle 
        let auxBindings = 
            (symbol.AuxiliaryStateNames, result.AuxShapes)
            ||> Array.map2 
                (fun name shape -> 
                    let shape = shape |> Array.map int
                    match bindings.TryGetValue(name) with
                    | true, AuxBinding(b) -> AuxBinding { b with Shape = Some shape}
                    //| true, _ ->  TODO: Log?
                    | _ -> AuxBinding {AuxBinding.Named name with Shape = Some shape}
                )
        let outBindings = 
            (symbol.OutputNames, result.OutputShapes)
            ||> Array.map2 
                (fun name shape -> 
                    let shape = shape |> Array.map int
                    match bindings.TryGetValue(name) with
                    | true, ArgBinding(a)-> ArgBinding {a with Shape = Some shape }
                    //| true, _ ->  TODO: Log?
                    | _ -> ArgBinding {ArgBinding.Named name with Shape = Some shape}
                )
        let inBindings = 
            (argNames, result.InputShapes)
            ||> Array.map2 
                (fun name shape -> 
                    let shape = shape |> Array.map int
                    match bindings.TryGetValue(name) with
                    | true, ArgBinding(a)-> ArgBinding {a with Shape = Some shape }
                    //| true, _ ->  TODO: Log?
                    | _ -> ArgBinding {ArgBinding.Named name with Shape = Some shape}
                )
        x.WithBindings(seq {yield! inBindings; yield! outBindings; yield! auxBindings})
    member x.InferTypes(symbol : Symbol) =    
        let argNames = symbol.ArgumentNames
        let result = 
            argNames
            |> Array.choose 
                (fun name -> 
                    match bindings.TryGetValue(name) with 
                    | true, ArgBinding {DataType = Some dt} -> Some (name, int dt.TypeFlag)
                    | _ -> None)
            |> Array.unzip
            ||> MXSymbol.inferTypePartial symbol.UnsafeHandle 
        let auxBindings = 
            (symbol.AuxiliaryStateNames, result.AuxTypes)
            ||> Array.map2 
                (fun name t -> 
                    match bindings.TryGetValue(name) with
                    | true, AuxBinding a -> AuxBinding { a with DataType = DataType.FromInt t}
                    //| true, _ ->  TODO: Log?
                    | _ ->  AuxBinding { AuxBinding.Named name with DataType = DataType.FromInt t} 
                )
        let outBindings = 
            (symbol.OutputNames, result.OutputTypes)
            ||> Array.map2 
                (fun name t -> 
                    match bindings.TryGetValue(name) with
                    | true, ArgBinding a -> ArgBinding {a with DataType = DataType.FromInt t}
                    //| true, _ ->  TODO: Log?
                    | _ -> ArgBinding { ArgBinding.Named name with DataType = DataType.FromInt t} 
                )
        let inBindings = 
            (argNames, result.InputTypes)
            ||> Array.map2 
                (fun name t -> 
                    match bindings.TryGetValue(name) with
                    | true, ArgBinding a -> ArgBinding {a with DataType = DataType.FromInt t}
                    //| true, _ ->  TODO: Log?
                    | _ -> ArgBinding { ArgBinding.Named name with DataType = DataType.FromInt t} 
                )
        x.WithBindings(seq {yield! inBindings; yield! outBindings; yield! auxBindings})
    member x.Bindings = bindings
    interface IEnumerable<Binding> with 
        member x.GetEnumerator() = bindings.Values.GetEnumerator()
        member x.GetEnumerator() = bindings.Values.GetEnumerator() :> System.Collections.IEnumerator


module BindMap = 
    let mapAux f (bm : BindMap) = 
        bm
        |> Seq.map 
            (function 
             | AuxBinding a -> f a |> AuxBinding
             | x -> x
            )
        |> Seq.map (fun (x : Binding) -> x.Name, x)
        |> dict 
        |> BindMap
    let mapArg f (bm : BindMap) = 
        bm
        |> Seq.map 
            (function 
             | ArgBinding a -> f a |> ArgBinding
             | x -> x
            )
        |> Seq.map (fun (x : Binding) -> x.Name, x)
        |> dict 
        |> BindMap
    let map f (bm : BindMap) = 
        bm
        |> Seq.map f
        |> Seq.map (fun (x : Binding) -> x.Name, x)
        |> dict 
        |> BindMap
    let ofSeq l = BindMap().WithBindings l
    let inferShapes (s : Symbol) (bm : BindMap) = bm.InferShapes s



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

 
type Executor(handle : SafeExecutorHandle, symbol, context, contextMap, inArgs, argGrad, gradReqType, auxStates, sharedExecutor, outputs) =   
    let mutable disposed = false
    new(symbol : Symbol, context : Context, contextMap : IDictionary<string,Context>, inArgs, argGrad, gradReqType, auxStates, sharedExecutor : Executor option) = 
        let inArgs = inArgs |> Seq.toArray
        let argGrad = argGrad |> Seq.toArray
        let gradReqType = gradReqType |> Seq.toArray
        let auxStates = auxStates |> Seq.toArray
        let inArgsHandles = inArgs |> Array.map (fun (x : NDArray) -> x.NDArrayHandle.UnsafeHandle)
        let argGradHandles = argGrad |> Array.map (fun (x : NDArray) -> x.NDArrayHandle.UnsafeHandle)
        let gradReqTypeHandles = gradReqType |> Array.map (fun (x : OpReqType) -> uint32 x.OpReqTypeInt)
        let auxStatesHandles = auxStates |> Array.map (fun (x : NDArray) -> x.NDArrayHandle.UnsafeHandle)
        let mapKeys,mapDevTypes,mapDevIds = 
            if contextMap.Count = 0 then 
                null,null,null
            else
                contextMap 
                |> Seq.map 
                    (fun kvp ->
                        kvp.Key, int kvp.Value.DeviceType, kvp.Value.DeviceId
                    )
                |> Seq.toArray
                |> Array.unzip3
        let sharedExecutorHandle = 
            match sharedExecutor with 
            | Some x ->
                x.UnsafeHandle
            | None -> 0n
        let h = MXExecutor.bindEX symbol.UnsafeHandle (int context.DeviceType) context.DeviceId mapKeys mapDevTypes mapDevIds inArgsHandles argGradHandles gradReqTypeHandles auxStatesHandles sharedExecutorHandle
        let safeHandle = new SafeExecutorHandle(h, true)
        let outputs = MXExecutor.outputs h |> Array.map (fun h -> new NDArray(new SafeNDArrayHandle(h, true)))
        // NOTE: We need to make sure all references get stored to prevent handles from being freed.
        new Executor(safeHandle,symbol,context,contextMap,inArgs,argGrad,gradReqType,auxStates,sharedExecutor,outputs)
    new(symbol : Symbol, context, inArgs, argGrad, gradReqType, auxStates) = 
        new Executor(symbol, context, Map.empty, inArgs,argGrad,gradReqType,auxStates,None)
    new(symbol : Symbol, context, bindings : BindMap) = 
        let args = symbol.ArgumentNames
        let inArgs, argGrad, gradReqType = 
            args 
            |> Array.map 
                (fun name ->
                    match bindings.TryGetValue(name) with 
                    | true, (ArgBinding b) ->  //TODO: exception clean up
                        let a = match b.NDArray with Some a -> a | None -> failwithf "Must provide %s" name
                        let g = match b.Grad with Some a -> a | None -> failwithf "Must provide %s" name
                        let t = match b.OpReqType with Some a -> a | None -> failwithf "Must provide %s" name
                        a,g,t
                    | _ -> failwithf "No binding for %s" name
                )
            |> Array.unzip3
        let aux = 
            symbol.AuxiliaryStateNames
            |> Array.map 
                (fun name ->
                    match bindings.TryGetValue(name) with 
                    | true, (AuxBinding b) ->  //TODO: exception clean up
                        let a = match b.NDArray with Some a -> a | None -> failwithf "Must provide %s" name
                        a
                    | _ -> failwithf "No binding for %s" name
                )
        new Executor(symbol, context, inArgs, argGrad, gradReqType, aux)
    member x.BindMap =  
        let args = Array.zip3 inArgs argGrad gradReqType
        seq {
            yield!
                (symbol.ArgumentNames, args)
                ||> Seq.map2
                    (fun name (a,g,t) ->
                        ArgBinding 
                            { 
                                Name = name
                                Shape = Some a.Shape
                                NDArray = Some a
                                Grad = Some g
                                OpReqType = Some t
                                //StorageType = Some a.StorageType //TODO: ndarray storage type
                                //DataType = a.DataType //TODO: 
                                StorageType = None 
                                DataType = None
                            }
                    )
            yield!
                (symbol.AuxiliaryStateNames, auxStates)
                ||> Seq.map2
                    (fun name a ->
                        AuxBinding 
                            { 
                                Name = name
                                Shape = Some a.Shape
                                NDArray = Some a
                                //StorageType = Some a.StorageType //TODO: ndarray storage type
                                //DataType = a.DataType //TODO: 
                                StorageType = None 
                                DataType = None
                            }
                    )
            yield!
                (symbol.OutputNames, outputs)
                ||> Seq.map2
                    (fun name a ->
                        ArgBinding 
                            { 
                                Name = name
                                Shape = Some a.Shape
                                NDArray = Some a
                                Grad = None
                                OpReqType = None
                                //StorageType = Some a.StorageType //TODO: ndarray storage type
                                //DataType = a.DataType //TODO: 
                                StorageType = None 
                                DataType = None
                            }
                    )
        }
        |> BindMap.ofSeq
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
    member x.Outputs = outputs
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
        member x.Bind(context, bindmap) = new Executor(x,context,bindmap)