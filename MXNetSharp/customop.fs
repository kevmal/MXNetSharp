namespace MXNetSharp.Operator
#nowarn "9"

open System.Collections.Generic
open System.Runtime.InteropServices

open MXNetSharp
open MXNetSharp.Interop
open System
open System.Net
open System.IO
open System.IO.Compression

open MXNetSharp.Interop
open MXNetSharp.PrimitiveOperators
open MXNetSharp.Interop.CApi
open System.Collections.Concurrent



type BackwardStorageTypes = 
    {
        InputGrad : StorageType []
        OutputGrad : StorageType []
        Input : StorageType []
        Output : StorageType []
        Auxiliary : StorageType []
    }
    
type ICustomOperation = 
    abstract member Forward : isTrain : bool *
                              req : OpReqType [] *
                              inData : NDArray [] *
                              outData : NDArray [] *
                              auxData : NDArray [] -> unit
    abstract member Backward : req : OpReqType [] *
                               inData : NDArray [] *
                               outData : NDArray [] *
                               inGrad : NDArray [] *
                               outGrad : NDArray [] *
                               auxData : NDArray [] -> unit
    
type ICustomOperationProperties = 
    abstract member ListArguments : unit -> string []
    abstract member ListOutputs : unit -> string []
    abstract member ListAuxiliaryStates : unit -> string []
    abstract member InferShape : inShape : int [][] -> int[][]*int[][]*int[][]
    abstract member InferBackwardStorageType : storageTypes : BackwardStorageTypes -> unit
    abstract member InferStorageType : inputStorageTypes : StorageType[] -> StorageType[] *StorageType[]*StorageType[] 
    abstract member InferType : inType : TypeFlag[] -> TypeFlag[]*TypeFlag[]*TypeFlag[]
    abstract member DeclareBackwardDependency : outGrad : int[] * inData : int[] * OutData : int[] -> int[]
    abstract member CreateOperator : context : Context * inShapes : int[][] * inDataTypes : TypeFlag[] -> ICustomOperation
    
[<AbstractClass>]
type CustomOperation() = 
    abstract member ListArguments : unit -> string []
    abstract member ListOutputs : unit -> string []
    abstract member ListAuxiliaryStates : unit -> string []
    abstract member InferShape : inShape : int [][] -> int[][]*int[][]*int[][]
    abstract member InferBackwardStorageType : storageTypes : BackwardStorageTypes -> unit
    abstract member InferStorageType : inputStorageTypes : StorageType[] -> StorageType[] *StorageType[]*StorageType[] 
    abstract member InferType : inType : TypeFlag[] -> TypeFlag[]*TypeFlag[]*TypeFlag[]
    abstract member DeclareBackwardDependency : outGrad : int[] * inData : int[] * OutData : int[] -> int[]
    abstract member CreateOperator : context : Context * inShapes : int[][] * inDataTypes : TypeFlag[] -> ICustomOperation
    default this.DeclareBackwardDependency(outGrad: int [], inData: int [], outData: int []): int [] = 
        [|
           yield! outGrad
           yield! inData
           yield! outData
        |]
    default this.InferBackwardStorageType(storageTypes : BackwardStorageTypes): unit = 
        for i = 0 to storageTypes.Auxiliary.Length - 1 do storageTypes.Auxiliary.[i] <- StorageType.Default
        for i = 0 to storageTypes.Input.Length - 1 do storageTypes.Input.[i] <- StorageType.Default
        for i = 0 to storageTypes.Output.Length - 1 do storageTypes.Output.[i] <- StorageType.Default
        for i = 0 to storageTypes.InputGrad.Length - 1 do storageTypes.InputGrad.[i] <- StorageType.Default
        for i = 0 to storageTypes.OutputGrad.Length - 1 do storageTypes.OutputGrad.[i] <- StorageType.Default
    default this.InferShape(inShape: int [] []): int [] [] * int [] [] * int [] [] = 
        let outType = this.ListOutputs() |> Array.map (fun x -> inShape.[0])
        inShape, outType, Array.empty
    default this.InferStorageType(inputStorageTypes : StorageType []): StorageType [] * StorageType [] * StorageType [] = 
        let outType = this.ListOutputs() |> Array.map (fun x -> StorageType.Default)
        let auxType = this.ListAuxiliaryStates() |> Array.map (fun x -> StorageType.Default)
        inputStorageTypes, outType, auxType
    default this.InferType(inType: TypeFlag []): TypeFlag [] * TypeFlag [] * TypeFlag [] = 
        let outType = this.ListOutputs() |> Array.map (fun x -> inType.[0])
        let auxType = this.ListAuxiliaryStates() |> Array.map (fun x -> inType.[0])
        inType, outType, auxType
    default this.ListArguments(): string [] = [|"data"|]
    default this.ListAuxiliaryStates(): string [] = Array.empty
    default this.ListOutputs(): string [] = [|"output"|]
    interface ICustomOperationProperties with
        member this.CreateOperator(context: Context, inShapes: int [] [], inDataTypes: TypeFlag []): ICustomOperation = 
            this.CreateOperator(context,inShapes,inDataTypes)
        member this.DeclareBackwardDependency(outGrad: int [], inData: int [], outData: int []): int [] = 
            this.DeclareBackwardDependency(outGrad, inData, outData)
        member this.InferBackwardStorageType(storageTypes: BackwardStorageTypes): unit = 
            this.InferBackwardStorageType(storageTypes)
        member this.InferShape(inShape: int [] []): int [] [] * int [] [] * int [] [] = 
            this.InferShape(inShape)
        member this.InferStorageType(inputStorageTypes: StorageType []): StorageType [] * StorageType [] * StorageType [] = 
            this.InferStorageType(inputStorageTypes)
        member this.InferType(inType: TypeFlag []): TypeFlag [] * TypeFlag [] * TypeFlag [] = 
            this.InferType(inType)
        member this.ListAuxiliaryStates(): string [] = 
            this.ListAuxiliaryStates()
        member this.ListOutputs(): string [] = 
            this.ListOutputs()
        member x.ListArguments() = 
            x.ListArguments()

type SafeHGlobal internal () = 
    inherit SafeHandle(0n, true)
    new(sz : int) as this = 
        new SafeHGlobal() then 
            this.SetHandle(Marshal.AllocHGlobal sz)
    static member AnsiString(str : string) = 
        let s = new SafeHGlobal()
        s.SetHandle(Marshal.StringToHGlobalAnsi str)
        s
    override x.IsInvalid = x.handle <= 0n
    override x.ReleaseHandle() = 
        Marshal.FreeHGlobal x.handle
        true
    member internal x.Pointer = 
        if not x.IsClosed then
            x.handle
        else
            ObjectDisposedException("SafeHGlobal", "HGlobal ptr has been freed") |> raise

type ResourceType = 
    | Disposable of IDisposable
    | ReferenceHolder of obj

type ResourceTracker(id, name) = 
    static let mutable idCounter = 0L
    static let lookup = ConcurrentDictionary<int64, ResourceTracker>()
    static let delete = 
        CustomOpDelFunc(fun id -> 
            let id = int64 id
            let scc,rt = lookup.TryGetValue(id)
            if scc then 
                lookup.TryRemove(id) |> ignore
                rt.Dispose()
                true
            else 
                false
        )
    let mutable disposed = false
    let lck = obj()
    let mutable resources = ResizeArray()
    let cache = Dictionary<string, nativeint>()
    static member CreateStored(name) =
        let id = Threading.Interlocked.Increment &idCounter
        let o = new ResourceTracker(id, name)
        lookup.[id] <- o
        o
    member x.Id : int64 = id
    member x.Alloc(size : int) = 
        lock(lck)
            (fun _ -> 
                if disposed || isNull resources then 
                    ObjectDisposedException("ResourceTracker", sprintf "ResourceTracker for %s has been disposed." name) |> raise
                let ptr = new SafeHGlobal(size)
                resources.Add (Disposable ptr)
                ptr.Pointer
            )
    member x.StringArray(strs : string []) = 
        lock(lck)
            (fun _ ->   
                if disposed || isNull resources then 
                    ObjectDisposedException("ResourceTracker", sprintf "ResourceTracker for %s has been disposed." name) |> raise
                let ptrs = Array.zeroCreate (strs.Length + 1) 
                for i = 0 to strs.Length - 1 do 
                    let strPtr = SafeHGlobal.AnsiString(strs.[i]) 
                    ptrs.[i] <- strPtr.Pointer
                    resources.Add (Disposable(strPtr))
                let ptr = x.Alloc(ptrs.Length*sizeof<IntPtr>)
                Marshal.Copy(ptrs, 0, ptr, ptrs.Length)
                ptr
            )
    member x.CachedStringArray(strs : string []) = 
        let key = strs |> Seq.map (fun x -> x.Replace("|", "||")) |> String.concat "|"
        lock(lck)
            (fun _ ->   
                if disposed || isNull resources then 
                    ObjectDisposedException("ResourceTracker", sprintf "ResourceTracker for %s has been disposed." name) |> raise
                let scc,v = cache.TryGetValue(key)
                if scc then 
                    v
                else 
                    let ptr = x.StringArray(strs)
                    cache.[key] <- ptr
                    ptr
            )
    member x.DelgatePointer(d : Delegate) = 
        lock(lck)
            (fun _ ->   
                if disposed || isNull resources then 
                    ObjectDisposedException("ResourceTracker", sprintf "ResourceTracker for %s has been disposed." name) |> raise
                resources.Add(ReferenceHolder d)
            )
        Marshal.GetFunctionPointerForDelegate d
    member x.WriteMxCallbackList(ptr :  MXCallbackList byref, l : (Delegate*nativeint) []) =
        let cbPtrArray=
            [|
                Marshal.GetFunctionPointerForDelegate delete
                yield! l |> Seq.map fst |> Seq.map (x.DelgatePointer)
            |]
        let cbPtr = x.Alloc(cbPtrArray.Length * sizeof<IntPtr>)
        Marshal.Copy(cbPtrArray,0,cbPtr,cbPtrArray.Length)
        let ctxPtrArray = 
            [|
                nativeint id
                yield! l |> Seq.map snd
            |]
        let ctxPtr = x.Alloc(ctxPtrArray.Length * sizeof<IntPtr>)
        Marshal.Copy(ctxPtrArray,0,ctxPtr,ctxPtrArray.Length)
        let N = cbPtrArray.Length
        ptr.num_callbacks <- N
        ptr.callbacks <- cbPtr
        ptr.contexts <- ctxPtr
    member x.Dispose(disposing) = 
        if not disposed then 
            if disposing then 
                lock(lck)
                    (fun _ -> 
                        if isNull resources then () else
                        resources
                        |> Seq.iter 
                            (function 
                             | Disposable d -> 
                                d.Dispose()
                             | ReferenceHolder _ -> 
                                ()
                            )
                        resources.Clear()
                        cache.Clear()
                        resources <- null
                    )
            disposed <- true
    member x.Dispose() = 
        x.Dispose(true)
        GC.SuppressFinalize(x)
    interface IDisposable with  
        member x.Dispose() = x.Dispose()

module CustomOp = 
    let register name (makeOpProps : IDictionary<string, string> -> ICustomOperationProperties) =
        let rt = ResourceTracker.CreateStored name
        let creator = CustomOpPropCreator(fun opType argCount keys values cbList -> 
            assert(opType = name)
            let args = 
                let keys = Helper.readStringArray argCount keys
                let values = Helper.readStringArray argCount values
                Array.zip keys values
                |> dict
            let opProp : ICustomOperationProperties = makeOpProps args
            let inferShape = 
                CustomOpInferShapeFunc(fun numTensor tensorDimsPtr tensorShapes state ->
                    let tensorDims : int [] = Helper.readStructArray numTensor tensorDimsPtr
                    let inCount = opProp.ListArguments().Length
                    let outCount = opProp.ListOutputs().Length
                    let auxCount = opProp.ListAuxiliaryStates().Length
                    assert(numTensor = inCount + outCount + auxCount)
                    let shapePtrs : IntPtr [] = Helper.readStructArray numTensor tensorShapes
                    let shapes : int [] [] = 
                        [|
                            for i = 0 to inCount - 1 do 
                                Helper.readStructArray tensorDims.[i] shapePtrs.[i]
                        |]
                    let inputShapes, outputShapes, auxShapes = opProp.InferShape shapes
                    assert(inputShapes.Length = inCount)
                    assert(outputShapes.Length = outCount)
                    assert(auxShapes.Length = auxCount)
                    let returnShapes = 
                        [|
                            yield! inputShapes
                            yield! outputShapes
                            yield! auxShapes
                        |]
                    let dims = returnShapes |> Array.map Array.length
                    Marshal.Copy(dims, 0, tensorDimsPtr, dims.Length)
                    let returnShapesPtrs =
                        returnShapes
                        |> Array.map 
                            (fun a ->
                                let mem = rt.Alloc(a.Length * sizeof<int>)
                                Marshal.Copy(a,0,mem,a.Length)
                                mem
                            )
                    Marshal.Copy(returnShapesPtrs, 0, tensorShapes, returnShapesPtrs.Length)
                    true
                )    
            let inferBackwardStorageType = CustomOpBackwardInferStorageTypeFunc(fun numTensor tensorTypesPtr tags state -> 
                let tensorTypes : StorageType [] = Helper.readStructArray numTensor tensorTypesPtr |> Array.map enum
                let tags : int [] =  Helper.readStructArray numTensor tags
                let tensors = Array.init 5 (fun _ -> ResizeArray())
                for i = 0 to int numTensor - 1 do 
                    tensors.[tags.[i]].Add(tensorTypes.[i])
                let tensors = 
                    {
                        OutputGrad = tensors.[3].ToArray()
                        Input = tensors.[0].ToArray()
                        Output = tensors.[1].ToArray()
                        InputGrad = tensors.[2].ToArray()
                        Auxiliary = tensors.[4].ToArray()
                    }
                opProp.InferBackwardStorageType(tensors)
                //TODO: ensure no undefined storage types 
                //REVIEW: It's odd there's no effort to return the storage types in the original order
                let retStorageTypes = 
                    [| 
                        yield! tensors.OutputGrad
                        yield! tensors.Input
                        yield! tensors.Output
                        yield! tensors.InputGrad
                        yield! tensors.Auxiliary
                    |]
                    |> Array.map int
                Marshal.Copy(retStorageTypes, 0, tensorTypesPtr, retStorageTypes.Length) |> ignore
                true
            )

            let inferStorageType = CustomOpInferStorageTypeFunc(fun numTensor stypesPtr state -> 
                let inCount = opProp.ListArguments().Length
                let outCount = opProp.ListOutputs().Length
                let auxCount = opProp.ListAuxiliaryStates().Length
                let tensorTypes : StorageType [] = Helper.readStructArray inCount stypesPtr |> Array.map enum
                let inStore, outStore, auxStore = opProp.InferStorageType tensorTypes
                assert(inStore.Length = inCount)
                assert(outStore.Length = outCount)
                assert(auxStore.Length = auxCount)
                let retStorageTypes = 
                    [| 
                        yield! inStore
                        yield! outStore
                        yield! auxStore
                    |]
                    |> Array.map int
                Marshal.Copy(retStorageTypes, 0, stypesPtr, retStorageTypes.Length) |> ignore
                true
            )

            let inferType = CustomOpInferStorageTypeFunc(fun numTensor typesPtr state -> 
                let inCount = opProp.ListArguments().Length
                let outCount = opProp.ListOutputs().Length
                let auxCount = opProp.ListAuxiliaryStates().Length
                let dtypes : TypeFlag [] = Helper.readStructArray inCount typesPtr |> Array.map enum
                assert(numTensor = int (inCount + outCount + auxCount))
                let inputTypes, outputTypes, auxTypes = opProp.InferType(dtypes)
                assert(inputTypes.Length = inCount)
                assert(outputTypes.Length = outCount)
                assert(auxTypes.Length = auxCount)
                let ret = 
                    [|
                        yield! inputTypes
                        yield! outputTypes
                        yield! auxTypes
                    |]
                    |> Array.map int
                assert(ret.Length = int numTensor)
                Marshal.Copy(ret, 0, typesPtr, int numTensor)
                true
            )
            let listOutputs = CustomOpListFunc(fun out _ ->
                let l = opProp.ListOutputs()
                out <- rt.CachedStringArray(l)
                true)
                
            let listArgs = CustomOpListFunc(fun out _ ->
                let l = opProp.ListArguments()
                out <- rt.CachedStringArray(l)
                true)

            let listAuxStates = CustomOpListFunc(fun out _ ->
                let l = opProp.ListAuxiliaryStates()
                out <- rt.CachedStringArray(l)
                true)
            
            let declareBackwardDep = CustomOpBwdDepFunc(fun outGrad inData outData numDep deps state -> 
                let inCount = opProp.ListArguments().Length
                let outCount = opProp.ListOutputs().Length
                let outGrad : int [] = Helper.readStructArray outCount outGrad
                let inData : int [] = Helper.readStructArray inCount inData
                let outData : int [] = Helper.readStructArray outCount outData
                let rdeps = opProp.DeclareBackwardDependency(outGrad |> Array.map int, inData |> Array.map int, outData |> Array.map int)
                numDep <- rdeps.Length
                let dptr = rt.Alloc(rdeps.Length*sizeof<int>)
                Marshal.Copy(rdeps, 0, dptr, rdeps.Length)
                deps <- dptr
                true
            )

            let createOp = CustomOpCreateFunc(fun ctx numInputs shapes ndims dtypes ret state -> 
                let rt = ResourceTracker.CreateStored(opType + "_createop")
                let context = 
                    match Context.TryParse(ctx) with 
                    | Some c -> c
                    | None -> failwithf "Could not parse context '%s'" ctx
                let ndims : int [] = Helper.readStructArray numInputs ndims
                let dtypes : TypeFlag [] = Helper.readStructArray numInputs dtypes |> Array.map enum
                let shapes = 
                    let shapePtrs : IntPtr [] = Helper.readStructArray numInputs shapes
                    ndims 
                    |> Array.mapi 
                        (fun i d ->
                            Helper.readStructArray d shapePtrs.[i]
                        )
                let cop = opProp.CreateOperator(context, shapes, dtypes) 
                let forward = CustomOpFBFunc(fun size ptrs tags reqs isTrain state -> 
                    let tensors = Array.init 5 (fun _ -> ResizeArray())
                    let tags = Helper.readStructArray size tags
                    let ndarrs : NDArrayHandle [] = Helper.readStructArray size ptrs
                    //REVIEW: writable flag on NDArray? TODO:
                    for i = 0 to int size - 1 do    
                        tensors.[tags.[i]].Add(new NDArray(new SafeNDArrayHandle(ndarrs.[i], true)))
                    let reqs : int [] = Helper.readStructArray tensors.[1].Count reqs
                    cop.Forward(isTrain, reqs |> Array.map OpReqType.FromInt, tensors.[0].ToArray(), tensors.[1].ToArray(), tensors.[4].ToArray())
                    true
                )
                let backward = CustomOpFBFunc(fun size ptrs tags reqs isTrain state -> 
                    let tensors = Array.init 5 (fun _ -> ResizeArray())
                    let tags = Helper.readStructArray size tags
                    let ndarrs : NDArrayHandle [] = Helper.readStructArray size ptrs
                    //REVIEW: writable flag on NDArray? TODO:
                    for i = 0 to int size - 1 do    
                        //REVIEW: storage type?
                        tensors.[tags.[i]].Add(new NDArray(new SafeNDArrayHandle(ndarrs.[i], true)))
                    let reqs : int [] = Helper.readStructArray tensors.[1].Count reqs
                    cop.Backward(reqs |> Array.map OpReqType.FromInt, 
                                 tensors.[0].ToArray(), 
                                 tensors.[1].ToArray(), 
                                 tensors.[2].ToArray(), 
                                 tensors.[3].ToArray(), 
                                 tensors.[4].ToArray())
                    true
                )

                rt.WriteMxCallbackList(&ret, 
                    [|
                        forward :> Delegate, 0n
                        backward :> Delegate, 0n
                    |])
                true
            )
            rt.WriteMxCallbackList(&cbList, 
                [|
                    listArgs :> Delegate, 0n
                    listOutputs :> Delegate, 0n
                    listAuxStates :> Delegate, 0n
                    inferShape :> Delegate, 0n
                    declareBackwardDep :> Delegate, 0n
                    createOp :> Delegate, 0n
                    inferType :> Delegate, 0n
                    inferStorageType :> Delegate, 0n
                    inferBackwardStorageType :> Delegate, 0n
                |])
            true
        )
        rt.DelgatePointer creator |> ignore
        MXLib.customOpRegister name creator 

