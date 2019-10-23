open System.Collections.Generic
open System.Runtime.InteropServices

open MXNetSharp
open MXNetSharp.Interop
open MXNetSharp.IO
open System
open System.Net
open System.IO
open System.IO.Compression

open MXNetSharp.Interop
open MXNetSharp.PrimitiveOperators
open MXNetSharp.Interop.CApi

//https://github.com/apache/incubator-mxnet/blob/62b063802634048fe9da0a736dd6ee429e410f27/python/mxnet/ndarray/ndarray.py#L57-L60
type StorageType = 
    | Undefined = -1
    | Default = 0
    | RowSparse = 1
    | CSR = 2

type BackwardStorageTypes = 
    {
        InputGrad : StorageType []
        OutputGrad : StorageType []
        Input : StorageType []
        Output : StorageType []
        Auxiliary : StorageType []
    }
    
let withName name (symbol : #Symbol) = symbol.Name <- name; symbol
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
    
type ICustomOperationProperties = //TODO: label args
    abstract member ListArguments : unit -> string []
    abstract member ListOutputs : unit -> string []
    abstract member ListAuxiliaryStates : unit -> string []
    abstract member InferShape : inShape : int [][] -> int[][]*int[][]*int[][]
    abstract member InferBackwardStorageType : storageTypes : BackwardStorageTypes -> unit
    abstract member InferStorageType : inputStorageTypes : StorageType[] -> StorageType[] *StorageType[]*StorageType[] 
    abstract member InferType : inType : TypeFlag[] -> TypeFlag[]*TypeFlag[]*TypeFlag[]
    abstract member DeclareBackwardDependency : outGrad : int[] * inData : int[] * OutData : int[] -> int[]
    abstract member CreateOperator : context : Context * inShapes : int[][] * inDataTypes : TypeFlag[] -> ICustomOperation
    
             
let testCustomOp = 
    {new ICustomOperation with
         member this.Backward(req: OpReqType [], inData: NDArray [], outData: NDArray [], inGrad: NDArray [], outGrad: NDArray [], auxData: NDArray []): unit = 
             printfn "Backward!!!!!!"
         member this.Forward(isTrain: bool, req: OpReqType [], inData: NDArray [], outData: NDArray [], auxData: NDArray []): unit = 
             printfn "Forward!!!!!!"
    }
let testOpProps = 
    {new ICustomOperationProperties with
         member this.CreateOperator(context: Context, inShapes: int [] [], inDataTypes: TypeFlag []): ICustomOperation = 
             testCustomOp
         member this.DeclareBackwardDependency(outGrad: int [], inData: int [], outData: int []): int [] = 
             [|
                yield! outGrad
                yield! inData
                yield! outData
             |]
         member this.InferBackwardStorageType(storageTypes : BackwardStorageTypes): unit = 
             for i = 0 to storageTypes.Auxiliary.Length - 1 do storageTypes.Auxiliary.[i] <- StorageType.Default
             for i = 0 to storageTypes.Input.Length - 1 do storageTypes.Input.[i] <- StorageType.Default
             for i = 0 to storageTypes.Output.Length - 1 do storageTypes.Output.[i] <- StorageType.Default
             for i = 0 to storageTypes.InputGrad.Length - 1 do storageTypes.InputGrad.[i] <- StorageType.Default
             for i = 0 to storageTypes.OutputGrad.Length - 1 do storageTypes.OutputGrad.[i] <- StorageType.Default
         member this.InferShape(inShape: int [] []): int [] [] * int [] [] * int [] [] = 
             let outType = this.ListOutputs() |> Array.map (fun x -> inShape.[0])
             inShape, outType, Array.empty
         member this.InferStorageType(inputStorageTypes : StorageType []): StorageType [] * StorageType [] * StorageType [] = 
             let outType = this.ListOutputs() |> Array.map (fun x -> StorageType.Default)
             let auxType = this.ListAuxiliaryStates() |> Array.map (fun x -> StorageType.Default)
             inputStorageTypes, outType, auxType
         member this.InferType(inType: TypeFlag []): TypeFlag [] * TypeFlag [] * TypeFlag [] = 
             let outType = this.ListOutputs() |> Array.map (fun x -> inType.[0])
             let auxType = this.ListAuxiliaryStates() |> Array.map (fun x -> inType.[0])
             inType, outType, auxType
         member this.ListArguments(): string [] = [|"data1"; "data2"|]
         member this.ListAuxiliaryStates(): string [] = Array.empty
         member this.ListOutputs(): string [] = [|"output"|]
    }

let refs = ResizeArray()    
let creator = CustomOpPropCreator(fun opType argCount keys values cbList -> 
    let opType = Helper.str opType
    printfn "creating... %s" opType
    printfn "arg count %d" argCount
    let args = 
        let keys = Helper.readStringArray argCount keys
        let values = Helper.readStringArray argCount values
        printfn "keys: %A" keys
        printfn "values: %A" values
        Array.zip keys values
        |> dict
    printfn "poo"
    let op : ICustomOperationProperties = testOpProps
    let inferShape = 
        CustomOpInferShapeFunc(fun numTensor tensorDimsPtr tensorShapes state ->
            printfn "CustomOpInferShapeFunc"
            printfn "numTensor %d" numTensor
            let tensorDims : int [] = Helper.readStructArray numTensor tensorDimsPtr
            printfn "tensorDims %A" tensorDims
            let inCount = op.ListArguments().Length
            let outCount = op.ListOutputs().Length
            let auxCount = op.ListAuxiliaryStates().Length
            printfn "totalCount %d" (inCount + outCount + auxCount)
            assert(numTensor = nativeint(inCount + outCount + auxCount))
            let shapePtrs : IntPtr [] = Helper.readStructArray numTensor tensorShapes
            printfn "shapePtrs: %A" shapePtrs
            let shapes : int [] [] = 
                [|
                    for i = 0 to inCount - 1 do 
                        Helper.readStructArray tensorDims.[i] shapePtrs.[i]
                |]
            printfn "shapes: %A" shapes
            let inputShapes, outputShapes, auxShapes = op.InferShape shapes
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
                        let ptr = Marshal.AllocHGlobal(a.Length * sizeof<int>)
                        Marshal.Copy(a,0,ptr,a.Length)
                        ptr
                    )
            Marshal.Copy(returnShapesPtrs, 0, tensorShapes, returnShapesPtrs.Length)
            1
        )    
    let inferBackwardStorageType = CustomOpBackwardInferStorageTypeFunc(fun numTensor tensorTypesPtr tags state -> 
        printfn "infer back storage type func"
        printfn "numTensor: %d" numTensor
        let tensorTypes : StorageType [] = Helper.readStructArray numTensor tensorTypesPtr |> Array.map enum
        printfn "tensorTypes: %A" tensorTypes
        let tags : int [] =  Helper.readStructArray numTensor tags
        printfn "tags: %A" tags
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
        op.InferBackwardStorageType(tensors)
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
        1
    )

    let inferStorageType = CustomOpInferStorageTypeFunc(fun numTensor stypesPtr state -> 
        printfn "infer storage type func"
        printfn "CustomOpInferStorageTypeFunc"
        printfn "numTensor %d" numTensor
        let inCount = op.ListArguments().Length
        let outCount = op.ListOutputs().Length
        let auxCount = op.ListAuxiliaryStates().Length
        let tensorTypes : StorageType [] = Helper.readStructArray inCount stypesPtr |> Array.map enum
        printfn "tensorTypes %A" tensorTypes
        let inStore, outStore, auxStore = op.InferStorageType tensorTypes
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
        1
    )

    let inferType = CustomOpInferStorageTypeFunc(fun numTensor typesPtr state -> 
        printfn "infer storage type func"
        printfn "CustomOpInferStorageTypeFunc"
        printfn "numTensor %d" numTensor
        let inCount = op.ListArguments().Length
        let outCount = op.ListOutputs().Length
        let auxCount = op.ListAuxiliaryStates().Length
        printfn "totalCount %d" (inCount + outCount + auxCount)
        let dtypes : TypeFlag [] = Helper.readStructArray inCount typesPtr |> Array.map enum
        printfn "tensorDataTypes %A" dtypes
        assert(numTensor = nativeint(inCount + outCount + auxCount))
        let inputTypes, outputTypes, auxTypes = op.InferType(dtypes)
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
        1
    )
    let listOutputs = CustomOpListFunc(fun out _ ->
        printfn "listOutputs"
        let ptrs = 
            [| 
                yield! op.ListOutputs() |> Array.map (Marshal.StringToHGlobalAnsi)
                yield 0n
            |]
        let ptr = Marshal.AllocHGlobal (ptrs.Length*sizeof<IntPtr>)
        Marshal.Copy(ptrs, 0, ptr, ptrs.Length)
        out <- ptr
        1)
        
    let listArgs = CustomOpListFunc(fun out _ ->
        printfn "listArgs"
        //let outStr = (op.ListArguments() |> String.concat "\u0000") + "\u0000\u0000"
        let ptrs = 
            [| 
                yield! op.ListArguments() |> Array.map (Marshal.StringToHGlobalAnsi)
                yield 0n
            |]
        let ptr = Marshal.AllocHGlobal (ptrs.Length*sizeof<IntPtr>)
        Marshal.Copy(ptrs, 0, ptr, ptrs.Length)
        out <- ptr
        

        //outStr.Replace("\u0000", "|") |> printfn "OutStr: %s"
        //out <- Marshal.StringToHGlobalAnsi outStr
        1)

    let listAuxStates = CustomOpListFunc(fun out _ ->
        let ptrs = 
            [| 
                yield! op.ListAuxiliaryStates() |> Array.map (Marshal.StringToHGlobalAnsi)
                yield 0n
            |]
        let ptr = Marshal.AllocHGlobal (ptrs.Length*sizeof<IntPtr>)
        Marshal.Copy(ptrs, 0, ptr, ptrs.Length)
        out <- ptr
        1)
    
    let declareBackwardDep = CustomOpBwdDepFunc(fun outGrad inData outData numDep deps state -> 
        printfn "declare backward Dep"
        let inCount = op.ListArguments().Length
        let outCount = op.ListOutputs().Length
        let outGrad : nativeint [] = Helper.readStructArray outCount outGrad
        let inData : nativeint [] = Helper.readStructArray inCount inData
        let outData : nativeint [] = Helper.readStructArray outCount outData
        printfn "outGrad: %A" outGrad
        printfn "inData: %A" inData
        printfn "outData: %A" outData
        let rdeps = op.DeclareBackwardDependency(outGrad |> Array.map int, inData |> Array.map int, outData |> Array.map int)
                    |> Array.map nativeint
        printfn "rdeps: %A" rdeps
        numDep <- nativeint rdeps.Length
        // REVIEW: register? see python code
        let dptr = Marshal.AllocHGlobal(rdeps.Length*sizeof<nativeint>)
        Marshal.Copy(rdeps, 0, dptr, rdeps.Length)
        deps <- dptr
        1
    )

    let createOp = CustomOpCreateFunc(fun ctx numInputs shapes ndims dtypes ret state -> 
        printfn "create op"
        printfn "ctx: %A" ctx
        printfn "numInputs: %d" numInputs
        //TODO: parse context
        let ndims : int [] = Helper.readStructArray numInputs ndims
        let dtypes : TypeFlag [] = Helper.readStructArray numInputs dtypes |> Array.map enum
        let shapes = 
            let shapePtrs : IntPtr [] = Helper.readStructArray numInputs shapes
            ndims 
            |> Array.mapi 
                (fun i d ->
                    Helper.readStructArray d shapePtrs.[i]
                )
        let cop = op.CreateOperator(CPU(0), shapes, dtypes) //TODO: right context
        let forward = CustomOpFBFunc(fun size ptrs tags reqs isTrain state -> 
            printfn "forward"
            printfn "size %d" size
            let tensors = Array.init 5 (fun _ -> ResizeArray())
            let tags = Helper.readStructArray size tags
            let ndarrs : NDArrayHandle [] = Helper.readStructArray size ptrs
            //REVIEW: writable flag on NDArray? TODO:
            for i = 0 to int size - 1 do    
                tensors.[tags.[i]].Add(new NDArray(new SafeNDArrayHandle(ndarrs.[i], true)))
            let reqs : int [] = Helper.readStructArray tensors.[1].Count reqs
            cop.Forward(isTrain, reqs |> Array.map OpReqType.FromInt, tensors.[0].ToArray(), tensors.[1].ToArray(), tensors.[4].ToArray())
            1
        )
        let backward = CustomOpFBFunc(fun size ptrs tags reqs isTrain state -> 
            printfn "backward"
            printfn "size %d" size
            let inCount = op.ListArguments().Length
            let outCount = op.ListOutputs().Length
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
            1
        )
        let delete = CustomOpDelFunc(fun _ -> printfn "delete"; 0)
        refs.Add (delete :> Delegate)
        refs.Add (forward :> Delegate)
        refs.Add (backward :> Delegate)
        // build MXCallbackList
        let cbPtrArray=
            [|
                (Marshal.GetFunctionPointerForDelegate delete)
                (Marshal.GetFunctionPointerForDelegate forward)
                (Marshal.GetFunctionPointerForDelegate backward)
            |]
        printfn "%A" cbPtrArray
        let cbPtr = Marshal.AllocHGlobal (cbPtrArray.Length * sizeof<IntPtr>)
        Marshal.Copy(cbPtrArray,0,cbPtr,cbPtrArray.Length)
        let ctxPtrArray = cbPtrArray |> Array.map (fun _ -> 0n)
        let ctxPtr = Marshal.AllocHGlobal (ctxPtrArray.Length * sizeof<IntPtr>)
        Marshal.Copy(ctxPtrArray,0,ctxPtr,ctxPtrArray.Length)
        let ptr = ret
        let N = cbPtrArray.Length |> int64
        NativeInterop.NativePtr.set (NativeInterop.NativePtr.ofNativeInt ptr) 0 N
        NativeInterop.NativePtr.set (NativeInterop.NativePtr.ofNativeInt (ptr + nativeint sizeof<int64>)) 0 cbPtr
        NativeInterop.NativePtr.set (NativeInterop.NativePtr.ofNativeInt (ptr + nativeint (sizeof<int64> + sizeof<IntPtr>))) 0 ctxPtr
        1
    )
    printfn "making cb struct"
    let delete = CustomOpDelFunc(fun _ -> printfn "delete"; 0)
    let cbPtrArray=
        [|
            (Marshal.GetFunctionPointerForDelegate delete)
            (Marshal.GetFunctionPointerForDelegate listArgs)
            (Marshal.GetFunctionPointerForDelegate listOutputs)
            (Marshal.GetFunctionPointerForDelegate listAuxStates)
            (Marshal.GetFunctionPointerForDelegate inferShape)
            (Marshal.GetFunctionPointerForDelegate declareBackwardDep)
            (Marshal.GetFunctionPointerForDelegate createOp)
            (Marshal.GetFunctionPointerForDelegate inferType)
            (Marshal.GetFunctionPointerForDelegate inferStorageType)
            (Marshal.GetFunctionPointerForDelegate inferBackwardStorageType)
        |]
    let cbPtr = Marshal.AllocHGlobal (cbPtrArray.Length * sizeof<IntPtr>)
    Marshal.Copy(cbPtrArray,0,cbPtr,cbPtrArray.Length)
    let ctxPtrArray = cbPtrArray |> Array.map (fun _ -> 0n)
    let ctxPtr = Marshal.AllocHGlobal (ctxPtrArray.Length * sizeof<IntPtr>)
    Marshal.Copy(ctxPtrArray,0,ctxPtr,ctxPtrArray.Length)
    let ptr = cbList 
    let N = cbPtrArray.Length |> int64
    NativeInterop.NativePtr.set (NativeInterop.NativePtr.ofNativeInt ptr) 0 N
    NativeInterop.NativePtr.set (NativeInterop.NativePtr.ofNativeInt (ptr + nativeint sizeof<int64>)) 0 cbPtr
    NativeInterop.NativePtr.set (NativeInterop.NativePtr.ofNativeInt (ptr + nativeint (sizeof<int64> + sizeof<IntPtr>))) 0 ctxPtr
    1
)
CApi.MXCustomOpRegister("myop", creator)

let a = new Variable("a")
let b = new Variable("b")

let cop = new SymbolOperator("Custom", 
        [
            "data", VarArg("numArgs", [|a :> Symbol; b :> Symbol|])
            "op_type", Parameter(Some(box "myop"))
            //"poo", Parameter(Some(box 100))
        ] |> Arguments
    )

printfn "oklau"

let crap() = 
    cop.UnsafeHandle

crap()
printfn "hello"

let inArgs,argGrad,reqs = 
    [|
        Operators.ZerosNDArray([5;5], ctx = "cpu(0)").[0],Operators.ZerosNDArray([5;5], ctx = "cpu(0)").[0],OpReqType.WriteTo
        Operators.ZerosNDArray([25;25], ctx = "cpu(0)").[0],Operators.ZerosNDArray([25;25], ctx = "cpu(0)").[0],OpReqType.WriteTo
    |]
    |> Array.unzip3

let exe = Executor(cop, CPU(0), inArgs, argGrad, reqs, Array.empty)



printfn "crap"

for i = 0 to 100 do 
    let o = exe.Forward(true)
    let f : float32[] = o.[0].ToArray()
    printfn "kkkkkkk"
    exe.Backward(argGrad)

GC.Collect()

for i = 0 to 100 do 
    let o = exe.Forward(true)
    let f : float32[] = o.[0].ToArray()
    printfn "kkkkkkk"
    exe.Backward(argGrad)

exe.Dispose()
GC.Collect()
printfn "bbbbbbb"

inArgs.[0].Dispose()
GC.Collect()
printfn "23423523"
inArgs.[0].Dispose()
GC.Collect()
printfn "9uwetr9hg"
let poo : float32[] = argGrad.[0].ToArray()

printfn "bbbbbbb"

    


