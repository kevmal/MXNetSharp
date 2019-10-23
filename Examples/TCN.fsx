open System.Collections.Generic
open MXNetSharp.Interop
open System.Runtime.InteropServices

#load "load.fsx"
open MXNetSharp
open MXNetSharp.Interop
open MXNetSharp.IO
open System
open System.Net
open System.IO
open System.IO.Compression

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
         member this.ListArguments(): string [] = [|"data"|]
         member this.ListAuxiliaryStates(): string [] = Array.empty
         member this.ListOutputs(): string [] = [|"output"|]
    }
    

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
    let op : ICustomOperationProperties = failwith "make op"
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
            assert(numTensor = inCount + outCount + auxCount)
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
            0
        )    
    let inferBackwardStorageType = CustomOpBackwardInferStorageTypeFunc(fun numTensor tensorTypesPtr tags state -> 
        printfn "infer back storage type func"
        printfn "numTensor: %d" numTensor
        let tensorTypes : StorageType [] = Helper.readStructArray numTensor tensorTypesPtr
        printfn "tensorTypes: %A" tensorTypes
        let tags : int [] =  Helper.readStructArray numTensor tags
        printfn "tags: %A" tags
        let tensors = Array.init 5 (fun _ -> ResizeArray())
        for i = 0 to numTensor - 1 do 
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
        0
    )

    let inferStorageType = CustomOpInferStorageTypeFunc(fun numTensor stypesPtr state -> 
        printfn "infer storage type func"
        printfn "CustomOpInferStorageTypeFunc"
        printfn "numTensor %d" numTensor
        let dtypes : TypeFlag [] = Helper.readStructArray numTensor stypesPtr
        printfn "tensorStorageTypes %A" dtypes
        let inCount = op.ListArguments().Length
        let outCount = op.ListOutputs().Length
        let auxCount = op.ListAuxiliaryStates().Length
        let tensorTypes : StorageType [] = Helper.readStructArray inCount stypesPtr
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
        0
    )

    let inferType = CustomOpInferStorageTypeFunc(fun numTensor typesPtr state -> 
        printfn "infer storage type func"
        printfn "CustomOpInferStorageTypeFunc"
        printfn "numTensor %d" numTensor
        let dtypes : TypeFlag [] = Helper.readStructArray numTensor typesPtr
        printfn "tensorDataTypes %A" dtypes
        let inCount = op.ListArguments().Length
        let outCount = op.ListOutputs().Length
        let auxCount = op.ListAuxiliaryStates().Length
        printfn "totalCount %d" (inCount + outCount + auxCount)
        assert(numTensor = inCount + outCount + auxCount)
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
        assert(ret.Length = numTensor)
        Marshal.Copy(ret, 0, typesPtr, numTensor)
        0
    )
    let listOutputs = CustomOpListFunc(fun out _ ->
        printfn "listOutputs"
        let outStr = (op.ListOutputs() |> String.concat "\u0000") + "\u0000"
        outStr.Replace("\u0000", "|") |> printfn "OutStr: %s"
        out <- Marshal.StringToHGlobalAnsi outStr
        0)
        
    let listArgs = CustomOpListFunc(fun out _ ->
        printfn "listArgs"
        let outStr = (op.ListArguments() |> String.concat "\u0000") + "\u0000"
        outStr.Replace("\u0000", "|") |> printfn "OutStr: %s"
        out <- Marshal.StringToHGlobalAnsi outStr
        0)

    let listAuxStates = CustomOpListFunc(fun out _ ->
        printfn "listArgs"
        let outStr = (op.ListAuxiliaryStates() |> String.concat "\u0000") + "\u0000"
        outStr.Replace("\u0000", "|") |> printfn "OutStr: %s"
        out <- Marshal.StringToHGlobalAnsi outStr
        0)
    
    let declareBackwardDep = CustomOpBwdDepFunc(fun outGrad inData outData numDep deps state -> 
        printfn "declare backward Dep"
        let inCount = op.ListArguments().Length
        let outCount = op.ListOutputs().Length
        let outGrad = Helper.readStructArray outCount outGrad
        let inData = Helper.readStructArray inCount inData
        let outData = Helper.readStructArray outCount outData
        let rdeps = op.DeclareBackwardDependency(outGrad, inData, outData)
        numDep <- rdeps.Length
        // REVIEW: register? see python code
        let dptr = Marshal.AllocHGlobal(rdeps.Length*sizeof<int>)
        Marshal.Copy(rdeps, 0, dptr, rdeps.Length)
        deps <- dptr
        0
    )

    let createOp = CustomOpCreateFunc(fun ctx numInputs shapes ndims dtypes ret state -> 
        printfn "create op"
        printfn "ctx: %A" ctx
        printfn "numInputs: %d" numInputs
        //TODO: parse context
        let ndims : int [] = Helper.readStructArray numInputs ndims
        let dtypes : TypeFlag [] = Helper.readStructArray numInputs dtypes
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
            for i = 0 to size - 1 do    
                tensors.[tags.[i]].Add(new NDArray(new SafeNDArrayHandle(ndarrs.[i], true)))
            let reqs : int [] = Helper.readStructArray tensors.[1].Count reqs
            cop.Forward(isTrain, reqs |> Array.map OpReqType.FromInt, tensors.[0].ToArray(), tensors.[1].ToArray(), tensors.[4].ToArray())
            0
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
            for i = 0 to size - 1 do    
                //REVIEW: storage type?
                tensors.[tags.[i]].Add(new NDArray(new SafeNDArrayHandle(ndarrs.[i], true)))
            let reqs : int [] = Helper.readStructArray tensors.[1].Count reqs
            cop.Backward(reqs |> Array.map OpReqType.FromInt, 
                         tensors.[0].ToArray(), 
                         tensors.[1].ToArray(), 
                         tensors.[2].ToArray(), 
                         tensors.[3].ToArray(), 
                         tensors.[4].ToArray())
            0
        )
        let delete = CustomOpDelFunc(fun _ -> printfn "delete"; 0)
        // build MXCallbackList
        let sz = sizeof<int> + // num_callbacks: int
                    sizeof<IntPtr>*3 + //MXCallbackList_callbacks[]
                    sizeof<int>*3 // Contexts
        let ptr = Marshal.AllocHGlobal sz
        let numCallbacksPtr : int nativeptr = ptr |> NativeInterop.NativePtr.ofNativeInt
        NativeInterop.NativePtr.set numCallbacksPtr 0 3 //
        let cbListptr : IntPtr nativeptr = ptr + nativeint sizeof<int> |> NativeInterop.NativePtr.ofNativeInt
        let ctxListptr : int nativeptr = ptr + nativeint(sizeof<int> + sizeof<IntPtr>*3) |> NativeInterop.NativePtr.ofNativeInt
        NativeInterop.NativePtr.set cbListptr 0 (Marshal.GetFunctionPointerForDelegate delete)
        NativeInterop.NativePtr.set cbListptr 1 (Marshal.GetFunctionPointerForDelegate forward)
        NativeInterop.NativePtr.set cbListptr 2 (Marshal.GetFunctionPointerForDelegate backward)
        NativeInterop.NativePtr.set ctxListptr 0 (int CustomOpCallbacks.kCustomOpDelete)
        NativeInterop.NativePtr.set ctxListptr 1 (int CustomOpCallbacks.kCustomOpForward)
        NativeInterop.NativePtr.set ctxListptr 2 (int CustomOpCallbacks.kCustomOpBackward)
        ret <- ptr
        0
    )
    let delete = CustomOpDelFunc(fun _ -> printfn "delete"; 0)
    let N = 10
    let sz = sizeof<int> + // num_callbacks: int
                sizeof<IntPtr>*N + //MXCallbackList_callbacks[]
                sizeof<int>*N // Contexts
    let ptr = Marshal.AllocHGlobal sz
    let numCallbacksPtr : int nativeptr = ptr |> NativeInterop.NativePtr.ofNativeInt
    NativeInterop.NativePtr.set numCallbacksPtr 0 3 //
    let cbListptr : IntPtr nativeptr = ptr + nativeint sizeof<int> |> NativeInterop.NativePtr.ofNativeInt
    let ctxListptr : int nativeptr = ptr + nativeint(sizeof<int> + sizeof<IntPtr>*N) |> NativeInterop.NativePtr.ofNativeInt
    NativeInterop.NativePtr.set cbListptr 0 (Marshal.GetFunctionPointerForDelegate delete)
    NativeInterop.NativePtr.set cbListptr 1 (Marshal.GetFunctionPointerForDelegate listArgs)
    NativeInterop.NativePtr.set cbListptr 2 (Marshal.GetFunctionPointerForDelegate listOutputs)
    NativeInterop.NativePtr.set cbListptr 3 (Marshal.GetFunctionPointerForDelegate listAuxStates)
    NativeInterop.NativePtr.set cbListptr 4 (Marshal.GetFunctionPointerForDelegate inferShape)
    NativeInterop.NativePtr.set cbListptr 5 (Marshal.GetFunctionPointerForDelegate declareBackwardDep)
    NativeInterop.NativePtr.set cbListptr 6 (Marshal.GetFunctionPointerForDelegate createOp)
    NativeInterop.NativePtr.set cbListptr 7 (Marshal.GetFunctionPointerForDelegate inferType)
    NativeInterop.NativePtr.set cbListptr 8 (Marshal.GetFunctionPointerForDelegate inferStorageType)
    NativeInterop.NativePtr.set cbListptr 9 (Marshal.GetFunctionPointerForDelegate inferBackwardStorageType)
    NativeInterop.NativePtr.set ctxListptr 0 (int CustomOpPropCallbacks.kCustomOpPropDelete)
    NativeInterop.NativePtr.set ctxListptr 1 (int CustomOpPropCallbacks.kCustomOpPropListArguments)
    NativeInterop.NativePtr.set ctxListptr 2 (int CustomOpPropCallbacks.kCustomOpPropListOutputs)
    NativeInterop.NativePtr.set ctxListptr 3 (int CustomOpPropCallbacks.kCustomOpPropListAuxiliaryStates)
    NativeInterop.NativePtr.set ctxListptr 4 (int CustomOpPropCallbacks.kCustomOpPropInferShape)
    NativeInterop.NativePtr.set ctxListptr 5 (int CustomOpPropCallbacks.kCustomOpPropDeclareBackwardDependency)
    NativeInterop.NativePtr.set ctxListptr 6 (int CustomOpPropCallbacks.kCustomOpPropCreateOperator)
    NativeInterop.NativePtr.set ctxListptr 7 (int CustomOpPropCallbacks.kCustomOpPropInferType)
    NativeInterop.NativePtr.set ctxListptr 8 (int CustomOpPropCallbacks.kCustomOpPropInferStorageType)
    NativeInterop.NativePtr.set ctxListptr 9 (int CustomOpPropCallbacks.kCustomOpPropBackwardInferStorageType)
    cbList <- ptr
    0
)
CApi.MXCustomOpRegister("myop", creator)

let a = new Variable("a")

let cop = new SymbolOperator("Custom", 
        [
            "data", VarArg("numArgs", [|a :> Symbol|])
            "op_type", Parameter(Some(box "myop"))
            "poo", Parameter(Some(box 100))
        ] |> Arguments
    )


cop.UnsafeHandle






[<System.Runtime.InteropServices.DllImport(MXNETLIB, CallingConvention = System.Runtime.InteropServices.CallingConvention.Cdecl, CharSet = System.Runtime.InteropServices.CharSet.Ansi)>]
extern int MXCustomOpRegister(string op_type, IntPtr creator)

let cptr = System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate(creator)


MXCustomOpRegister("meop", cptr)

let h = NNVM.getOpHandle "myop"

MXLib.getLastError()

let a = Variable "a"
let customOp = new Custom("myop", a)

customOp.UnsafeHandle

L2Normalization
let b = Variable "b"
let c = a*b |> withName "mulop"

let d = new Variable("d")

MXSymbol.compose c.UnsafeHandle  null [|"a"|] [|d.UnsafeHandle|]


let weightNormalization (s : Symbol) (skip : string []) = 
    let refs = ResizeArray()
    s.InputSymbols
    |> Seq.choose
        (fun x ->
            if Array.contains x.Name skip then 
                None
            else
                let name = x.Name 
                let g = new Variable(name + "_g")
                let v = new Variable(name + "_v")
                let w = (g / sqrt(new Sum(new Square(v)))) * v |> withName (name + "_w")
                refs.Add w
                Some(name, w.UnsafeHandle)
        )
    |> Seq.toArray
    |> Array.unzip
    ||> MXSymbol.compose s.UnsafeHandle null         
    refs

let rrs = weightNormalization c ([|"b"|])


MXSymbol.saveToJSON c.UnsafeHandle

let temporalBlock name outputCount kernelSize stride dilation padding dropout inputVars x = 
    let conv1 = new Convolution(data = x, 
                                kernel = [kernelSize],  
                                numFilter = outputCount, 
                                stride = [stride], 
                                dilate = [dilation], 
                                pad = [padding],  
                                noBias = false,
                                Name = name + "_conv1")
    let wm1 = weightNormalization conv1 inputVars
    let conv1Sliced = new Slice(conv1, [0; 0; 0], [0; 0; -padding], [0;0;1])
    let relu1 = new Relu(conv1Sliced, Name = name + "_relu1")
    let dropout1 = new Dropout(relu1, dropout, DropoutMode.Training)
    let conv2 = new Convolution(data = dropout1, 
                                kernel = [kernelSize],  
                                numFilter = outputCount, 
                                stride = [stride], 
                                dilate = [dilation], 
                                pad = [padding],  
                                noBias = false,
                                Name = name + "_conv2")
    let wm2 = weightNormalization conv1 inputVars
    let conv2Sliced = new Slice(conv2, [0; 0; 0], [0; 0; -padding], [0;0;1])
    let relu2 = new Relu(conv2Sliced, Name = name + "_relu2")
    let dropout2 = new Dropout(relu1, dropout, DropoutMode.Training)
    ()





let batchSize = 64

let context = GPU(0)

let ensure (url : string) (file : string) = 
    if not(File.Exists file) then
        use wc = new WebClient()
        if url.ToLower().EndsWith ".gz" && not (file.EndsWith ".gz") then 
            let tmpFile = Path.GetTempFileName()
            wc.DownloadFile(url, tmpFile)
            do 
                use gfile = File.OpenRead(tmpFile)
                use g = new GZipStream(gfile,CompressionMode.Decompress)
                use f = File.OpenWrite(file)
                g.CopyTo(f)
            File.Delete tmpFile
        else
            wc.DownloadFile(url, file)

ensure "http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz" "train-images-idx3-ubyte"
ensure "http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz" "train-labels-idx1-ubyte"
ensure "http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz" "t10k-images-idx3-ubyte"
ensure "http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz" "t10k-labels-idx1-ubyte"

let trainIter = new MNISTIter(@"train-images-idx3-ubyte", 
                              @"train-labels-idx1-ubyte",
                              batchSize = batchSize,
                              flat = false)

let valIter   = new MNISTIter(@"t10k-images-idx3-ubyte", 
                              @"t10k-labels-idx1-ubyte",
                              batchSize = batchSize,
                              flat = false)

let xIn = new Variable("X")
let scaledInput = xIn + 0.0 //1.0 - xIn/255.0
let dropProb = 0.2
let decInChannels = 1
let nLatent = 10

let reshapedDim = [batchSize;decInChannels;7;7]
let inputsDecoder = 49*decInChannels / 2
let conv name kernelSize strides outFeatureMapCount x = 
    let y = new Convolution(data = x, numFilter = outFeatureMapCount, kernel=[kernelSize;kernelSize], stride = [strides;strides], Name = name + "_conv")
    new LeakyReLU(y, Name = name + "_lrelu")


let convTranspose name kernelSize (strides : int) outFeatureMapCount x = 
    let y = new Deconvolution(data = x, numFilter = outFeatureMapCount, kernel = [kernelSize; kernelSize], stride = [strides; strides], Name = name + "_dconv")
    new Relu(y,Name = name + "_relu")
    //let numInputChannels = x.Shape.[2]
    //let convParams = C.Parameter([kernelSize; kernelSize; outFeatureMapCount; numInputChannels], CNTKLib.GlorotUniformInitializer())
    //printfn "%A" (x.Shape.Dimensions,strides)
    //let rr = CNTKLib.ReLU(CNTKLib.ConvolutionTranspose(convParams, x, [strides;strides;numInputChannels]))//, BoolVector.Repeat(false,3), BoolVector.Repeat(true,3), [x.Shape.[0]*strides;x.Shape.[1]*strides;outFeatureMapCount]))
    //printfn "rr %A" (rr.Shape.Dimensions)
    //rr
    
let encoder x dropProb = 
    let layer name s x = 
        let c1 = conv name 4 s 64 x
        new Dropout(c1, dropProb, mode = DropoutMode.Training, Name = name + "_dropout")
    let c1 = layer "eL1" 2 x
    let c2 = layer "eL2" 1 c1
    let c3 = layer "eL3" 1 c2
    let mn = new FullyConnected(data = c3, numHidden = nLatent, noBias = false, flatten = true, Name = "e_mn")
    let sd = 0.5*(new FullyConnected(data = c3, numHidden = nLatent, noBias = false, flatten = true, Name = "e_sd"))
    let epsilon = new RandomNormalLike(mn, Name = "e_ep")
    let z = mn + epsilon * (exp sd)
    z.Name <- "ez"
    z, mn, sd

let decoder (sample : Symbol) dropProb =    
    let d1 = new FullyConnected(data = sample, numHidden = inputsDecoder, noBias = false, Name = "d_fc1")
    let d1 = new LeakyReLU(d1, Name = "d_relu1")
    let d2 = new FullyConnected(data = d1, numHidden = inputsDecoder*2 + 1, noBias = false, Name = "d_fc2")
    let d2 = new LeakyReLU(d2, Name = "d_relu2")
    let x = new Reshape(d2, reshapedDim, Name = "d_dx")
    let layer name s x = 
        let c1 = convTranspose name 4 s 64 x
        new Dropout(c1, dropProb, mode = DropoutMode.Training, Name = name + "_dropout")
    let c1 = layer "d_L1" 2 x
    let c2 = layer "d_L2" 1 c1
    let c3 = convTranspose "d_L3" 4 1 64 c2
    let z = 
        let fc = new FullyConnected(data = c3, numHidden = 28*28, Name = "d_fc3")
        new Activation(fc, ActType.Sigmoid, Name = "d_z")
    let dec = new Reshape(z, [batchSize; 1; 28; 28])
    dec.Name <- "decoder"
    dec


let sampled, mn, sd = encoder scaledInput dropProb

let dec = decoder sampled dropProb
let unscaledOut = dec //((0.0-dec) + 1.0)*255.0 // 1.0 - xIn/255.0

let imgLoss = new Sum(new Square(dec - scaledInput))
let latentLoss = -0.5*(new Sum(1.0 + 2.0*sd - new Square(mn) - exp(2.0*sd)))
let loss = new MakeLoss(new Mean(imgLoss + latentLoss))

let inputs = loss.InputSymbols


let dataShape = [batchSize;1;28;28]
let (k,i,d) = MXSymbol.keyShapeToCsrForm uint32 [| "X", Array.ofSeq dataShape |]
let inferResult = MXSymbol.inferShape loss.UnsafeHandle k i d
loss.ArgumentNames |> Seq.length

inferResult.InputShapes |> Seq.length
loss.InputSymbols |> Seq.length
loss.InputSymbols
|> Seq.iter 
    (fun x ->
        printfn "%s" x.Name
    )

type Train = 
    | NoTrain of NDArray
    | TrainParameters of NDArray*NDArray*NDArray*NDArray

let ps = 
    inputs
    |> Array.zip inferResult.InputShapes
    |> Array.map
        (fun (s,x) ->
            if x.Name = "X" then NoTrain(new NDArray(shape = dataShape, context = context))
            else
                let s = s |> Seq.map int
                TrainParameters(
                    Operators.RandomUniformNDArray(-0.1, 0.1, s, ctx = context.ToString()).[0], 
                        Operators.ZerosNDArray(s, ctx = context.ToString()).[0],
                        Operators.ZerosNDArray(s, ctx = context.ToString()).[0],
                        Operators.ZerosNDArray(s, ctx = context.ToString()).[0])
        )
    
let lr = 0.0005

let exe, texe = 
    let inArgs = 
        ps 
        |> Array.map (function
            | TrainParameters(a,_,_,_)
            | NoTrain a -> a)
    let grads = 
        ps 
        |> Array.map (function
            | TrainParameters(_,g,_,_) -> g
            | NoTrain a -> new NDArray())
    let g = 
        ps 
        |> Array.map (function
            | TrainParameters(_,g,_,_) -> OpReqType.WriteTo
            | NoTrain a -> OpReqType.NullOp)
    let e1 = new Executor(loss, context, inArgs, grads, g, [])
    let e2 = new Executor(unscaledOut, context, inArgs, grads, g, [])
    e1,e2

let xa = ps |> Seq.pick (function NoTrain a -> Some a | _ -> None)

let testZs = Operators.RandomNormalNDArray(shape = [batchSize; 3], ctx = context.ToString()).[0]




let singleBmp pixs = 
    let bitmap = System.Drawing.Bitmap(28*8,28*8)
    let mutable col = 0
    let mutable row = 0
    pixs 
    |> Seq.chunkBySize (28*28)
    |> Seq.iter 
        (fun s ->
            s 
            |> Seq.chunkBySize 28
            |> Seq.iteri
                (fun i xs ->
                    xs 
                    |> Seq.iteri 
                        (fun j x ->
                            let xx = (1.f - x)*255.f |> min 255.f |> max 0.f |> round |> int
                            bitmap.SetPixel(col*27 + j,row*27 + i,Drawing.Color.FromArgb(xx, xx, xx))
                        )
                )
            row <- row + 1
            if row = 8 then 
                row <- 0 
                col <- col + 1
    )
    bitmap


let bmps pixs = 
    pixs 
    |> Seq.chunkBySize (28*28)
    |> Seq.map 
        (fun s ->
            let bitmap = System.Drawing.Bitmap(28,28)
            s 
            |> Seq.chunkBySize 28
            |> Seq.iteri
                (fun i xs ->
                    xs 
                    |> Seq.iteri 
                        (fun j x ->
                            let xx = (1.f - x)*255.f |> min 255.f |> max 0.f |> round |> int
                            bitmap.SetPixel(i,j,Drawing.Color.FromArgb(xx, xx, xx))
                        )
                )
            bitmap
    )
    |> Seq.toArray

let f = Windows.Forms.Form(Visible = true)
let p = Windows.Forms.PictureBox(Dock = Windows.Forms.DockStyle.Fill)
let p2 = Windows.Forms.PictureBox(Dock = Windows.Forms.DockStyle.Fill)
let split = Windows.Forms.SplitContainer(Dock = Windows.Forms.DockStyle.Fill)
split.Panel1.Controls.Add p
split.Panel2.Controls.Add p2
f.Controls.Add split
p.SizeMode <- Windows.Forms.PictureBoxSizeMode.Zoom
p2.SizeMode <- Windows.Forms.PictureBoxSizeMode.Zoom

valIter.Reset()
valIter.Next() |> ignore

let update epoch mb = 
    valIter.GetData().CopyTo(xa)
    let loss : float32 = exe.Forward(false).[0].ToArray().[0]
    let imgs = texe.Forward(false)
    p.Invoke(Action(fun() ->
        p.Image <- singleBmp (xa.ToArray())
        p2.Image <- singleBmp ( imgs.[0].ToArray())
        f.Text <- sprintf "Epoch % 4d  Mb % 7d  Loss: %f" epoch mb loss
    )) |> ignore

update 0

split.KeyUp
|> Observable.add
    (fun k ->
        if k.KeyCode = Windows.Forms.Keys.Space then 
            printfn  "Next test batch"
            if not(valIter.Next()) then 
                valIter.Reset() 
                valIter.Next() |> ignore
    )

let trainTask = 
    async {
        let mutable epoch = 0
        let mutable mb = 0
        while true do
            trainIter.Reset()
            while (trainIter.Next()) do 
                trainIter.GetData().CopyTo(xa)
                let eoutput = exe.Forward(true).[0]
                exe.Backward()
                ps
                |> Array.iter 
                    (function 
                     | NoTrain _ -> ()
                     | TrainParameters(w,g,mu,sd) -> Operators.AdamUpdate([w],w,g,mu,sd,lr)
                     )
                mb <- mb + 1
                if mb % 10 = 0 then 
                    update epoch mb
                //printfn "%f" (eoutput.ToArray() : float32[]).[0]
            epoch <- epoch + 1
            update epoch mb
    } |> Async.StartAsTask        









    


