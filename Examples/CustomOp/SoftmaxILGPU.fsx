open System.Text

#load "../load.fsx"
#load "../loadilgpu.fsx"
#r @"E:\profile\Projects\repo\MXNetSharp\packages\System.Runtime.CompilerServices.Unsafe.4.5.0\lib\netstandard2.0\System.Runtime.CompilerServices.Unsafe.dll"
open MXNetSharp
open MXNetSharp.Interop
open MXNetSharp.SymbolOperators
open MXNetSharp.Operator
open MXNetSharp.SymbolArgument
open ILGPU
open ILGPU.Runtime
open ILGPU.Runtime.Cuda
open ILGPU.Algorithms

let inline (!) (x : NDArray) =
        let ptr = MXNDArray.getData x.UnsafeHandle
        let wrap = ILGPU.Runtime.ViewPointerWrapper.Create(ptr)
        let len = x.Shape |> Array.reduce (*)
        new ArrayView< ^a>(wrap, Index 0, Index len)

type ConstAttribute() = inherit System.Attribute()
type MySoftMaxImpl() = 
    static member Forward(index : Index, [<Const>] x : ArrayView<float32>, y : ArrayView<float32>, rowSize, req) = 
        let offset = rowSize*index.X
        let mutable mx = x.[offset]
        for i = 1 to rowSize - 1 do 
            if mx < x.[offset + i] then 
                mx <- x.[offset + i]
        let mutable sum = LanguagePrimitives.GenericZero
        for i = 0 to rowSize - 1 do 
            sum <- XMath.Exp(x.[offset + i] - mx) + sum
        if req = 1 then 
            for i = 0 to rowSize - 1 do 
                y.[offset + i] <- XMath.Exp(x.[offset + i] - mx) / sum
        elif req = 2 then 
            for i = 0 to rowSize - 1 do 
                y.[offset + i] <- XMath.Exp(x.[offset + i] - mx) / sum + y.[offset + i] 


let ctx = Context()
ctx.EnableAlgorithms()
let acc = CudaAccelerator(ctx)


let minfo = typeof<MySoftMaxImpl>.GetMethod("Forward")
let mutable spec = Runtime.KernelSpecialization.Empty
let ck = acc.Backend.Compile(minfo, &spec)
let k = acc.LoadKernel(minfo)

let inp = (GPU 0).Arange(1.0,11.0).Reshape(2,5)
let inp2 = (GPU 0).Arange(1.0,11.0).Reshape(2,5)

let aa : float32 ArrayView = !inp
let bb : float32 ArrayView  = !inp2
//k.Launch(acc.DefaultStream, Index 10, aa, bb, 5, 1)
//acc.Synchronize()

let f = 
    CApi.EngineSyncFunc
        (fun _ _ -> 
            k.Launch(acc.DefaultStream, Index 10, aa, bb, 5, 1)
            printfn "Sleep"
            Async.Sleep 5000 |> Async.RunSynchronously
            printfn "Down"
            acc.Synchronize()
        )
let qq  = [| 0uy; 0uy; 0uy; 0x02uy |]
let contextHandle = NativeInterop.NativePtr.stackalloc 2
NativeInterop.NativePtr.set contextHandle 0 1 //(System.BitConverter.ToInt32(qq,0))
NativeInterop.NativePtr.set contextHandle 1 0
MXEngine.pushSyncND f 0n null (NativeInterop.NativePtr.toNativeInt contextHandle) ([|inp.UnsafeHandle|]) ([|inp2.UnsafeHandle|]) 0n 0 "poo"

printfn "continue"
NDArray.WaitAll()
printfn "done waiting"



inp2.ToFloat32Array()



ctxptr

inp2.ToFloat32Array()


CustomOp.register "mysoftmax"
    (fun ins ->
        {new CustomOperation() with
             member this.CreateOperator(context, inShapes: int [] [], inDataTypes: TypeFlag []): ICustomOperation = 
                 {new ICustomOperation with
                      member this.Backward(req: OpReqType [], inData: NDArray [], outData: NDArray [], inGrad: NDArray [], outGrad: NDArray [], auxData: NDArray []): unit = 
                          ()                            
                      member this.Forward(isTrain: bool, req: OpReqType [], inData: NDArray [], outData: NDArray [], auxData: NDArray []): unit = 
                          if req.[0] = OpReqType.NullOp then 
                              () 
                          else
                              let x = inData.[0]
                              let y = outData.[0]
                              match y.DataType with 
                              | Some DataType.Float32 -> fwdSingle.Launch([|x:>obj;y:>_;x.Shape.[1]:>_; int req.[0]:>_|], 0, (1,1,1), (x.Shape.[0],1,1), 0u)
                              | Some DataType.Float64 -> fwdDouble.Launch([|x:>obj;y:>_;x.Shape.[1]:>_; int req.[0]:>_|], 0, (1,1,1), (x.Shape.[0],1,1), 0u)
                              | y -> failwithf "%A data type not supported" y
                 }
             //member this.DeclareBackwardDependency(outGrad: int [], inData: int [], outData: int []): int [] = 
             //    raise (System.NotImplementedException())
             //member this.InferBackwardStorageType(storageTypes: BackwardStorageTypes): unit = 
             //    raise (System.NotImplementedException())
             member this.InferShape(inShape: int [] []): int [] [] * int [] [] * int [] [] = 
                 let dataShape = inShape.[0]
                 let labelShape = [|inShape.[0].[0]|]
                 let outputShape = inShape.[0]
                 [|dataShape; labelShape|], [|outputShape|], Array.empty
             //member this.InferStorageType(inputStorageTypes: StorageType []): StorageType [] * StorageType [] * StorageType [] = 
             //    let outType = this.ListOutputs() |> Array.map (fun x -> StorageType.Default)
             //    let auxType = this.ListAuxiliaryStates() |> Array.map (fun x -> StorageType.Default)
             //    inputStorageTypes, outType, auxType
             //member this.InferType(inType: TypeFlag []): TypeFlag [] * TypeFlag [] * TypeFlag [] = 
             //    let outType = this.ListOutputs() |> Array.map (fun x -> inType.[0])
             //    let auxType = this.ListAuxiliaryStates() |> Array.map (fun x -> inType.[0])
             //    inType, outType, auxType
             member this.ListArguments(): string [] = [|"data"; "label"|]
             //member this.ListAuxiliaryStates(): string [] = Array.empty
             //member this.ListOutputs(): string [] = [|"output"|]
        } :> _
    )
 
type MySoftmax private (operatorArguments) = 
    inherit SymbolOperator("Custom", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MySoftmax(args)
    override this.WithArguments(args : Arguments<Symbol>) = new MySoftmax(this.OperatorArguments.AddReplace(args)) :> Symbol
    new(?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "op_type", Parameter(Some("mysoftmax" :> obj))
            ]
        new MySoftmax(Arguments<Symbol>(operatorArguments))

    member __.Data = operatorArguments.GetInput "data"

open MXNetSharp
let x = Input("x", ndarray = inp)
let op = MySoftmax(x)

let bm = 
    op.Bindings
    |> Bindings.inferShapes op
    |> Bindings.init (fun _ s -> (GPU 0).Zeros(s))

let exe = op.Bind(GPU 0, bm)
exe.Forward(true)
exe.Outputs.[0].ToFloat32Array()







