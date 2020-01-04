#load "../load.fsx"
open Load

open MXNetSharp
open MXNetSharp.PrimitiveOperators
open MXNetSharp.SymbolOperators
open MXNetSharp.Operator
open MXNetSharp.SymbolArgument



let forwardSrc = """
template<class DType>
__global__ void fwd(const DType* x, DType* y, const int row_size, const int req) {
    const int offset = row_size * threadIdx.x;
    DType max = x[offset];
    for(int i = 1; i < row_size; ++i) {
        if(max < x[offset + i]) {
            max = x[offset + i];
        }
    }
    DType sum = 0;
    for(int i = 0; i < row_size; ++i) {
        sum += exp(x[offset + i] - max);
    }
    switch(req) {
        case 1:
            for(int i = 0; i < row_size; ++i) {
                y[offset + i] = exp(x[offset + i] - max) / sum;
            }
            break;
        case 2:
            for(int i = 0; i < row_size; ++i) {
                y[offset + i] += exp(x[offset + i] - max) / sum;
            }
            break;
    }
}"""
let backwardSrc = """
template<class DType>
__global__ void bwd(const DType* l, const DType* y, DType* dx, const int req) {
    const int z = static_cast<int>(l[blockIdx.x]);
    const int i = threadIdx.x + blockDim.x * blockIdx.x;
    if(req == 1) {
        dx[i]  = threadIdx.x == z ? y[i] - 1 : y[i];
    } else {
        dx[i] += threadIdx.x == z ? y[i] - 1 : y[i];
    }
}
"""


let fwdModule = Rtc.CudaModule(forwardSrc, exports = ["fwd<float>"; "fwd<double>"])
let fwdSingle = fwdModule.Kernel("fwd<float>", "const float* x, float* y, const int row_size, const int req")
let fwdDouble = fwdModule.Kernel("fwd<double>", "const double* x, double* y, const int row_size, const int req")
let bwdModule = Rtc.CudaModule(backwardSrc, exports = ["bwd<float>"; "bwd<double>"])
let bwdSingle = bwdModule.Kernel("bwd<float>", "const float* l, const float* y, float* dx, const int row_size, const int req")
let bwdDouble = bwdModule.Kernel("bwd<double>", "const double* l, const double* y, double* dx, const int req")


CustomOp.register "mysoftmax"
    (fun ins ->
        {new CustomOperation() with
             member this.CreateOperator(context: Context, inShapes: int [] [], inDataTypes: TypeFlag []): ICustomOperation = 
                 {new ICustomOperation with
                      member this.Backward(req: OpReqType [], inData: NDArray [], outData: NDArray [], inGrad: NDArray [], outGrad: NDArray [], auxData: NDArray []): unit = 
                          if req.[0] = OpReqType.NullOp then 
                              () 
                          else
                              let l = inData.[0]
                              let y = outData.[0]
                              let dx = inGrad.[0]
                              match y.DataType with 
                              | Some DataType.Float32 -> bwdSingle.Launch([|l:>obj;y:>_;dx:>_; int req.[0]:>_|], 0, (y.Shape.[0],1,1), (y.Shape.[0],1,1), 0u)
                              | Some DataType.Float64 -> bwdDouble.Launch([|l:>obj;y:>_;dx:>_; int req.[0]:>_|], 0, (y.Shape.[0],1,1), (y.Shape.[0],1,1), 0u)
                              | y -> failwithf "%A data type not supported" y
                            
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

let inp = (GPU 0).Arange(1.0,11.0).Reshape(2,5)

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







