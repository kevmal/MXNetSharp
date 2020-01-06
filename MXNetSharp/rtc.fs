namespace MXNetSharp.Rtc
#nowarn "9"
open MXNetSharp
open MXNetSharp.Interop
open System.Text.RegularExpressions
open FSharp.NativeInterop

type CudaKernelArgument = 
    {
        Name : string
        IsNDArray : bool
        IsConst : bool
        Type : TypeFlag
    }
type CudaKernel internal (handle : SafeCudaKernelHandle, name : string, args : CudaKernelArgument seq) = 
    let kernelArgs = args |> Seq.toArray
    member x.Handle = handle
    member x.Launch(args : obj[], deviceId : int, gridDims, blockDims, sharedMem) = 
        if args.Length <> kernelArgs.Length then 
            invalidArg "args" (sprintf "CudaKernel(%s) expects %d arguments but got %d"  name kernelArgs.Length args.Length)
        let a = Array.zeroCreate args.Length
        let inline alloc (x : ^a) = 
            let h : nativeptr< ^a> = NativePtr.stackalloc 1
            NativePtr.set h 0 x
            NativePtr.toNativeInt h
        for i = 0 to args.Length - 1 do 
            let k = kernelArgs.[i]
            match k, args.[i] with 
            | {IsNDArray = true}, (:? NDArray as nd) -> 
                if nd.DataTypeFlag <> k.Type then 
                    invalidArg k.Name (sprintf "Expecting NDArray of type %A but is of type %A" k.Type nd.DataTypeFlag)
                else
                    a.[i] <- nd.UnsafeHandle
            | {IsNDArray = true}, _ -> 
                invalidArg k.Name (sprintf "Expecting NDArray of type %A but received type %A" k.Type (a.GetType().Name))
            | {IsNDArray = false}, _ -> 
                let h = 
                    match k.Type, args.[i] with 
                    | TypeFlag.None, _ -> invalidOp "Kernel argument has type None"
                    | TypeFlag.Float32, (:? float32 as n)-> alloc n
                    | TypeFlag.Float64, (:? double as n)-> alloc n
                    | TypeFlag.Float16, (:? int16 as n)-> alloc n
                    | TypeFlag.Uint8, (:? uint8 as n)-> alloc n
                    | TypeFlag.Int32, (:? int32 as n)-> alloc n
                    | TypeFlag.Int8, (:? int8 as n)-> alloc n
                    | TypeFlag.Int64, (:? int64 as n)-> alloc n
                    | _ -> invalidArg k.Name (sprintf "Expecting argument (%s) of type %A received type %s" k.Name k.Type (a.GetType().Name))
                a.[i] <- h
        let gx,gy,gz = gridDims
        let bx,by,bz = blockDims
        MXRtc.cudaKernelCall handle.UnsafeHandle deviceId a (uint32 gx) (uint32 gy) (uint32 gz) (uint32 bx) (uint32 by) (uint32 bz) sharedMem
    member x.Launch(args, ctx : Context, gridDims, blockDims, sharedMem) = 
        let deviceId = 
            match ctx with 
            | GPU(x) -> x
            | _ -> invalidArg "ctx" "Cuda kernel can only be launched on GPU"
        x.Launch(args,deviceId,gridDims,blockDims,sharedMem)

type CudaModule internal (handle : SafeCudaModuleHandle) = 
    new(src : string, ?options : string seq, ?exports : string seq) = 
        let options = defaultArg (options |> Option.map Seq.toArray) Array.empty 
        let exports = defaultArg (exports |> Option.map Seq.toArray) Array.empty 
        CudaModule(new SafeCudaModuleHandle(MXRtc.cudaModuleCreate src options exports, true))
    member x.Handle = handle
    member x.Kernel(name, args : CudaKernelArgument seq) = 
        let args = args |> Seq.toArray
        let bint b = if b then 1 else 0
        let isNDArray, isConst, dtype = 
            args 
            |> Array.map (fun a -> bint a.IsNDArray, bint a.IsConst, int a.Type)
            |> Array.unzip3
        let h = 
            let h = MXRtc.cudaKernelCreate handle.UnsafeHandle name isNDArray isConst dtype
            new SafeCudaKernelHandle(h, true)
        CudaKernel(h, name, args)
    member x.Kernel(name : string, signature : string) = 
        let pattern = Regex(@"^\s*(const)?\s*([\w_]+)\s*(\*)?\s*([\w_]+)?\s*$")
        let args =
            Regex.Replace(signature, @"\s+", " ").Split(',')
            |> Array.mapi 
                (fun i arg -> 
                    let m = pattern.Match(arg)
                    if not m.Success || m.Groups.[2].Value = "const" then 
                        invalidArg "signature" 
                            (sprintf "Invalid function prototype \"%s\". Must be in the form of \"(const) type (*) (name)\"" arg)
                    else    
                        let dtype = 
                            match m.Groups.[2].Value with 
                            | "float" -> TypeFlag.Float32
                            | "double" -> TypeFlag.Float64
                            | "__half" ->  TypeFlag.Float16
                            | "uint8_t" -> TypeFlag.Uint8
                            | "int" -> TypeFlag.Int32
                            | "int32_t" -> TypeFlag.Int32
                            | "int8_t" -> TypeFlag.Int8
                            | "char" -> TypeFlag.Int8
                            | "int64_t" -> TypeFlag.Int64
                            | t -> 
                                invalidArg "signature" 
                                    (sprintf "Unsupported kernel argument type %s. Supported types are float, double, __half, uint8_t, int, int32_t, int8_t, char and int64_t" t)
                        {
                            IsNDArray = m.Groups.[3].Success
                            IsConst = m.Groups.[1].Success
                            Type = dtype
                            Name = if m.Groups.[4].Success then m.Groups.[4].Value else (sprintf "kernel_arg%d" i)
                        }
                )
        x.Kernel(name, args)




    
