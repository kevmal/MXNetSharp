namespace MXNetSharp

open System.Runtime.InteropServices
open System
open MXNetSharp.Interop
open System.Runtime.CompilerServices
open System

//https://github.com/apache/incubator-mxnet/blob/62b063802634048fe9da0a736dd6ee429e410f27/python/mxnet/ndarray/ndarray.py#L57-L60
type StorageType = 
    | Undefined = -1
    | Default = 0
    | RowSparse = 1
    | CSR = 2


// defined in mshadow/base.h
// https://github.com/apache/incubator-mxnet/blob/618c4811e417fb86cbb3fc0f7f38d55972eeb2af/3rdparty/mshadow/mshadow/base.h#L306
type TypeFlag = 
    | None = -1
    | Float32 = 0
    | Float64 = 1
    | Float16 = 2
    | Uint8 = 3
    | Int32 = 4
    | Int8  = 5
    | Int64 = 6


// defined in cpp-package/include/mxnet-cpp/ndarray.h
// https://github.com/apache/incubator-mxnet/blob/745a41ca1a6d74a645911de8af46dece03db93ea/cpp-package/include/mxnet-cpp/ndarray.h#L41
type DeviceTypeEnum =
    | CPU = 1
    | GPU = 2
    | CPUPinned = 3

type DeviceType = 
    | CPU 
    | GPU
    | CPUPinned 
    member x.FromInt(n) = 
        match n with
        | 1 -> CPU
        | 2 -> GPU
        | 3 -> CPUPinned
        | _ -> invalidArg "n" (sprintf "Unknown device %d" n) 
    member x.ToEnum() = 
        match x with 
        | CPU -> DeviceTypeEnum.CPU
        | GPU -> DeviceTypeEnum.GPU
        | CPUPinned -> DeviceTypeEnum.CPUPinned
    member x.ToInt() = x.ToEnum() |> int


type Context = 
    | CPU of int
    | GPU of int
    | CPUPinned of int
    override x.ToString() = 
        match x with 
        | CPU id -> sprintf "cpu(%d)" id
        | GPU id -> sprintf "gpu(%d)" id
        | CPUPinned id -> sprintf "cpupinned(%d)" id
    member x.DeviceType = 
        match x with 
        | CPU _ -> DeviceTypeEnum.CPU
        | GPU _ -> DeviceTypeEnum.GPU
        | CPUPinned _ -> DeviceTypeEnum.CPUPinned
    member x.DeviceId = 
        match x with 
        | CPU n 
        | GPU n 
        | CPUPinned n -> n
    static member TryParse(str : String) = 
        let str2 = str.Trim().ToLower()
        let tryPrefix (prefix : string) f = 
            if str2.StartsWith prefix then 
                let scc,v = Int32.TryParse(str2.Substring(prefix.Length).Trim(')'))
                if scc then Some (f v) else None
            else 
                None
        seq{
            "cpu(", CPU
            "gpu(", GPU
            "cpupinned(", CPUPinned
        }
        |> Seq.tryPick (fun (p,f) -> tryPrefix p f)
        

type DataType = 
    | Float16
    | Float32
    | Float64
    | Int32
    | Int64
    | Int8
    | UInt8
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"
            | Int32 -> "int32"
            | Int64 -> "int64"
            | Int8 -> "int8"
            | UInt8 -> "uint8"
    member x.Type = 
        match x with 
        | Float16 -> None
        | Float32 -> Some(typeof<float32>)
        | Float64 -> Some(typeof<double>)
        | Int32 -> Some(typeof<int>)
        | Int64 -> Some(typeof<int64>)
        | Int8 -> Some(typeof<sbyte>)
        | UInt8 -> Some(typeof<byte>)
    member x.TypeFlag =  
        match x with 
        | Float16 -> TypeFlag.Float16
        | Float32 -> TypeFlag.Float32
        | Float64 -> TypeFlag.Float64
        | Int32 -> TypeFlag.Int32
        | Int64 -> TypeFlag.Int64
        | Int8 -> TypeFlag.Int8
        | UInt8 -> TypeFlag.Uint8
    static member FromTypeFlag(typeflag : TypeFlag) = 
        match typeflag with 
        | TypeFlag.None -> None 
        | TypeFlag.Float16 -> Some Float16
        | TypeFlag.Float32 -> Some Float32
        | TypeFlag.Float64 -> Some Float64
        | TypeFlag.Int32 -> Some Int32
        | TypeFlag.Int64 -> Some Int64
        | TypeFlag.Int8 -> Some Int8
        | TypeFlag.Uint8 -> Some UInt8
        | _ -> None
    static member FromInt(typeFlagInt) = DataType.FromTypeFlag(enum typeFlagInt)
        
        


// From https://github.com/apache/incubator-mxnet/blob/225f71f744ac5e7bd29868b6d3ba0e4fe2527c43/cpp-package/include/mxnet-cpp/base.h#L39
type OpReqType =
    | NullOp
    | WriteTo
    | WriteInplace
    | AddTo
    member x.OpReqTypeInt = 
        match x with 
        | NullOp -> 0
        | WriteTo -> 1
        | WriteInplace -> 2
        | AddTo -> 3
    static member FromInt(i : int) = 
        match i with 
        | 0 -> NullOp
        | 1 -> WriteTo
        | 2 -> WriteInplace
        | 3 -> AddTo
        | _ -> invalidArg "i" (sprintf "OpReqType must be in {0,1,2,3}. Received %d" i)

type SafeSymbolHandle(owner) = 
    inherit SafeHandle(0n, true)
    new() = new SafeSymbolHandle(true)
    new(ptr,owner) as this = new SafeSymbolHandle(owner) then this.SetHandle(ptr)
    override x.IsInvalid = x.handle <= 0n
    override x.ReleaseHandle() = CApi.MXSymbolFree x.handle = 0
    member internal x.UnsafeHandle = 
        if not x.IsClosed then
            x.handle
        else
            ObjectDisposedException("SafeSymbolHandle", "Symbol handle has been closed") |> raise


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
        
[<Extension>]
type ValueStringExtensions = ValueStringExtensions with
    [<Extension>] 
    static member ValueString(x : int option seq) = x |> Seq.map (function Some x -> string x | _ -> "None") |> String.concat "," |> sprintf "[%s]"
    [<Extension>] 
    static member ValueString(x : int seq) = x |> Seq.map string |> String.concat "," |> sprintf "[%s]"
    [<Extension>] 
    static member ValueString(x : int64 seq) = x |> Seq.map string |> String.concat "," |> sprintf "[%s]"
    [<Extension>] 
    static member ValueString(x : double seq) = x |> Seq.map string |> String.concat "," |> sprintf "[%s]"
    [<Extension>] 
    static member ValueString(x : bool) = if x then "1" else "0"
    [<Extension>] 
    static member ValueString(x : string) = x
    [<Extension>] 
    static member ValueString(x : obj) = 
        match x with 
        | :? bool as x -> x.ValueString()
        | :? string as x -> x
        | :? seq<int> as x -> x.ValueString()
        | :? seq<int option> as x -> x.ValueString()
        | :? seq<double> as x -> x.ValueString()
        | :? seq<int64> as x -> x.ValueString()
        | _ -> string x
        



module Util = 
    let inline internal valueStringHelper (_ : ^a) (x : ^b) = 
        ((^a or ^b) : (static member ValueString : ^b -> string) (x))
    let inline valueString (x : ^t) = valueStringHelper ValueStringExtensions x
