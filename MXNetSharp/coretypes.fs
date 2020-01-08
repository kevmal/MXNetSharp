namespace MXNetSharp

open System.Runtime.InteropServices
open System
open MXNetSharp.Interop
open System.Runtime.CompilerServices
open System
open MXNetSharp.Interop

/// Add new axis within indexer
type NewAxis = NewAxis

type SliceRange(?start : int64, ?stop : int64, ?step : int64) = 
    member x.Start = start
    member x.Stop = stop
    member x.Step = step


//https://github.com/apache/incubator-mxnet/blob/62b063802634048fe9da0a736dd6ee429e410f27/python/mxnet/ndarray/ndarray.py#L57-L60
type StorageType = 
    | Undefined
    | Default
    | RowSparse
    | CSR
    static member FromInt(n) = 
        match n with 
        | -1 -> Undefined
        | 0 -> Default
        | 1 -> RowSparse
        | 2 -> CSR
        | _ -> invalidArg "n" (sprintf "Storage type %d is not surported" n)
    member x.ToInt() = 
        match x with 
        | Undefined -> -1
        | Default -> 0
        | RowSparse -> 1
        | CSR -> 2
    static member op_Explicit(st : StorageType) = st.ToInt()
    override x.ToString() =
        match x with
            | CSR -> "csr"
            | Default -> "default"
            | RowSparse -> "row_sparse"
            | Undefined -> ""

type DeviceType = 
    | CPU 
    | GPU
    | CPUPinned 
    static member FromInt(n) = 
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
    static member op_Explicit(st : DeviceType) = st.ToInt()


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
    static member FromDeviceTypeAndId(deviceType : DeviceTypeEnum, id : int) = 
        match deviceType with 
        | DeviceTypeEnum.CPU -> CPU(id)
        | DeviceTypeEnum.GPU -> GPU(id)
        | DeviceTypeEnum.CPUPinned -> CPUPinned(id)
        | _ -> invalidArg "deviceType" (sprintf "Device type %d not suported" (int deviceType))
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
    | Bool
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"
            | Int32 -> "int32"
            | Int64 -> "int64"
            | Int8 -> "int8"
            | UInt8 -> "uint8"
            | Bool -> "bool"
    member x.Type = 
        match x with 
        | Float16 -> None
        | Float32 -> Some(typeof<float32>)
        | Float64 -> Some(typeof<double>)
        | Int32 -> Some(typeof<int>)
        | Int64 -> Some(typeof<int64>)
        | Int8 -> Some(typeof<sbyte>)
        | UInt8 -> Some(typeof<byte>)
        | Bool -> Some(typeof<bool>)
    member x.TypeFlag =  
        match x with 
        | Float16 -> TypeFlag.Float16
        | Float32 -> TypeFlag.Float32
        | Float64 -> TypeFlag.Float64
        | Int32 -> TypeFlag.Int32
        | Int64 -> TypeFlag.Int64
        | Int8 -> TypeFlag.Int8
        | UInt8 -> TypeFlag.Uint8
        | Bool -> TypeFlag.Bool
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
        | TypeFlag.Bool -> Some Bool
        | _ -> None
    static member FromInt(typeFlagInt) = DataType.FromTypeFlag(enum typeFlagInt)
    static member FromNetType(t : Type) = 
        if t = typeof<float32> then 
            Float32
        elif t = typeof<double> then 
            Float64
        elif t = typeof<int> then 
            Int32
        elif t = typeof<int64> then 
            Int64
        elif t = typeof<sbyte> then 
            Int8
        elif t = typeof<byte> then 
            UInt8
        elif t = typeof<bool> then 
            Bool
        else
            failwithf "No corresponding MXNet type for type %s" (t.Name)
    static member TryFromNetType(t : Type) = 
        if t = typeof<float32> then 
            Some Float32
        elif t = typeof<double> then 
            Some Float64
        elif t = typeof<int> then 
            Some Int32
        elif t = typeof<int64> then 
            Some Int64
        elif t = typeof<sbyte> then 
            Some Int8
        elif t = typeof<byte> then 
            Some UInt8
        elif t = typeof<bool> then 
            Some Bool
        else
            None
    static member FromNetType<'a>() = 
        if typeof<'a> = typeof<float32> then 
            Float32
        elif typeof<'a> = typeof<double> then 
            Float64
        elif typeof<'a> = typeof<int> then 
            Int32
        elif typeof<'a> = typeof<int64> then 
            Int64
        elif typeof<'a> = typeof<sbyte> then 
            Int8
        elif typeof<'a> = typeof<byte> then 
            UInt8
        elif typeof<'a> = typeof<bool> then 
            Bool
        else
            failwithf "No corresponding MXNet type for type %s" (typeof<'a>.Name)
    static member TryFromNetType<'a>() = 
        if typeof<'a> = typeof<float32> then 
            Some Float32
        elif typeof<'a> = typeof<double> then 
            Some Float64
        elif typeof<'a> = typeof<int> then 
            Some Int32
        elif typeof<'a> = typeof<int64> then 
            Some Int64
        elif typeof<'a> = typeof<sbyte> then 
            Some Int8
        elif typeof<'a> = typeof<byte> then 
            Some UInt8
        elif typeof<'a> = typeof<bool> then 
            Some Bool
        else
            None


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
    static member op_Explicit(st : OpReqType) = st.OpReqTypeInt
    static member op_Explicit(st : OpReqType) = uint32 st.OpReqTypeInt

type ArrayConverter private () = 
    static member inline Float32(a : float32 []) = a
    static member inline Float32(a : double []) = a |> Array.map float32
    static member inline Float32(a : int32 []) = a |> Array.map float32
    static member inline Float32(a : int64 []) = a |> Array.map float32
    static member inline Float32(a : decimal []) = a |> Array.map float32
    static member inline Float32(a : int8 []) = a |> Array.map float32
    static member inline Float32(a : uint8 []) = a |> Array.map float32
    static member inline Float32(a : bool []) : float32 [] = a |> Array.map (fun x -> if x then LanguagePrimitives.GenericOne else LanguagePrimitives.GenericZero)
    static member inline Float32(a : 'a []) = 
        if typeof<'a> = typeof<float32> then 
            ArrayConverter.Float32(unbox(box a) : float32 [])
        elif typeof<'a> = typeof<double> then 
            ArrayConverter.Float32(unbox(box a) : double [])
        elif typeof<'a> = typeof<int32> then 
            ArrayConverter.Float32(unbox(box a) : int32 [])
        elif typeof<'a> = typeof<int64> then 
            ArrayConverter.Float32(unbox(box a) : int64 [])
        elif typeof<'a> = typeof<decimal> then 
            ArrayConverter.Float32(unbox(box a) : decimal [])
        elif typeof<'a> = typeof<int8> then 
            ArrayConverter.Float32(unbox(box a) : int8 [])
        elif typeof<'a> = typeof<uint8> then 
            ArrayConverter.Float32(unbox(box a) : uint8 [])
        elif typeof<'a> = typeof<bool> then 
            ArrayConverter.Float32(unbox(box a) : bool [])
        else
            a |> Array.map (fun x -> Convert.ChangeType(x, typeof<float32>) :?> float32)
    static member inline Float64(a : float32 []) = a |> Array.map double
    static member inline Float64(a : double []) = a 
    static member inline Float64(a : int32 []) = a |> Array.map double
    static member inline Float64(a : int64 []) = a |> Array.map double
    static member inline Float64(a : decimal []) = a |> Array.map double
    static member inline Float64(a : int8 []) = a |> Array.map double
    static member inline Float64(a : uint8 []) = a |> Array.map double
    static member inline Float64(a : bool []) : double [] = a |> Array.map (fun x -> if x then LanguagePrimitives.GenericOne else LanguagePrimitives.GenericZero)
    static member inline Float64(a : 'a []) = 
        if typeof<'a> = typeof<float32> then 
            ArrayConverter.Float64(unbox(box a) : float32 [])
        elif typeof<'a> = typeof<double> then 
            ArrayConverter.Float64(unbox(box a) : double [])
        elif typeof<'a> = typeof<int32> then 
            ArrayConverter.Float64(unbox(box a) : int32 [])
        elif typeof<'a> = typeof<int64> then 
            ArrayConverter.Float64(unbox(box a) : int64 [])
        elif typeof<'a> = typeof<decimal> then 
            ArrayConverter.Float64(unbox(box a) : decimal [])
        elif typeof<'a> = typeof<int8> then 
            ArrayConverter.Float64(unbox(box a) : int8 [])
        elif typeof<'a> = typeof<uint8> then 
            ArrayConverter.Float64(unbox(box a) : uint8 [])
        elif typeof<'a> = typeof<bool> then 
            ArrayConverter.Float64(unbox(box a) : bool [])
        else
            a |> Array.map (fun x -> Convert.ChangeType(x, typeof<double>) :?> double)
    static member inline Int32(a : float32 []) = a |> Array.map int
    static member inline Int32(a : double []) = a |> Array.map int
    static member inline Int32(a : int32 []) = a 
    static member inline Int32(a : int64 []) = a |> Array.map int
    static member inline Int32(a : decimal []) = a |> Array.map int
    static member inline Int32(a : int8 []) = a |> Array.map int
    static member inline Int32(a : uint8 []) = a |> Array.map int
    static member inline Int32(a : bool []) : int [] = a |> Array.map (fun x -> if x then LanguagePrimitives.GenericOne else LanguagePrimitives.GenericZero)
    static member inline Int32(a : 'a []) = 
        if typeof<'a> = typeof<float32> then 
            ArrayConverter.Int32(unbox(box a) : float32 [])
        elif typeof<'a> = typeof<double> then 
            ArrayConverter.Int32(unbox(box a) : double [])
        elif typeof<'a> = typeof<int32> then 
            ArrayConverter.Int32(unbox(box a) : int32 [])
        elif typeof<'a> = typeof<int64> then 
            ArrayConverter.Int32(unbox(box a) : int64 [])
        elif typeof<'a> = typeof<decimal> then 
            ArrayConverter.Int32(unbox(box a) : decimal [])
        elif typeof<'a> = typeof<int8> then 
            ArrayConverter.Int32(unbox(box a) : int8 [])
        elif typeof<'a> = typeof<uint8> then 
            ArrayConverter.Int32(unbox(box a) : uint8 [])
        elif typeof<'a> = typeof<bool> then 
            ArrayConverter.Int32(unbox(box a) : bool [])
        else
            a |> Array.map (fun x -> Convert.ChangeType(x, typeof<int>) :?> int)
    static member inline Int64(a : float32 []) = a |> Array.map int64
    static member inline Int64(a : double []) = a |> Array.map int64
    static member inline Int64(a : int32 []) = a |> Array.map int64
    static member inline Int64(a : int64 []) = a
    static member inline Int64(a : decimal []) = a |> Array.map int64
    static member inline Int64(a : int8 []) = a |> Array.map int64
    static member inline Int64(a : uint8 []) = a |> Array.map int64
    static member inline Int64(a : bool []) : int64 [] = a |> Array.map (fun x -> if x then LanguagePrimitives.GenericOne else LanguagePrimitives.GenericZero)
    static member inline Int64(a : 'a []) = 
        if typeof<'a> = typeof<float32> then 
            ArrayConverter.Int64(unbox(box a) : float32 [])
        elif typeof<'a> = typeof<double> then 
            ArrayConverter.Int64(unbox(box a) : double [])
        elif typeof<'a> = typeof<int32> then 
            ArrayConverter.Int64(unbox(box a) : int32 [])
        elif typeof<'a> = typeof<int64> then 
            ArrayConverter.Int64(unbox(box a) : int64 [])
        elif typeof<'a> = typeof<decimal> then 
            ArrayConverter.Int64(unbox(box a) : decimal [])
        elif typeof<'a> = typeof<int8> then 
            ArrayConverter.Int64(unbox(box a) : int8 [])
        elif typeof<'a> = typeof<uint8> then 
            ArrayConverter.Int64(unbox(box a) : uint8 [])
        elif typeof<'a> = typeof<bool> then 
            ArrayConverter.Int64(unbox(box a) : bool [])
        else
            a |> Array.map (fun x -> Convert.ChangeType(x, typeof<int64>) :?> int64)
    static member inline Int8(a : float32 []) = a |> Array.map int8
    static member inline Int8(a : double []) = a |> Array.map int8
    static member inline Int8(a : int32 []) = a |> Array.map int8
    static member inline Int8(a : int64 []) = a |> Array.map int8
    static member inline Int8(a : decimal []) = a |> Array.map int8
    static member inline Int8(a : int8 []) = a
    static member inline Int8(a : uint8 []) = a |> Array.map int8
    static member inline Int8(a : bool []) : int8 [] = a |> Array.map (fun x -> if x then LanguagePrimitives.GenericOne else LanguagePrimitives.GenericZero)
    static member inline Int8(a : 'a []) = 
        if typeof<'a> = typeof<float32> then 
            ArrayConverter.Int8(unbox(box a) : float32 [])
        elif typeof<'a> = typeof<double> then 
            ArrayConverter.Int8(unbox(box a) : double [])
        elif typeof<'a> = typeof<int32> then 
            ArrayConverter.Int8(unbox(box a) : int32 [])
        elif typeof<'a> = typeof<int64> then 
            ArrayConverter.Int8(unbox(box a) : int64 [])
        elif typeof<'a> = typeof<decimal> then 
            ArrayConverter.Int8(unbox(box a) : decimal [])
        elif typeof<'a> = typeof<int8> then 
            ArrayConverter.Int8(unbox(box a) : int8 [])
        elif typeof<'a> = typeof<uint8> then 
            ArrayConverter.Int8(unbox(box a) : uint8 [])
        elif typeof<'a> = typeof<bool> then 
            ArrayConverter.Int8(unbox(box a) : bool [])
        else
            a |> Array.map (fun x -> Convert.ChangeType(x, typeof<int8>) :?> int8)
    static member inline UInt8(a : float32 []) = a |> Array.map uint8
    static member inline UInt8(a : double []) = a |> Array.map uint8
    static member inline UInt8(a : int32 []) = a |> Array.map uint8
    static member inline UInt8(a : int64 []) = a |> Array.map uint8
    static member inline UInt8(a : decimal []) = a |> Array.map uint8
    static member inline UInt8(a : int8 []) = a |> Array.map uint8
    static member inline UInt8(a : uint8 []) = a
    static member inline UInt8(a : bool []) : uint8 [] = a |> Array.map (fun x -> if x then LanguagePrimitives.GenericOne else LanguagePrimitives.GenericZero)
    static member inline UInt8(a : 'a []) = 
        if typeof<'a> = typeof<float32> then 
            ArrayConverter.UInt8(unbox(box a) : float32 [])
        elif typeof<'a> = typeof<double> then 
            ArrayConverter.UInt8(unbox(box a) : double [])
        elif typeof<'a> = typeof<int32> then 
            ArrayConverter.UInt8(unbox(box a) : int32 [])
        elif typeof<'a> = typeof<int64> then 
            ArrayConverter.UInt8(unbox(box a) : int64 [])
        elif typeof<'a> = typeof<decimal> then 
            ArrayConverter.UInt8(unbox(box a) : decimal [])
        elif typeof<'a> = typeof<int8> then 
            ArrayConverter.UInt8(unbox(box a) : int8 [])
        elif typeof<'a> = typeof<uint8> then 
            ArrayConverter.UInt8(unbox(box a) : uint8 [])
        elif typeof<'a> = typeof<bool> then 
            ArrayConverter.UInt8(unbox(box a) : bool [])
        else
            a |> Array.map (fun x -> Convert.ChangeType(x, typeof<uint8>) :?> uint8)
    static member inline Bool(a : float32 []) = a |> Array.map (fun x -> x <> LanguagePrimitives.GenericZero)
    static member inline Bool(a : double []) = a |> Array.map (fun x -> x <> LanguagePrimitives.GenericZero)
    static member inline Bool(a : int32 []) = a |> Array.map (fun x -> x <> LanguagePrimitives.GenericZero)
    static member inline Bool(a : int64 []) = a |> Array.map (fun x -> x <> LanguagePrimitives.GenericZero)
    static member inline Bool(a : decimal []) = a |> Array.map (fun x -> x <> LanguagePrimitives.GenericZero)
    static member inline Bool(a : int8 []) = a |> Array.map (fun x -> x <> LanguagePrimitives.GenericZero)
    static member inline Bool(a : uint8 []) = a |> Array.map (fun x -> x <> LanguagePrimitives.GenericZero)
    static member inline Bool(a : bool []) = a
    static member inline Bool(a : 'a []) = 
        if typeof<'a> = typeof<float32> then 
            ArrayConverter.Bool(unbox(box a) : float32 [])
        elif typeof<'a> = typeof<double> then 
            ArrayConverter.Bool(unbox(box a) : double [])
        elif typeof<'a> = typeof<int32> then 
            ArrayConverter.Bool(unbox(box a) : int32 [])
        elif typeof<'a> = typeof<int64> then 
            ArrayConverter.Bool(unbox(box a) : int64 [])
        elif typeof<'a> = typeof<decimal> then 
            ArrayConverter.Bool(unbox(box a) : decimal [])
        elif typeof<'a> = typeof<int8> then 
            ArrayConverter.Bool(unbox(box a) : int8 [])
        elif typeof<'a> = typeof<uint8> then 
            ArrayConverter.Bool(unbox(box a) : uint8 [])
        elif typeof<'a> = typeof<bool> then 
            ArrayConverter.Bool(unbox(box a) : bool [])
        else
            a |> Array.map (fun x -> Convert.ChangeType(x, typeof<bool>) :?> bool)





module Util = 
    let inline internal valueStringHelper (_ : ^a) (x : ^b) = 
        ((^a or ^b) : (static member ValueString : ^b -> string) (x))
    let inline valueString (x : ^t) = valueStringHelper ValueStringExtensions x

    let inline convertArray (a : ^a []) (dtype : DataType) = 
        match dtype with 
        | Float16 -> failwith "float16 not supported yet" //TODO: float16
        | Float32 -> ArrayConverter.Float32(a) :> Array
        | Float64 -> ArrayConverter.Float64(a) :> Array
        | Int32 -> ArrayConverter.Int32(a) :> Array
        | Int64 -> ArrayConverter.Int64(a) :> Array
        | Int8 -> ArrayConverter.Int8(a) :> Array
        | UInt8 -> ArrayConverter.UInt8(a) :> Array
        | Bool -> ArrayConverter.Bool(a) :> Array
