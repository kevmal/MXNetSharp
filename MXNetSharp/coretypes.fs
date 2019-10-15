namespace MXNetSharp

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
        
module Util = 
    let inline internal retype (x: 'T) : 'U = (# "" x: 'U #)
    let inline valueString (x : ^a) = 
        if LanguagePrimitives.PhysicalEquality typeof< ^a> typeof<bool> then 
            if retype x then "1" else "0"
        elif LanguagePrimitives.PhysicalEquality typeof< ^a> typeof<string> then 
            retype x
        elif LanguagePrimitives.PhysicalEquality typeof< ^a> typeof<int []> then 
            retype x |> Array.map string |> String.concat "," |> sprintf "[%s]"
        elif LanguagePrimitives.PhysicalEquality typeof< ^a> typeof<int seq> then 
            retype x |> Seq.map string |> String.concat "," |> sprintf "[%s]"
        else 
            string x
        
    
