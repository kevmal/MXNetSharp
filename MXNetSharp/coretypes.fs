namespace MXNetSharp


// defined in cpp-package/include/mxnet-cpp/ndarray.h
// https://github.com/apache/incubator-mxnet/blob/745a41ca1a6d74a645911de8af46dece03db93ea/cpp-package/include/mxnet-cpp/ndarray.h#L41
type DeviceType = 
    | CPU
    | GPU
    | CPUPinned
    member x.DeviceTypeIntr = 
        match x with 
        | CPU -> 1
        | GPU -> 2
        | CPUPinned -> 3


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
    
type Context = 
    {
        DeviceType : DeviceType
        DeviceId : int
    }



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
        else 
            string x
        
    
