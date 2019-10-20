namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop

[<AutoOpen>]
module GeneratedArgumentTypes = 

    type ContribBilinearResize2DMode = 
        | Like
        | OddScale
        | Size
        | ToEvenDown
        | ToEvenUp
        | ToOddDown
        | ToOddUp
        override x.ToString() =
            match x with
                | Like -> "like"
                | OddScale -> "odd_scale"
                | Size -> "size"
                | ToEvenDown -> "to_even_down"
                | ToEvenUp -> "to_even_up"
                | ToOddDown -> "to_odd_down"
                | ToOddUp -> "to_odd_up"
    
    type Format = 
        | Center
        | Corner
        override x.ToString() =
            match x with
                | Center -> "center"
                | Corner -> "corner"
    
    type LeakyReLUType = 
        | Elu
        | Gelu
        | Leaky
        | Prelu
        | Rrelu
        | Selu
        override x.ToString() =
            match x with
                | Elu -> "elu"
                | Gelu -> "gelu"
                | Leaky -> "leaky"
                | Prelu -> "prelu"
                | Rrelu -> "rrelu"
                | Selu -> "selu"
    
    type ActType = 
        | Relu
        | Sigmoid
        | Softrelu
        | Softsign
        | Tanh
        override x.ToString() =
            match x with
                | Relu -> "relu"
                | Sigmoid -> "sigmoid"
                | Softrelu -> "softrelu"
                | Softsign -> "softsign"
                | Tanh -> "tanh"
    
    type CudnnTune = 
        | Fastest
        | LimitedWorkspace
        | Off
        override x.ToString() =
            match x with
                | Fastest -> "fastest"
                | LimitedWorkspace -> "limited_workspace"
                | Off -> "off"
    
    type ConvolutionLayout = 
        | NCDHW
        | NCHW
        | NCW
        | NDHWC
        | NHWC
        override x.ToString() =
            match x with
                | NCDHW -> "NCDHW"
                | NCHW -> "NCHW"
                | NCW -> "NCW"
                | NDHWC -> "NDHWC"
                | NHWC -> "NHWC"
    
    type BlankLabel = 
        | First
        | Last
        override x.ToString() =
            match x with
                | First -> "first"
                | Last -> "last"
    
    type DeconvolutionLayout = 
        | NCDHW
        | NCHW
        | NCW
        | NDHWC
        | NHWC
        override x.ToString() =
            match x with
                | NCDHW -> "NCDHW"
                | NCHW -> "NCHW"
                | NCW -> "NCW"
                | NDHWC -> "NDHWC"
                | NHWC -> "NHWC"
    
    type DropoutMode = 
        | Always
        | Training
        override x.ToString() =
            match x with
                | Always -> "always"
                | Training -> "training"
    
    type FloatDType = 
        | Float16
        | Float32
        | Float64
        override x.ToString() =
            match x with
                | Float16 -> "float16"
                | Float32 -> "float32"
                | Float64 -> "float64"
    
    type PoolType = 
        | Avg
        | Lp
        | Max
        | Sum
        override x.ToString() =
            match x with
                | Avg -> "avg"
                | Lp -> "lp"
                | Max -> "max"
                | Sum -> "sum"
    
    type PoolingConvention = 
        | Full
        | Same
        | Valid
        override x.ToString() =
            match x with
                | Full -> "full"
                | Same -> "same"
                | Valid -> "valid"
    
    type PoolingLayout = 
        | NCDHW
        | NCHW
        | NCW
        | NDHWC
        | NHWC
        | NWC
        override x.ToString() =
            match x with
                | NCDHW -> "NCDHW"
                | NCHW -> "NCHW"
                | NCW -> "NCW"
                | NDHWC -> "NDHWC"
                | NHWC -> "NHWC"
                | NWC -> "NWC"
    
    type SoftmaxActivationMode = 
        | Channel
        | Instance
        override x.ToString() =
            match x with
                | Channel -> "channel"
                | Instance -> "instance"
    
    type SampleType = 
        | Bilinear
        | Nearest
        override x.ToString() =
            match x with
                | Bilinear -> "bilinear"
                | Nearest -> "nearest"
    
    type MultiInputMode = 
        | Concat
        | Sum
        override x.ToString() =
            match x with
                | Concat -> "concat"
                | Sum -> "sum"
    
    type NpSumDtype = 
        | Float16
        | Float32
        | Float64
        | Int32
        | Int64
        | Int8
        override x.ToString() =
            match x with
                | Float16 -> "float16"
                | Float32 -> "float32"
                | Float64 -> "float64"
                | Int32 -> "int32"
                | Int64 -> "int64"
                | Int8 -> "int8"
    
    type NpProdDtype = 
        | Float16
        | Float32
        | Float64
        | Int32
        | Int64
        | Int8
        override x.ToString() =
            match x with
                | Float16 -> "float16"
                | Float32 -> "float32"
                | Float64 -> "float64"
                | Int32 -> "int32"
                | Int64 -> "int64"
                | Int8 -> "int8"
    
    type NpiMeanDtype = 
        | Float16
        | Float32
        | Float64
        | Int32
        | Int64
        | Int8
        override x.ToString() =
            match x with
                | Float16 -> "float16"
                | Float32 -> "float32"
                | Float64 -> "float64"
                | Int32 -> "int32"
                | Int64 -> "int64"
                | Int8 -> "int8"
    
    type NpiStdDtype = 
        | Float16
        | Float32
        | Float64
        | Int32
        | Int64
        | Int8
        override x.ToString() =
            match x with
                | Float16 -> "float16"
                | Float32 -> "float32"
                | Float64 -> "float64"
                | Int32 -> "int32"
                | Int64 -> "int64"
                | Int8 -> "int8"
    
    type NpiVarDtype = 
        | Float16
        | Float32
        | Float64
        | Int32
        | Int64
        | Int8
        override x.ToString() =
            match x with
                | Float16 -> "float16"
                | Float32 -> "float32"
                | Float64 -> "float64"
                | Int32 -> "int32"
                | Int64 -> "int64"
                | Int8 -> "int8"
    
    type NpCumsumDtype = 
        | Float16
        | Float32
        | Float64
        | Int32
        | Int64
        | Int8
        override x.ToString() =
            match x with
                | Float16 -> "float16"
                | Float32 -> "float32"
                | Float64 -> "float64"
                | Int32 -> "int32"
                | Int64 -> "int64"
                | Int8 -> "int8"
    
    type IntOrFloatDType = 
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
    
    type PadMode = 
        | Constant
        | Edge
        | Reflect
        override x.ToString() =
            match x with
                | Constant -> "constant"
                | Edge -> "edge"
                | Reflect -> "reflect"
    
    type ContribDequantizeOutType = 
        | Float32
        override x.ToString() =
            match x with
                | Float32 -> "float32"
    
    type ContribQuantizeOutType = 
        | Int8
        | Uint8
        override x.ToString() =
            match x with
                | Int8 -> "int8"
                | Uint8 -> "uint8"
    
    type ContribQuantizeV2OutType = 
        | Auto
        | Int8
        | Uint8
        override x.ToString() =
            match x with
                | Auto -> "auto"
                | Int8 -> "int8"
                | Uint8 -> "uint8"
    
    type ContribQuantizedConvLayout = 
        | NCDHW
        | NCHW
        | NCW
        | NDHWC
        | NHWC
        override x.ToString() =
            match x with
                | NCDHW -> "NCDHW"
                | NCHW -> "NCHW"
                | NCW -> "NCW"
                | NDHWC -> "NDHWC"
                | NHWC -> "NHWC"
    
    type ContribQuantizedPoolingLayout = 
        | NCDHW
        | NCHW
        | NCW
        | NDHWC
        | NHWC
        | NWC
        override x.ToString() =
            match x with
                | NCDHW -> "NCDHW"
                | NCHW -> "NCHW"
                | NCW -> "NCW"
                | NDHWC -> "NDHWC"
                | NHWC -> "NHWC"
                | NWC -> "NWC"
    
    type ContribRequantizeOutType = 
        | Auto
        | Int8
        | Uint8
        override x.ToString() =
            match x with
                | Auto -> "auto"
                | Int8 -> "int8"
                | Uint8 -> "uint8"
    
    type SampleMultinomialDtype = 
        | Float16
        | Float32
        | Float64
        | Int32
        | Uint8
        override x.ToString() =
            match x with
                | Float16 -> "float16"
                | Float32 -> "float32"
                | Float64 -> "float64"
                | Int32 -> "int32"
                | Uint8 -> "uint8"
    
    type RandomRandintDtype = 
        | Int32
        | Int64
        override x.ToString() =
            match x with
                | Int32 -> "int32"
                | Int64 -> "int64"
    
    type RNNMode = 
        | Gru
        | Lstm
        | RnnRelu
        | RnnTanh
        override x.ToString() =
            match x with
                | Gru -> "gru"
                | Lstm -> "lstm"
                | RnnRelu -> "rnn_relu"
                | RnnTanh -> "rnn_tanh"
    
    type Normalization = 
        | Batch
        | Null
        | Valid
        override x.ToString() =
            match x with
                | Batch -> "batch"
                | Null -> "null"
                | Valid -> "valid"
    
    type OutDtype = 
        | Float16
        | Float32
        | Float64
        | Int32
        | Int64
        | Int8
        override x.ToString() =
            match x with
                | Float16 -> "float16"
                | Float32 -> "float32"
                | Float64 -> "float64"
                | Int32 -> "int32"
                | Int64 -> "int64"
                | Int8 -> "int8"
    
    type PickMode = 
        | Clip
        | Wrap
        override x.ToString() =
            match x with
                | Clip -> "clip"
                | Wrap -> "wrap"
    
    type Stype = 
        | Csr
        | Default
        | RowSparse
        override x.ToString() =
            match x with
                | Csr -> "csr"
                | Default -> "default"
                | RowSparse -> "row_sparse"
    
    type ForwardStype = 
        | Csr
        | Default
        | RowSparse
        override x.ToString() =
            match x with
                | Csr -> "csr"
                | Default -> "default"
                | RowSparse -> "row_sparse"
    
    type TakeMode = 
        | Clip
        | Raise
        | Wrap
        override x.ToString() =
            match x with
                | Clip -> "clip"
                | Raise -> "raise"
                | Wrap -> "wrap"
    
    type RetTyp = 
        | Both
        | Indices
        | Mask
        | Value
        override x.ToString() =
            match x with
                | Both -> "both"
                | Indices -> "indices"
                | Mask -> "mask"
                | Value -> "value"
    
    type TopkDtype = 
        | Float16
        | Float32
        | Float64
        | Int32
        | Int64
        | Uint8
        override x.ToString() =
            match x with
                | Float16 -> "float16"
                | Float32 -> "float32"
                | Float64 -> "float64"
                | Int32 -> "int32"
                | Int64 -> "int64"
                | Uint8 -> "uint8"
    
    type ArgsortDtype = 
        | Float16
        | Float32
        | Float64
        | Int32
        | Int64
        | Uint8
        override x.ToString() =
            match x with
                | Float16 -> "float16"
                | Float32 -> "float32"
                | Float64 -> "float64"
                | Int32 -> "int32"
                | Int64 -> "int64"
                | Uint8 -> "uint8"
    
    type ContribDeformableConvolutionLayout = 
        | NCDHW
        | NCHW
        | NCW
        override x.ToString() =
            match x with
                | NCDHW -> "NCDHW"
                | NCHW -> "NCHW"
                | NCW -> "NCW"
    
    type ConvolutionV1Layout = 
        | NCDHW
        | NCHW
        | NDHWC
        | NHWC
        override x.ToString() =
            match x with
                | NCDHW -> "NCDHW"
                | NCHW -> "NCHW"
                | NDHWC -> "NDHWC"
                | NHWC -> "NHWC"
    
    type GridGeneratorTransformType = 
        | Affine
        | Warp
        override x.ToString() =
            match x with
                | Affine -> "affine"
                | Warp -> "warp"
    
    type L2NormalizationMode = 
        | Channel
        | Instance
        | Spatial
        override x.ToString() =
            match x with
                | Channel -> "channel"
                | Instance -> "instance"
                | Spatial -> "spatial"
    
    type SpatialTransformerTransformType = 
        | Affine
        override x.ToString() =
            match x with
                | Affine -> "affine"
    
    type SamplerType = 
        | Bilinear
        override x.ToString() =
            match x with
                | Bilinear -> "bilinear"

