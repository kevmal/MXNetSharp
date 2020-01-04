namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop


[<AutoOpen>]
module GeneratedArgumentTypes = 

    [<RequireQualifiedAccess>]
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
    
    [<RequireQualifiedAccess>]
    type Format = 
        | Center
        | Corner
        override x.ToString() =
            match x with
                | Center -> "center"
                | Corner -> "corner"
    
    [<RequireQualifiedAccess>]
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
    
    [<RequireQualifiedAccess>]
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
    
    [<RequireQualifiedAccess>]
    type CudnnTune = 
        | Fastest
        | LimitedWorkspace
        | Off
        override x.ToString() =
            match x with
                | Fastest -> "fastest"
                | LimitedWorkspace -> "limited_workspace"
                | Off -> "off"
    
    [<RequireQualifiedAccess>]
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
    
    [<RequireQualifiedAccess>]
    type BlankLabel = 
        | First
        | Last
        override x.ToString() =
            match x with
                | First -> "first"
                | Last -> "last"
    
    [<RequireQualifiedAccess>]
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
    
    [<RequireQualifiedAccess>]
    type DropoutMode = 
        | Always
        | Training
        override x.ToString() =
            match x with
                | Always -> "always"
                | Training -> "training"
    
    [<RequireQualifiedAccess>]
    type FloatDType = 
        | Float16
        | Float32
        | Float64
        override x.ToString() =
            match x with
                | Float16 -> "float16"
                | Float32 -> "float32"
                | Float64 -> "float64"
    
    [<RequireQualifiedAccess>]
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
    
    [<RequireQualifiedAccess>]
    type PoolingConvention = 
        | Full
        | Same
        | Valid
        override x.ToString() =
            match x with
                | Full -> "full"
                | Same -> "same"
                | Valid -> "valid"
    
    [<RequireQualifiedAccess>]
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
    
    [<RequireQualifiedAccess>]
    type SoftmaxActivationMode = 
        | Channel
        | Instance
        override x.ToString() =
            match x with
                | Channel -> "channel"
                | Instance -> "instance"
    
    [<RequireQualifiedAccess>]
    type SampleType = 
        | Bilinear
        | Nearest
        override x.ToString() =
            match x with
                | Bilinear -> "bilinear"
                | Nearest -> "nearest"
    
    [<RequireQualifiedAccess>]
    type MultiInputMode = 
        | Concat
        | Sum
        override x.ToString() =
            match x with
                | Concat -> "concat"
                | Sum -> "sum"
    
    [<RequireQualifiedAccess>]
    type PadMode = 
        | Constant
        | Edge
        | Reflect
        override x.ToString() =
            match x with
                | Constant -> "constant"
                | Edge -> "edge"
                | Reflect -> "reflect"
    
    [<RequireQualifiedAccess>]
    type ContribDequantizeOutType = 
        | Float32
        override x.ToString() =
            match x with
                | Float32 -> "float32"
    
    [<RequireQualifiedAccess>]
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
    
    [<RequireQualifiedAccess>]
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
    
    [<RequireQualifiedAccess>]
    type ContribQuantizeOutType = 
        | Int8
        | Uint8
        override x.ToString() =
            match x with
                | Int8 -> "int8"
                | Uint8 -> "uint8"
    
    [<RequireQualifiedAccess>]
    type ContribQuantizeV2OutType = 
        | Auto
        | Int8
        | Uint8
        override x.ToString() =
            match x with
                | Auto -> "auto"
                | Int8 -> "int8"
                | Uint8 -> "uint8"
    
    [<RequireQualifiedAccess>]
    type ContribRequantizeOutType = 
        | Auto
        | Int8
        | Uint8
        override x.ToString() =
            match x with
                | Auto -> "auto"
                | Int8 -> "int8"
                | Uint8 -> "uint8"
    
    [<RequireQualifiedAccess>]
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
    
    [<RequireQualifiedAccess>]
    type RandomRandintDtype = 
        | Int32
        | Int64
        override x.ToString() =
            match x with
                | Int32 -> "int32"
                | Int64 -> "int64"
    
    [<RequireQualifiedAccess>]
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
    
    [<RequireQualifiedAccess>]
    type Normalization = 
        | Batch
        | Null
        | Valid
        override x.ToString() =
            match x with
                | Batch -> "batch"
                | Null -> "null"
                | Valid -> "valid"
    
    [<RequireQualifiedAccess>]
    type PickMode = 
        | Clip
        | Wrap
        override x.ToString() =
            match x with
                | Clip -> "clip"
                | Wrap -> "wrap"
    
    [<RequireQualifiedAccess>]
    type Stype = 
        | Csr
        | Default
        | RowSparse
        override x.ToString() =
            match x with
                | Csr -> "csr"
                | Default -> "default"
                | RowSparse -> "row_sparse"
    
    [<RequireQualifiedAccess>]
    type ForwardStype = 
        | Csr
        | Default
        | RowSparse
        override x.ToString() =
            match x with
                | Csr -> "csr"
                | Default -> "default"
                | RowSparse -> "row_sparse"
    
    [<RequireQualifiedAccess>]
    type TakeMode = 
        | Clip
        | Raise
        | Wrap
        override x.ToString() =
            match x with
                | Clip -> "clip"
                | Raise -> "raise"
                | Wrap -> "wrap"
    
    [<RequireQualifiedAccess>]
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
    
    [<RequireQualifiedAccess>]
    type ContribDeformableConvolutionLayout = 
        | NCDHW
        | NCHW
        | NCW
        override x.ToString() =
            match x with
                | NCDHW -> "NCDHW"
                | NCHW -> "NCHW"
                | NCW -> "NCW"
    
    [<RequireQualifiedAccess>]
    type ContribModulatedDeformableConvolutionLayout = 
        | NCDHW
        | NCHW
        | NCW
        override x.ToString() =
            match x with
                | NCDHW -> "NCDHW"
                | NCHW -> "NCHW"
                | NCW -> "NCW"
    
    [<RequireQualifiedAccess>]
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
    
    [<RequireQualifiedAccess>]
    type GridGeneratorTransformType = 
        | Affine
        | Warp
        override x.ToString() =
            match x with
                | Affine -> "affine"
                | Warp -> "warp"
    
    [<RequireQualifiedAccess>]
    type L2NormalizationMode = 
        | Channel
        | Instance
        | Spatial
        override x.ToString() =
            match x with
                | Channel -> "channel"
                | Instance -> "instance"
                | Spatial -> "spatial"
    
    [<RequireQualifiedAccess>]
    type SpatialTransformerTransformType = 
        | Affine
        override x.ToString() =
            match x with
                | Affine -> "affine"
    
    [<RequireQualifiedAccess>]
    type SamplerType = 
        | Bilinear
        override x.ToString() =
            match x with
                | Bilinear -> "bilinear"

