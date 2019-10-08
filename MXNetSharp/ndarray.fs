namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop

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

type SoftmaxDtype = 
    | Float16
    | Float32
    | Float64
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"

type SoftminDtype = 
    | Float16
    | Float32
    | Float64
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"

type LogSoftmaxDtype = 
    | Float16
    | Float32
    | Float64
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"

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

type SampleUniformDtype = 
    | Float16
    | Float32
    | Float64
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"

type SampleNormalDtype = 
    | Float16
    | Float32
    | Float64
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"

type SampleGammaDtype = 
    | Float16
    | Float32
    | Float64
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"

type SampleExponentialDtype = 
    | Float16
    | Float32
    | Float64
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"

type SamplePoissonDtype = 
    | Float16
    | Float32
    | Float64
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"

type SampleNegativeBinomialDtype = 
    | Float16
    | Float32
    | Float64
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"

type SampleGeneralizedNegativeBinomialDtype = 
    | Float16
    | Float32
    | Float64
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"

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

type AmpCastDtype = 
    | Float16
    | Float32
    | Float64
    | Int32
    | Int64
    | Int8
    | Uint8
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"
            | Int32 -> "int32"
            | Int64 -> "int64"
            | Int8 -> "int8"
            | Uint8 -> "uint8"

type PickMode = 
    | Clip
    | Wrap
    override x.ToString() =
        match x with
            | Clip -> "clip"
            | Wrap -> "wrap"

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

type CastDtype = 
    | Float16
    | Float32
    | Float64
    | Int32
    | Int64
    | Int8
    | Uint8
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"
            | Int32 -> "int32"
            | Int64 -> "int64"
            | Int8 -> "int8"
            | Uint8 -> "uint8"

type EmbeddingDtype = 
    | Float16
    | Float32
    | Float64
    | Int32
    | Int64
    | Int8
    | Uint8
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"
            | Int32 -> "int32"
            | Int64 -> "int64"
            | Int8 -> "int8"
            | Uint8 -> "uint8"

type ContribSparseEmbeddingDtype = 
    | Float16
    | Float32
    | Float64
    | Int32
    | Int64
    | Int8
    | Uint8
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"
            | Int32 -> "int32"
            | Int64 -> "int64"
            | Int8 -> "int8"
            | Uint8 -> "uint8"

type TakeMode = 
    | Clip
    | Raise
    | Wrap
    override x.ToString() =
        match x with
            | Clip -> "clip"
            | Raise -> "raise"
            | Wrap -> "wrap"

type OneHotDtype = 
    | Float16
    | Float32
    | Float64
    | Int32
    | Int64
    | Int8
    | Uint8
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"
            | Int32 -> "int32"
            | Int64 -> "int64"
            | Int8 -> "int8"
            | Uint8 -> "uint8"

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
    | Uint8
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"
            | Int32 -> "int32"
            | Uint8 -> "uint8"

type ArgsortDtype = 
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

type NDArray() = 
    member x.NDArrayHandle = failwith "" 

    /// <param name="data">input data list</param>
    static member CachedOp([<ParamArray>] data : NDArray[]) =
        let creator = AtomicSymbolCreator.FromName "_CachedOp"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Decode image with OpenCV. 
    /// Note: return image in RGB by default, instead of OpenCV&#39;s default BGR.</summary>
    /// <param name="buf">Buffer containing binary encoded image</param>
    /// <param name="flag">Convert decoded image to grayscale (0) or color (1).</param>
    /// <param name="toRgb">Whether to convert decoded image to mxnet&#39;s default RGB format (instead of opencv&#39;s default BGR).</param>
    static member Cvimdecode(buf : NDArray, [<Optional; DefaultParameterValue(1)>] flag : int, [<Optional; DefaultParameterValue(true)>] toRgb : bool) =
        let creator = AtomicSymbolCreator.FromName "_cvimdecode"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|buf.NDArrayHandle|]
                                                 [|"flag"; "to_rgb"|]
                                                 [|flag.ToString(); toRgb.ToString()|]
        outputs

    /// <summary>Resize image with OpenCV. 
    /// </summary>
    /// <param name="src">source image</param>
    /// <param name="w">Width of resized image.</param>
    /// <param name="h">Height of resized image.</param>
    /// <param name="interp">Interpolation method (default=cv2.INTER_LINEAR).</param>
    static member Cvimresize(src : NDArray, w : int, h : int, [<Optional; DefaultParameterValue(1)>] interp : int) =
        let creator = AtomicSymbolCreator.FromName "_cvimresize"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|src.NDArrayHandle|]
                                                 [|"w"; "h"; "interp"|]
                                                 [|w.ToString(); h.ToString(); interp.ToString()|]
        outputs

    /// <summary>Pad image border with OpenCV. 
    /// </summary>
    /// <param name="src">source image</param>
    /// <param name="top">Top margin.</param>
    /// <param name="bot">Bottom margin.</param>
    /// <param name="left">Left margin.</param>
    /// <param name="right">Right margin.</param>
    /// <param name="fillingType">Filling type (default=cv2.BORDER_CONSTANT).</param>
    /// <param name="value">(Deprecated! Use ``values`` instead.) Fill with single value.</param>
    /// <param name="values">Fill with value(RGB[A] or gray), up to 4 channels.</param>
    static member CvcopyMakeBorder(src : NDArray, 
                                   top : int, 
                                   bot : int, 
                                   left : int, 
                                   right : int, 
                                   [<Optional; DefaultParameterValue(0)>] fillingType : int, 
                                   value : double, 
                                   values : string (*REVIEW: What's the type here?*)) =
        let creator = AtomicSymbolCreator.FromName "_cvcopyMakeBorder"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|src.NDArrayHandle|]
                                                 [|"top"; "bot"; "left"; "right"; "type"; "value"; "values"|]
                                                 [|top.ToString(); bot.ToString(); left.ToString(); right.ToString(); fillingType.ToString(); value.ToString(); values.ToString()|]
        outputs

    /// <param name="data">input data</param>
    static member Copyto(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_copyto"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Batch normalization.
    /// 
    /// This operator is DEPRECATED. Perform BatchNorm on the input.
    /// 
    /// Normalizes a data batch by mean and variance, and applies a scale ``gamma`` as
    /// well as offset ``beta``.
    /// 
    /// Assume the input has more than one dimension and we normalize along axis 1.
    /// We first compute the mean and variance along this axis:
    /// 
    /// .. math::
    /// 
    ///   data\_mean[i] = mean(data[:,i,:,...]) \\
    ///   data\_var[i] = var(data[:,i,:,...])
    /// 
    /// Then compute the normalized output, which has the same shape as input, as following:
    /// 
    /// .. math::
    /// 
    ///   out[:,i,:,...] = \frac{data[:,i,:,...] - data\_mean[i]}{\sqrt{data\_var[i]+\epsilon}} * gamma[i] + beta[i]
    /// 
    /// Both *mean* and *var* returns a scalar by treating the input as a vector.
    /// 
    /// Assume the input has size *k* on axis 1, then both ``gamma`` and ``beta``
    /// have shape *(k,)*. If ``output_mean_var`` is set to be true, then outputs both ``data_mean`` and
    /// ``data_var`` as well, which are needed for the backward pass.
    /// 
    /// Besides the inputs and the outputs, this operator accepts two auxiliary
    /// states, ``moving_mean`` and ``moving_var``, which are *k*-length
    /// vectors. They are global statistics for the whole dataset, which are updated
    /// by::
    /// 
    ///   moving_mean = moving_mean * momentum + data_mean * (1 - momentum)
    ///   moving_var = moving_var * momentum + data_var * (1 - momentum)
    /// 
    /// If ``use_global_stats`` is set to be true, then ``moving_mean`` and
    /// ``moving_var`` are used instead of ``data_mean`` and ``data_var`` to compute
    /// the output. It is often used during inference.
    /// 
    /// Both ``gamma`` and ``beta`` are learnable parameters. But if ``fix_gamma`` is true,
    /// then set ``gamma`` to 1 and its gradient to 0.
    /// 
    /// There&#39;s no sparse support for this operator, and it will exhibit problematic behavior if used with
    /// sparse tensors.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\batch_norm_v1.cc:L95</summary>
    /// <param name="data">Input data to batch normalization</param>
    /// <param name="gamma">gamma array</param>
    /// <param name="beta">beta array</param>
    /// <param name="eps">Epsilon to prevent div 0</param>
    /// <param name="momentum">Momentum for moving average</param>
    /// <param name="fixGamma">Fix gamma while training</param>
    /// <param name="useGlobalStats">Whether use global moving statistics instead of local batch-norm. This will force change batch-norm into a scale shift operator.</param>
    /// <param name="outputMeanVar">Output All,normal mean and var</param>
    static member BatchNormV1(data : NDArray, 
                              gamma : NDArray, 
                              beta : NDArray, 
                              [<Optional; DefaultParameterValue(0.00100000005)>] eps : float, 
                              [<Optional; DefaultParameterValue(0.899999976)>] momentum : float, 
                              [<Optional; DefaultParameterValue(true)>] fixGamma : bool, 
                              [<Optional; DefaultParameterValue(false)>] useGlobalStats : bool, 
                              [<Optional; DefaultParameterValue(false)>] outputMeanVar : bool) =
        let creator = AtomicSymbolCreator.FromName "BatchNorm_v1"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; gamma.NDArrayHandle; beta.NDArrayHandle|]
                                                 [|"eps"; "momentum"; "fix_gamma"; "use_global_stats"; "output_mean_var"|]
                                                 [|eps.ToString(); momentum.ToString(); fixGamma.ToString(); useGlobalStats.ToString(); outputMeanVar.ToString()|]
        outputs

    /// <summary>Update function for multi-precision AdamW optimizer.
    /// 
    /// AdamW is seen as a modification of Adam by decoupling the weight decay from the
    /// optimization steps taken w.r.t. the loss function.
    /// 
    /// Adam update consists of the following steps, where g represents gradient and m, v
    /// are 1st and 2nd order moment estimates (mean and variance).
    /// 
    /// .. math::
    /// 
    ///  g_t = \nabla J(W_{t-1})\\
    ///  m_t = \beta_1 m_{t-1} + (1 - \beta_1) g_t\\
    ///  v_t = \beta_2 v_{t-1} + (1 - \beta_2) g_t^2\\
    ///  W_t = W_{t-1} - \eta_t (\alpha \frac{ m_t }{ \sqrt{ v_t } + \epsilon } + wd W_{t-1})
    /// 
    /// It updates the weights using::
    /// 
    ///  m = beta1*m + (1-beta1)*grad
    ///  v = beta2*v + (1-beta2)*(grad**2)
    ///  w -= eta * (learning_rate * m / (sqrt(v) + epsilon) + w * wd)
    /// 
    /// Note that gradient is rescaled to grad = rescale_grad * grad. If rescale_grad is NaN, Inf, or 0,
    /// the update is skipped.
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\adamw.cc:L77</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="mean">Moving mean</param>
    /// <param name="var">Moving variance</param>
    /// <param name="weight32">Weight32</param>
    /// <param name="rescaleGrad">Rescale gradient to rescale_grad * grad. If NaN, Inf, or 0, the update is skipped.</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="beta1">The decay rate for the 1st moment estimates.</param>
    /// <param name="beta2">The decay rate for the 2nd moment estimates.</param>
    /// <param name="epsilon">A small constant for numerical stability.</param>
    /// <param name="wd">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
    /// <param name="eta">Learning rate schedule multiplier</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    static member MpAdamwUpdate(weight : NDArray, 
                                grad : NDArray, 
                                mean : NDArray, 
                                var : NDArray, 
                                weight32 : NDArray, 
                                rescaleGrad : NDArray, 
                                lr : float, 
                                [<Optional; DefaultParameterValue(0.899999976)>] beta1 : float, 
                                [<Optional; DefaultParameterValue(0.999000013)>] beta2 : float, 
                                [<Optional; DefaultParameterValue(9.99999994E-09)>] epsilon : float, 
                                [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                                eta : float, 
                                [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float) =
        let creator = AtomicSymbolCreator.FromName "_mp_adamw_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle; mean.NDArrayHandle; var.NDArrayHandle; weight32.NDArrayHandle; rescaleGrad.NDArrayHandle|]
                                                 [|"lr"; "beta1"; "beta2"; "epsilon"; "wd"; "eta"; "clip_gradient"|]
                                                 [|lr.ToString(); beta1.ToString(); beta2.ToString(); epsilon.ToString(); wd.ToString(); eta.ToString(); clipGradient.ToString()|]
        outputs

    /// <summary>Update function for AdamW optimizer. AdamW is seen as a modification of
    /// Adam by decoupling the weight decay from the optimization steps taken w.r.t. the loss function.
    /// 
    /// Adam update consists of the following steps, where g represents gradient and m, v
    /// are 1st and 2nd order moment estimates (mean and variance).
    /// 
    /// .. math::
    /// 
    ///  g_t = \nabla J(W_{t-1})\\
    ///  m_t = \beta_1 m_{t-1} + (1 - \beta_1) g_t\\
    ///  v_t = \beta_2 v_{t-1} + (1 - \beta_2) g_t^2\\
    ///  W_t = W_{t-1} - \eta_t (\alpha \frac{ m_t }{ \sqrt{ v_t } + \epsilon } + wd W_{t-1})
    /// 
    /// It updates the weights using::
    /// 
    ///  m = beta1*m + (1-beta1)*grad
    ///  v = beta2*v + (1-beta2)*(grad**2)
    ///  w -= eta * (learning_rate * m / (sqrt(v) + epsilon) + w * wd)
    /// 
    /// Note that gradient is rescaled to grad = rescale_grad * grad. If rescale_grad is NaN, Inf, or 0,
    /// the update is skipped.
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\adamw.cc:L120</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="mean">Moving mean</param>
    /// <param name="var">Moving variance</param>
    /// <param name="rescaleGrad">Rescale gradient to rescale_grad * grad. If NaN, Inf, or 0, the update is skipped.</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="beta1">The decay rate for the 1st moment estimates.</param>
    /// <param name="beta2">The decay rate for the 2nd moment estimates.</param>
    /// <param name="epsilon">A small constant for numerical stability.</param>
    /// <param name="wd">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
    /// <param name="eta">Learning rate schedule multiplier</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    static member AdamwUpdate(weight : NDArray, 
                              grad : NDArray, 
                              mean : NDArray, 
                              var : NDArray, 
                              rescaleGrad : NDArray, 
                              lr : float, 
                              [<Optional; DefaultParameterValue(0.899999976)>] beta1 : float, 
                              [<Optional; DefaultParameterValue(0.999000013)>] beta2 : float, 
                              [<Optional; DefaultParameterValue(9.99999994E-09)>] epsilon : float, 
                              [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                              eta : float, 
                              [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float) =
        let creator = AtomicSymbolCreator.FromName "_adamw_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle; mean.NDArrayHandle; var.NDArrayHandle; rescaleGrad.NDArrayHandle|]
                                                 [|"lr"; "beta1"; "beta2"; "epsilon"; "wd"; "eta"; "clip_gradient"|]
                                                 [|lr.ToString(); beta1.ToString(); beta2.ToString(); epsilon.ToString(); wd.ToString(); eta.ToString(); clipGradient.ToString()|]
        outputs

    /// <summary>
    /// Applies a 2D adaptive average pooling over a 4D input with the shape of (NCHW).
    /// The pooling kernel and stride sizes are automatically chosen for desired output sizes.
    /// 
    /// - If a single integer is provided for output_size, the output size is \
    ///   (N x C x output_size x output_size) for any input (NCHW).
    /// 
    /// - If a tuple of integers (height, width) are provided for output_size, the output size is \
    ///   (N x C x height x width) for any input (NCHW).
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\adaptive_avg_pooling.cc:L214</summary>
    /// <param name="data">Input data</param>
    /// <param name="outputSize">output size</param>
    static member ContribAdaptiveAvgPooling2D(data : NDArray, [<Optional>] outputSize : int) =
        let creator = AtomicSymbolCreator.FromName "_contrib_AdaptiveAvgPooling2D"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"output_size"|]
                                                 [|(if isNull (outputSize :> obj) then "[]" else outputSize.ToString())|]
        outputs

    /// <summary>
    /// Applies a 2D adaptive average pooling over a 4D input with the shape of (NCHW).
    /// The pooling kernel and stride sizes are automatically chosen for desired output sizes.
    /// 
    /// - If a single integer is provided for output_size, the output size is \
    ///   (N x C x output_size x output_size) for any input (NCHW).
    /// 
    /// - If a tuple of integers (height, width) are provided for output_size, the output size is \
    ///   (N x C x height x width) for any input (NCHW).
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\adaptive_avg_pooling.cc:L214</summary>
    /// <param name="data">Input data</param>
    /// <param name="height">height</param>
    /// <param name="width">width</param>
    static member ContribAdaptiveAvgPooling2D(data : NDArray, [<Optional>] height : int, [<Optional>] width : int) =
        let creator = AtomicSymbolCreator.FromName "_contrib_AdaptiveAvgPooling2D"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"output_size"|]
                                                 [|(if isNull (height :> obj) then "[]" else (height, width).ToString())|]
        outputs

    /// <summary>Check if all the float numbers in the array are finite (used for AMP)
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\all_finite.cc:L101</summary>
    /// <param name="data">Array</param>
    /// <param name="initOutput">Initialize output to 1.</param>
    static member AllFinite(data : NDArray, [<Optional; DefaultParameterValue(true)>] initOutput : bool) =
        let creator = AtomicSymbolCreator.FromName "all_finite"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"init_output"|]
                                                 [|initOutput.ToString()|]
        outputs

    /// <summary>Check if all the float numbers in all the arrays are finite (used for AMP)
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\all_finite.cc:L133</summary>
    /// <param name="data">Arrays</param>
    /// <param name="numArrays">Number of arrays.</param>
    /// <param name="initOutput">Initialize output to 1.</param>
    static member MultiAllFinite([<ParamArray>] data : NDArray[], [<Optional; DefaultParameterValue(1)>] numArrays : int, [<Optional; DefaultParameterValue(true)>] initOutput : bool) =
        let creator = AtomicSymbolCreator.FromName "multi_all_finite"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 [|"num_arrays"; "init_output"|]
                                                 [|numArrays.ToString(); initOutput.ToString()|]
        outputs

    /// <summary>
    /// Perform 2D resizing (upsampling or downsampling) for 4D input using bilinear interpolation.
    /// 
    /// Expected input is a 4 dimensional NDArray (NCHW) and the output
    /// with the shape of (N x C x height x width). 
    /// The key idea of bilinear interpolation is to perform linear interpolation
    /// first in one direction, and then again in the other direction. See the wikipedia of
    /// `Bilinear interpolation  &lt;https://en.wikipedia.org/wiki/Bilinear_interpolation&gt;`_
    /// for more details.
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\bilinear_resize.cc:L193</summary>
    /// <param name="data">Input data</param>
    /// <param name="like">Resize data to it&#39;s shape</param>
    /// <param name="height">output height (required, but ignored if scale_height is defined or mode is not &quot;size&quot;)</param>
    /// <param name="width">output width (required, but ignored if scale_width is defined or mode is not &quot;size&quot;)</param>
    /// <param name="scaleHeight">sampling scale of the height (optional, used in modes &quot;scale&quot; and &quot;odd_scale&quot;)</param>
    /// <param name="scaleWidth">sampling scale of the width (optional, used in modes &quot;scale&quot; and &quot;odd_scale&quot;)</param>
    /// <param name="mode">resizing mode. &quot;simple&quot; - output height equals parameter &quot;height&quot; if &quot;scale_height&quot; parameter is not defined or input height multiplied by &quot;scale_height&quot; otherwise. Same for width;&quot;odd_scale&quot; - if original height or width is odd, then result height is calculated like result_h = (original_h - 1) * scale + 1; for scale &gt; 1 the result shape would be like if we did deconvolution with kernel = (1, 1) and stride = (height_scale, width_scale); and for scale &lt; 1 shape would be like we did convolution with kernel = (1, 1) and stride = (int(1 / height_scale), int( 1/ width_scale);&quot;like&quot; - resize first input to the height and width of second input; &quot;to_even_down&quot; - resize input to nearest lower even height and width (if original height is odd then result height = original height - 1);&quot;to_even_up&quot; - resize input to nearest bigger even height and width (if original height is odd then result height = original height + 1);&quot;to_odd_down&quot; - resize input to nearest odd height and width (if original height is odd then result height = original height - 1);&quot;to_odd_up&quot; - resize input to nearest odd height and width (if original height is odd then result height = original height + 1);</param>
    static member ContribBilinearResize2D(data : NDArray, 
                                          like : NDArray, 
                                          [<Optional; DefaultParameterValue(1)>] height : int, 
                                          [<Optional; DefaultParameterValue(1)>] width : int, 
                                          [<Optional>] scaleHeight : float Nullable, 
                                          [<Optional>] scaleWidth : float Nullable, 
                                          [<Optional>] mode : ContribBilinearResize2DMode) =
        let creator = AtomicSymbolCreator.FromName "_contrib_BilinearResize2D"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; like.NDArrayHandle|]
                                                 [|"height"; "width"; "scale_height"; "scale_width"; "mode"|]
                                                 [|height.ToString(); width.ToString(); scaleHeight.ToString(); scaleWidth.ToString(); (if isNull (mode :> obj) then "size" else mode.ToString())|]
        outputs

    /// <summary>
    /// Perform 2D resizing (upsampling or downsampling) for 4D input using bilinear interpolation.
    /// 
    /// Expected input is a 4 dimensional NDArray (NCHW) and the output
    /// with the shape of (N x C x height x width). 
    /// The key idea of bilinear interpolation is to perform linear interpolation
    /// first in one direction, and then again in the other direction. See the wikipedia of
    /// `Bilinear interpolation  &lt;https://en.wikipedia.org/wiki/Bilinear_interpolation&gt;`_
    /// for more details.
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\bilinear_resize.cc:L193</summary>
    /// <param name="data">Input data</param>
    /// <param name="like">Resize data to it&#39;s shape</param>
    /// <param name="height">output height (required, but ignored if scale_height is defined or mode is not &quot;size&quot;)</param>
    /// <param name="width">output width (required, but ignored if scale_width is defined or mode is not &quot;size&quot;)</param>
    /// <param name="scaleHeight">sampling scale of the height (optional, used in modes &quot;scale&quot; and &quot;odd_scale&quot;)</param>
    /// <param name="scaleWidth">sampling scale of the width (optional, used in modes &quot;scale&quot; and &quot;odd_scale&quot;)</param>
    /// <param name="mode">resizing mode. &quot;simple&quot; - output height equals parameter &quot;height&quot; if &quot;scale_height&quot; parameter is not defined or input height multiplied by &quot;scale_height&quot; otherwise. Same for width;&quot;odd_scale&quot; - if original height or width is odd, then result height is calculated like result_h = (original_h - 1) * scale + 1; for scale &gt; 1 the result shape would be like if we did deconvolution with kernel = (1, 1) and stride = (height_scale, width_scale); and for scale &lt; 1 shape would be like we did convolution with kernel = (1, 1) and stride = (int(1 / height_scale), int( 1/ width_scale);&quot;like&quot; - resize first input to the height and width of second input; &quot;to_even_down&quot; - resize input to nearest lower even height and width (if original height is odd then result height = original height - 1);&quot;to_even_up&quot; - resize input to nearest bigger even height and width (if original height is odd then result height = original height + 1);&quot;to_odd_down&quot; - resize input to nearest odd height and width (if original height is odd then result height = original height - 1);&quot;to_odd_up&quot; - resize input to nearest odd height and width (if original height is odd then result height = original height + 1);</param>
    static member ContribBilinearResize2D(data : NDArray, 
                                          like : NDArray, 
                                          ?height : int, 
                                          ?width : int, 
                                          ?scaleHeight : float, 
                                          ?scaleWidth : float, 
                                          ?mode : ContribBilinearResize2DMode) =
        let creator = AtomicSymbolCreator.FromName "_contrib_BilinearResize2D"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; like.NDArrayHandle|]
                                                 [|"height"; "width"; "scale_height"; "scale_width"; "mode"|]
                                                 [|(match height with None -> "1" | _ -> height.ToString()); (match width with None -> "1" | _ -> width.ToString()); (match scaleHeight with None -> "None" | _ -> scaleHeight.ToString()); (match scaleWidth with None -> "None" | _ -> scaleWidth.ToString()); (match mode with None -> "size" | _ -> mode.ToString())|]
        outputs

    /// <summary>
    /// Given an n-d NDArray data, and a 1-d NDArray index,
    /// the operator produces an un-predeterminable shaped n-d NDArray out,
    /// which stands for the rows in x where the corresonding element in index is non-zero.
    /// 
    /// &gt;&gt;&gt; data = mx.nd.array([[1, 2, 3],[4, 5, 6],[7, 8, 9]])
    /// &gt;&gt;&gt; index = mx.nd.array([0, 1, 0])
    /// &gt;&gt;&gt; out = mx.nd.contrib.boolean_mask(data, index)
    /// &gt;&gt;&gt; out
    /// 
    /// [[4. 5. 6.]]
    /// &lt;NDArray 1x3 @cpu(0)&gt;
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\boolean_mask.cc:L211</summary>
    /// <param name="data">Data</param>
    /// <param name="index">Mask</param>
    /// <param name="axis">An integer that represents the axis in NDArray to mask from.</param>
    static member ContribBooleanMask(data : NDArray, index : NDArray, [<Optional; DefaultParameterValue(0)>] axis : int) =
        let creator = AtomicSymbolCreator.FromName "_contrib_boolean_mask"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; index.NDArrayHandle|]
                                                 [|"axis"|]
                                                 [|axis.ToString()|]
        outputs

    /// <summary>Apply non-maximum suppression to input.
    /// 
    /// The output will be sorted in descending order according to `score`. Boxes with
    /// overlaps larger than `overlap_thresh`, smaller scores and background boxes
    /// will be removed and filled with -1, the corresponding position will be recorded
    /// for backward propogation.
    /// 
    /// During back-propagation, the gradient will be copied to the original
    /// position according to the input index. For positions that have been suppressed,
    /// the in_grad will be assigned 0.
    /// In summary, gradients are sticked to its boxes, will either be moved or discarded
    /// according to its original index in input.
    /// 
    /// Input requirements::
    /// 
    ///   1. Input tensor have at least 2 dimensions, (n, k), any higher dims will be regarded
    ///   as batch, e.g. (a, b, c, d, n, k) == (a*b*c*d, n, k)
    ///   2. n is the number of boxes in each batch
    ///   3. k is the width of each box item.
    /// 
    /// By default, a box is [id, score, xmin, ymin, xmax, ymax, ...],
    /// additional elements are allowed.
    /// 
    /// - `id_index`: optional, use -1 to ignore, useful if `force_suppress=False`, which means
    ///   we will skip highly overlapped boxes if one is `apple` while the other is `car`.
    /// 
    /// - `background_id`: optional, default=-1, class id for background boxes, useful
    ///   when `id_index &gt;= 0` which means boxes with background id will be filtered before nms.
    /// 
    /// - `coord_start`: required, default=2, the starting index of the 4 coordinates.
    ///   Two formats are supported:
    /// 
    ///     - `corner`: [xmin, ymin, xmax, ymax]
    /// 
    ///     - `center`: [x, y, width, height]
    /// 
    /// - `score_index`: required, default=1, box score/confidence.
    ///   When two boxes overlap IOU &gt; `overlap_thresh`, the one with smaller score will be suppressed.
    /// 
    /// - `in_format` and `out_format`: default=&#39;corner&#39;, specify in/out box formats.
    /// 
    /// Examples::
    /// 
    ///   x = [[0, 0.5, 0.1, 0.1, 0.2, 0.2], [1, 0.4, 0.1, 0.1, 0.2, 0.2],
    ///        [0, 0.3, 0.1, 0.1, 0.14, 0.14], [2, 0.6, 0.5, 0.5, 0.7, 0.8]]
    ///   box_nms(x, overlap_thresh=0.1, coord_start=2, score_index=1, id_index=0,
    ///       force_suppress=True, in_format=&#39;corner&#39;, out_typ=&#39;corner&#39;) =
    ///       [[2, 0.6, 0.5, 0.5, 0.7, 0.8], [0, 0.5, 0.1, 0.1, 0.2, 0.2],
    ///        [-1, -1, -1, -1, -1, -1], [-1, -1, -1, -1, -1, -1]]
    ///   out_grad = [[0.1, 0.1, 0.1, 0.1, 0.1, 0.1], [0.2, 0.2, 0.2, 0.2, 0.2, 0.2],
    ///               [0.3, 0.3, 0.3, 0.3, 0.3, 0.3], [0.4, 0.4, 0.4, 0.4, 0.4, 0.4]]
    ///   # exe.backward
    ///   in_grad = [[0.2, 0.2, 0.2, 0.2, 0.2, 0.2], [0, 0, 0, 0, 0, 0],
    ///              [0, 0, 0, 0, 0, 0], [0.1, 0.1, 0.1, 0.1, 0.1, 0.1]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\bounding_box.cc:L93</summary>
    /// <param name="data">The input</param>
    /// <param name="overlapThresh">Overlapping(IoU) threshold to suppress object with smaller score.</param>
    /// <param name="validThresh">Filter input boxes to those whose scores greater than valid_thresh.</param>
    /// <param name="topk">Apply nms to topk boxes with descending scores, -1 to no restriction.</param>
    /// <param name="coordStart">Start index of the consecutive 4 coordinates.</param>
    /// <param name="scoreIndex">Index of the scores/confidence of boxes.</param>
    /// <param name="idIndex">Optional, index of the class categories, -1 to disable.</param>
    /// <param name="backgroundId">Optional, id of the background class which will be ignored in nms.</param>
    /// <param name="forceSuppress">Optional, if set false and id_index is provided, nms will only apply to boxes belongs to the same category</param>
    /// <param name="inFormat">The input box encoding type. 
    ///  &quot;corner&quot; means boxes are encoded as [xmin, ymin, xmax, ymax], &quot;center&quot; means boxes are encodes as [x, y, width, height].</param>
    /// <param name="outFormat">The output box encoding type. 
    ///  &quot;corner&quot; means boxes are encoded as [xmin, ymin, xmax, ymax], &quot;center&quot; means boxes are encodes as [x, y, width, height].</param>
    static member ContribBoxNms(data : NDArray, 
                                [<Optional; DefaultParameterValue(0.5)>] overlapThresh : float, 
                                [<Optional; DefaultParameterValue(0.0)>] validThresh : float, 
                                [<Optional; DefaultParameterValue(-1)>] topk : int, 
                                [<Optional; DefaultParameterValue(2)>] coordStart : int, 
                                [<Optional; DefaultParameterValue(1)>] scoreIndex : int, 
                                [<Optional; DefaultParameterValue(-1)>] idIndex : int, 
                                [<Optional; DefaultParameterValue(-1)>] backgroundId : int, 
                                [<Optional; DefaultParameterValue(false)>] forceSuppress : bool, 
                                [<Optional>] inFormat : Format, 
                                [<Optional>] outFormat : Format) =
        let creator = AtomicSymbolCreator.FromName "_contrib_box_nms"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"overlap_thresh"; "valid_thresh"; "topk"; "coord_start"; "score_index"; "id_index"; "background_id"; "force_suppress"; "in_format"; "out_format"|]
                                                 [|overlapThresh.ToString(); validThresh.ToString(); topk.ToString(); coordStart.ToString(); scoreIndex.ToString(); idIndex.ToString(); backgroundId.ToString(); forceSuppress.ToString(); (if isNull (inFormat :> obj) then "corner" else inFormat.ToString()); (if isNull (outFormat :> obj) then "corner" else outFormat.ToString())|]
        outputs

    /// <summary>Bounding box overlap of two arrays.
    ///   The overlap is defined as Intersection-over-Union, aka, IOU.
    ///   - lhs: (a_1, a_2, ..., a_n, 4) array
    ///   - rhs: (b_1, b_2, ..., b_n, 4) array
    ///   - output: (a_1, a_2, ..., a_n, b_1, b_2, ..., b_n) array
    /// 
    ///   Note::
    /// 
    ///     Zero gradients are back-propagated in this op for now.
    /// 
    ///   Example::
    /// 
    ///     x = [[0.5, 0.5, 1.0, 1.0], [0.0, 0.0, 0.5, 0.5]]
    ///     y = [[0.25, 0.25, 0.75, 0.75]]
    ///     box_iou(x, y, format=&#39;corner&#39;) = [[0.1428], [0.1428]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\bounding_box.cc:L134</summary>
    /// <param name="lhs">The first input</param>
    /// <param name="rhs">The second input</param>
    /// <param name="format">The box encoding type. 
    ///  &quot;corner&quot; means boxes are encoded as [xmin, ymin, xmax, ymax], &quot;center&quot; means boxes are encodes as [x, y, width, height].</param>
    static member ContribBoxIou(lhs : NDArray, rhs : NDArray, [<Optional>] format : Format) =
        let creator = AtomicSymbolCreator.FromName "_contrib_box_iou"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 [|"format"|]
                                                 [|(if isNull (format :> obj) then "corner" else format.ToString())|]
        outputs

    /// <summary>Compute bipartite matching.
    ///   The matching is performed on score matrix with shape [B, N, M]
    ///   - B: batch_size
    ///   - N: number of rows to match
    ///   - M: number of columns as reference to be matched against.
    /// 
    ///   Returns:
    ///   x : matched column indices. -1 indicating non-matched elements in rows.
    ///   y : matched row indices.
    /// 
    ///   Note::
    /// 
    ///     Zero gradients are back-propagated in this op for now.
    /// 
    ///   Example::
    /// 
    ///     s = [[0.5, 0.6], [0.1, 0.2], [0.3, 0.4]]
    ///     x, y = bipartite_matching(x, threshold=1e-12, is_ascend=False)
    ///     x = [1, -1, 0]
    ///     y = [2, 0]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\bounding_box.cc:L180</summary>
    /// <param name="data">The input</param>
    /// <param name="isAscend">Use ascend order for scores instead of descending. Please set threshold accordingly.</param>
    /// <param name="threshold">Ignore matching when score &lt; thresh, if is_ascend=false, or ignore score &gt; thresh, if is_ascend=true.</param>
    /// <param name="topk">Limit the number of matches to topk, set -1 for no limit</param>
    static member ContribBipartiteMatching(data : NDArray, [<Optional; DefaultParameterValue(false)>] isAscend : bool, threshold : float, [<Optional; DefaultParameterValue(-1)>] topk : int) =
        let creator = AtomicSymbolCreator.FromName "_contrib_bipartite_matching"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"is_ascend"; "threshold"; "topk"|]
                                                 [|isAscend.ToString(); threshold.ToString(); topk.ToString()|]
        outputs

    /// <summary>This operator samples sub-graphs from a csr graph via an
    /// uniform probability. The operator is designed for DGL.
    /// 
    /// The operator outputs three sets of NDArrays to represent the sampled results
    /// (the number of NDArrays in each set is the same as the number of seed NDArrays):
    /// 1) a set of 1D NDArrays containing the sampled vertices, 2) a set of CSRNDArrays representing
    /// the sampled edges, 3) a set of 1D NDArrays indicating the layer where a vertex is sampled.
    /// The first set of 1D NDArrays have a length of max_num_vertices+1. The last element in an NDArray
    /// indicate the acutal number of vertices in a subgraph. The third set of NDArrays have a length
    /// of max_num_vertices, and the valid number of vertices is the same as the ones in the first set.
    /// 
    /// Example:
    /// 
    ///    .. code:: python
    /// 
    ///   shape = (5, 5)
    ///   data_np = np.array([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20], dtype=np.int64)
    ///   indices_np = np.array([1,2,3,4,0,2,3,4,0,1,3,4,0,1,2,4,0,1,2,3], dtype=np.int64)
    ///   indptr_np = np.array([0,4,8,12,16,20], dtype=np.int64)
    ///   a = mx.nd.sparse.csr_matrix((data_np, indices_np, indptr_np), shape=shape)
    ///   a.asnumpy()
    ///   seed = mx.nd.array([0,1,2,3,4], dtype=np.int64)
    ///   out = mx.nd.contrib.dgl_csr_neighbor_uniform_sample(a, seed, num_args=2, num_hops=1, num_neighbor=2, max_num_vertices=5)
    /// 
    ///   out[0]
    ///   [0 1 2 3 4 5]
    ///   &lt;NDArray 6 @cpu(0)&gt;
    /// 
    ///   out[1].asnumpy()
    ///   array([[ 0,  1,  0,  3,  0],
    ///          [ 5,  0,  0,  7,  0],
    ///          [ 9,  0,  0, 11,  0],
    ///          [13,  0, 15,  0,  0],
    ///          [17,  0, 19,  0,  0]])
    /// 
    ///   out[2]
    ///   [0 0 0 0 0]
    ///   &lt;NDArray 5 @cpu(0)&gt;
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\dgl_graph.cc:L784</summary>
    /// <param name="csrMatrix">csr matrix</param>
    /// <param name="seedArrays">seed vertices</param>
    /// <param name="numArgs">Number of input NDArray.</param>
    /// <param name="numHops">Number of hops.</param>
    /// <param name="numNeighbor">Number of neighbor.</param>
    /// <param name="maxNumVertices">Max number of vertices.</param>
    static member ContribDglCsrNeighborUniformSample(csrMatrix : NDArray, 
                                                     [<ParamArray>] seedArrays : NDArray[], 
                                                     numArgs : int, 
                                                     numHops : int, 
                                                     numNeighbor : int, 
                                                     maxNumVertices : int) =
        let creator = AtomicSymbolCreator.FromName "_contrib_dgl_csr_neighbor_uniform_sample"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|csrMatrix.NDArrayHandle|]
                                                 [|"num_args"; "num_hops"; "num_neighbor"; "max_num_vertices"|]
                                                 [|numArgs.ToString(); numHops.ToString(); numNeighbor.ToString(); maxNumVertices.ToString()|]
        outputs

    /// <summary>This operator samples sub-graph from a csr graph via an
    /// non-uniform probability. The operator is designed for DGL.
    /// 
    /// The operator outputs four sets of NDArrays to represent the sampled results
    /// (the number of NDArrays in each set is the same as the number of seed NDArrays):
    /// 1) a set of 1D NDArrays containing the sampled vertices, 2) a set of CSRNDArrays representing
    /// the sampled edges, 3) a set of 1D NDArrays with the probability that vertices are sampled,
    /// 4) a set of 1D NDArrays indicating the layer where a vertex is sampled.
    /// The first set of 1D NDArrays have a length of max_num_vertices+1. The last element in an NDArray
    /// indicate the acutal number of vertices in a subgraph. The third and fourth set of NDArrays have a length
    /// of max_num_vertices, and the valid number of vertices is the same as the ones in the first set.
    /// 
    /// Example:
    /// 
    ///    .. code:: python
    /// 
    ///   shape = (5, 5)
    ///   prob = mx.nd.array([0.9, 0.8, 0.2, 0.4, 0.1], dtype=np.float32)
    ///   data_np = np.array([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20], dtype=np.int64)
    ///   indices_np = np.array([1,2,3,4,0,2,3,4,0,1,3,4,0,1,2,4,0,1,2,3], dtype=np.int64)
    ///   indptr_np = np.array([0,4,8,12,16,20], dtype=np.int64)
    ///   a = mx.nd.sparse.csr_matrix((data_np, indices_np, indptr_np), shape=shape)
    ///   seed = mx.nd.array([0,1,2,3,4], dtype=np.int64)
    ///   out = mx.nd.contrib.dgl_csr_neighbor_non_uniform_sample(a, prob, seed, num_args=3, num_hops=1, num_neighbor=2, max_num_vertices=5)
    /// 
    ///   out[0]
    ///   [0 1 2 3 4 5]
    ///   &lt;NDArray 6 @cpu(0)&gt;
    /// 
    ///   out[1].asnumpy()
    ///   array([[ 0,  1,  2,  0,  0],
    ///          [ 5,  0,  6,  0,  0],
    ///          [ 9, 10,  0,  0,  0],
    ///          [13, 14,  0,  0,  0],
    ///          [ 0, 18, 19,  0,  0]])
    /// 
    ///   out[2]
    ///   [0.9 0.8 0.2 0.4 0.1]
    ///   &lt;NDArray 5 @cpu(0)&gt;
    /// 
    ///   out[3]
    ///   [0 0 0 0 0]
    ///   &lt;NDArray 5 @cpu(0)&gt;
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\dgl_graph.cc:L883</summary>
    /// <param name="csrMatrix">csr matrix</param>
    /// <param name="probability">probability vector</param>
    /// <param name="seedArrays">seed vertices</param>
    /// <param name="numArgs">Number of input NDArray.</param>
    /// <param name="numHops">Number of hops.</param>
    /// <param name="numNeighbor">Number of neighbor.</param>
    /// <param name="maxNumVertices">Max number of vertices.</param>
    static member ContribDglCsrNeighborNonUniformSample(csrMatrix : NDArray, 
                                                        probability : NDArray, 
                                                        [<ParamArray>] seedArrays : NDArray[], 
                                                        numArgs : int, 
                                                        numHops : int, 
                                                        numNeighbor : int, 
                                                        maxNumVertices : int) =
        let creator = AtomicSymbolCreator.FromName "_contrib_dgl_csr_neighbor_non_uniform_sample"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|csrMatrix.NDArrayHandle; probability.NDArrayHandle|]
                                                 [|"num_args"; "num_hops"; "num_neighbor"; "max_num_vertices"|]
                                                 [|numArgs.ToString(); numHops.ToString(); numNeighbor.ToString(); maxNumVertices.ToString()|]
        outputs

    /// <summary>This operator constructs an induced subgraph for
    /// a given set of vertices from a graph. The operator accepts multiple
    /// sets of vertices as input. For each set of vertices, it returns a pair
    /// of CSR matrices if return_mapping is True: the first matrix contains edges
    /// with new edge Ids, the second matrix contains edges with the original
    /// edge Ids.
    /// 
    /// Example:
    /// 
    ///    .. code:: python
    /// 
    ///      x=[[1, 0, 0, 2],
    ///        [3, 0, 4, 0],
    ///        [0, 5, 0, 0],
    ///        [0, 6, 7, 0]]
    ///      v = [0, 1, 2]
    ///      dgl_subgraph(x, v, return_mapping=True) =
    ///        [[1, 0, 0],
    ///         [2, 0, 3],
    ///         [0, 4, 0]],
    ///        [[1, 0, 0],
    ///         [3, 0, 4],
    ///         [0, 5, 0]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\dgl_graph.cc:L1140</summary>
    /// <param name="graph">Input graph where we sample vertices.</param>
    /// <param name="data">The input arrays that include data arrays and states.</param>
    /// <param name="numArgs">Number of input arguments, including all symbol inputs.</param>
    /// <param name="returnMapping">Return mapping of vid and eid between the subgraph and the parent graph.</param>
    static member ContribDglSubgraph(graph : NDArray, [<ParamArray>] data : NDArray[], numArgs : int, returnMapping : bool) =
        let creator = AtomicSymbolCreator.FromName "_contrib_dgl_subgraph"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|graph.NDArrayHandle|]
                                                 [|"num_args"; "return_mapping"|]
                                                 [|numArgs.ToString(); returnMapping.ToString()|]
        outputs

    /// <summary>This operator implements the edge_id function for a graph
    /// stored in a CSR matrix (the value of the CSR stores the edge Id of the graph).
    /// output[i] = input[u[i], v[i]] if there is an edge between u[i] and v[i]],
    /// otherwise output[i] will be -1. Both u and v should be 1D vectors.
    /// 
    /// Example:
    /// 
    ///    .. code:: python
    /// 
    ///       x = [[ 1, 0, 0 ],
    ///            [ 0, 2, 0 ],
    ///            [ 0, 0, 3 ]]
    ///       u = [ 0, 0, 1, 1, 2, 2 ]
    ///       v = [ 0, 1, 1, 2, 0, 2 ]
    ///       edge_id(x, u, v) = [ 1, -1, 2, -1, -1, 3 ]
    /// 
    /// The storage type of ``edge_id`` output depends on storage types of inputs
    ///   - edge_id(csr, default, default) = default
    ///   - default and rsp inputs are not supported
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\dgl_graph.cc:L1321</summary>
    /// <param name="data">Input ndarray</param>
    /// <param name="u">u ndarray</param>
    /// <param name="v">v ndarray</param>
    static member ContribEdgeId(data : NDArray, u : NDArray, v : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_contrib_edge_id"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; u.NDArrayHandle; v.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>This operator converts a CSR matrix whose values are edge Ids
    /// to an adjacency matrix whose values are ones. The output CSR matrix always has
    /// the data value of float32.
    /// 
    /// Example:
    /// 
    ///    .. code:: python
    /// 
    ///   x = [[ 1, 0, 0 ],
    ///        [ 0, 2, 0 ],
    ///        [ 0, 0, 3 ]]
    ///   dgl_adjacency(x) =
    ///       [[ 1, 0, 0 ],
    ///        [ 0, 1, 0 ],
    ///        [ 0, 0, 1 ]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\dgl_graph.cc:L1393</summary>
    /// <param name="data">Input ndarray</param>
    static member ContribDglAdjacency(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_contrib_dgl_adjacency"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>This operator compacts a CSR matrix generated by
    /// dgl_csr_neighbor_uniform_sample and dgl_csr_neighbor_non_uniform_sample.
    /// The CSR matrices generated by these two operators may have many empty
    /// rows at the end and many empty columns. This operator removes these
    /// empty rows and empty columns.
    /// 
    /// Example:
    /// 
    ///    .. code:: python
    /// 
    ///   shape = (5, 5)
    ///   data_np = np.array([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20], dtype=np.int64)
    ///   indices_np = np.array([1,2,3,4,0,2,3,4,0,1,3,4,0,1,2,4,0,1,2,3], dtype=np.int64)
    ///   indptr_np = np.array([0,4,8,12,16,20], dtype=np.int64)
    ///   a = mx.nd.sparse.csr_matrix((data_np, indices_np, indptr_np), shape=shape)
    ///   seed = mx.nd.array([0,1,2,3,4], dtype=np.int64)
    ///   out = mx.nd.contrib.dgl_csr_neighbor_uniform_sample(a, seed, num_args=2, num_hops=1,
    ///           num_neighbor=2, max_num_vertices=6)
    ///   subg_v = out[0]
    ///   subg = out[1]
    ///   compact = mx.nd.contrib.dgl_graph_compact(subg, subg_v,
    ///           graph_sizes=(subg_v[-1].asnumpy()[0]), return_mapping=False)
    /// 
    ///   compact.asnumpy()
    ///   array([[0, 0, 0, 1, 0],
    ///          [2, 0, 3, 0, 0],
    ///          [0, 4, 0, 0, 5],
    ///          [0, 6, 0, 0, 7],
    ///          [8, 9, 0, 0, 0]])
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\dgl_graph.cc:L1582</summary>
    /// <param name="graphData">Input graphs and input vertex Ids.</param>
    /// <param name="numArgs">Number of input arguments.</param>
    /// <param name="returnMapping">Return mapping of vid and eid between the subgraph and the parent graph.</param>
    /// <param name="graphSizes">the number of vertices in each graph.</param>
    static member ContribDglGraphCompact([<ParamArray>] graphData : NDArray[], numArgs : int, returnMapping : bool, graphSizes : int) =
        let creator = AtomicSymbolCreator.FromName "_contrib_dgl_graph_compact"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 [|"num_args"; "return_mapping"; "graph_sizes"|]
                                                 [|numArgs.ToString(); returnMapping.ToString(); graphSizes.ToString()|]
        outputs

    /// <summary>This operator implements the gradient multiplier function.
    /// In forward pass it acts as an identity transform. During backpropagation it
    /// multiplies the gradient from the subsequent level by a scalar factor lambda and passes it to
    /// the preceding layer.
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\gradient_multiplier_op.cc:L78</summary>
    /// <param name="data">The input array.</param>
    /// <param name="scalar">lambda multiplier</param>
    static member ContribGradientmultiplier(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_contrib_gradientmultiplier"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member ContribBackwardGradientmultiplier(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_contrib_backward_gradientmultiplier"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <summary>Computes the log likelihood of a univariate Hawkes process.
    /// 
    /// The log likelihood is calculated on point process observations represented
    /// as *ragged* matrices for *lags* (interarrival times w.r.t. the previous point),
    /// and *marks* (identifiers for the process ID). Note that each mark is considered independent,
    /// i.e., computes the joint likelihood of a set of Hawkes processes determined by the conditional intensity:
    /// 
    /// .. math::
    /// 
    ///   \lambda_k^*(t) = \lambda_k + \alpha_k \sum_{\{t_i &lt; t, y_i = k\}} \beta_k \exp(-\beta_k (t - t_i))
    /// 
    /// where :math:`\lambda_k` specifies the background intensity ``lda``, :math:`\alpha_k` specifies the *branching ratio* or ``alpha``, and :math:`\beta_k` the delay density parameter ``beta``.
    /// 
    /// ``lags`` and ``marks`` are two NDArrays of shape (N, T) and correspond to the representation of the point process observation, the first dimension corresponds to the batch index, and the second to the sequence. These are &quot;left-aligned&quot; *ragged* matrices (the first index of the second dimension is the beginning of every sequence. The length of each sequence is given by ``valid_length``, of shape (N,) where ``valid_length[i]`` corresponds to the number of valid points in ``lags[i, :]`` and ``marks[i, :]``.
    /// 
    /// ``max_time`` is the length of the observation period of the point process. That is, specifying ``max_time[i] = 5`` computes the likelihood of the i-th sample as observed on the time interval :math:`(0, 5]`. Naturally, the sum of all valid ``lags[i, :valid_length[i]]`` must be less than or equal to 5.
    /// 
    /// The input ``state`` specifies the *memory* of the Hawkes process. Invoking the memoryless property of exponential decays, we compute the *memory* as
    /// 
    /// .. math::
    /// 
    ///     s_k(t) = \sum_{t_i &lt; t} \exp(-\beta_k (t - t_i)).
    /// 
    /// The ``state`` to be provided is :math:`s_k(0)` and carries the added intensity due to past events before the current batch. :math:`s_k(T)` is returned from the function where :math:`T` is ``max_time[T]``.
    /// 
    /// Example::
    /// 
    ///   # define the Hawkes process parameters
    ///   lda = nd.array([1.5, 2.0, 3.0]).tile((N, 1))
    ///   alpha = nd.array([0.2, 0.3, 0.4])  # branching ratios should be &lt; 1
    ///   beta = nd.array([1.0, 2.0, 3.0])
    /// 
    ///   # the &quot;data&quot;, or observations
    ///   ia_times = nd.array([[6, 7, 8, 9], [1, 2, 3, 4], [3, 4, 5, 6], [8, 9, 10, 11]])
    ///   marks = nd.zeros((N, T)).astype(np.int32)
    /// 
    ///   # starting &quot;state&quot; of the process
    ///   states = nd.zeros((N, K))
    /// 
    ///   valid_length = nd.array([1, 2, 3, 4])  # number of valid points in each sequence
    ///   max_time = nd.ones((N,)) * 100.0  # length of the observation period
    /// 
    ///   A = nd.contrib.hawkesll(
    ///       lda, alpha, beta, states, ia_times, marks, valid_length, max_time
    ///   )
    /// 
    /// References:
    /// 
    /// -  Bacry, E., Mastromatteo, I., &amp; Muzy, J. F. (2015).
    ///    Hawkes processes in finance. Market Microstructure and Liquidity
    ///    , 1(01), 1550005.
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\hawkes_ll.cc:L84</summary>
    /// <param name="lda">Shape (N, K) The intensity for each of the K processes, for each sample</param>
    /// <param name="alpha">Shape (K,) The infectivity factor (branching ratio) for each process</param>
    /// <param name="beta">Shape (K,) The decay parameter for each process</param>
    /// <param name="state">Shape (N, K) the Hawkes state for each process</param>
    /// <param name="lags">Shape (N, T) the interarrival times</param>
    /// <param name="marks">Shape (N, T) the marks (process ids)</param>
    /// <param name="validLength">The number of valid points in the process</param>
    /// <param name="maxTime">the length of the interval where the processes were sampled</param>
    static member ContribHawkesll(lda : NDArray, 
                                  alpha : NDArray, 
                                  beta : NDArray, 
                                  state : NDArray, 
                                  lags : NDArray, 
                                  marks : NDArray, 
                                  validLength : NDArray, 
                                  maxTime : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_contrib_hawkesll"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lda.NDArrayHandle; alpha.NDArrayHandle; beta.NDArrayHandle; state.NDArrayHandle; lags.NDArrayHandle; marks.NDArrayHandle; validLength.NDArrayHandle; maxTime.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns an array of indexes of the input array.
    /// 
    /// For an input array with shape  :math:`(d_1, d_2, ..., d_n)`, `index_array` returns a
    /// :math:`(d_1, d_2, ..., d_n, n)` array `idx`, where
    /// :math:`idx[i_1, i_2, ..., i_n, :] = [i_1, i_2, ..., i_n]`.
    /// 
    /// Additionally, when the parameter `axes` is specified, `idx` will be a
    /// :math:`(d_1, d_2, ..., d_n, m)` array where `m` is the length of `axes`, and the following
    /// equality will hold: :math:`idx[i_1, i_2, ..., i_n, j] = i_{axes[j]}`.
    /// 
    /// Examples::
    /// 
    ///     x = mx.nd.ones((3, 2))
    /// 
    ///     mx.nd.contrib.index_array(x) = [[[0 0]
    ///                                      [0 1]]
    /// 
    ///                                     [[1 0]
    ///                                      [1 1]]
    /// 
    ///                                     [[2 0]
    ///                                      [2 1]]]
    /// 
    ///     x = mx.nd.ones((3, 2, 2))
    /// 
    ///     mx.nd.contrib.index_array(x, axes=(1, 0)) = [[[[0 0]
    ///                                                    [0 0]]
    /// 
    ///                                                   [[1 0]
    ///                                                    [1 0]]]
    /// 
    /// 
    ///                                                  [[[0 1]
    ///                                                    [0 1]]
    /// 
    ///                                                   [[1 1]
    ///                                                    [1 1]]]
    /// 
    /// 
    ///                                                  [[[0 2]
    ///                                                    [0 2]]
    /// 
    ///                                                   [[1 2]
    ///                                                    [1 2]]]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\index_array.cc:L118</summary>
    /// <param name="data">Input data</param>
    /// <param name="axes">The axes to include in the index array. Supports negative values.</param>
    static member ContribIndexArray(data : NDArray, [<Optional>] axes : int seq) =
        let creator = AtomicSymbolCreator.FromName "_contrib_index_array"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axes"|]
                                                 [|axes.ToString()|]
        outputs

    /// <summary>Copies the elements of a `new_tensor` into the `old_tensor`.
    /// 
    /// This operator copies the elements by selecting the indices in the order given in `index`.
    /// The output will be a new tensor containing the rest elements of old tensor and
    /// the copied elements of new tensor.
    /// For example, if `index[i] == j`, then the `i` th row of `new_tensor` is copied to the
    /// `j` th row of output.
    /// 
    /// The `index` must be a vector and it must have the same size with the `0` th dimension of
    /// `new_tensor`. Also, the `0` th dimension of old_tensor must `&gt;=` the `0` th dimension of
    /// `new_tensor`, or an error will be raised.
    /// 
    /// Examples::
    /// 
    ///     x = mx.nd.zeros((5,3))
    ///     t = mx.nd.array([[1,2,3],[4,5,6],[7,8,9]])
    ///     index = mx.nd.array([0,4,2])
    /// 
    ///     mx.nd.contrib.index_copy(x, index, t)
    /// 
    ///     [[1. 2. 3.]
    ///      [0. 0. 0.]
    ///      [7. 8. 9.]
    ///      [0. 0. 0.]
    ///      [4. 5. 6.]]
    ///     &lt;NDArray 5x3 @cpu(0)&gt;
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\index_copy.cc:L183</summary>
    /// <param name="oldTensor">Old tensor</param>
    /// <param name="indexVector">Index vector</param>
    /// <param name="newTensor">New tensor to be copied</param>
    static member ContribIndexCopy(oldTensor : NDArray, indexVector : NDArray, newTensor : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_contrib_index_copy"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|oldTensor.NDArrayHandle; indexVector.NDArrayHandle; newTensor.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Computes the Khatri-Rao product of the input matrices.
    /// 
    /// Given a collection of :math:`n` input matrices,
    /// 
    /// .. math::
    ///    A_1 \in \mathbb{R}^{M_1 \times M}, \ldots, A_n \in \mathbb{R}^{M_n \times N},
    /// 
    /// the (column-wise) Khatri-Rao product is defined as the matrix,
    /// 
    /// .. math::
    ///    X = A_1 \otimes \cdots \otimes A_n \in \mathbb{R}^{(M_1 \cdots M_n) \times N},
    /// 
    /// where the :math:`k` th column is equal to the column-wise outer product
    /// :math:`{A_1}_k \otimes \cdots \otimes {A_n}_k` where :math:`{A_i}_k` is the kth
    /// column of the ith matrix.
    /// 
    /// Example::
    /// 
    ///   &gt;&gt;&gt; A = mx.nd.array([[1, -1],
    ///   &gt;&gt;&gt;                  [2, -3]])
    ///   &gt;&gt;&gt; B = mx.nd.array([[1, 4],
    ///   &gt;&gt;&gt;                  [2, 5],
    ///   &gt;&gt;&gt;                  [3, 6]])
    ///   &gt;&gt;&gt; C = mx.nd.khatri_rao(A, B)
    ///   &gt;&gt;&gt; print(C.asnumpy())
    ///   [[  1.  -4.]
    ///    [  2.  -5.]
    ///    [  3.  -6.]
    ///    [  2. -12.]
    ///    [  4. -15.]
    ///    [  6. -18.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\krprod.cc:L108</summary>
    /// <param name="args">Positional input matrices</param>
    static member KhatriRao([<ParamArray>] args : NDArray[]) =
        let creator = AtomicSymbolCreator.FromName "khatri_rao"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Number of stored values for a sparse tensor, including explicit zeros.
    /// 
    /// This operator only supports CSR matrix on CPU.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\nnz.cc:L177</summary>
    /// <param name="data">Input</param>
    /// <param name="axis">Select between the number of values across the whole matrix, in each column, or in each row.</param>
    static member ContribGetnnz(data : NDArray, [<Optional>] axis : int Nullable) =
        let creator = AtomicSymbolCreator.FromName "_contrib_getnnz"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"|]
                                                 [|axis.ToString()|]
        outputs

    /// <summary>Number of stored values for a sparse tensor, including explicit zeros.
    /// 
    /// This operator only supports CSR matrix on CPU.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\nnz.cc:L177</summary>
    /// <param name="data">Input</param>
    /// <param name="axis">Select between the number of values across the whole matrix, in each column, or in each row.</param>
    static member ContribGetnnz(data : NDArray, ?axis : int) =
        let creator = AtomicSymbolCreator.FromName "_contrib_getnnz"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"|]
                                                 [|(match axis with None -> "None" | _ -> axis.ToString())|]
        outputs

    /// <summary>Update function for Group AdaGrad optimizer.
    /// 
    /// Referenced from *Adaptive Subgradient Methods for Online Learning and Stochastic Optimization*,
    /// and available at http://www.jmlr.org/papers/volume12/duchi11a/duchi11a.pdf but
    /// uses only a single learning rate for every row of the parameter array.
    /// 
    /// Updates are applied by::
    /// 
    ///     grad = clip(grad * rescale_grad, clip_gradient)
    ///     history += mean(square(grad), axis=1, keepdims=True)
    ///     div = grad / sqrt(history + float_stable_eps)
    ///     weight -= div * lr
    /// 
    /// Weights are updated lazily if the gradient is sparse.
    /// 
    /// Note that non-zero values for the weight decay option are not supported.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\optimizer_op.cc:L71</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="history">History</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    /// <param name="epsilon">Epsilon for numerical stability</param>
    static member ContribGroupAdagradUpdate(weight : NDArray, 
                                            grad : NDArray, 
                                            history : NDArray, 
                                            lr : float, 
                                            [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
                                            [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float, 
                                            [<Optional; DefaultParameterValue(9.99999975E-06)>] epsilon : float) =
        let creator = AtomicSymbolCreator.FromName "_contrib_group_adagrad_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle; history.NDArrayHandle|]
                                                 [|"lr"; "rescale_grad"; "clip_gradient"; "epsilon"|]
                                                 [|lr.ToString(); rescaleGrad.ToString(); clipGradient.ToString(); epsilon.ToString()|]
        outputs

    /// <summary>This operators implements the quadratic function.
    /// 
    /// .. math::
    ///     f(x) = ax^2+bx+c
    /// 
    /// where :math:`x` is an input tensor and all operations
    /// in the function are element-wise.
    /// 
    /// Example::
    /// 
    ///   x = [[1, 2], [3, 4]]
    ///   y = quadratic(data=x, a=1, b=2, c=3)
    ///   y = [[6, 11], [18, 27]]
    /// 
    /// The storage type of ``quadratic`` output depends on storage types of inputs
    ///   - quadratic(csr, a, b, 0) = csr
    ///   - quadratic(default, a, b, c) = default
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\quadratic_op.cc:L50</summary>
    /// <param name="data">Input ndarray</param>
    /// <param name="a">Coefficient of the quadratic term in the quadratic function.</param>
    /// <param name="b">Coefficient of the linear term in the quadratic function.</param>
    /// <param name="c">Constant term in the quadratic function.</param>
    static member ContribQuadratic(data : NDArray, [<Optional; DefaultParameterValue(0.0)>] a : float, [<Optional; DefaultParameterValue(0.0)>] b : float, [<Optional; DefaultParameterValue(0.0)>] c : float) =
        let creator = AtomicSymbolCreator.FromName "_contrib_quadratic"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"a"; "b"; "c"|]
                                                 [|a.ToString(); b.ToString(); c.ToString()|]
        outputs

    /// <summary>
    /// This operator takes a 4D feature map as an input array and region proposals as `rois`,
    /// then align the feature map over sub-regions of input and produces a fixed-sized output array.
    /// This operator is typically used in Faster R-CNN &amp; Mask R-CNN networks.
    /// 
    /// Different from ROI pooling, ROI Align removes the harsh quantization, properly aligning
    /// the extracted features with the input. RoIAlign computes the value of each sampling point
    /// by bilinear interpolation from the nearby grid points on the feature map. No quantization is
    /// performed on any coordinates involved in the RoI, its bins, or the sampling points.
    /// Bilinear interpolation is used to compute the exact values of the
    /// input features at four regularly sampled locations in each RoI bin.
    /// Then the feature map can be aggregated by avgpooling.
    /// 
    /// 
    /// References
    /// ----------
    /// 
    /// He, Kaiming, et al. &quot;Mask R-CNN.&quot; ICCV, 2017
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\roi_align.cc:L538</summary>
    /// <param name="data">Input data to the pooling operator, a 4D Feature maps</param>
    /// <param name="rois">Bounding box coordinates, a 2D array</param>
    /// <param name="pooledSize">ROI Align output roi feature map height and width: (h, w)</param>
    /// <param name="spatialScale">Ratio of input feature map height (or w) to raw image height (or w). Equals the reciprocal of total stride in convolutional layers</param>
    /// <param name="sampleRatio">Optional sampling ratio of ROI align, using adaptive size by default.</param>
    /// <param name="positionSensitive">Whether to perform position-sensitive RoI pooling. PSRoIPooling is first proposaled by R-FCN and it can reduce the input channels by ph*pw times, where (ph, pw) is the pooled_size</param>
    static member ContribROIAlign(data : NDArray, 
                                  rois : NDArray, 
                                  pooledSize : int seq, 
                                  spatialScale : float, 
                                  [<Optional; DefaultParameterValue(-1)>] sampleRatio : int, 
                                  [<Optional; DefaultParameterValue(false)>] positionSensitive : bool) =
        let creator = AtomicSymbolCreator.FromName "_contrib_ROIAlign"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; rois.NDArrayHandle|]
                                                 [|"pooled_size"; "spatial_scale"; "sample_ratio"; "position_sensitive"|]
                                                 [|pooledSize.ToString(); spatialScale.ToString(); sampleRatio.ToString(); positionSensitive.ToString()|]
        outputs

    /// <summary>
    /// This operator takes a 4D feature map as an input array and region proposals as `rois`,
    /// then align the feature map over sub-regions of input and produces a fixed-sized output array.
    /// This operator is typically used in Faster R-CNN &amp; Mask R-CNN networks.
    /// 
    /// Different from ROI pooling, ROI Align removes the harsh quantization, properly aligning
    /// the extracted features with the input. RoIAlign computes the value of each sampling point
    /// by bilinear interpolation from the nearby grid points on the feature map. No quantization is
    /// performed on any coordinates involved in the RoI, its bins, or the sampling points.
    /// Bilinear interpolation is used to compute the exact values of the
    /// input features at four regularly sampled locations in each RoI bin.
    /// Then the feature map can be aggregated by avgpooling.
    /// 
    /// 
    /// References
    /// ----------
    /// 
    /// He, Kaiming, et al. &quot;Mask R-CNN.&quot; ICCV, 2017
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\roi_align.cc:L538</summary>
    /// <param name="data">Input data to the pooling operator, a 4D Feature maps</param>
    /// <param name="rois">Bounding box coordinates, a 2D array</param>
    /// <param name="height">ROI Align output roi feature map height</param>
    /// <param name="width">ROI Align output roi feature map width</param>
    /// <param name="spatialScale">Ratio of input feature map height (or w) to raw image height (or w). Equals the reciprocal of total stride in convolutional layers</param>
    /// <param name="sampleRatio">Optional sampling ratio of ROI align, using adaptive size by default.</param>
    /// <param name="positionSensitive">Whether to perform position-sensitive RoI pooling. PSRoIPooling is first proposaled by R-FCN and it can reduce the input channels by ph*pw times, where (ph, pw) is the pooled_size</param>
    static member ContribROIAlign(data : NDArray, 
                                  rois : NDArray, 
                                  height : int, 
                                  width : int, 
                                  spatialScale : float, 
                                  [<Optional; DefaultParameterValue(-1)>] sampleRatio : int, 
                                  [<Optional; DefaultParameterValue(false)>] positionSensitive : bool) =
        let creator = AtomicSymbolCreator.FromName "_contrib_ROIAlign"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; rois.NDArrayHandle|]
                                                 [|"pooled_size"; "spatial_scale"; "sample_ratio"; "position_sensitive"|]
                                                 [|(height, width).ToString(); spatialScale.ToString(); sampleRatio.ToString(); positionSensitive.ToString()|]
        outputs

    /// <summary>Batch normalization.
    /// 
    /// Normalizes a data batch by mean and variance, and applies a scale ``gamma`` as
    /// well as offset ``beta``.
    /// Standard BN [1]_ implementation only normalize the data within each device.
    /// SyncBN normalizes the input within the whole mini-batch.
    /// We follow the sync-onece implmentation described in the paper [2]_.
    /// 
    /// Assume the input has more than one dimension and we normalize along axis 1.
    /// We first compute the mean and variance along this axis:
    /// 
    /// .. math::
    /// 
    ///   data\_mean[i] = mean(data[:,i,:,...]) \\
    ///   data\_var[i] = var(data[:,i,:,...])
    /// 
    /// Then compute the normalized output, which has the same shape as input, as following:
    /// 
    /// .. math::
    /// 
    ///   out[:,i,:,...] = \frac{data[:,i,:,...] - data\_mean[i]}{\sqrt{data\_var[i]+\epsilon}} * gamma[i] + beta[i]
    /// 
    /// Both *mean* and *var* returns a scalar by treating the input as a vector.
    /// 
    /// Assume the input has size *k* on axis 1, then both ``gamma`` and ``beta``
    /// have shape *(k,)*. If ``output_mean_var`` is set to be true, then outputs both ``data_mean`` and
    /// ``data_var`` as well, which are needed for the backward pass.
    /// 
    /// Besides the inputs and the outputs, this operator accepts two auxiliary
    /// states, ``moving_mean`` and ``moving_var``, which are *k*-length
    /// vectors. They are global statistics for the whole dataset, which are updated
    /// by::
    /// 
    ///   moving_mean = moving_mean * momentum + data_mean * (1 - momentum)
    ///   moving_var = moving_var * momentum + data_var * (1 - momentum)
    /// 
    /// If ``use_global_stats`` is set to be true, then ``moving_mean`` and
    /// ``moving_var`` are used instead of ``data_mean`` and ``data_var`` to compute
    /// the output. It is often used during inference.
    /// 
    /// Both ``gamma`` and ``beta`` are learnable parameters. But if ``fix_gamma`` is true,
    /// then set ``gamma`` to 1 and its gradient to 0.
    /// 
    /// Reference:
    ///   .. [1] Ioffe, Sergey, and Christian Szegedy. &quot;Batch normalization: Accelerating \
    ///     deep network training by reducing internal covariate shift.&quot; *ICML 2015*
    ///   .. [2] Hang Zhang, Kristin Dana, Jianping Shi, Zhongyue Zhang, Xiaogang Wang, \
    ///     Ambrish Tyagi, and Amit Agrawal. &quot;Context Encoding for Semantic Segmentation.&quot; *CVPR 2018*
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\sync_batch_norm.cc:L97</summary>
    /// <param name="data">Input data to batch normalization</param>
    /// <param name="gamma">gamma array</param>
    /// <param name="beta">beta array</param>
    /// <param name="movingMean">running mean of input</param>
    /// <param name="movingVar">running variance of input</param>
    /// <param name="eps">Epsilon to prevent div 0</param>
    /// <param name="momentum">Momentum for moving average</param>
    /// <param name="fixGamma">Fix gamma while training</param>
    /// <param name="useGlobalStats">Whether use global moving statistics instead of local batch-norm. This will force change batch-norm into a scale shift operator.</param>
    /// <param name="outputMeanVar">Output All,normal mean and var</param>
    /// <param name="ndev">The count of GPU devices</param>
    /// <param name="key">Hash key for synchronization, please set the same hash key for same layer, Block.prefix is typically used as in :class:`gluon.nn.contrib.SyncBatchNorm`.</param>
    static member ContribSyncBatchNorm(data : NDArray, 
                                       gamma : NDArray, 
                                       beta : NDArray, 
                                       movingMean : NDArray, 
                                       movingVar : NDArray, 
                                       [<Optional; DefaultParameterValue(0.00100000005)>] eps : float, 
                                       [<Optional; DefaultParameterValue(0.899999976)>] momentum : float, 
                                       [<Optional; DefaultParameterValue(true)>] fixGamma : bool, 
                                       [<Optional; DefaultParameterValue(false)>] useGlobalStats : bool, 
                                       [<Optional; DefaultParameterValue(false)>] outputMeanVar : bool, 
                                       [<Optional; DefaultParameterValue(1)>] ndev : int, 
                                       key : string) =
        let creator = AtomicSymbolCreator.FromName "_contrib_SyncBatchNorm"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; gamma.NDArrayHandle; beta.NDArrayHandle; movingMean.NDArrayHandle; movingVar.NDArrayHandle|]
                                                 [|"eps"; "momentum"; "fix_gamma"; "use_global_stats"; "output_mean_var"; "ndev"; "key"|]
                                                 [|eps.ToString(); momentum.ToString(); fixGamma.ToString(); useGlobalStats.ToString(); outputMeanVar.ToString(); ndev.ToString(); key.ToString()|]
        outputs

    /// <summary>Rescale the input by the square root of the channel dimension.
    /// 
    ///    out = data / sqrt(data.shape[-1])
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\transformer.cc:L38</summary>
    /// <param name="data">The input array.</param>
    static member ContribDivSqrtDim(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_contrib_div_sqrt_dim"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Apply a custom operator implemented in a frontend language (like Python).
    /// 
    /// Custom operators should override required methods like `forward` and `backward`.
    /// The custom operator must be registered before it can be used.
    /// Please check the tutorial here: http://mxnet.io/faq/new_op.html.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\custom\custom.cc:L546</summary>
    /// <param name="data">Input data for the custom operator.</param>
    /// <param name="opType">Name of the custom operator. This is the name that is passed to `mx.operator.register` to register the operator.</param>
    static member Custom([<ParamArray>] data : NDArray[], opType : string) =
        let creator = AtomicSymbolCreator.FromName "Custom"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 [|"op_type"|]
                                                 [|opType.ToString()|]
        outputs

    /// <summary>Apply a sparse regularization to the output a sigmoid activation function.</summary>
    /// <param name="data">Input data.</param>
    /// <param name="sparsenessTarget">The sparseness target</param>
    /// <param name="penalty">The tradeoff parameter for the sparseness penalty</param>
    /// <param name="momentum">The momentum for running average</param>
    static member IdentityAttachKLSparseReg(data : NDArray, [<Optional; DefaultParameterValue(0.100000001)>] sparsenessTarget : float, [<Optional; DefaultParameterValue(0.00100000005)>] penalty : float, [<Optional; DefaultParameterValue(0.899999976)>] momentum : float) =
        let creator = AtomicSymbolCreator.FromName "IdentityAttachKLSparseReg"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"sparseness_target"; "penalty"; "momentum"|]
                                                 [|sparsenessTarget.ToString(); penalty.ToString(); momentum.ToString()|]
        outputs

    /// <summary>Crop an image NDArray of shape (H x W x C) or (N x H x W x C) 
    /// to the given size.
    /// Example:
    ///     .. code-block:: python
    ///         image = mx.nd.random.uniform(0, 255, (4, 2, 3)).astype(dtype=np.uint8)
    ///         mx.nd.image.crop(image, 1, 1, 2, 2)
    ///             [[[144  34   4]
    ///               [ 82 157  38]]
    /// 
    ///              [[156 111 230]
    ///               [177  25  15]]]
    ///             &lt;NDArray 2x2x3 @cpu(0)&gt;
    ///         image = mx.nd.random.uniform(0, 255, (2, 4, 2, 3)).astype(dtype=np.uint8)
    ///         mx.nd.image.crop(image, 1, 1, 2, 2)            
    ///             [[[[ 35 198  50]
    ///                [242  94 168]]
    /// 
    ///               [[223 119 129]
    ///                [249  14 154]]]
    /// 
    /// 
    ///               [[[137 215 106]
    ///                 [ 79 174 133]]
    /// 
    ///                [[116 142 109]
    ///                 [ 35 239  50]]]]
    ///             &lt;NDArray 2x2x2x3 @cpu(0)&gt;
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\crop.cc:L65</summary>
    /// <param name="data">The input.</param>
    /// <param name="x">Left boundary of the cropping area.</param>
    /// <param name="y">Top boundary of the cropping area.</param>
    /// <param name="width">Width of the cropping area.</param>
    /// <param name="height">Height of the cropping area.</param>
    static member ImageCrop(data : NDArray, 
                            x : int, 
                            y : int, 
                            width : int, 
                            height : int) =
        let creator = AtomicSymbolCreator.FromName "_image_crop"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"x"; "y"; "width"; "height"|]
                                                 [|x.ToString(); y.ToString(); width.ToString(); height.ToString()|]
        outputs

    /// <summary>Converts an image NDArray of shape (H x W x C) or (N x H x W x C) 
    /// with values in the range [0, 255] to a tensor NDArray of shape (C x H x W) or (N x C x H x W)
    /// with values in the range [0, 1]
    /// 
    /// Example:
    ///     .. code-block:: python
    ///         image = mx.nd.random.uniform(0, 255, (4, 2, 3)).astype(dtype=np.uint8)
    ///         to_tensor(image)
    ///             [[[ 0.85490197  0.72156864]
    ///               [ 0.09019608  0.74117649]
    ///               [ 0.61960787  0.92941177]
    ///               [ 0.96470588  0.1882353 ]]
    ///              [[ 0.6156863   0.73725492]
    ///               [ 0.46666667  0.98039216]
    ///               [ 0.44705883  0.45490196]
    ///               [ 0.01960784  0.8509804 ]]
    ///              [[ 0.39607844  0.03137255]
    ///               [ 0.72156864  0.52941179]
    ///               [ 0.16470589  0.7647059 ]
    ///               [ 0.05490196  0.70588237]]]
    ///              &lt;NDArray 3x4x2 @cpu(0)&gt;
    /// 
    ///         image = mx.nd.random.uniform(0, 255, (2, 4, 2, 3)).astype(dtype=np.uint8)
    ///         to_tensor(image)
    ///             [[[[0.11764706 0.5803922 ]
    ///                [0.9411765  0.10588235]
    ///                [0.2627451  0.73333335]
    ///                [0.5647059  0.32156864]]
    ///               [[0.7176471  0.14117648]
    ///                [0.75686276 0.4117647 ]
    ///                [0.18431373 0.45490196]
    ///                [0.13333334 0.6156863 ]]
    ///               [[0.6392157  0.5372549 ]
    ///                [0.52156866 0.47058824]
    ///                [0.77254903 0.21568628]
    ///                [0.01568628 0.14901961]]]
    ///              [[[0.6117647  0.38431373]
    ///                [0.6784314  0.6117647 ]
    ///                [0.69411767 0.96862745]
    ///                [0.67058825 0.35686275]]
    ///               [[0.21960784 0.9411765 ]
    ///                [0.44705883 0.43529412]
    ///                [0.09803922 0.6666667 ]
    ///                [0.16862746 0.1254902 ]]
    ///               [[0.6156863  0.9019608 ]
    ///                [0.35686275 0.9019608 ]
    ///                [0.05882353 0.6509804 ]
    ///                [0.20784314 0.7490196 ]]]]
    ///             &lt;NDArray 2x3x4x2 @cpu(0)&gt;
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\image_random.cc:L91</summary>
    /// <param name="data">Input ndarray</param>
    static member ImageToTensor(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_image_to_tensor"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Normalize an tensor of shape (C x H x W) or (N x C x H x W) with mean and
    ///     standard deviation.
    /// 
    ///     Given mean `(m1, ..., mn)` and std `(s\ :sub:`1`\ , ..., s\ :sub:`n`)` for `n` channels,
    ///     this transform normalizes each channel of the input tensor with:
    /// 
    /// .. math::
    /// 
    ///         output[i] = (input[i] - m\ :sub:`i`\ ) / s\ :sub:`i`
    /// 
    ///     If mean or std is scalar, the same value will be applied to all channels.
    /// 
    ///     Default value for mean is 0.0 and stand deviation is 1.0.
    /// 
    /// Example:
    /// 
    ///     .. code-block:: python
    ///         image = mx.nd.random.uniform(0, 1, (3, 4, 2))
    ///         normalize(image, mean=(0, 1, 2), std=(3, 2, 1))
    ///             [[[ 0.18293785  0.19761486]
    ///               [ 0.23839645  0.28142193]
    ///               [ 0.20092112  0.28598186]
    ///               [ 0.18162774  0.28241724]]
    ///              [[-0.2881726  -0.18821815]
    ///               [-0.17705294 -0.30780914]
    ///               [-0.2812064  -0.3512327 ]
    ///               [-0.05411351 -0.4716435 ]]
    ///              [[-1.0363373  -1.7273437 ]
    ///               [-1.6165586  -1.5223348 ]
    ///               [-1.208275   -1.1878313 ]
    ///               [-1.4711051  -1.5200229 ]]]
    ///             &lt;NDArray 3x4x2 @cpu(0)&gt;
    /// 
    ///         image = mx.nd.random.uniform(0, 1, (2, 3, 4, 2))
    ///         normalize(image, mean=(0, 1, 2), std=(3, 2, 1))
    ///             [[[[ 0.18934818  0.13092826]
    ///                [ 0.3085322   0.27869293]
    ///                [ 0.02367868  0.11246539]
    ///                [ 0.0290431   0.2160573 ]]
    ///               [[-0.4898908  -0.31587923]
    ///                [-0.08369008 -0.02142242]
    ///                [-0.11092162 -0.42982462]
    ///                [-0.06499392 -0.06495637]]
    ///               [[-1.0213816  -1.526392  ]
    ///                [-1.2008414  -1.1990893 ]
    ///                [-1.5385206  -1.4795225 ]
    ///                [-1.2194707  -1.3211205 ]]]
    ///              [[[ 0.03942481  0.24021089]
    ///                [ 0.21330701  0.1940066 ]
    ///                [ 0.04778443  0.17912441]
    ///                [ 0.31488964  0.25287187]]
    ///               [[-0.23907584 -0.4470462 ]
    ///                [-0.29266903 -0.2631998 ]
    ///                [-0.3677222  -0.40683383]
    ///                [-0.11288315 -0.13154092]]
    ///               [[-1.5438497  -1.7834496 ]
    ///                [-1.431566   -1.8647819 ]
    ///                [-1.9812102  -1.675859  ]
    ///                [-1.3823645  -1.8503251 ]]]]
    ///             &lt;NDArray 2x3x4x2 @cpu(0)&gt;
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\image_random.cc:L165</summary>
    /// <param name="data">Input ndarray</param>
    /// <param name="mean">Sequence of means for each channel. Default value is 0.</param>
    /// <param name="std">Sequence of standard deviations for each channel. Default value is 1.</param>
    static member ImageNormalize(data : NDArray, mean : double [], std : double []) =
        let creator = AtomicSymbolCreator.FromName "_image_normalize"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"mean"; "std"|]
                                                 [|mean.ToString(); std.ToString()|]
        outputs

    /// <summary>
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\image_random.cc:L192</summary>
    /// <param name="data">The input.</param>
    static member ImageFlipLeftRight(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_image_flip_left_right"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\image_random.cc:L196</summary>
    /// <param name="data">The input.</param>
    static member ImageRandomFlipLeftRight(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_image_random_flip_left_right"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\image_random.cc:L200</summary>
    /// <param name="data">The input.</param>
    static member ImageFlipTopBottom(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_image_flip_top_bottom"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\image_random.cc:L204</summary>
    /// <param name="data">The input.</param>
    static member ImageRandomFlipTopBottom(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_image_random_flip_top_bottom"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\image_random.cc:L208</summary>
    /// <param name="data">The input.</param>
    /// <param name="minFactor">Minimum factor.</param>
    /// <param name="maxFactor">Maximum factor.</param>
    static member ImageRandomBrightness(data : NDArray, minFactor : float, maxFactor : float) =
        let creator = AtomicSymbolCreator.FromName "_image_random_brightness"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"min_factor"; "max_factor"|]
                                                 [|minFactor.ToString(); maxFactor.ToString()|]
        outputs

    /// <summary>
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\image_random.cc:L214</summary>
    /// <param name="data">The input.</param>
    /// <param name="minFactor">Minimum factor.</param>
    /// <param name="maxFactor">Maximum factor.</param>
    static member ImageRandomContrast(data : NDArray, minFactor : float, maxFactor : float) =
        let creator = AtomicSymbolCreator.FromName "_image_random_contrast"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"min_factor"; "max_factor"|]
                                                 [|minFactor.ToString(); maxFactor.ToString()|]
        outputs

    /// <summary>
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\image_random.cc:L221</summary>
    /// <param name="data">The input.</param>
    /// <param name="minFactor">Minimum factor.</param>
    /// <param name="maxFactor">Maximum factor.</param>
    static member ImageRandomSaturation(data : NDArray, minFactor : float, maxFactor : float) =
        let creator = AtomicSymbolCreator.FromName "_image_random_saturation"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"min_factor"; "max_factor"|]
                                                 [|minFactor.ToString(); maxFactor.ToString()|]
        outputs

    /// <summary>
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\image_random.cc:L228</summary>
    /// <param name="data">The input.</param>
    /// <param name="minFactor">Minimum factor.</param>
    /// <param name="maxFactor">Maximum factor.</param>
    static member ImageRandomHue(data : NDArray, minFactor : float, maxFactor : float) =
        let creator = AtomicSymbolCreator.FromName "_image_random_hue"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"min_factor"; "max_factor"|]
                                                 [|minFactor.ToString(); maxFactor.ToString()|]
        outputs

    /// <summary>
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\image_random.cc:L235</summary>
    /// <param name="data">The input.</param>
    /// <param name="brightness">How much to jitter brightness.</param>
    /// <param name="contrast">How much to jitter contrast.</param>
    /// <param name="saturation">How much to jitter saturation.</param>
    /// <param name="hue">How much to jitter hue.</param>
    static member ImageRandomColorJitter(data : NDArray, 
                                         brightness : float, 
                                         contrast : float, 
                                         saturation : float, 
                                         hue : float) =
        let creator = AtomicSymbolCreator.FromName "_image_random_color_jitter"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"brightness"; "contrast"; "saturation"; "hue"|]
                                                 [|brightness.ToString(); contrast.ToString(); saturation.ToString(); hue.ToString()|]
        outputs

    /// <summary>Randomly add PCA noise. Follow the AlexNet style.
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\image_random.cc:L249</summary>
    /// <param name="data">The input.</param>
    /// <param name="alphaStd">Level of the lighting noise.</param>
    static member ImageRandomLighting(data : NDArray, [<Optional; DefaultParameterValue(0.0500000007)>] alphaStd : float) =
        let creator = AtomicSymbolCreator.FromName "_image_random_lighting"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"alpha_std"|]
                                                 [|alphaStd.ToString()|]
        outputs

    /// <summary>Resize an image NDArray of shape (H x W x C) or (N x H x W x C) 
    /// to the given size
    /// Example:
    ///     .. code-block:: python
    ///         image = mx.nd.random.uniform(0, 255, (4, 2, 3)).astype(dtype=np.uint8)
    ///         mx.nd.image.resize(image, (3, 3))
    ///             [[[124 111 197]
    ///               [158  80 155]
    ///               [193  50 112]]
    /// 
    ///              [[110 100 113]
    ///               [134 165 148]
    ///               [157 231 182]]
    /// 
    ///              [[202 176 134]
    ///               [174 191 149]
    ///               [147 207 164]]]
    ///             &lt;NDArray 3x3x3 @cpu(0)&gt;
    ///         image = mx.nd.random.uniform(0, 255, (2, 4, 2, 3)).astype(dtype=np.uint8)
    ///         mx.nd.image.resize(image, (2, 2))            
    ///             [[[[ 59 133  80]
    ///                [187 114 153]]
    /// 
    ///               [[ 38 142  39]
    ///                [207 131 124]]]
    /// 
    /// 
    ///               [[[117 125 136]
    ///                [191 166 150]]
    /// 
    ///               [[129  63 113]
    ///                [182 109  48]]]]
    ///             &lt;NDArray 2x2x2x3 @cpu(0)&gt;
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\resize.cc:L70</summary>
    /// <param name="data">The input.</param>
    /// <param name="outputSize">Size of new image</param>
    /// <param name="keepRatio">Whether to resize the short edge or both edges to `size`, if size is give as an integer.</param>
    /// <param name="interp">Interpolation method for resizing. By default uses bilinear interpolationOptions are INTER_NEAREST - a nearest-neighbor interpolationINTER_LINEAR - a bilinear interpolationINTER_AREA - resampling using pixel area relationINTER_CUBIC - a bicubic interpolation over 4x4 pixel neighborhoodINTER_LANCZOS4 - a Lanczos interpolation over 8x8 pixel neighborhoodNote that the GPU version only support bilinear interpolation(1) and the result on cpu would be slightly different from gpu.It uses opencv resize function which tend to align center on cpuwhile using contrib.bilinearResize2D which aligns corner on gpu</param>
    static member ImageResize(data : NDArray, [<Optional>] size : int, [<Optional; DefaultParameterValue(false)>] keepRatio : bool, [<Optional; DefaultParameterValue(1)>] interp : int) =
        let creator = AtomicSymbolCreator.FromName "_image_resize"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"size"; "keep_ratio"; "interp"|]
                                                 [|(if isNull (size :> obj) then "[]" else size.ToString()); keepRatio.ToString(); interp.ToString()|]
        outputs

    /// <summary>Resize an image NDArray of shape (H x W x C) or (N x H x W x C) 
    /// to the given size
    /// Example:
    ///     .. code-block:: python
    ///         image = mx.nd.random.uniform(0, 255, (4, 2, 3)).astype(dtype=np.uint8)
    ///         mx.nd.image.resize(image, (3, 3))
    ///             [[[124 111 197]
    ///               [158  80 155]
    ///               [193  50 112]]
    /// 
    ///              [[110 100 113]
    ///               [134 165 148]
    ///               [157 231 182]]
    /// 
    ///              [[202 176 134]
    ///               [174 191 149]
    ///               [147 207 164]]]
    ///             &lt;NDArray 3x3x3 @cpu(0)&gt;
    ///         image = mx.nd.random.uniform(0, 255, (2, 4, 2, 3)).astype(dtype=np.uint8)
    ///         mx.nd.image.resize(image, (2, 2))            
    ///             [[[[ 59 133  80]
    ///                [187 114 153]]
    /// 
    ///               [[ 38 142  39]
    ///                [207 131 124]]]
    /// 
    /// 
    ///               [[[117 125 136]
    ///                [191 166 150]]
    /// 
    ///               [[129  63 113]
    ///                [182 109  48]]]]
    ///             &lt;NDArray 2x2x2x3 @cpu(0)&gt;
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\resize.cc:L70</summary>
    /// <param name="data">The input.</param>
    /// <param name="height">Height of new image</param>
    /// <param name="width">ROI Align output roi feature map width</param>
    /// <param name="keepRatio">Whether to resize the short edge or both edges to `size`, if size is give as an integer.</param>
    /// <param name="interp">Interpolation method for resizing. By default uses bilinear interpolationOptions are INTER_NEAREST - a nearest-neighbor interpolationINTER_LINEAR - a bilinear interpolationINTER_AREA - resampling using pixel area relationINTER_CUBIC - a bicubic interpolation over 4x4 pixel neighborhoodINTER_LANCZOS4 - a Lanczos interpolation over 8x8 pixel neighborhoodNote that the GPU version only support bilinear interpolation(1) and the result on cpu would be slightly different from gpu.It uses opencv resize function which tend to align center on cpuwhile using contrib.bilinearResize2D which aligns corner on gpu</param>
    static member ImageResize(data : NDArray, 
                              [<Optional>] height : int, 
                              [<Optional>] width : int, 
                              [<Optional; DefaultParameterValue(false)>] keepRatio : bool, 
                              [<Optional; DefaultParameterValue(1)>] interp : int) =
        let creator = AtomicSymbolCreator.FromName "_image_resize"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"size"; "keep_ratio"; "interp"|]
                                                 [|(if isNull (height :> obj) then "[]" else (height, width).ToString()); keepRatio.ToString(); interp.ToString()|]
        outputs

    /// <summary>Applies Leaky rectified linear unit activation element-wise to the input.
    /// 
    /// Leaky ReLUs attempt to fix the &quot;dying ReLU&quot; problem by allowing a small `slope`
    /// when the input is negative and has a slope of one when input is positive.
    /// 
    /// The following modified ReLU Activation functions are supported:
    /// 
    /// - *elu*: Exponential Linear Unit. `y = x &gt; 0 ? x : slope * (exp(x)-1)`
    /// - *selu*: Scaled Exponential Linear Unit. `y = lambda * (x &gt; 0 ? x : alpha * (exp(x) - 1))` where
    ///   *lambda = 1.0507009873554804934193349852946* and *alpha = 1.6732632423543772848170429916717*.
    /// - *leaky*: Leaky ReLU. `y = x &gt; 0 ? x : slope * x`
    /// - *prelu*: Parametric ReLU. This is same as *leaky* except that `slope` is learnt during training.
    /// - *rrelu*: Randomized ReLU. same as *leaky* but the `slope` is uniformly and randomly chosen from
    ///   *[lower_bound, upper_bound)* for training, while fixed to be
    ///   *(lower_bound+upper_bound)/2* for inference.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\leaky_relu.cc:L65</summary>
    /// <param name="data">Input data to activation function.</param>
    /// <param name="gamma">Slope parameter for PReLU. Only required when act_type is &#39;prelu&#39;. It should be either a vector of size 1, or the same size as the second dimension of data.</param>
    /// <param name="actType">Activation function to be applied.</param>
    /// <param name="slope">Init slope for the activation. (For leaky and elu only)</param>
    /// <param name="lowerBound">Lower bound of random slope. (For rrelu only)</param>
    /// <param name="upperBound">Upper bound of random slope. (For rrelu only)</param>
    static member LeakyReLU(data : NDArray, 
                            gamma : NDArray, 
                            [<Optional>] actType : LeakyReLUType, 
                            [<Optional; DefaultParameterValue(0.25)>] slope : float, 
                            [<Optional; DefaultParameterValue(0.125)>] lowerBound : float, 
                            [<Optional; DefaultParameterValue(0.333999991)>] upperBound : float) =
        let creator = AtomicSymbolCreator.FromName "LeakyReLU"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; gamma.NDArrayHandle|]
                                                 [|"act_type"; "slope"; "lower_bound"; "upper_bound"|]
                                                 [|(if isNull (actType :> obj) then "leaky" else actType.ToString()); slope.ToString(); lowerBound.ToString(); upperBound.ToString()|]
        outputs

    /// <summary>Calculate cross entropy of softmax output and one-hot label.
    /// 
    /// - This operator computes the cross entropy in two steps:
    ///   - Applies softmax function on the input array.
    ///   - Computes and returns the cross entropy loss between the softmax output and the labels.
    /// 
    /// - The softmax function and cross entropy loss is given by:
    /// 
    ///   - Softmax Function:
    /// 
    ///   .. math:: \text{softmax}(x)_i = \frac{exp(x_i)}{\sum_j exp(x_j)}
    /// 
    ///   - Cross Entropy Function:
    /// 
    ///   .. math:: \text{CE(label, output)} = - \sum_i \text{label}_i \log(\text{output}_i)
    /// 
    /// Example::
    /// 
    ///   x = [[1, 2, 3],
    ///        [11, 7, 5]]
    /// 
    ///   label = [2, 0]
    /// 
    ///   softmax(x) = [[0.09003057, 0.24472848, 0.66524094],
    ///                 [0.97962922, 0.01794253, 0.00242826]]
    /// 
    ///   softmax_cross_entropy(data, label) = - log(0.66524084) - log(0.97962922) = 0.4281871
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\loss_binary_op.cc:L59</summary>
    /// <param name="data">Input data</param>
    /// <param name="label">Input label</param>
    static member SoftmaxCrossEntropy(data : NDArray, label : NDArray) =
        let creator = AtomicSymbolCreator.FromName "softmax_cross_entropy"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; label.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Applies an activation function element-wise to the input.
    /// 
    /// The following activation functions are supported:
    /// 
    /// - `relu`: Rectified Linear Unit, :math:`y = max(x, 0)`
    /// - `sigmoid`: :math:`y = \frac{1}{1 + exp(-x)}`
    /// - `tanh`: Hyperbolic tangent, :math:`y = \frac{exp(x) - exp(-x)}{exp(x) + exp(-x)}`
    /// - `softrelu`: Soft ReLU, or SoftPlus, :math:`y = log(1 + exp(x))`
    /// - `softsign`: :math:`y = \frac{x}{1 + abs(x)}`
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\activation.cc:L167</summary>
    /// <param name="data">The input array.</param>
    /// <param name="actType">Activation function to be applied.</param>
    static member Activation(data : NDArray, actType : ActType) =
        let creator = AtomicSymbolCreator.FromName "Activation"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"act_type"|]
                                                 [|actType.ToString()|]
        outputs

    /// <summary>Batch normalization.
    /// 
    /// Normalizes a data batch by mean and variance, and applies a scale ``gamma`` as
    /// well as offset ``beta``.
    /// 
    /// Assume the input has more than one dimension and we normalize along axis 1.
    /// We first compute the mean and variance along this axis:
    /// 
    /// .. math::
    /// 
    ///   data\_mean[i] = mean(data[:,i,:,...]) \\
    ///   data\_var[i] = var(data[:,i,:,...])
    /// 
    /// Then compute the normalized output, which has the same shape as input, as following:
    /// 
    /// .. math::
    /// 
    ///   out[:,i,:,...] = \frac{data[:,i,:,...] - data\_mean[i]}{\sqrt{data\_var[i]+\epsilon}} * gamma[i] + beta[i]
    /// 
    /// Both *mean* and *var* returns a scalar by treating the input as a vector.
    /// 
    /// Assume the input has size *k* on axis 1, then both ``gamma`` and ``beta``
    /// have shape *(k,)*. If ``output_mean_var`` is set to be true, then outputs both ``data_mean`` and
    /// the inverse of ``data_var``, which are needed for the backward pass. Note that gradient of these
    /// two outputs are blocked.
    /// 
    /// Besides the inputs and the outputs, this operator accepts two auxiliary
    /// states, ``moving_mean`` and ``moving_var``, which are *k*-length
    /// vectors. They are global statistics for the whole dataset, which are updated
    /// by::
    /// 
    ///   moving_mean = moving_mean * momentum + data_mean * (1 - momentum)
    ///   moving_var = moving_var * momentum + data_var * (1 - momentum)
    /// 
    /// If ``use_global_stats`` is set to be true, then ``moving_mean`` and
    /// ``moving_var`` are used instead of ``data_mean`` and ``data_var`` to compute
    /// the output. It is often used during inference.
    /// 
    /// The parameter ``axis`` specifies which axis of the input shape denotes
    /// the &#39;channel&#39; (separately normalized groups).  The default is 1.  Specifying -1 sets the channel
    /// axis to be the last item in the input shape.
    /// 
    /// Both ``gamma`` and ``beta`` are learnable parameters. But if ``fix_gamma`` is true,
    /// then set ``gamma`` to 1 and its gradient to 0.
    /// 
    /// .. Note::
    ///   When ``fix_gamma`` is set to True, no sparse support is provided. If ``fix_gamma is`` set to False,
    ///   the sparse tensors will fallback.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\batch_norm.cc:L572</summary>
    /// <param name="data">Input data to batch normalization</param>
    /// <param name="gamma">gamma array</param>
    /// <param name="beta">beta array</param>
    /// <param name="movingMean">running mean of input</param>
    /// <param name="movingVar">running variance of input</param>
    /// <param name="eps">Epsilon to prevent div 0. Must be no less than CUDNN_BN_MIN_EPSILON defined in cudnn.h when using cudnn (usually 1e-5)</param>
    /// <param name="momentum">Momentum for moving average</param>
    /// <param name="fixGamma">Fix gamma while training</param>
    /// <param name="useGlobalStats">Whether use global moving statistics instead of local batch-norm. This will force change batch-norm into a scale shift operator.</param>
    /// <param name="outputMeanVar">Output the mean and inverse std </param>
    /// <param name="axis">Specify which shape axis the channel is specified</param>
    /// <param name="cudnnOff">Do not select CUDNN operator, if available</param>
    static member BatchNorm(data : NDArray, 
                            gamma : NDArray, 
                            beta : NDArray, 
                            movingMean : NDArray, 
                            movingVar : NDArray, 
                            eps : double, 
                            [<Optional; DefaultParameterValue(0.899999976)>] momentum : float, 
                            [<Optional; DefaultParameterValue(true)>] fixGamma : bool, 
                            [<Optional; DefaultParameterValue(false)>] useGlobalStats : bool, 
                            [<Optional; DefaultParameterValue(false)>] outputMeanVar : bool, 
                            [<Optional; DefaultParameterValue(1)>] axis : int, 
                            [<Optional; DefaultParameterValue(false)>] cudnnOff : bool) =
        let creator = AtomicSymbolCreator.FromName "BatchNorm"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; gamma.NDArrayHandle; beta.NDArrayHandle; movingMean.NDArrayHandle; movingVar.NDArrayHandle|]
                                                 [|"eps"; "momentum"; "fix_gamma"; "use_global_stats"; "output_mean_var"; "axis"; "cudnn_off"|]
                                                 [|eps.ToString(); momentum.ToString(); fixGamma.ToString(); useGlobalStats.ToString(); outputMeanVar.ToString(); axis.ToString(); cudnnOff.ToString()|]
        outputs

    /// <summary>Joins input arrays along a given axis.
    /// 
    /// .. note:: `Concat` is deprecated. Use `concat` instead.
    /// 
    /// The dimensions of the input arrays should be the same except the axis along
    /// which they will be concatenated.
    /// The dimension of the output array along the concatenated axis will be equal
    /// to the sum of the corresponding dimensions of the input arrays.
    /// 
    /// The storage type of ``concat`` output depends on storage types of inputs
    /// 
    /// - concat(csr, csr, ..., csr, dim=0) = csr
    /// - otherwise, ``concat`` generates output with default storage
    /// 
    /// Example::
    /// 
    ///    x = [[1,1],[2,2]]
    ///    y = [[3,3],[4,4],[5,5]]
    ///    z = [[6,6], [7,7],[8,8]]
    /// 
    ///    concat(x,y,z,dim=0) = [[ 1.,  1.],
    ///                           [ 2.,  2.],
    ///                           [ 3.,  3.],
    ///                           [ 4.,  4.],
    ///                           [ 5.,  5.],
    ///                           [ 6.,  6.],
    ///                           [ 7.,  7.],
    ///                           [ 8.,  8.]]
    /// 
    ///    Note that you cannot concat x,y,z along dimension 1 since dimension
    ///    0 is not the same for all the input arrays.
    /// 
    ///    concat(y,z,dim=1) = [[ 3.,  3.,  6.,  6.],
    ///                          [ 4.,  4.,  7.,  7.],
    ///                          [ 5.,  5.,  8.,  8.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\concat.cc:L371</summary>
    /// <param name="data">List of arrays to concatenate</param>
    /// <param name="numArgs">Number of inputs to be concated.</param>
    /// <param name="dim">the dimension to be concated.</param>
    static member Concat([<ParamArray>] data : NDArray[], numArgs : int, [<Optional; DefaultParameterValue(1)>] dim : int) =
        let creator = AtomicSymbolCreator.FromName "Concat"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 [|"num_args"; "dim"|]
                                                 [|numArgs.ToString(); dim.ToString()|]
        outputs

    /// <param name="data">List of arrays to concatenate</param>
    /// <param name="numArgs">Number of inputs to be concated.</param>
    /// <param name="dim">the dimension to be concated.</param>
    static member RnnParamConcat([<ParamArray>] data : NDArray[], numArgs : int, [<Optional; DefaultParameterValue(1)>] dim : int) =
        let creator = AtomicSymbolCreator.FromName "_rnn_param_concat"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 [|"num_args"; "dim"|]
                                                 [|numArgs.ToString(); dim.ToString()|]
        outputs

    /// <summary>Compute *N*-D convolution on *(N+2)*-D input.
    /// 
    /// In the 2-D convolution, given input data with shape *(batch_size,
    /// channel, height, width)*, the output is computed by
    /// 
    /// .. math::
    /// 
    ///    out[n,i,:,:] = bias[i] + \sum_{j=0}^{channel} data[n,j,:,:] \star
    ///    weight[i,j,:,:]
    /// 
    /// where :math:`\star` is the 2-D cross-correlation operator.
    /// 
    /// For general 2-D convolution, the shapes are
    /// 
    /// - **data**: *(batch_size, channel, height, width)*
    /// - **weight**: *(num_filter, channel, kernel[0], kernel[1])*
    /// - **bias**: *(num_filter,)*
    /// - **out**: *(batch_size, num_filter, out_height, out_width)*.
    /// 
    /// Define::
    /// 
    ///   f(x,k,p,s,d) = floor((x+2*p-d*(k-1)-1)/s)+1
    /// 
    /// then we have::
    /// 
    ///   out_height=f(height, kernel[0], pad[0], stride[0], dilate[0])
    ///   out_width=f(width, kernel[1], pad[1], stride[1], dilate[1])
    /// 
    /// If ``no_bias`` is set to be true, then the ``bias`` term is ignored.
    /// 
    /// The default data ``layout`` is *NCHW*, namely *(batch_size, channel, height,
    /// width)*. We can choose other layouts such as *NWC*.
    /// 
    /// If ``num_group`` is larger than 1, denoted by *g*, then split the input ``data``
    /// evenly into *g* parts along the channel axis, and also evenly split ``weight``
    /// along the first dimension. Next compute the convolution on the *i*-th part of
    /// the data with the *i*-th weight part. The output is obtained by concatenating all
    /// the *g* results.
    /// 
    /// 1-D convolution does not have *height* dimension but only *width* in space.
    /// 
    /// - **data**: *(batch_size, channel, width)*
    /// - **weight**: *(num_filter, channel, kernel[0])*
    /// - **bias**: *(num_filter,)*
    /// - **out**: *(batch_size, num_filter, out_width)*.
    /// 
    /// 3-D convolution adds an additional *depth* dimension besides *height* and
    /// *width*. The shapes are
    /// 
    /// - **data**: *(batch_size, channel, depth, height, width)*
    /// - **weight**: *(num_filter, channel, kernel[0], kernel[1], kernel[2])*
    /// - **bias**: *(num_filter,)*
    /// - **out**: *(batch_size, num_filter, out_depth, out_height, out_width)*.
    /// 
    /// Both ``weight`` and ``bias`` are learnable parameters.
    /// 
    /// There are other options to tune the performance.
    /// 
    /// - **cudnn_tune**: enable this option leads to higher startup time but may give
    ///   faster speed. Options are
    /// 
    ///   - **off**: no tuning
    ///   - **limited_workspace**:run test and pick the fastest algorithm that doesn&#39;t
    ///     exceed workspace limit.
    ///   - **fastest**: pick the fastest algorithm and ignore workspace limit.
    ///   - **None** (default): the behavior is determined by environment variable
    ///     ``MXNET_CUDNN_AUTOTUNE_DEFAULT``. 0 for off, 1 for limited workspace
    ///     (default), 2 for fastest.
    /// 
    /// - **workspace**: A large number leads to more (GPU) memory usage but may improve
    ///   the performance.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\convolution.cc:L472</summary>
    /// <param name="data">Input data to the ConvolutionOp.</param>
    /// <param name="weight">Weight matrix.</param>
    /// <param name="bias">Bias parameter.</param>
    /// <param name="kernel">Convolution kernel size: (w,), (h, w) or (d, h, w)</param>
    /// <param name="stride">Convolution stride: (w,), (h, w) or (d, h, w). Defaults to 1 for each dimension.</param>
    /// <param name="dilate">Convolution dilate: (w,), (h, w) or (d, h, w). Defaults to 1 for each dimension.</param>
    /// <param name="pad">Zero pad for convolution: (w,), (h, w) or (d, h, w). Defaults to no padding.</param>
    /// <param name="numFilter">Convolution filter(channel) number</param>
    /// <param name="numGroup">Number of group partitions.</param>
    /// <param name="workspace">Maximum temporary workspace allowed (MB) in convolution.This parameter has two usages. When CUDNN is not used, it determines the effective batch size of the convolution kernel. When CUDNN is used, it controls the maximum temporary storage used for tuning the best CUDNN kernel when `limited_workspace` strategy is used.</param>
    /// <param name="noBias">Whether to disable bias parameter.</param>
    /// <param name="cudnnTune">Whether to pick convolution algo by running performance test.</param>
    /// <param name="cudnnOff">Turn off cudnn for this layer.</param>
    /// <param name="layout">Set layout for input, output and weight. Empty for
    ///     default layout: NCW for 1d, NCHW for 2d and NCDHW for 3d.NHWC and NDHWC are only supported on GPU.</param>
    static member Convolution(data : NDArray, 
                              weight : NDArray, 
                              bias : NDArray, 
                              kernel : int seq, 
                              [<Optional>] stride : int seq, 
                              [<Optional>] dilate : int seq, 
                              [<Optional>] pad : int seq, 
                              numFilter : int, 
                              numGroup : int, 
                              workspace : int64, 
                              [<Optional; DefaultParameterValue(false)>] noBias : bool, 
                              [<Optional>] cudnnTune : CudnnTune, 
                              [<Optional; DefaultParameterValue(false)>] cudnnOff : bool, 
                              [<Optional>] layout : ConvolutionLayout) =
        let creator = AtomicSymbolCreator.FromName "Convolution"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; weight.NDArrayHandle; bias.NDArrayHandle|]
                                                 [|"kernel"; "stride"; "dilate"; "pad"; "num_filter"; "num_group"; "workspace"; "no_bias"; "cudnn_tune"; "cudnn_off"; "layout"|]
                                                 [|kernel.ToString(); (if isNull (stride :> obj) then "[]" else stride.ToString()); (if isNull (dilate :> obj) then "[]" else dilate.ToString()); (if isNull (pad :> obj) then "[]" else pad.ToString()); numFilter.ToString(); numGroup.ToString(); workspace.ToString(); noBias.ToString(); (if isNull (cudnnTune :> obj) then "None" else cudnnTune.ToString()); cudnnOff.ToString(); (if isNull (layout :> obj) then "None" else layout.ToString())|]
        outputs

    /// <summary>Connectionist Temporal Classification Loss.
    /// 
    /// .. note:: The existing alias ``contrib_CTCLoss`` is deprecated.
    /// 
    /// The shapes of the inputs and outputs:
    /// 
    /// - **data**: `(sequence_length, batch_size, alphabet_size)`
    /// - **label**: `(batch_size, label_sequence_length)`
    /// - **out**: `(batch_size)`
    /// 
    /// The `data` tensor consists of sequences of activation vectors (without applying softmax),
    /// with i-th channel in the last dimension corresponding to i-th label
    /// for i between 0 and alphabet_size-1 (i.e always 0-indexed).
    /// Alphabet size should include one additional value reserved for blank label.
    /// When `blank_label` is ``&quot;first&quot;``, the ``0``-th channel is be reserved for
    /// activation of blank label, or otherwise if it is &quot;last&quot;, ``(alphabet_size-1)``-th channel should be
    /// reserved for blank label.
    /// 
    /// ``label`` is an index matrix of integers. When `blank_label` is ``&quot;first&quot;``,
    /// the value 0 is then reserved for blank label, and should not be passed in this matrix. Otherwise,
    /// when `blank_label` is ``&quot;last&quot;``, the value `(alphabet_size-1)` is reserved for blank label.
    /// 
    /// If a sequence of labels is shorter than *label_sequence_length*, use the special
    /// padding value at the end of the sequence to conform it to the correct
    /// length. The padding value is `0` when `blank_label` is ``&quot;first&quot;``, and `-1` otherwise.
    /// 
    /// For example, suppose the vocabulary is `[a, b, c]`, and in one batch we have three sequences
    /// &#39;ba&#39;, &#39;cbb&#39;, and &#39;abac&#39;. When `blank_label` is ``&quot;first&quot;``, we can index the labels as
    /// `{&#39;a&#39;: 1, &#39;b&#39;: 2, &#39;c&#39;: 3}`, and we reserve the 0-th channel for blank label in data tensor.
    /// The resulting `label` tensor should be padded to be::
    /// 
    ///   [[2, 1, 0, 0], [3, 2, 2, 0], [1, 2, 1, 3]]
    /// 
    /// When `blank_label` is ``&quot;last&quot;``, we can index the labels as
    /// `{&#39;a&#39;: 0, &#39;b&#39;: 1, &#39;c&#39;: 2}`, and we reserve the channel index 3 for blank label in data tensor.
    /// The resulting `label` tensor should be padded to be::
    /// 
    ///   [[1, 0, -1, -1], [2, 1, 1, -1], [0, 1, 0, 2]]
    /// 
    /// ``out`` is a list of CTC loss values, one per example in the batch.
    /// 
    /// See *Connectionist Temporal Classification: Labelling Unsegmented
    /// Sequence Data with Recurrent Neural Networks*, A. Graves *et al*. for more
    /// information on the definition and the algorithm.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\ctc_loss.cc:L100</summary>
    /// <param name="data">Input ndarray</param>
    /// <param name="label">Ground-truth labels for the loss.</param>
    /// <param name="dataLengths">Lengths of data for each of the samples. Only required when use_data_lengths is true.</param>
    /// <param name="labelLengths">Lengths of labels for each of the samples. Only required when use_label_lengths is true.</param>
    /// <param name="useDataLengths">Whether the data lenghts are decided by `data_lengths`. If false, the lengths are equal to the max sequence length.</param>
    /// <param name="useLabelLengths">Whether the label lenghts are decided by `label_lengths`, or derived from `padding_mask`. If false, the lengths are derived from the first occurrence of the value of `padding_mask`. The value of `padding_mask` is ``0`` when first CTC label is reserved for blank, and ``-1`` when last label is reserved for blank. See `blank_label`.</param>
    /// <param name="blankLabel">Set the label that is reserved for blank label.If &quot;first&quot;, 0-th label is reserved, and label values for tokens in the vocabulary are between ``1`` and ``alphabet_size-1``, and the padding mask is ``-1``. If &quot;last&quot;, last label value ``alphabet_size-1`` is reserved for blank label instead, and label values for tokens in the vocabulary are between ``0`` and ``alphabet_size-2``, and the padding mask is ``0``.</param>
    static member CTCLoss(data : NDArray, 
                          label : NDArray, 
                          dataLengths : NDArray, 
                          labelLengths : NDArray, 
                          [<Optional; DefaultParameterValue(false)>] useDataLengths : bool, 
                          [<Optional; DefaultParameterValue(false)>] useLabelLengths : bool, 
                          [<Optional>] blankLabel : BlankLabel) =
        let creator = AtomicSymbolCreator.FromName "CTCLoss"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; label.NDArrayHandle; dataLengths.NDArrayHandle; labelLengths.NDArrayHandle|]
                                                 [|"use_data_lengths"; "use_label_lengths"; "blank_label"|]
                                                 [|useDataLengths.ToString(); useLabelLengths.ToString(); (if isNull (blankLabel :> obj) then "first" else blankLabel.ToString())|]
        outputs

    /// <summary>Computes 1D or 2D transposed convolution (aka fractionally strided convolution) of the input tensor. This operation can be seen as the gradient of Convolution operation with respect to its input. Convolution usually reduces the size of the input. Transposed convolution works the other way, going from a smaller input to a larger output while preserving the connectivity pattern.</summary>
    /// <param name="data">Input tensor to the deconvolution operation.</param>
    /// <param name="weight">Weights representing the kernel.</param>
    /// <param name="bias">Bias added to the result after the deconvolution operation.</param>
    /// <param name="kernel">Deconvolution kernel size: (w,), (h, w) or (d, h, w). This is same as the kernel size used for the corresponding convolution</param>
    /// <param name="stride">The stride used for the corresponding convolution: (w,), (h, w) or (d, h, w). Defaults to 1 for each dimension.</param>
    /// <param name="dilate">Dilation factor for each dimension of the input: (w,), (h, w) or (d, h, w). Defaults to 1 for each dimension.</param>
    /// <param name="pad">The amount of implicit zero padding added during convolution for each dimension of the input: (w,), (h, w) or (d, h, w). ``(kernel-1)/2`` is usually a good choice. If `target_shape` is set, `pad` will be ignored and a padding that will generate the target shape will be used. Defaults to no padding.</param>
    /// <param name="adj">Adjustment for output shape: (w,), (h, w) or (d, h, w). If `target_shape` is set, `adj` will be ignored and computed accordingly.</param>
    /// <param name="targetShape">Shape of the output tensor: (w,), (h, w) or (d, h, w).</param>
    /// <param name="numFilter">Number of output filters.</param>
    /// <param name="numGroup">Number of groups partition.</param>
    /// <param name="workspace">Maximum temporary workspace allowed (MB) in deconvolution.This parameter has two usages. When CUDNN is not used, it determines the effective batch size of the deconvolution kernel. When CUDNN is used, it controls the maximum temporary storage used for tuning the best CUDNN kernel when `limited_workspace` strategy is used.</param>
    /// <param name="noBias">Whether to disable bias parameter.</param>
    /// <param name="cudnnTune">Whether to pick convolution algorithm by running performance test.</param>
    /// <param name="cudnnOff">Turn off cudnn for this layer.</param>
    /// <param name="layout">Set layout for input, output and weight. Empty for default layout, NCW for 1d, NCHW for 2d and NCDHW for 3d.NHWC and NDHWC are only supported on GPU.</param>
    static member Deconvolution(data : NDArray, 
                                weight : NDArray, 
                                bias : NDArray, 
                                kernel : int seq, 
                                [<Optional>] stride : int seq, 
                                [<Optional>] dilate : int seq, 
                                [<Optional>] pad : int seq, 
                                [<Optional>] adj : int seq, 
                                [<Optional>] targetShape : int seq, 
                                numFilter : int, 
                                numGroup : int, 
                                workspace : int64, 
                                [<Optional; DefaultParameterValue(true)>] noBias : bool, 
                                [<Optional>] cudnnTune : CudnnTune, 
                                [<Optional; DefaultParameterValue(false)>] cudnnOff : bool, 
                                [<Optional>] layout : DeconvolutionLayout) =
        let creator = AtomicSymbolCreator.FromName "Deconvolution"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; weight.NDArrayHandle; bias.NDArrayHandle|]
                                                 [|"kernel"; "stride"; "dilate"; "pad"; "adj"; "target_shape"; "num_filter"; "num_group"; "workspace"; "no_bias"; "cudnn_tune"; "cudnn_off"; "layout"|]
                                                 [|kernel.ToString(); (if isNull (stride :> obj) then "[]" else stride.ToString()); (if isNull (dilate :> obj) then "[]" else dilate.ToString()); (if isNull (pad :> obj) then "[]" else pad.ToString()); (if isNull (adj :> obj) then "[]" else adj.ToString()); (if isNull (targetShape :> obj) then "[]" else targetShape.ToString()); numFilter.ToString(); numGroup.ToString(); workspace.ToString(); noBias.ToString(); (if isNull (cudnnTune :> obj) then "None" else cudnnTune.ToString()); cudnnOff.ToString(); (if isNull (layout :> obj) then "None" else layout.ToString())|]
        outputs

    /// <summary>Applies dropout operation to input array.
    /// 
    /// - During training, each element of the input is set to zero with probability p.
    ///   The whole array is rescaled by :math:`1/(1-p)` to keep the expected
    ///   sum of the input unchanged.
    /// 
    /// - During testing, this operator does not change the input if mode is &#39;training&#39;.
    ///   If mode is &#39;always&#39;, the same computaion as during training will be applied.
    /// 
    /// Example::
    /// 
    ///   random.seed(998)
    ///   input_array = array([[3., 0.5,  -0.5,  2., 7.],
    ///                       [2., -0.4,   7.,  3., 0.2]])
    ///   a = symbol.Variable(&#39;a&#39;)
    ///   dropout = symbol.Dropout(a, p = 0.2)
    ///   executor = dropout.simple_bind(a = input_array.shape)
    /// 
    ///   ## If training
    ///   executor.forward(is_train = True, a = input_array)
    ///   executor.outputs
    ///   [[ 3.75   0.625 -0.     2.5    8.75 ]
    ///    [ 2.5   -0.5    8.75   3.75   0.   ]]
    /// 
    ///   ## If testing
    ///   executor.forward(is_train = False, a = input_array)
    ///   executor.outputs
    ///   [[ 3.     0.5   -0.5    2.     7.   ]
    ///    [ 2.    -0.4    7.     3.     0.2  ]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\dropout.cc:L97</summary>
    /// <param name="data">Input array to which dropout will be applied.</param>
    /// <param name="p">Fraction of the input that gets dropped out during training time.</param>
    /// <param name="mode">Whether to only turn on dropout during training or to also turn on for inference.</param>
    /// <param name="axes">Axes for variational dropout kernel.</param>
    /// <param name="cudnnOff">Whether to turn off cudnn in dropout operator. This option is ignored if axes is specified.</param>
    static member Dropout(data : NDArray, 
                          [<Optional; DefaultParameterValue(0.5)>] p : float, 
                          [<Optional>] mode : DropoutMode, 
                          [<Optional>] axes : int seq, 
                          [<Optional>] cudnnOff : bool Nullable) =
        let creator = AtomicSymbolCreator.FromName "Dropout"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"p"; "mode"; "axes"; "cudnn_off"|]
                                                 [|p.ToString(); (if isNull (mode :> obj) then "training" else mode.ToString()); (if isNull (axes :> obj) then "[]" else axes.ToString()); cudnnOff.ToString()|]
        outputs

    /// <summary>Applies dropout operation to input array.
    /// 
    /// - During training, each element of the input is set to zero with probability p.
    ///   The whole array is rescaled by :math:`1/(1-p)` to keep the expected
    ///   sum of the input unchanged.
    /// 
    /// - During testing, this operator does not change the input if mode is &#39;training&#39;.
    ///   If mode is &#39;always&#39;, the same computaion as during training will be applied.
    /// 
    /// Example::
    /// 
    ///   random.seed(998)
    ///   input_array = array([[3., 0.5,  -0.5,  2., 7.],
    ///                       [2., -0.4,   7.,  3., 0.2]])
    ///   a = symbol.Variable(&#39;a&#39;)
    ///   dropout = symbol.Dropout(a, p = 0.2)
    ///   executor = dropout.simple_bind(a = input_array.shape)
    /// 
    ///   ## If training
    ///   executor.forward(is_train = True, a = input_array)
    ///   executor.outputs
    ///   [[ 3.75   0.625 -0.     2.5    8.75 ]
    ///    [ 2.5   -0.5    8.75   3.75   0.   ]]
    /// 
    ///   ## If testing
    ///   executor.forward(is_train = False, a = input_array)
    ///   executor.outputs
    ///   [[ 3.     0.5   -0.5    2.     7.   ]
    ///    [ 2.    -0.4    7.     3.     0.2  ]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\dropout.cc:L97</summary>
    /// <param name="data">Input array to which dropout will be applied.</param>
    /// <param name="p">Fraction of the input that gets dropped out during training time.</param>
    /// <param name="mode">Whether to only turn on dropout during training or to also turn on for inference.</param>
    /// <param name="axes">Axes for variational dropout kernel.</param>
    /// <param name="cudnnOff">Whether to turn off cudnn in dropout operator. This option is ignored if axes is specified.</param>
    static member Dropout(data : NDArray, 
                          ?p : float, 
                          ?mode : DropoutMode, 
                          ?axes : int seq, 
                          ?cudnnOff : bool) =
        let creator = AtomicSymbolCreator.FromName "Dropout"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"p"; "mode"; "axes"; "cudnn_off"|]
                                                 [|(match p with None -> "0.5" | _ -> p.ToString()); (match mode with None -> "training" | _ -> mode.ToString()); (match axes with None -> "[]" | _ -> axes.ToString()); (match cudnnOff with None -> "None" | _ -> cudnnOff.ToString())|]
        outputs

    /// <summary>Applies a linear transformation: :math:`Y = XW^T + b`.
    /// 
    /// If ``flatten`` is set to be true, then the shapes are:
    /// 
    /// - **data**: `(batch_size, x1, x2, ..., xn)`
    /// - **weight**: `(num_hidden, x1 * x2 * ... * xn)`
    /// - **bias**: `(num_hidden,)`
    /// - **out**: `(batch_size, num_hidden)`
    /// 
    /// If ``flatten`` is set to be false, then the shapes are:
    /// 
    /// - **data**: `(x1, x2, ..., xn, input_dim)`
    /// - **weight**: `(num_hidden, input_dim)`
    /// - **bias**: `(num_hidden,)`
    /// - **out**: `(x1, x2, ..., xn, num_hidden)`
    /// 
    /// The learnable parameters include both ``weight`` and ``bias``.
    /// 
    /// If ``no_bias`` is set to be true, then the ``bias`` term is ignored.
    /// 
    /// .. Note::
    /// 
    ///     The sparse support for FullyConnected is limited to forward evaluation with `row_sparse`
    ///     weight and bias, where the length of `weight.indices` and `bias.indices` must be equal
    ///     to `num_hidden`. This could be useful for model inference with `row_sparse` weights
    ///     trained with importance sampling or noise contrastive estimation.
    /// 
    ///     To compute linear transformation with &#39;csr&#39; sparse data, sparse.dot is recommended instead
    ///     of sparse.FullyConnected.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\fully_connected.cc:L277</summary>
    /// <param name="data">Input data.</param>
    /// <param name="weight">Weight matrix.</param>
    /// <param name="bias">Bias parameter.</param>
    /// <param name="numHidden">Number of hidden nodes of the output.</param>
    /// <param name="noBias">Whether to disable bias parameter.</param>
    /// <param name="flatten">Whether to collapse all but the first axis of the input data tensor.</param>
    static member FullyConnected(data : NDArray, 
                                 weight : NDArray, 
                                 bias : NDArray, 
                                 numHidden : int, 
                                 [<Optional; DefaultParameterValue(false)>] noBias : bool, 
                                 [<Optional; DefaultParameterValue(true)>] flatten : bool) =
        let creator = AtomicSymbolCreator.FromName "FullyConnected"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; weight.NDArrayHandle; bias.NDArrayHandle|]
                                                 [|"num_hidden"; "no_bias"; "flatten"|]
                                                 [|numHidden.ToString(); noBias.ToString(); flatten.ToString()|]
        outputs

    /// <summary>Layer normalization.
    /// 
    /// Normalizes the channels of the input tensor by mean and variance, and applies a scale ``gamma`` as
    /// well as offset ``beta``.
    /// 
    /// Assume the input has more than one dimension and we normalize along axis 1.
    /// We first compute the mean and variance along this axis and then 
    /// compute the normalized output, which has the same shape as input, as following:
    /// 
    /// .. math::
    /// 
    ///   out = \frac{data - mean(data, axis)}{\sqrt{var(data, axis) + \epsilon}} * gamma + beta
    /// 
    /// Both ``gamma`` and ``beta`` are learnable parameters.
    /// 
    /// Unlike BatchNorm and InstanceNorm,  the *mean* and *var* are computed along the channel dimension.
    /// 
    /// Assume the input has size *k* on axis 1, then both ``gamma`` and ``beta``
    /// have shape *(k,)*. If ``output_mean_var`` is set to be true, then outputs both ``data_mean`` and
    /// ``data_std``. Note that no gradient will be passed through these two outputs.
    /// 
    /// The parameter ``axis`` specifies which axis of the input shape denotes
    /// the &#39;channel&#39; (separately normalized groups).  The default is -1, which sets the channel
    /// axis to be the last item in the input shape.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\layer_norm.cc:L155</summary>
    /// <param name="data">Input data to layer normalization</param>
    /// <param name="gamma">gamma array</param>
    /// <param name="beta">beta array</param>
    /// <param name="axis">The axis to perform layer normalization. Usually, this should be be axis of the channel dimension. Negative values means indexing from right to left.</param>
    /// <param name="eps">An `epsilon` parameter to prevent division by 0.</param>
    /// <param name="outputMeanVar">Output the mean and std calculated along the given axis.</param>
    static member LayerNorm(data : NDArray, 
                            gamma : NDArray, 
                            beta : NDArray, 
                            [<Optional; DefaultParameterValue(-1)>] axis : int, 
                            [<Optional; DefaultParameterValue(9.99999975E-06)>] eps : float, 
                            [<Optional; DefaultParameterValue(false)>] outputMeanVar : bool) =
        let creator = AtomicSymbolCreator.FromName "LayerNorm"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; gamma.NDArrayHandle; beta.NDArrayHandle|]
                                                 [|"axis"; "eps"; "output_mean_var"|]
                                                 [|axis.ToString(); eps.ToString(); outputMeanVar.ToString()|]
        outputs

    /// <summary>Applies local response normalization to the input.
    /// 
    /// The local response normalization layer performs &quot;lateral inhibition&quot; by normalizing
    /// over local input regions.
    /// 
    /// If :math:`a_{x,y}^{i}` is the activity of a neuron computed by applying kernel :math:`i` at position
    /// :math:`(x, y)` and then applying the ReLU nonlinearity, the response-normalized
    /// activity :math:`b_{x,y}^{i}` is given by the expression:
    /// 
    /// .. math::
    ///    b_{x,y}^{i} = \frac{a_{x,y}^{i}}{\Bigg({k + \frac{\alpha}{n} \sum_{j=max(0, i-\frac{n}{2})}^{min(N-1, i+\frac{n}{2})} (a_{x,y}^{j})^{2}}\Bigg)^{\beta}}
    /// 
    /// where the sum runs over :math:`n` &quot;adjacent&quot; kernel maps at the same spatial position, and :math:`N` is the total
    /// number of kernels in the layer.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\lrn.cc:L164</summary>
    /// <param name="data">Input data to LRN</param>
    /// <param name="alpha">The variance scaling parameter :math:`lpha` in the LRN expression.</param>
    /// <param name="beta">The power parameter :math:`eta` in the LRN expression.</param>
    /// <param name="knorm">The parameter :math:`k` in the LRN expression.</param>
    /// <param name="nsize">normalization window width in elements.</param>
    static member LRN(data : NDArray, 
                      [<Optional; DefaultParameterValue(9.99999975E-05)>] alpha : float, 
                      [<Optional; DefaultParameterValue(0.75)>] beta : float, 
                      [<Optional; DefaultParameterValue(2.0)>] knorm : float, 
                      nsize : int) =
        let creator = AtomicSymbolCreator.FromName "LRN"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"alpha"; "beta"; "knorm"; "nsize"|]
                                                 [|alpha.ToString(); beta.ToString(); knorm.ToString(); nsize.ToString()|]
        outputs

    /// <summary>
    /// Calculate the mean and variance of `data`.
    /// 
    /// The mean and variance are calculated by aggregating the contents of data across axes.
    /// If x is 1-D and axes = [0] this is just the mean and variance of a vector.
    /// 
    /// Example:
    /// 
    ///      x = [[1, 2, 3], [4, 5, 6]]
    ///      mean, var = moments(data=x, axes=[0])
    ///      mean = [2.5, 3.5, 4.5]
    ///      var = [2.25, 2.25, 2.25]
    ///      mean, var = moments(data=x, axes=[1])
    ///      mean = [2.0, 5.0]
    ///      var = [0.66666667, 0.66666667]
    ///      mean, var = moments(data=x, axis=[0, 1])
    ///      mean = [3.5]
    ///      var = [2.9166667]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\moments.cc:L54</summary>
    /// <param name="data">Input ndarray</param>
    /// <param name="axes">Array of ints. Axes along which to compute mean and variance.</param>
    /// <param name="keepdims">produce moments with the same dimensionality as the input.</param>
    static member Moments(data : NDArray, [<Optional>] axes : int seq, [<Optional; DefaultParameterValue(false)>] keepdims : bool) =
        let creator = AtomicSymbolCreator.FromName "moments"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axes"; "keepdims"|]
                                                 [|axes.ToString(); keepdims.ToString()|]
        outputs

    /// <summary>Performs pooling on the input.
    /// 
    /// The shapes for 1-D pooling are
    /// 
    /// - **data** and **out**: *(batch_size, channel, width)* (NCW layout) or
    ///   *(batch_size, width, channel)* (NWC layout),
    /// 
    /// The shapes for 2-D pooling are
    /// 
    /// - **data** and **out**: *(batch_size, channel, height, width)* (NCHW layout) or
    ///   *(batch_size, height, width, channel)* (NHWC layout),
    /// 
    ///     out_height = f(height, kernel[0], pad[0], stride[0])
    ///     out_width = f(width, kernel[1], pad[1], stride[1])
    /// 
    /// The definition of *f* depends on ``pooling_convention``, which has two options:
    /// 
    /// - **valid** (default)::
    /// 
    ///     f(x, k, p, s) = floor((x+2*p-k)/s)+1
    /// 
    /// - **full**, which is compatible with Caffe::
    /// 
    ///     f(x, k, p, s) = ceil((x+2*p-k)/s)+1
    /// 
    /// But ``global_pool`` is set to be true, then do a global pooling, namely reset
    /// ``kernel=(height, width)``.
    /// 
    /// Three pooling options are supported by ``pool_type``:
    /// 
    /// - **avg**: average pooling
    /// - **max**: max pooling
    /// - **sum**: sum pooling
    /// - **lp**: Lp pooling
    /// 
    /// For 3-D pooling, an additional *depth* dimension is added before
    /// *height*. Namely the input data and output will have shape *(batch_size, channel, depth,
    /// height, width)* (NCDHW layout) or *(batch_size, depth, height, width, channel)* (NDHWC layout).
    /// 
    /// Notes on Lp pooling:
    /// 
    /// Lp pooling was first introduced by this paper: https://arxiv.org/pdf/1204.3968.pdf.
    /// L-1 pooling is simply sum pooling, while L-inf pooling is simply max pooling.
    /// We can see that Lp pooling stands between those two, in practice the most common value for p is 2.
    /// 
    /// For each window ``X``, the mathematical expression for Lp pooling is:
    /// 
    /// :math:`f(X) = \sqrt[p]{\sum_{x}^{X} x^p}`
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\pooling.cc:L416</summary>
    /// <param name="data">Input data to the pooling operator.</param>
    /// <param name="kernel">Pooling kernel size: (y, x) or (d, y, x)</param>
    /// <param name="poolType">Pooling type to be applied.</param>
    /// <param name="globalPool">Ignore kernel size, do global pooling based on current input feature map. </param>
    /// <param name="cudnnOff">Turn off cudnn pooling and use MXNet pooling operator. </param>
    /// <param name="poolingConvention">Pooling convention to be applied.</param>
    /// <param name="stride">Stride: for pooling (y, x) or (d, y, x). Defaults to 1 for each dimension.</param>
    /// <param name="pad">Pad for pooling: (y, x) or (d, y, x). Defaults to no padding.</param>
    /// <param name="pValue">Value of p for Lp pooling, can be 1 or 2, required for Lp Pooling.</param>
    /// <param name="countIncludePad">Only used for AvgPool, specify whether to count padding elements for averagecalculation. For example, with a 5*5 kernel on a 3*3 corner of a image,the sum of the 9 valid elements will be divided by 25 if this is set to true,or it will be divided by 9 if this is set to false. Defaults to true.</param>
    /// <param name="layout">Set layout for input and output. Empty for
    ///     default layout: NCW for 1d, NCHW for 2d and NCDHW for 3d.</param>
    static member Pooling(data : NDArray, 
                          [<Optional>] kernel : int seq, 
                          [<Optional>] poolType : PoolType, 
                          [<Optional; DefaultParameterValue(false)>] globalPool : bool, 
                          [<Optional; DefaultParameterValue(false)>] cudnnOff : bool, 
                          [<Optional>] poolingConvention : PoolingConvention, 
                          [<Optional>] stride : int seq, 
                          [<Optional>] pad : int seq, 
                          [<Optional>] pValue : int Nullable, 
                          [<Optional>] countIncludePad : bool Nullable, 
                          [<Optional>] layout : PoolingLayout) =
        let creator = AtomicSymbolCreator.FromName "Pooling"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"kernel"; "pool_type"; "global_pool"; "cudnn_off"; "pooling_convention"; "stride"; "pad"; "p_value"; "count_include_pad"; "layout"|]
                                                 [|(if isNull (kernel :> obj) then "[]" else kernel.ToString()); (if isNull (poolType :> obj) then "max" else poolType.ToString()); globalPool.ToString(); cudnnOff.ToString(); (if isNull (poolingConvention :> obj) then "valid" else poolingConvention.ToString()); (if isNull (stride :> obj) then "[]" else stride.ToString()); (if isNull (pad :> obj) then "[]" else pad.ToString()); pValue.ToString(); countIncludePad.ToString(); (if isNull (layout :> obj) then "None" else layout.ToString())|]
        outputs

    /// <summary>Performs pooling on the input.
    /// 
    /// The shapes for 1-D pooling are
    /// 
    /// - **data** and **out**: *(batch_size, channel, width)* (NCW layout) or
    ///   *(batch_size, width, channel)* (NWC layout),
    /// 
    /// The shapes for 2-D pooling are
    /// 
    /// - **data** and **out**: *(batch_size, channel, height, width)* (NCHW layout) or
    ///   *(batch_size, height, width, channel)* (NHWC layout),
    /// 
    ///     out_height = f(height, kernel[0], pad[0], stride[0])
    ///     out_width = f(width, kernel[1], pad[1], stride[1])
    /// 
    /// The definition of *f* depends on ``pooling_convention``, which has two options:
    /// 
    /// - **valid** (default)::
    /// 
    ///     f(x, k, p, s) = floor((x+2*p-k)/s)+1
    /// 
    /// - **full**, which is compatible with Caffe::
    /// 
    ///     f(x, k, p, s) = ceil((x+2*p-k)/s)+1
    /// 
    /// But ``global_pool`` is set to be true, then do a global pooling, namely reset
    /// ``kernel=(height, width)``.
    /// 
    /// Three pooling options are supported by ``pool_type``:
    /// 
    /// - **avg**: average pooling
    /// - **max**: max pooling
    /// - **sum**: sum pooling
    /// - **lp**: Lp pooling
    /// 
    /// For 3-D pooling, an additional *depth* dimension is added before
    /// *height*. Namely the input data and output will have shape *(batch_size, channel, depth,
    /// height, width)* (NCDHW layout) or *(batch_size, depth, height, width, channel)* (NDHWC layout).
    /// 
    /// Notes on Lp pooling:
    /// 
    /// Lp pooling was first introduced by this paper: https://arxiv.org/pdf/1204.3968.pdf.
    /// L-1 pooling is simply sum pooling, while L-inf pooling is simply max pooling.
    /// We can see that Lp pooling stands between those two, in practice the most common value for p is 2.
    /// 
    /// For each window ``X``, the mathematical expression for Lp pooling is:
    /// 
    /// :math:`f(X) = \sqrt[p]{\sum_{x}^{X} x^p}`
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\pooling.cc:L416</summary>
    /// <param name="data">Input data to the pooling operator.</param>
    /// <param name="kernel">Pooling kernel size: (y, x) or (d, y, x)</param>
    /// <param name="poolType">Pooling type to be applied.</param>
    /// <param name="globalPool">Ignore kernel size, do global pooling based on current input feature map. </param>
    /// <param name="cudnnOff">Turn off cudnn pooling and use MXNet pooling operator. </param>
    /// <param name="poolingConvention">Pooling convention to be applied.</param>
    /// <param name="stride">Stride: for pooling (y, x) or (d, y, x). Defaults to 1 for each dimension.</param>
    /// <param name="pad">Pad for pooling: (y, x) or (d, y, x). Defaults to no padding.</param>
    /// <param name="pValue">Value of p for Lp pooling, can be 1 or 2, required for Lp Pooling.</param>
    /// <param name="countIncludePad">Only used for AvgPool, specify whether to count padding elements for averagecalculation. For example, with a 5*5 kernel on a 3*3 corner of a image,the sum of the 9 valid elements will be divided by 25 if this is set to true,or it will be divided by 9 if this is set to false. Defaults to true.</param>
    /// <param name="layout">Set layout for input and output. Empty for
    ///     default layout: NCW for 1d, NCHW for 2d and NCDHW for 3d.</param>
    static member Pooling(data : NDArray, 
                          ?kernel : int seq, 
                          ?poolType : PoolType, 
                          ?globalPool : bool, 
                          ?cudnnOff : bool, 
                          ?poolingConvention : PoolingConvention, 
                          ?stride : int seq, 
                          ?pad : int seq, 
                          ?pValue : int, 
                          ?countIncludePad : bool, 
                          ?layout : PoolingLayout) =
        let creator = AtomicSymbolCreator.FromName "Pooling"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"kernel"; "pool_type"; "global_pool"; "cudnn_off"; "pooling_convention"; "stride"; "pad"; "p_value"; "count_include_pad"; "layout"|]
                                                 [|(match kernel with None -> "[]" | _ -> kernel.ToString()); (match poolType with None -> "max" | _ -> poolType.ToString()); (match globalPool with None -> "false" | _ -> globalPool.ToString()); (match cudnnOff with None -> "false" | _ -> cudnnOff.ToString()); (match poolingConvention with None -> "valid" | _ -> poolingConvention.ToString()); (match stride with None -> "[]" | _ -> stride.ToString()); (match pad with None -> "[]" | _ -> pad.ToString()); (match pValue with None -> "None" | _ -> pValue.ToString()); (match countIncludePad with None -> "None" | _ -> countIncludePad.ToString()); (match layout with None -> "None" | _ -> layout.ToString())|]
        outputs

    /// <summary>Applies the softmax function.
    /// 
    /// The resulting array contains elements in the range (0,1) and the elements along the given axis sum up to 1.
    /// 
    /// .. math::
    ///    softmax(\mathbf{z/t})_j = \frac{e^{z_j/t}}{\sum_{k=1}^K e^{z_k/t}}
    /// 
    /// for :math:`j = 1, ..., K`
    /// 
    /// t is the temperature parameter in softmax function. By default, t equals 1.0
    /// 
    /// Example::
    /// 
    ///   x = [[ 1.  1.  1.]
    ///        [ 1.  1.  1.]]
    /// 
    ///   softmax(x,axis=0) = [[ 0.5  0.5  0.5]
    ///                        [ 0.5  0.5  0.5]]
    /// 
    ///   softmax(x,axis=1) = [[ 0.33333334,  0.33333334,  0.33333334],
    ///                        [ 0.33333334,  0.33333334,  0.33333334]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\softmax.cc:L93</summary>
    /// <param name="data">The input array.</param>
    /// <param name="axis">The axis along which to compute softmax.</param>
    /// <param name="temperature">Temperature parameter in softmax</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to the same as input&#39;s dtype if not defined (dtype=None).</param>
    static member Softmax(data : NDArray, [<Optional; DefaultParameterValue(-1)>] axis : int, [<Optional>] temperature : float Nullable, [<Optional>] dtype : SoftmaxDtype) =
        let creator = AtomicSymbolCreator.FromName "softmax"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "temperature"; "dtype"|]
                                                 [|axis.ToString(); temperature.ToString(); (if isNull (dtype :> obj) then "None" else dtype.ToString())|]
        outputs

    /// <summary>Applies the softmax function.
    /// 
    /// The resulting array contains elements in the range (0,1) and the elements along the given axis sum up to 1.
    /// 
    /// .. math::
    ///    softmax(\mathbf{z/t})_j = \frac{e^{z_j/t}}{\sum_{k=1}^K e^{z_k/t}}
    /// 
    /// for :math:`j = 1, ..., K`
    /// 
    /// t is the temperature parameter in softmax function. By default, t equals 1.0
    /// 
    /// Example::
    /// 
    ///   x = [[ 1.  1.  1.]
    ///        [ 1.  1.  1.]]
    /// 
    ///   softmax(x,axis=0) = [[ 0.5  0.5  0.5]
    ///                        [ 0.5  0.5  0.5]]
    /// 
    ///   softmax(x,axis=1) = [[ 0.33333334,  0.33333334,  0.33333334],
    ///                        [ 0.33333334,  0.33333334,  0.33333334]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\softmax.cc:L93</summary>
    /// <param name="data">The input array.</param>
    /// <param name="axis">The axis along which to compute softmax.</param>
    /// <param name="temperature">Temperature parameter in softmax</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to the same as input&#39;s dtype if not defined (dtype=None).</param>
    static member Softmax(data : NDArray, ?axis : int, ?temperature : float, ?dtype : SoftmaxDtype) =
        let creator = AtomicSymbolCreator.FromName "softmax"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "temperature"; "dtype"|]
                                                 [|(match axis with None -> "-1" | _ -> axis.ToString()); (match temperature with None -> "None" | _ -> temperature.ToString()); (match dtype with None -> "None" | _ -> dtype.ToString())|]
        outputs

    /// <param name="args">Positional input arguments</param>
    static member BackwardSoftmax([<ParamArray>] args : NDArray[]) =
        let creator = AtomicSymbolCreator.FromName "_backward_softmax"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Applies the softmin function.
    /// 
    /// The resulting array contains elements in the range (0,1) and the elements along the given axis sum
    /// up to 1.
    /// 
    /// .. math::
    ///    softmin(\mathbf{z/t})_j = \frac{e^{-z_j/t}}{\sum_{k=1}^K e^{-z_k/t}}
    /// 
    /// for :math:`j = 1, ..., K`
    /// 
    /// t is the temperature parameter in softmax function. By default, t equals 1.0
    /// 
    /// Example::
    /// 
    ///   x = [[ 1.  2.  3.]
    ///        [ 3.  2.  1.]]
    /// 
    ///   softmin(x,axis=0) = [[ 0.88079703,  0.5,  0.11920292],
    ///                        [ 0.11920292,  0.5,  0.88079703]]
    /// 
    ///   softmin(x,axis=1) = [[ 0.66524094,  0.24472848,  0.09003057],
    ///                        [ 0.09003057,  0.24472848,  0.66524094]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\softmax.cc:L153</summary>
    /// <param name="data">The input array.</param>
    /// <param name="axis">The axis along which to compute softmax.</param>
    /// <param name="temperature">Temperature parameter in softmax</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to the same as input&#39;s dtype if not defined (dtype=None).</param>
    static member Softmin(data : NDArray, [<Optional; DefaultParameterValue(-1)>] axis : int, [<Optional>] temperature : float Nullable, [<Optional>] dtype : SoftminDtype) =
        let creator = AtomicSymbolCreator.FromName "softmin"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "temperature"; "dtype"|]
                                                 [|axis.ToString(); temperature.ToString(); (if isNull (dtype :> obj) then "None" else dtype.ToString())|]
        outputs

    /// <summary>Applies the softmin function.
    /// 
    /// The resulting array contains elements in the range (0,1) and the elements along the given axis sum
    /// up to 1.
    /// 
    /// .. math::
    ///    softmin(\mathbf{z/t})_j = \frac{e^{-z_j/t}}{\sum_{k=1}^K e^{-z_k/t}}
    /// 
    /// for :math:`j = 1, ..., K`
    /// 
    /// t is the temperature parameter in softmax function. By default, t equals 1.0
    /// 
    /// Example::
    /// 
    ///   x = [[ 1.  2.  3.]
    ///        [ 3.  2.  1.]]
    /// 
    ///   softmin(x,axis=0) = [[ 0.88079703,  0.5,  0.11920292],
    ///                        [ 0.11920292,  0.5,  0.88079703]]
    /// 
    ///   softmin(x,axis=1) = [[ 0.66524094,  0.24472848,  0.09003057],
    ///                        [ 0.09003057,  0.24472848,  0.66524094]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\softmax.cc:L153</summary>
    /// <param name="data">The input array.</param>
    /// <param name="axis">The axis along which to compute softmax.</param>
    /// <param name="temperature">Temperature parameter in softmax</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to the same as input&#39;s dtype if not defined (dtype=None).</param>
    static member Softmin(data : NDArray, ?axis : int, ?temperature : float, ?dtype : SoftminDtype) =
        let creator = AtomicSymbolCreator.FromName "softmin"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "temperature"; "dtype"|]
                                                 [|(match axis with None -> "-1" | _ -> axis.ToString()); (match temperature with None -> "None" | _ -> temperature.ToString()); (match dtype with None -> "None" | _ -> dtype.ToString())|]
        outputs

    /// <param name="args">Positional input arguments</param>
    static member BackwardSoftmin([<ParamArray>] args : NDArray[]) =
        let creator = AtomicSymbolCreator.FromName "_backward_softmin"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Computes the log softmax of the input.
    /// This is equivalent to computing softmax followed by log.
    /// 
    /// Examples::
    /// 
    ///   &gt;&gt;&gt; x = mx.nd.array([1, 2, .1])
    ///   &gt;&gt;&gt; mx.nd.log_softmax(x).asnumpy()
    ///   array([-1.41702998, -0.41702995, -2.31702995], dtype=float32)
    /// 
    ///   &gt;&gt;&gt; x = mx.nd.array( [[1, 2, .1],[.1, 2, 1]] )
    ///   &gt;&gt;&gt; mx.nd.log_softmax(x, axis=0).asnumpy()
    ///   array([[-0.34115392, -0.69314718, -1.24115396],
    ///          [-1.24115396, -0.69314718, -0.34115392]], dtype=float32)
    /// 
    /// 
    /// </summary>
    /// <param name="data">The input array.</param>
    /// <param name="axis">The axis along which to compute softmax.</param>
    /// <param name="temperature">Temperature parameter in softmax</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to the same as input&#39;s dtype if not defined (dtype=None).</param>
    static member LogSoftmax(data : NDArray, [<Optional; DefaultParameterValue(-1)>] axis : int, [<Optional>] temperature : float Nullable, [<Optional>] dtype : LogSoftmaxDtype) =
        let creator = AtomicSymbolCreator.FromName "log_softmax"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "temperature"; "dtype"|]
                                                 [|axis.ToString(); temperature.ToString(); (if isNull (dtype :> obj) then "None" else dtype.ToString())|]
        outputs

    /// <summary>Computes the log softmax of the input.
    /// This is equivalent to computing softmax followed by log.
    /// 
    /// Examples::
    /// 
    ///   &gt;&gt;&gt; x = mx.nd.array([1, 2, .1])
    ///   &gt;&gt;&gt; mx.nd.log_softmax(x).asnumpy()
    ///   array([-1.41702998, -0.41702995, -2.31702995], dtype=float32)
    /// 
    ///   &gt;&gt;&gt; x = mx.nd.array( [[1, 2, .1],[.1, 2, 1]] )
    ///   &gt;&gt;&gt; mx.nd.log_softmax(x, axis=0).asnumpy()
    ///   array([[-0.34115392, -0.69314718, -1.24115396],
    ///          [-1.24115396, -0.69314718, -0.34115392]], dtype=float32)
    /// 
    /// 
    /// </summary>
    /// <param name="data">The input array.</param>
    /// <param name="axis">The axis along which to compute softmax.</param>
    /// <param name="temperature">Temperature parameter in softmax</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to the same as input&#39;s dtype if not defined (dtype=None).</param>
    static member LogSoftmax(data : NDArray, ?axis : int, ?temperature : float, ?dtype : LogSoftmaxDtype) =
        let creator = AtomicSymbolCreator.FromName "log_softmax"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "temperature"; "dtype"|]
                                                 [|(match axis with None -> "-1" | _ -> axis.ToString()); (match temperature with None -> "None" | _ -> temperature.ToString()); (match dtype with None -> "None" | _ -> dtype.ToString())|]
        outputs

    /// <param name="args">Positional input arguments</param>
    static member BackwardLogSoftmax([<ParamArray>] args : NDArray[]) =
        let creator = AtomicSymbolCreator.FromName "_backward_log_softmax"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Applies softmax activation to input. This is intended for internal layers.
    /// 
    /// .. note::
    /// 
    ///   This operator has been deprecated, please use `softmax`.
    /// 
    /// If `mode` = ``instance``, this operator will compute a softmax for each instance in the batch.
    /// This is the default mode.
    /// 
    /// If `mode` = ``channel``, this operator will compute a k-class softmax at each position
    /// of each instance, where `k` = ``num_channel``. This mode can only be used when the input array
    /// has at least 3 dimensions.
    /// This can be used for `fully convolutional network`, `image segmentation`, etc.
    /// 
    /// Example::
    /// 
    ///   &gt;&gt;&gt; input_array = mx.nd.array([[3., 0.5, -0.5, 2., 7.],
    ///   &gt;&gt;&gt;                            [2., -.4, 7.,   3., 0.2]])
    ///   &gt;&gt;&gt; softmax_act = mx.nd.SoftmaxActivation(input_array)
    ///   &gt;&gt;&gt; print softmax_act.asnumpy()
    ///   [[  1.78322066e-02   1.46375655e-03   5.38485940e-04   6.56010211e-03   9.73605454e-01]
    ///    [  6.56221947e-03   5.95310994e-04   9.73919690e-01   1.78379621e-02   1.08472735e-03]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\softmax_activation.cc:L59</summary>
    /// <param name="data">The input array.</param>
    /// <param name="mode">Specifies how to compute the softmax. If set to ``instance``, it computes softmax for each instance. If set to ``channel``, It computes cross channel softmax for each position of each instance.</param>
    static member SoftmaxActivation(data : NDArray, [<Optional>] mode : SoftmaxActivationMode) =
        let creator = AtomicSymbolCreator.FromName "SoftmaxActivation"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"mode"|]
                                                 [|(if isNull (mode :> obj) then "instance" else mode.ToString())|]
        outputs

    /// <summary>Upsamples the given input data.
    /// 
    /// Two algorithms (``sample_type``) are available for upsampling:
    /// 
    /// - Nearest Neighbor
    /// - Bilinear
    /// 
    /// **Nearest Neighbor Upsampling**
    /// 
    /// Input data is expected to be NCHW.
    /// 
    /// Example::
    /// 
    ///   x = [[[[1. 1. 1.]
    ///          [1. 1. 1.]
    ///          [1. 1. 1.]]]]
    /// 
    ///   UpSampling(x, scale=2, sample_type=&#39;nearest&#39;) = [[[[1. 1. 1. 1. 1. 1.]
    ///                                                      [1. 1. 1. 1. 1. 1.]
    ///                                                      [1. 1. 1. 1. 1. 1.]
    ///                                                      [1. 1. 1. 1. 1. 1.]
    ///                                                      [1. 1. 1. 1. 1. 1.]
    ///                                                      [1. 1. 1. 1. 1. 1.]]]]
    /// 
    /// **Bilinear Upsampling**
    /// 
    /// Uses `deconvolution` algorithm under the hood. You need provide both input data and the kernel.
    /// 
    /// Input data is expected to be NCHW.
    /// 
    /// `num_filter` is expected to be same as the number of channels.
    /// 
    /// Example::
    /// 
    ///   x = [[[[1. 1. 1.]
    ///          [1. 1. 1.]
    ///          [1. 1. 1.]]]]
    /// 
    ///   w = [[[[1. 1. 1. 1.]
    ///          [1. 1. 1. 1.]
    ///          [1. 1. 1. 1.]
    ///          [1. 1. 1. 1.]]]]
    ///   
    ///   UpSampling(x, w, scale=2, sample_type=&#39;bilinear&#39;, num_filter=1) = [[[[1. 2. 2. 2. 2. 1.]
    ///                                                                        [2. 4. 4. 4. 4. 2.]
    ///                                                                        [2. 4. 4. 4. 4. 2.]
    ///                                                                        [2. 4. 4. 4. 4. 2.]
    ///                                                                        [2. 4. 4. 4. 4. 2.]
    ///                                                                        [1. 2. 2. 2. 2. 1.]]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\nn\upsampling.cc:L173</summary>
    /// <param name="data">Array of tensors to upsample. For bilinear upsampling, there should be 2 inputs - 1 data and 1 weight.</param>
    /// <param name="scale">Up sampling scale</param>
    /// <param name="numFilter">Input filter. Only used by bilinear sample_type.Since bilinear upsampling uses deconvolution, num_filters is set to the number of channels.</param>
    /// <param name="sampleType">upsampling method</param>
    /// <param name="multiInputMode">How to handle multiple input. concat means concatenate upsampled images along the channel dimension. sum means add all images together, only available for nearest neighbor upsampling.</param>
    /// <param name="numArgs">Number of inputs to be upsampled. For nearest neighbor upsampling, this can be 1-N; the size of output will be(scale*h_0,scale*w_0) and all other inputs will be upsampled to thesame size. For bilinear upsampling this must be 2; 1 input and 1 weight.</param>
    /// <param name="workspace">Tmp workspace for deconvolution (MB)</param>
    static member UpSampling([<ParamArray>] data : NDArray[], 
                             scale : int, 
                             [<Optional; DefaultParameterValue(0)>] numFilter : int, 
                             sampleType : SampleType, 
                             [<Optional>] multiInputMode : MultiInputMode, 
                             numArgs : int, 
                             workspace : int64) =
        let creator = AtomicSymbolCreator.FromName "UpSampling"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 [|"scale"; "num_filter"; "sample_type"; "multi_input_mode"; "num_args"; "workspace"|]
                                                 [|scale.ToString(); numFilter.ToString(); sampleType.ToString(); (if isNull (multiInputMode :> obj) then "concat" else multiInputMode.ToString()); numArgs.ToString(); workspace.ToString()|]
        outputs

    /// <summary>Update function for SignSGD optimizer.
    /// 
    /// .. math::
    /// 
    ///  g_t = \nabla J(W_{t-1})\\
    ///  W_t = W_{t-1} - \eta_t \text{sign}(g_t)
    /// 
    /// It updates the weights using::
    /// 
    ///  weight = weight - learning_rate * sign(gradient)
    /// 
    /// .. note::
    ///    - sparse ndarray not supported for this optimizer yet.
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L61</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="wd">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
    /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    static member SignsgdUpdate(weight : NDArray, 
                                grad : NDArray, 
                                lr : float, 
                                [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                                [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
                                [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float) =
        let creator = AtomicSymbolCreator.FromName "signsgd_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle|]
                                                 [|"lr"; "wd"; "rescale_grad"; "clip_gradient"|]
                                                 [|lr.ToString(); wd.ToString(); rescaleGrad.ToString(); clipGradient.ToString()|]
        outputs

    /// <summary>SIGN momentUM (Signum) optimizer.
    /// 
    /// .. math::
    /// 
    ///  g_t = \nabla J(W_{t-1})\\
    ///  m_t = \beta m_{t-1} + (1 - \beta) g_t\\
    ///  W_t = W_{t-1} - \eta_t \text{sign}(m_t)
    /// 
    /// It updates the weights using::
    ///  state = momentum * state + (1-momentum) * gradient
    ///  weight = weight - learning_rate * sign(state)
    /// 
    /// Where the parameter ``momentum`` is the decay rate of momentum estimates at each epoch.
    /// 
    /// .. note::
    ///    - sparse ndarray not supported for this optimizer yet.
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L90</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="mom">Momentum</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="momentum">The decay rate of momentum estimates at each epoch.</param>
    /// <param name="wd">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
    /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    /// <param name="wdLh">The amount of weight decay that does not go into gradient/momentum calculationsotherwise do weight decay algorithmically only.</param>
    static member SignumUpdate(weight : NDArray, 
                               grad : NDArray, 
                               mom : NDArray, 
                               lr : float, 
                               [<Optional; DefaultParameterValue(0.0)>] momentum : float, 
                               [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                               [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
                               [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float, 
                               [<Optional; DefaultParameterValue(0.0)>] wdLh : float) =
        let creator = AtomicSymbolCreator.FromName "signum_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle; mom.NDArrayHandle|]
                                                 [|"lr"; "momentum"; "wd"; "rescale_grad"; "clip_gradient"; "wd_lh"|]
                                                 [|lr.ToString(); momentum.ToString(); wd.ToString(); rescaleGrad.ToString(); clipGradient.ToString(); wdLh.ToString()|]
        outputs

    /// <summary>Update function for Stochastic Gradient Descent (SGD) optimizer.
    /// 
    /// It updates the weights using::
    /// 
    ///  weight = weight - learning_rate * (gradient + wd * weight)
    /// 
    /// However, if gradient is of ``row_sparse`` storage type and ``lazy_update`` is True,
    /// only the row slices whose indices appear in grad.indices are updated::
    /// 
    ///  for row in gradient.indices:
    ///      weight[row] = weight[row] - learning_rate * (gradient[row] + wd * weight[row])
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L522</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="wd">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
    /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    /// <param name="lazyUpdate">If true, lazy updates are applied if gradient&#39;s stype is row_sparse.</param>
    static member SgdUpdate(weight : NDArray, 
                            grad : NDArray, 
                            lr : float, 
                            [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                            [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
                            [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float, 
                            [<Optional; DefaultParameterValue(true)>] lazyUpdate : bool) =
        let creator = AtomicSymbolCreator.FromName "sgd_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle|]
                                                 [|"lr"; "wd"; "rescale_grad"; "clip_gradient"; "lazy_update"|]
                                                 [|lr.ToString(); wd.ToString(); rescaleGrad.ToString(); clipGradient.ToString(); lazyUpdate.ToString()|]
        outputs

    /// <summary>Momentum update function for Stochastic Gradient Descent (SGD) optimizer.
    /// 
    /// Momentum update has better convergence rates on neural networks. Mathematically it looks
    /// like below:
    /// 
    /// .. math::
    /// 
    ///   v_1 = \alpha * \nabla J(W_0)\\
    ///   v_t = \gamma v_{t-1} - \alpha * \nabla J(W_{t-1})\\
    ///   W_t = W_{t-1} + v_t
    /// 
    /// It updates the weights using::
    /// 
    ///   v = momentum * v - learning_rate * gradient
    ///   weight += v
    /// 
    /// Where the parameter ``momentum`` is the decay rate of momentum estimates at each epoch.
    /// 
    /// However, if grad&#39;s storage type is ``row_sparse``, ``lazy_update`` is True and weight&#39;s storage
    /// type is the same as momentum&#39;s storage type,
    /// only the row slices whose indices appear in grad.indices are updated (for both weight and momentum)::
    /// 
    ///   for row in gradient.indices:
    ///       v[row] = momentum[row] * v[row] - learning_rate * gradient[row]
    ///       weight[row] += v[row]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L563</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="mom">Momentum</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="momentum">The decay rate of momentum estimates at each epoch.</param>
    /// <param name="wd">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
    /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    /// <param name="lazyUpdate">If true, lazy updates are applied if gradient&#39;s stype is row_sparse and both weight and momentum have the same stype</param>
    static member SgdMomUpdate(weight : NDArray, 
                               grad : NDArray, 
                               mom : NDArray, 
                               lr : float, 
                               [<Optional; DefaultParameterValue(0.0)>] momentum : float, 
                               [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                               [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
                               [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float, 
                               [<Optional; DefaultParameterValue(true)>] lazyUpdate : bool) =
        let creator = AtomicSymbolCreator.FromName "sgd_mom_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle; mom.NDArrayHandle|]
                                                 [|"lr"; "momentum"; "wd"; "rescale_grad"; "clip_gradient"; "lazy_update"|]
                                                 [|lr.ToString(); momentum.ToString(); wd.ToString(); rescaleGrad.ToString(); clipGradient.ToString(); lazyUpdate.ToString()|]
        outputs

    /// <summary>Updater function for multi-precision sgd optimizer</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">gradient</param>
    /// <param name="weight32">Weight32</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="wd">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
    /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    /// <param name="lazyUpdate">If true, lazy updates are applied if gradient&#39;s stype is row_sparse.</param>
    static member MpSgdUpdate(weight : NDArray, 
                              grad : NDArray, 
                              weight32 : NDArray, 
                              lr : float, 
                              [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                              [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
                              [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float, 
                              [<Optional; DefaultParameterValue(true)>] lazyUpdate : bool) =
        let creator = AtomicSymbolCreator.FromName "mp_sgd_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle; weight32.NDArrayHandle|]
                                                 [|"lr"; "wd"; "rescale_grad"; "clip_gradient"; "lazy_update"|]
                                                 [|lr.ToString(); wd.ToString(); rescaleGrad.ToString(); clipGradient.ToString(); lazyUpdate.ToString()|]
        outputs

    /// <summary>Updater function for multi-precision sgd optimizer</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="mom">Momentum</param>
    /// <param name="weight32">Weight32</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="momentum">The decay rate of momentum estimates at each epoch.</param>
    /// <param name="wd">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
    /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    /// <param name="lazyUpdate">If true, lazy updates are applied if gradient&#39;s stype is row_sparse and both weight and momentum have the same stype</param>
    static member MpSgdMomUpdate(weight : NDArray, 
                                 grad : NDArray, 
                                 mom : NDArray, 
                                 weight32 : NDArray, 
                                 lr : float, 
                                 [<Optional; DefaultParameterValue(0.0)>] momentum : float, 
                                 [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                                 [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
                                 [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float, 
                                 [<Optional; DefaultParameterValue(true)>] lazyUpdate : bool) =
        let creator = AtomicSymbolCreator.FromName "mp_sgd_mom_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle; mom.NDArrayHandle; weight32.NDArrayHandle|]
                                                 [|"lr"; "momentum"; "wd"; "rescale_grad"; "clip_gradient"; "lazy_update"|]
                                                 [|lr.ToString(); momentum.ToString(); wd.ToString(); rescaleGrad.ToString(); clipGradient.ToString(); lazyUpdate.ToString()|]
        outputs

    /// <summary>The FTML optimizer described in
    /// *FTML - Follow the Moving Leader in Deep Learning*,
    /// available at http://proceedings.mlr.press/v70/zheng17a/zheng17a.pdf.
    /// 
    /// .. math::
    /// 
    ///  g_t = \nabla J(W_{t-1})\\
    ///  v_t = \beta_2 v_{t-1} + (1 - \beta_2) g_t^2\\
    ///  d_t = \frac{ 1 - \beta_1^t }{ \eta_t } (\sqrt{ \frac{ v_t }{ 1 - \beta_2^t } } + \epsilon)
    ///  \sigma_t = d_t - \beta_1 d_{t-1}
    ///  z_t = \beta_1 z_{ t-1 } + (1 - \beta_1^t) g_t - \sigma_t W_{t-1}
    ///  W_t = - \frac{ z_t }{ d_t }
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L638</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="d">Internal state ``d_t``</param>
    /// <param name="v">Internal state ``v_t``</param>
    /// <param name="z">Internal state ``z_t``</param>
    /// <param name="lr">Learning rate.</param>
    /// <param name="beta1">Generally close to 0.5.</param>
    /// <param name="beta2">Generally close to 1.</param>
    /// <param name="epsilon">Epsilon to prevent div 0.</param>
    /// <param name="t">Number of update.</param>
    /// <param name="wd">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
    /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
    /// <param name="clipGrad">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    static member FtmlUpdate(weight : NDArray, 
                             grad : NDArray, 
                             d : NDArray, 
                             v : NDArray, 
                             z : NDArray, 
                             lr : float, 
                             [<Optional; DefaultParameterValue(0.600000024)>] beta1 : float, 
                             [<Optional; DefaultParameterValue(0.999000013)>] beta2 : float, 
                             epsilon : double, 
                             t : int, 
                             [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                             [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
                             [<Optional; DefaultParameterValue(-1.0)>] clipGrad : float) =
        let creator = AtomicSymbolCreator.FromName "ftml_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle; d.NDArrayHandle; v.NDArrayHandle; z.NDArrayHandle|]
                                                 [|"lr"; "beta1"; "beta2"; "epsilon"; "t"; "wd"; "rescale_grad"; "clip_grad"|]
                                                 [|lr.ToString(); beta1.ToString(); beta2.ToString(); epsilon.ToString(); t.ToString(); wd.ToString(); rescaleGrad.ToString(); clipGrad.ToString()|]
        outputs

    /// <summary>Update function for Adam optimizer. Adam is seen as a generalization
    /// of AdaGrad.
    /// 
    /// Adam update consists of the following steps, where g represents gradient and m, v
    /// are 1st and 2nd order moment estimates (mean and variance).
    /// 
    /// .. math::
    /// 
    ///  g_t = \nabla J(W_{t-1})\\
    ///  m_t = \beta_1 m_{t-1} + (1 - \beta_1) g_t\\
    ///  v_t = \beta_2 v_{t-1} + (1 - \beta_2) g_t^2\\
    ///  W_t = W_{t-1} - \alpha \frac{ m_t }{ \sqrt{ v_t } + \epsilon }
    /// 
    /// It updates the weights using::
    /// 
    ///  m = beta1*m + (1-beta1)*grad
    ///  v = beta2*v + (1-beta2)*(grad**2)
    ///  w += - learning_rate * m / (sqrt(v) + epsilon)
    /// 
    /// However, if grad&#39;s storage type is ``row_sparse``, ``lazy_update`` is True and the storage
    /// type of weight is the same as those of m and v,
    /// only the row slices whose indices appear in grad.indices are updated (for w, m and v)::
    /// 
    ///  for row in grad.indices:
    ///      m[row] = beta1*m[row] + (1-beta1)*grad[row]
    ///      v[row] = beta2*v[row] + (1-beta2)*(grad[row]**2)
    ///      w[row] += - learning_rate * m[row] / (sqrt(v[row]) + epsilon)
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L686</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="mean">Moving mean</param>
    /// <param name="var">Moving variance</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="beta1">The decay rate for the 1st moment estimates.</param>
    /// <param name="beta2">The decay rate for the 2nd moment estimates.</param>
    /// <param name="epsilon">A small constant for numerical stability.</param>
    /// <param name="wd">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
    /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    /// <param name="lazyUpdate">If true, lazy updates are applied if gradient&#39;s stype is row_sparse and all of w, m and v have the same stype</param>
    static member AdamUpdate(weight : NDArray, 
                             grad : NDArray, 
                             mean : NDArray, 
                             var : NDArray, 
                             lr : float, 
                             [<Optional; DefaultParameterValue(0.899999976)>] beta1 : float, 
                             [<Optional; DefaultParameterValue(0.999000013)>] beta2 : float, 
                             [<Optional; DefaultParameterValue(9.99999994E-09)>] epsilon : float, 
                             [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                             [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
                             [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float, 
                             [<Optional; DefaultParameterValue(true)>] lazyUpdate : bool) =
        let creator = AtomicSymbolCreator.FromName "adam_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle; mean.NDArrayHandle; var.NDArrayHandle|]
                                                 [|"lr"; "beta1"; "beta2"; "epsilon"; "wd"; "rescale_grad"; "clip_gradient"; "lazy_update"|]
                                                 [|lr.ToString(); beta1.ToString(); beta2.ToString(); epsilon.ToString(); wd.ToString(); rescaleGrad.ToString(); clipGradient.ToString(); lazyUpdate.ToString()|]
        outputs

    /// <summary>Update function for Nesterov Accelerated Gradient( NAG) optimizer.
    /// It updates the weights using the following formula,
    /// 
    /// .. math::
    ///   v_t = \gamma v_{t-1} + \eta * \nabla J(W_{t-1} - \gamma v_{t-1})\\
    ///   W_t = W_{t-1} - v_t
    /// 
    /// Where 
    /// :math:`\eta` is the learning rate of the optimizer
    /// :math:`\gamma` is the decay rate of the momentum estimate
    /// :math:`\v_t` is the update vector at time step `t`
    /// :math:`\W_t` is the weight vector at time step `t`
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L724</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="mom">Momentum</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="momentum">The decay rate of momentum estimates at each epoch.</param>
    /// <param name="wd">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
    /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    static member NagMomUpdate(weight : NDArray, 
                               grad : NDArray, 
                               mom : NDArray, 
                               lr : float, 
                               [<Optional; DefaultParameterValue(0.0)>] momentum : float, 
                               [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                               [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
                               [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float) =
        let creator = AtomicSymbolCreator.FromName "nag_mom_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle; mom.NDArrayHandle|]
                                                 [|"lr"; "momentum"; "wd"; "rescale_grad"; "clip_gradient"|]
                                                 [|lr.ToString(); momentum.ToString(); wd.ToString(); rescaleGrad.ToString(); clipGradient.ToString()|]
        outputs

    /// <summary>Update function for multi-precision Nesterov Accelerated Gradient( NAG) optimizer.
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L743</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="mom">Momentum</param>
    /// <param name="weight32">Weight32</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="momentum">The decay rate of momentum estimates at each epoch.</param>
    /// <param name="wd">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
    /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    static member MpNagMomUpdate(weight : NDArray, 
                                 grad : NDArray, 
                                 mom : NDArray, 
                                 weight32 : NDArray, 
                                 lr : float, 
                                 [<Optional; DefaultParameterValue(0.0)>] momentum : float, 
                                 [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                                 [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
                                 [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float) =
        let creator = AtomicSymbolCreator.FromName "mp_nag_mom_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle; mom.NDArrayHandle; weight32.NDArrayHandle|]
                                                 [|"lr"; "momentum"; "wd"; "rescale_grad"; "clip_gradient"|]
                                                 [|lr.ToString(); momentum.ToString(); wd.ToString(); rescaleGrad.ToString(); clipGradient.ToString()|]
        outputs

    /// <summary>Update function for `RMSProp` optimizer.
    /// 
    /// `RMSprop` is a variant of stochastic gradient descent where the gradients are
    /// divided by a cache which grows with the sum of squares of recent gradients?
    /// 
    /// `RMSProp` is similar to `AdaGrad`, a popular variant of `SGD` which adaptively
    /// tunes the learning rate of each parameter. `AdaGrad` lowers the learning rate for
    /// each parameter monotonically over the course of training.
    /// While this is analytically motivated for convex optimizations, it may not be ideal
    /// for non-convex problems. `RMSProp` deals with this heuristically by allowing the
    /// learning rates to rebound as the denominator decays over time.
    /// 
    /// Define the Root Mean Square (RMS) error criterion of the gradient as
    /// :math:`RMS[g]_t = \sqrt{E[g^2]_t + \epsilon}`, where :math:`g` represents
    /// gradient and :math:`E[g^2]_t` is the decaying average over past squared gradient.
    /// 
    /// The :math:`E[g^2]_t` is given by:
    /// 
    /// .. math::
    ///   E[g^2]_t = \gamma * E[g^2]_{t-1} + (1-\gamma) * g_t^2
    /// 
    /// The update step is
    /// 
    /// .. math::
    ///   \theta_{t+1} = \theta_t - \frac{\eta}{RMS[g]_t} g_t
    /// 
    /// The RMSProp code follows the version in
    /// http://www.cs.toronto.edu/~tijmen/csc321/slides/lecture_slides_lec6.pdf
    /// Tieleman &amp; Hinton, 2012.
    /// 
    /// Hinton suggests the momentum term :math:`\gamma` to be 0.9 and the learning rate
    /// :math:`\eta` to be 0.001.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L795</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="n">n</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="gamma1">The decay rate of momentum estimates.</param>
    /// <param name="epsilon">A small constant for numerical stability.</param>
    /// <param name="wd">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
    /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    /// <param name="clipWeights">Clip weights to the range of [-clip_weights, clip_weights] If clip_weights &lt;= 0, weight clipping is turned off. weights = max(min(weights, clip_weights), -clip_weights).</param>
    static member RmspropUpdate(weight : NDArray, 
                                grad : NDArray, 
                                n : NDArray, 
                                lr : float, 
                                [<Optional; DefaultParameterValue(0.949999988)>] gamma1 : float, 
                                [<Optional; DefaultParameterValue(9.99999994E-09)>] epsilon : float, 
                                [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                                [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
                                [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float, 
                                [<Optional; DefaultParameterValue(-1.0)>] clipWeights : float) =
        let creator = AtomicSymbolCreator.FromName "rmsprop_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle; n.NDArrayHandle|]
                                                 [|"lr"; "gamma1"; "epsilon"; "wd"; "rescale_grad"; "clip_gradient"; "clip_weights"|]
                                                 [|lr.ToString(); gamma1.ToString(); epsilon.ToString(); wd.ToString(); rescaleGrad.ToString(); clipGradient.ToString(); clipWeights.ToString()|]
        outputs

    /// <summary>Update function for RMSPropAlex optimizer.
    /// 
    /// `RMSPropAlex` is non-centered version of `RMSProp`.
    /// 
    /// Define :math:`E[g^2]_t` is the decaying average over past squared gradient and
    /// :math:`E[g]_t` is the decaying average over past gradient.
    /// 
    /// .. math::
    ///   E[g^2]_t = \gamma_1 * E[g^2]_{t-1} + (1 - \gamma_1) * g_t^2\\
    ///   E[g]_t = \gamma_1 * E[g]_{t-1} + (1 - \gamma_1) * g_t\\
    ///   \Delta_t = \gamma_2 * \Delta_{t-1} - \frac{\eta}{\sqrt{E[g^2]_t - E[g]_t^2 + \epsilon}} g_t\\
    /// 
    /// The update step is
    /// 
    /// .. math::
    ///   \theta_{t+1} = \theta_t + \Delta_t
    /// 
    /// The RMSPropAlex code follows the version in
    /// http://arxiv.org/pdf/1308.0850v5.pdf Eq(38) - Eq(45) by Alex Graves, 2013.
    /// 
    /// Graves suggests the momentum term :math:`\gamma_1` to be 0.95, :math:`\gamma_2`
    /// to be 0.9 and the learning rate :math:`\eta` to be 0.0001.
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L834</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="n">n</param>
    /// <param name="g">g</param>
    /// <param name="delta">delta</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="gamma1">Decay rate.</param>
    /// <param name="gamma2">Decay rate.</param>
    /// <param name="epsilon">A small constant for numerical stability.</param>
    /// <param name="wd">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
    /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    /// <param name="clipWeights">Clip weights to the range of [-clip_weights, clip_weights] If clip_weights &lt;= 0, weight clipping is turned off. weights = max(min(weights, clip_weights), -clip_weights).</param>
    static member RmspropalexUpdate(weight : NDArray, 
                                    grad : NDArray, 
                                    n : NDArray, 
                                    g : NDArray, 
                                    delta : NDArray, 
                                    lr : float, 
                                    [<Optional; DefaultParameterValue(0.949999988)>] gamma1 : float, 
                                    [<Optional; DefaultParameterValue(0.899999976)>] gamma2 : float, 
                                    [<Optional; DefaultParameterValue(9.99999994E-09)>] epsilon : float, 
                                    [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                                    [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
                                    [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float, 
                                    [<Optional; DefaultParameterValue(-1.0)>] clipWeights : float) =
        let creator = AtomicSymbolCreator.FromName "rmspropalex_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle; n.NDArrayHandle; g.NDArrayHandle; delta.NDArrayHandle|]
                                                 [|"lr"; "gamma1"; "gamma2"; "epsilon"; "wd"; "rescale_grad"; "clip_gradient"; "clip_weights"|]
                                                 [|lr.ToString(); gamma1.ToString(); gamma2.ToString(); epsilon.ToString(); wd.ToString(); rescaleGrad.ToString(); clipGradient.ToString(); clipWeights.ToString()|]
        outputs

    /// <summary>Update function for Ftrl optimizer.
    /// Referenced from *Ad Click Prediction: a View from the Trenches*, available at
    /// http://dl.acm.org/citation.cfm?id=2488200.
    /// 
    /// It updates the weights using::
    /// 
    ///  rescaled_grad = clip(grad * rescale_grad, clip_gradient)
    ///  z += rescaled_grad - (sqrt(n + rescaled_grad**2) - sqrt(n)) * weight / learning_rate
    ///  n += rescaled_grad**2
    ///  w = (sign(z) * lamda1 - z) / ((beta + sqrt(n)) / learning_rate + wd) * (abs(z) &gt; lamda1)
    /// 
    /// If w, z and n are all of ``row_sparse`` storage type,
    /// only the row slices whose indices appear in grad.indices are updated (for w, z and n)::
    /// 
    ///  for row in grad.indices:
    ///      rescaled_grad[row] = clip(grad[row] * rescale_grad, clip_gradient)
    ///      z[row] += rescaled_grad[row] - (sqrt(n[row] + rescaled_grad[row]**2) - sqrt(n[row])) * weight[row] / learning_rate
    ///      n[row] += rescaled_grad[row]**2
    ///      w[row] = (sign(z[row]) * lamda1 - z[row]) / ((beta + sqrt(n[row])) / learning_rate + wd) * (abs(z[row]) &gt; lamda1)
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L874</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="z">z</param>
    /// <param name="n">Square of grad</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="lamda1">The L1 regularization coefficient.</param>
    /// <param name="beta">Per-Coordinate Learning Rate beta.</param>
    /// <param name="wd">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
    /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    static member FtrlUpdate(weight : NDArray, 
                             grad : NDArray, 
                             z : NDArray, 
                             n : NDArray, 
                             lr : float, 
                             [<Optional; DefaultParameterValue(0.00999999978)>] lamda1 : float, 
                             [<Optional; DefaultParameterValue(1.0)>] beta : float, 
                             [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                             [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
                             [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float) =
        let creator = AtomicSymbolCreator.FromName "ftrl_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle; z.NDArrayHandle; n.NDArrayHandle|]
                                                 [|"lr"; "lamda1"; "beta"; "wd"; "rescale_grad"; "clip_gradient"|]
                                                 [|lr.ToString(); lamda1.ToString(); beta.ToString(); wd.ToString(); rescaleGrad.ToString(); clipGradient.ToString()|]
        outputs

    /// <summary>Update function for AdaGrad optimizer.
    /// 
    /// Referenced from *Adaptive Subgradient Methods for Online Learning and Stochastic Optimization*,
    /// and available at http://www.jmlr.org/papers/volume12/duchi11a/duchi11a.pdf.
    /// 
    /// Updates are applied by::
    /// 
    ///     rescaled_grad = clip(grad * rescale_grad, clip_gradient)
    ///     history = history + square(rescaled_grad)
    ///     w = w - learning_rate * rescaled_grad / sqrt(history + epsilon)
    /// 
    /// Note that non-zero values for the weight decay option are not supported.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L907</summary>
    /// <param name="weight">Weight</param>
    /// <param name="grad">Gradient</param>
    /// <param name="history">History</param>
    /// <param name="lr">Learning rate</param>
    /// <param name="epsilon">epsilon</param>
    /// <param name="wd">weight decay</param>
    /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
    /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
    static member SparseAdagradUpdate(weight : NDArray, 
                                      grad : NDArray, 
                                      history : NDArray, 
                                      lr : float, 
                                      [<Optional; DefaultParameterValue(1.00000001E-07)>] epsilon : float, 
                                      [<Optional; DefaultParameterValue(0.0)>] wd : float, 
                                      [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
                                      [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float) =
        let creator = AtomicSymbolCreator.FromName "_sparse_adagrad_update"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|weight.NDArrayHandle; grad.NDArrayHandle; history.NDArrayHandle|]
                                                 [|"lr"; "epsilon"; "wd"; "rescale_grad"; "clip_gradient"|]
                                                 [|lr.ToString(); epsilon.ToString(); wd.ToString(); rescaleGrad.ToString(); clipGradient.ToString()|]
        outputs

    /// <summary>Pads an input array with a constant or edge values of the array.
    /// 
    /// .. note:: `Pad` is deprecated. Use `pad` instead.
    /// 
    /// .. note:: Current implementation only supports 4D and 5D input arrays with padding applied
    ///    only on axes 1, 2 and 3. Expects axes 4 and 5 in `pad_width` to be zero.
    /// 
    /// This operation pads an input array with either a `constant_value` or edge values
    /// along each axis of the input array. The amount of padding is specified by `pad_width`.
    /// 
    /// `pad_width` is a tuple of integer padding widths for each axis of the format
    /// ``(before_1, after_1, ... , before_N, after_N)``. The `pad_width` should be of length ``2*N``
    /// where ``N`` is the number of dimensions of the array.
    /// 
    /// For dimension ``N`` of the input array, ``before_N`` and ``after_N`` indicates how many values
    /// to add before and after the elements of the array along dimension ``N``.
    /// The widths of the higher two dimensions ``before_1``, ``after_1``, ``before_2``,
    /// ``after_2`` must be 0.
    /// 
    /// Example::
    /// 
    ///    x = [[[[  1.   2.   3.]
    ///           [  4.   5.   6.]]
    /// 
    ///          [[  7.   8.   9.]
    ///           [ 10.  11.  12.]]]
    /// 
    /// 
    ///         [[[ 11.  12.  13.]
    ///           [ 14.  15.  16.]]
    /// 
    ///          [[ 17.  18.  19.]
    ///           [ 20.  21.  22.]]]]
    /// 
    ///    pad(x,mode=&quot;edge&quot;, pad_width=(0,0,0,0,1,1,1,1)) =
    /// 
    ///          [[[[  1.   1.   2.   3.   3.]
    ///             [  1.   1.   2.   3.   3.]
    ///             [  4.   4.   5.   6.   6.]
    ///             [  4.   4.   5.   6.   6.]]
    /// 
    ///            [[  7.   7.   8.   9.   9.]
    ///             [  7.   7.   8.   9.   9.]
    ///             [ 10.  10.  11.  12.  12.]
    ///             [ 10.  10.  11.  12.  12.]]]
    /// 
    /// 
    ///           [[[ 11.  11.  12.  13.  13.]
    ///             [ 11.  11.  12.  13.  13.]
    ///             [ 14.  14.  15.  16.  16.]
    ///             [ 14.  14.  15.  16.  16.]]
    /// 
    ///            [[ 17.  17.  18.  19.  19.]
    ///             [ 17.  17.  18.  19.  19.]
    ///             [ 20.  20.  21.  22.  22.]
    ///             [ 20.  20.  21.  22.  22.]]]]
    /// 
    ///    pad(x, mode=&quot;constant&quot;, constant_value=0, pad_width=(0,0,0,0,1,1,1,1)) =
    /// 
    ///          [[[[  0.   0.   0.   0.   0.]
    ///             [  0.   1.   2.   3.   0.]
    ///             [  0.   4.   5.   6.   0.]
    ///             [  0.   0.   0.   0.   0.]]
    /// 
    ///            [[  0.   0.   0.   0.   0.]
    ///             [  0.   7.   8.   9.   0.]
    ///             [  0.  10.  11.  12.   0.]
    ///             [  0.   0.   0.   0.   0.]]]
    /// 
    /// 
    ///           [[[  0.   0.   0.   0.   0.]
    ///             [  0.  11.  12.  13.   0.]
    ///             [  0.  14.  15.  16.   0.]
    ///             [  0.   0.   0.   0.   0.]]
    /// 
    ///            [[  0.   0.   0.   0.   0.]
    ///             [  0.  17.  18.  19.   0.]
    ///             [  0.  20.  21.  22.   0.]
    ///             [  0.   0.   0.   0.   0.]]]]
    /// 
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\pad.cc:L766</summary>
    /// <param name="data">An n-dimensional input array.</param>
    /// <param name="mode">Padding type to use. &quot;constant&quot; pads with `constant_value` &quot;edge&quot; pads using the edge values of the input array &quot;reflect&quot; pads by reflecting values with respect to the edges.</param>
    /// <param name="padWidth">Widths of the padding regions applied to the edges of each axis. It is a tuple of integer padding widths for each axis of the format ``(before_1, after_1, ... , before_N, after_N)``. It should be of length ``2*N`` where ``N`` is the number of dimensions of the array.This is equivalent to pad_width in numpy.pad, but flattened.</param>
    /// <param name="constantValue">The value used for padding when `mode` is &quot;constant&quot;.</param>
    static member Pad(data : NDArray, mode : PadMode, padWidth : int seq, constantValue : double) =
        let creator = AtomicSymbolCreator.FromName "Pad"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"mode"; "pad_width"; "constant_value"|]
                                                 [|mode.ToString(); padWidth.ToString(); constantValue.ToString()|]
        outputs

    /// <summary>Dequantize the input tensor into a float tensor.
    /// min_range and max_range are scalar floats that specify the range for
    /// the output data.
    /// 
    /// When input data type is `uint8`, the output is calculated using the following equation:
    /// 
    /// `out[i] = in[i] * (max_range - min_range) / 255.0`,
    /// 
    /// When input data type is `int8`, the output is calculate using the following equation
    /// by keep zero centered for the quantized value:
    /// 
    /// `out[i] = in[i] * MaxAbs(min_range, max_range) / 127.0`,
    /// 
    /// .. Note::
    ///     This operator only supports forward propogation. DO NOT use it in training.
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\quantization\dequantize.cc:L83</summary>
    /// <param name="data">A ndarray/symbol of type `uint8`</param>
    /// <param name="minRange">The minimum scalar value possibly produced for the input in float32</param>
    /// <param name="maxRange">The maximum scalar value possibly produced for the input in float32</param>
    /// <param name="outType">Output data type.</param>
    static member ContribDequantize(data : NDArray, minRange : NDArray, maxRange : NDArray, [<Optional>] outType : ContribDequantizeOutType) =
        let creator = AtomicSymbolCreator.FromName "_contrib_dequantize"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; minRange.NDArrayHandle; maxRange.NDArrayHandle|]
                                                 [|"out_type"|]
                                                 [|(if isNull (outType :> obj) then "float32" else outType.ToString())|]
        outputs

    /// <summary>Quantize a input tensor from float to `out_type`,
    /// with user-specified `min_range` and `max_range`.
    /// 
    /// min_range and max_range are scalar floats that specify the range for
    /// the input data.
    /// 
    /// When out_type is `uint8`, the output is calculated using the following equation:
    /// 
    /// `out[i] = (in[i] - min_range) * range(OUTPUT_TYPE) / (max_range - min_range) + 0.5`,
    /// 
    /// where `range(T) = numeric_limits&lt;T&gt;::max() - numeric_limits&lt;T&gt;::min()`.
    /// 
    /// When out_type is `int8`, the output is calculate using the following equation
    /// by keep zero centered for the quantized value:
    /// 
    /// `out[i] = sign(in[i]) * min(abs(in[i] * scale + 0.5f, quantized_range)`,
    /// 
    /// where
    /// `quantized_range = MinAbs(max(int8), min(int8))` and
    /// `scale = quantized_range / MaxAbs(min_range, max_range).`
    /// 
    /// .. Note::
    ///     This operator only supports forward propagation. DO NOT use it in training.
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\quantization\quantize.cc:L74</summary>
    /// <param name="data">A ndarray/symbol of type `float32`</param>
    /// <param name="minRange">The minimum scalar value possibly produced for the input</param>
    /// <param name="maxRange">The maximum scalar value possibly produced for the input</param>
    /// <param name="outType">Output data type.</param>
    static member ContribQuantize(data : NDArray, minRange : NDArray, maxRange : NDArray, [<Optional>] outType : ContribQuantizeOutType) =
        let creator = AtomicSymbolCreator.FromName "_contrib_quantize"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; minRange.NDArrayHandle; maxRange.NDArrayHandle|]
                                                 [|"out_type"|]
                                                 [|(if isNull (outType :> obj) then "uint8" else outType.ToString())|]
        outputs

    /// <summary>Quantize a input tensor from float to `out_type`,
    /// with user-specified `min_calib_range` and `max_calib_range` or the input range collected at runtime.
    /// 
    /// Output `min_range` and `max_range` are scalar floats that specify the range for the input data.
    /// 
    /// When out_type is `uint8`, the output is calculated using the following equation:
    /// 
    /// `out[i] = (in[i] - min_range) * range(OUTPUT_TYPE) / (max_range - min_range) + 0.5`,
    /// 
    /// where `range(T) = numeric_limits&lt;T&gt;::max() - numeric_limits&lt;T&gt;::min()`.
    /// 
    /// When out_type is `int8`, the output is calculate using the following equation
    /// by keep zero centered for the quantized value:
    /// 
    /// `out[i] = sign(in[i]) * min(abs(in[i] * scale + 0.5f, quantized_range)`,
    /// 
    /// where
    /// `quantized_range = MinAbs(max(int8), min(int8))` and
    /// `scale = quantized_range / MaxAbs(min_range, max_range).`
    /// 
    /// When out_type is `auto`, the output type is automatically determined by min_calib_range if presented.
    /// If min_calib_range &lt; 0.0f, the output type will be int8, otherwise will be uint8.
    /// If min_calib_range isn&#39;t presented, the output type will be int8.
    /// 
    /// .. Note::
    ///     This operator only supports forward propagation. DO NOT use it in training.
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\quantization\quantize_v2.cc:L92</summary>
    /// <param name="data">A ndarray/symbol of type `float32`</param>
    /// <param name="outType">Output data type. `auto` can be specified to automatically determine output type according to min_calib_range.</param>
    /// <param name="minCalibRange">The minimum scalar value in the form of float32. If present, it will be used to quantize the fp32 data into int8 or uint8.</param>
    /// <param name="maxCalibRange">The maximum scalar value in the form of float32. If present, it will be used to quantize the fp32 data into int8 or uint8.</param>
    static member ContribQuantizeV2(data : NDArray, [<Optional>] outType : ContribQuantizeV2OutType, [<Optional>] minCalibRange : float Nullable, [<Optional>] maxCalibRange : float Nullable) =
        let creator = AtomicSymbolCreator.FromName "_contrib_quantize_v2"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"out_type"; "min_calib_range"; "max_calib_range"|]
                                                 [|(if isNull (outType :> obj) then "int8" else outType.ToString()); minCalibRange.ToString(); maxCalibRange.ToString()|]
        outputs

    /// <summary>Quantize a input tensor from float to `out_type`,
    /// with user-specified `min_calib_range` and `max_calib_range` or the input range collected at runtime.
    /// 
    /// Output `min_range` and `max_range` are scalar floats that specify the range for the input data.
    /// 
    /// When out_type is `uint8`, the output is calculated using the following equation:
    /// 
    /// `out[i] = (in[i] - min_range) * range(OUTPUT_TYPE) / (max_range - min_range) + 0.5`,
    /// 
    /// where `range(T) = numeric_limits&lt;T&gt;::max() - numeric_limits&lt;T&gt;::min()`.
    /// 
    /// When out_type is `int8`, the output is calculate using the following equation
    /// by keep zero centered for the quantized value:
    /// 
    /// `out[i] = sign(in[i]) * min(abs(in[i] * scale + 0.5f, quantized_range)`,
    /// 
    /// where
    /// `quantized_range = MinAbs(max(int8), min(int8))` and
    /// `scale = quantized_range / MaxAbs(min_range, max_range).`
    /// 
    /// When out_type is `auto`, the output type is automatically determined by min_calib_range if presented.
    /// If min_calib_range &lt; 0.0f, the output type will be int8, otherwise will be uint8.
    /// If min_calib_range isn&#39;t presented, the output type will be int8.
    /// 
    /// .. Note::
    ///     This operator only supports forward propagation. DO NOT use it in training.
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\quantization\quantize_v2.cc:L92</summary>
    /// <param name="data">A ndarray/symbol of type `float32`</param>
    /// <param name="outType">Output data type. `auto` can be specified to automatically determine output type according to min_calib_range.</param>
    /// <param name="minCalibRange">The minimum scalar value in the form of float32. If present, it will be used to quantize the fp32 data into int8 or uint8.</param>
    /// <param name="maxCalibRange">The maximum scalar value in the form of float32. If present, it will be used to quantize the fp32 data into int8 or uint8.</param>
    static member ContribQuantizeV2(data : NDArray, ?outType : ContribQuantizeV2OutType, ?minCalibRange : float, ?maxCalibRange : float) =
        let creator = AtomicSymbolCreator.FromName "_contrib_quantize_v2"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"out_type"; "min_calib_range"; "max_calib_range"|]
                                                 [|(match outType with None -> "int8" | _ -> outType.ToString()); (match minCalibRange with None -> "None" | _ -> minCalibRange.ToString()); (match maxCalibRange with None -> "None" | _ -> maxCalibRange.ToString())|]
        outputs

    /// <summary>Activation operator for input and output data type of int8.
    /// The input and output data comes with min and max thresholds for quantizing
    /// the float32 data into int8.
    /// 
    /// .. Note::
    ///      This operator only supports forward propogation. DO NOT use it in training.
    ///      This operator only supports `relu`
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\quantization\quantized_activation.cc:L91</summary>
    /// <param name="data">Input data.</param>
    /// <param name="minData">Minimum value of data.</param>
    /// <param name="maxData">Maximum value of data.</param>
    /// <param name="actType">Activation function to be applied.</param>
    static member ContribQuantizedAct(data : NDArray, minData : NDArray, maxData : NDArray, actType : ActType) =
        let creator = AtomicSymbolCreator.FromName "_contrib_quantized_act"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; minData.NDArrayHandle; maxData.NDArrayHandle|]
                                                 [|"act_type"|]
                                                 [|actType.ToString()|]
        outputs

    /// <summary>Joins input arrays along a given axis.
    /// 
    /// The dimensions of the input arrays should be the same except the axis along
    /// which they will be concatenated.
    /// The dimension of the output array along the concatenated axis will be equal
    /// to the sum of the corresponding dimensions of the input arrays.
    /// All inputs with different min/max will be rescaled by using largest [min, max] pairs.
    /// If any input holds int8, then the output will be int8. Otherwise output will be uint8.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\quantization\quantized_concat.cc:L108</summary>
    /// <param name="data">List of arrays to concatenate</param>
    /// <param name="numArgs">Number of inputs to be concated.</param>
    /// <param name="dim">the dimension to be concated.</param>
    static member ContribQuantizedConcat([<ParamArray>] data : NDArray[], numArgs : int, [<Optional; DefaultParameterValue(1)>] dim : int) =
        let creator = AtomicSymbolCreator.FromName "_contrib_quantized_concat"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 [|"num_args"; "dim"|]
                                                 [|numArgs.ToString(); dim.ToString()|]
        outputs

    /// <summary>Convolution operator for input, weight and bias data type of int8,
    /// and accumulates in type int32 for the output. For each argument, two more arguments of type
    /// float32 must be provided representing the thresholds of quantizing argument from data
    /// type float32 to int8. The final outputs contain the convolution result in int32, and min
    /// and max thresholds representing the threholds for quantizing the float32 output into int32.
    /// 
    /// .. Note::
    ///     This operator only supports forward propogation. DO NOT use it in training.
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\quantization\quantized_conv.cc:L137</summary>
    /// <param name="data">Input data.</param>
    /// <param name="weight">weight.</param>
    /// <param name="bias">bias.</param>
    /// <param name="minData">Minimum value of data.</param>
    /// <param name="maxData">Maximum value of data.</param>
    /// <param name="minWeight">Minimum value of weight.</param>
    /// <param name="maxWeight">Maximum value of weight.</param>
    /// <param name="minBias">Minimum value of bias.</param>
    /// <param name="maxBias">Maximum value of bias.</param>
    /// <param name="kernel">Convolution kernel size: (w,), (h, w) or (d, h, w)</param>
    /// <param name="stride">Convolution stride: (w,), (h, w) or (d, h, w). Defaults to 1 for each dimension.</param>
    /// <param name="dilate">Convolution dilate: (w,), (h, w) or (d, h, w). Defaults to 1 for each dimension.</param>
    /// <param name="pad">Zero pad for convolution: (w,), (h, w) or (d, h, w). Defaults to no padding.</param>
    /// <param name="numFilter">Convolution filter(channel) number</param>
    /// <param name="numGroup">Number of group partitions.</param>
    /// <param name="workspace">Maximum temporary workspace allowed (MB) in convolution.This parameter has two usages. When CUDNN is not used, it determines the effective batch size of the convolution kernel. When CUDNN is used, it controls the maximum temporary storage used for tuning the best CUDNN kernel when `limited_workspace` strategy is used.</param>
    /// <param name="noBias">Whether to disable bias parameter.</param>
    /// <param name="cudnnTune">Whether to pick convolution algo by running performance test.</param>
    /// <param name="cudnnOff">Turn off cudnn for this layer.</param>
    /// <param name="layout">Set layout for input, output and weight. Empty for
    ///     default layout: NCW for 1d, NCHW for 2d and NCDHW for 3d.NHWC and NDHWC are only supported on GPU.</param>
    static member ContribQuantizedConv(data : NDArray, 
                                       weight : NDArray, 
                                       bias : NDArray, 
                                       minData : NDArray, 
                                       maxData : NDArray, 
                                       minWeight : NDArray, 
                                       maxWeight : NDArray, 
                                       minBias : NDArray, 
                                       maxBias : NDArray, 
                                       kernel : int seq, 
                                       [<Optional>] stride : int seq, 
                                       [<Optional>] dilate : int seq, 
                                       [<Optional>] pad : int seq, 
                                       numFilter : int, 
                                       numGroup : int, 
                                       workspace : int64, 
                                       [<Optional; DefaultParameterValue(false)>] noBias : bool, 
                                       [<Optional>] cudnnTune : CudnnTune, 
                                       [<Optional; DefaultParameterValue(false)>] cudnnOff : bool, 
                                       [<Optional>] layout : ContribQuantizedConvLayout) =
        let creator = AtomicSymbolCreator.FromName "_contrib_quantized_conv"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; weight.NDArrayHandle; bias.NDArrayHandle; minData.NDArrayHandle; maxData.NDArrayHandle; minWeight.NDArrayHandle; maxWeight.NDArrayHandle; minBias.NDArrayHandle; maxBias.NDArrayHandle|]
                                                 [|"kernel"; "stride"; "dilate"; "pad"; "num_filter"; "num_group"; "workspace"; "no_bias"; "cudnn_tune"; "cudnn_off"; "layout"|]
                                                 [|kernel.ToString(); (if isNull (stride :> obj) then "[]" else stride.ToString()); (if isNull (dilate :> obj) then "[]" else dilate.ToString()); (if isNull (pad :> obj) then "[]" else pad.ToString()); numFilter.ToString(); numGroup.ToString(); workspace.ToString(); noBias.ToString(); (if isNull (cudnnTune :> obj) then "None" else cudnnTune.ToString()); cudnnOff.ToString(); (if isNull (layout :> obj) then "None" else layout.ToString())|]
        outputs

    /// <summary>elemwise_add operator for input dataA and input dataB data type of int8,
    /// and accumulates in type int32 for the output. For each argument, two more arguments of type
    /// float32 must be provided representing the thresholds of quantizing argument from data
    /// type float32 to int8. The final outputs contain result in int32, and min
    /// and max thresholds representing the threholds for quantizing the float32 output into int32.
    /// 
    /// .. Note::
    ///     This operator only supports forward propogation. DO NOT use it in training.
    /// 
    /// </summary>
    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    /// <param name="lhsMin">3rd input</param>
    /// <param name="lhsMax">4th input</param>
    /// <param name="rhsMin">5th input</param>
    /// <param name="rhsMax">6th input</param>
    static member ContribQuantizedElemwiseAdd(lhs : NDArray, 
                                              rhs : NDArray, 
                                              lhsMin : NDArray, 
                                              lhsMax : NDArray, 
                                              rhsMin : NDArray, 
                                              rhsMax : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_contrib_quantized_elemwise_add"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle; lhsMin.NDArrayHandle; lhsMax.NDArrayHandle; rhsMin.NDArrayHandle; rhsMax.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="data">A ndarray/symbol of type `float32`</param>
    /// <param name="minData">The minimum scalar value possibly produced for the data</param>
    /// <param name="maxData">The maximum scalar value possibly produced for the data</param>
    static member ContribQuantizedFlatten(data : NDArray, minData : NDArray, maxData : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_contrib_quantized_flatten"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; minData.NDArrayHandle; maxData.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Flattens the input array into a 2-D array by collapsing the higher dimensions.
    /// 
    /// .. note:: `Flatten` is deprecated. Use `flatten` instead.
    /// 
    /// For an input array with shape ``(d1, d2, ..., dk)``, `flatten` operation reshapes
    /// the input array into an output array of shape ``(d1, d2*...*dk)``.
    /// 
    /// Note that the bahavior of this function is different from numpy.ndarray.flatten,
    /// which behaves similar to mxnet.ndarray.reshape((-1,)).
    /// 
    /// Example::
    /// 
    ///     x = [[
    ///         [1,2,3],
    ///         [4,5,6],
    ///         [7,8,9]
    ///     ],
    ///     [    [1,2,3],
    ///         [4,5,6],
    ///         [7,8,9]
    ///     ]],
    /// 
    ///     flatten(x) = [[ 1.,  2.,  3.,  4.,  5.,  6.,  7.,  8.,  9.],
    ///        [ 1.,  2.,  3.,  4.,  5.,  6.,  7.,  8.,  9.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L293</summary>
    /// <param name="data">Input array.</param>
    static member Flatten(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "Flatten"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Fully Connected operator for input, weight and bias data type of int8,
    /// and accumulates in type int32 for the output. For each argument, two more arguments of type
    /// float32 must be provided representing the thresholds of quantizing argument from data
    /// type float32 to int8. The final outputs contain the convolution result in int32, and min
    /// and max thresholds representing the threholds for quantizing the float32 output into int32.
    /// 
    /// .. Note::
    ///     This operator only supports forward propogation. DO NOT use it in training.
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\quantization\quantized_fully_connected.cc:L313</summary>
    /// <param name="data">Input data.</param>
    /// <param name="weight">weight.</param>
    /// <param name="bias">bias.</param>
    /// <param name="minData">Minimum value of data.</param>
    /// <param name="maxData">Maximum value of data.</param>
    /// <param name="minWeight">Minimum value of weight.</param>
    /// <param name="maxWeight">Maximum value of weight.</param>
    /// <param name="minBias">Minimum value of bias.</param>
    /// <param name="maxBias">Maximum value of bias.</param>
    /// <param name="numHidden">Number of hidden nodes of the output.</param>
    /// <param name="noBias">Whether to disable bias parameter.</param>
    /// <param name="flatten">Whether to collapse all but the first axis of the input data tensor.</param>
    static member ContribQuantizedFullyConnected(data : NDArray, 
                                                 weight : NDArray, 
                                                 bias : NDArray, 
                                                 minData : NDArray, 
                                                 maxData : NDArray, 
                                                 minWeight : NDArray, 
                                                 maxWeight : NDArray, 
                                                 minBias : NDArray, 
                                                 maxBias : NDArray, 
                                                 numHidden : int, 
                                                 [<Optional; DefaultParameterValue(false)>] noBias : bool, 
                                                 [<Optional; DefaultParameterValue(true)>] flatten : bool) =
        let creator = AtomicSymbolCreator.FromName "_contrib_quantized_fully_connected"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; weight.NDArrayHandle; bias.NDArrayHandle; minData.NDArrayHandle; maxData.NDArrayHandle; minWeight.NDArrayHandle; maxWeight.NDArrayHandle; minBias.NDArrayHandle; maxBias.NDArrayHandle|]
                                                 [|"num_hidden"; "no_bias"; "flatten"|]
                                                 [|numHidden.ToString(); noBias.ToString(); flatten.ToString()|]
        outputs

    /// <summary>Pooling operator for input and output data type of int8.
    /// The input and output data comes with min and max thresholds for quantizing
    /// the float32 data into int8.
    /// 
    /// .. Note::
    ///     This operator only supports forward propogation. DO NOT use it in training.
    ///     This operator only supports `pool_type` of `avg` or `max`.
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\quantization\quantized_pooling.cc:L145</summary>
    /// <param name="data">Input data.</param>
    /// <param name="minData">Minimum value of data.</param>
    /// <param name="maxData">Maximum value of data.</param>
    /// <param name="kernel">Pooling kernel size: (y, x) or (d, y, x)</param>
    /// <param name="poolType">Pooling type to be applied.</param>
    /// <param name="globalPool">Ignore kernel size, do global pooling based on current input feature map. </param>
    /// <param name="cudnnOff">Turn off cudnn pooling and use MXNet pooling operator. </param>
    /// <param name="poolingConvention">Pooling convention to be applied.</param>
    /// <param name="stride">Stride: for pooling (y, x) or (d, y, x). Defaults to 1 for each dimension.</param>
    /// <param name="pad">Pad for pooling: (y, x) or (d, y, x). Defaults to no padding.</param>
    /// <param name="pValue">Value of p for Lp pooling, can be 1 or 2, required for Lp Pooling.</param>
    /// <param name="countIncludePad">Only used for AvgPool, specify whether to count padding elements for averagecalculation. For example, with a 5*5 kernel on a 3*3 corner of a image,the sum of the 9 valid elements will be divided by 25 if this is set to true,or it will be divided by 9 if this is set to false. Defaults to true.</param>
    /// <param name="layout">Set layout for input and output. Empty for
    ///     default layout: NCW for 1d, NCHW for 2d and NCDHW for 3d.</param>
    static member ContribQuantizedPooling(data : NDArray, 
                                          minData : NDArray, 
                                          maxData : NDArray, 
                                          [<Optional>] kernel : int seq, 
                                          [<Optional>] poolType : PoolType, 
                                          [<Optional; DefaultParameterValue(false)>] globalPool : bool, 
                                          [<Optional; DefaultParameterValue(false)>] cudnnOff : bool, 
                                          [<Optional>] poolingConvention : PoolingConvention, 
                                          [<Optional>] stride : int seq, 
                                          [<Optional>] pad : int seq, 
                                          [<Optional>] pValue : int Nullable, 
                                          [<Optional>] countIncludePad : bool Nullable, 
                                          [<Optional>] layout : ContribQuantizedPoolingLayout) =
        let creator = AtomicSymbolCreator.FromName "_contrib_quantized_pooling"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; minData.NDArrayHandle; maxData.NDArrayHandle|]
                                                 [|"kernel"; "pool_type"; "global_pool"; "cudnn_off"; "pooling_convention"; "stride"; "pad"; "p_value"; "count_include_pad"; "layout"|]
                                                 [|(if isNull (kernel :> obj) then "[]" else kernel.ToString()); (if isNull (poolType :> obj) then "max" else poolType.ToString()); globalPool.ToString(); cudnnOff.ToString(); (if isNull (poolingConvention :> obj) then "valid" else poolingConvention.ToString()); (if isNull (stride :> obj) then "[]" else stride.ToString()); (if isNull (pad :> obj) then "[]" else pad.ToString()); pValue.ToString(); countIncludePad.ToString(); (if isNull (layout :> obj) then "None" else layout.ToString())|]
        outputs

    /// <summary>Pooling operator for input and output data type of int8.
    /// The input and output data comes with min and max thresholds for quantizing
    /// the float32 data into int8.
    /// 
    /// .. Note::
    ///     This operator only supports forward propogation. DO NOT use it in training.
    ///     This operator only supports `pool_type` of `avg` or `max`.
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\quantization\quantized_pooling.cc:L145</summary>
    /// <param name="data">Input data.</param>
    /// <param name="minData">Minimum value of data.</param>
    /// <param name="maxData">Maximum value of data.</param>
    /// <param name="kernel">Pooling kernel size: (y, x) or (d, y, x)</param>
    /// <param name="poolType">Pooling type to be applied.</param>
    /// <param name="globalPool">Ignore kernel size, do global pooling based on current input feature map. </param>
    /// <param name="cudnnOff">Turn off cudnn pooling and use MXNet pooling operator. </param>
    /// <param name="poolingConvention">Pooling convention to be applied.</param>
    /// <param name="stride">Stride: for pooling (y, x) or (d, y, x). Defaults to 1 for each dimension.</param>
    /// <param name="pad">Pad for pooling: (y, x) or (d, y, x). Defaults to no padding.</param>
    /// <param name="pValue">Value of p for Lp pooling, can be 1 or 2, required for Lp Pooling.</param>
    /// <param name="countIncludePad">Only used for AvgPool, specify whether to count padding elements for averagecalculation. For example, with a 5*5 kernel on a 3*3 corner of a image,the sum of the 9 valid elements will be divided by 25 if this is set to true,or it will be divided by 9 if this is set to false. Defaults to true.</param>
    /// <param name="layout">Set layout for input and output. Empty for
    ///     default layout: NCW for 1d, NCHW for 2d and NCDHW for 3d.</param>
    static member ContribQuantizedPooling(data : NDArray, 
                                          minData : NDArray, 
                                          maxData : NDArray, 
                                          ?kernel : int seq, 
                                          ?poolType : PoolType, 
                                          ?globalPool : bool, 
                                          ?cudnnOff : bool, 
                                          ?poolingConvention : PoolingConvention, 
                                          ?stride : int seq, 
                                          ?pad : int seq, 
                                          ?pValue : int, 
                                          ?countIncludePad : bool, 
                                          ?layout : ContribQuantizedPoolingLayout) =
        let creator = AtomicSymbolCreator.FromName "_contrib_quantized_pooling"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; minData.NDArrayHandle; maxData.NDArrayHandle|]
                                                 [|"kernel"; "pool_type"; "global_pool"; "cudnn_off"; "pooling_convention"; "stride"; "pad"; "p_value"; "count_include_pad"; "layout"|]
                                                 [|(match kernel with None -> "[]" | _ -> kernel.ToString()); (match poolType with None -> "max" | _ -> poolType.ToString()); (match globalPool with None -> "false" | _ -> globalPool.ToString()); (match cudnnOff with None -> "false" | _ -> cudnnOff.ToString()); (match poolingConvention with None -> "valid" | _ -> poolingConvention.ToString()); (match stride with None -> "[]" | _ -> stride.ToString()); (match pad with None -> "[]" | _ -> pad.ToString()); (match pValue with None -> "None" | _ -> pValue.ToString()); (match countIncludePad with None -> "None" | _ -> countIncludePad.ToString()); (match layout with None -> "None" | _ -> layout.ToString())|]
        outputs

    /// <summary>Given data that is quantized in int32 and the corresponding thresholds,
    /// requantize the data into int8 using min and max thresholds either calculated at runtime
    /// or from calibration. It&#39;s highly recommended to pre-calucate the min and max thresholds
    /// through calibration since it is able to save the runtime of the operator and improve the
    /// inference accuracy.
    /// 
    /// .. Note::
    ///     This operator only supports forward propogation. DO NOT use it in training.
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\quantization\requantize.cc:L60</summary>
    /// <param name="data">A ndarray/symbol of type `int32`</param>
    /// <param name="minRange">The original minimum scalar value in the form of float32 used for quantizing data into int32.</param>
    /// <param name="maxRange">The original maximum scalar value in the form of float32 used for quantizing data into int32.</param>
    /// <param name="outType">Output data type. `auto` can be specified to automatically determine output type according to min_calib_range.</param>
    /// <param name="minCalibRange">The minimum scalar value in the form of float32 obtained through calibration. If present, it will be used to requantize the int32 data into int8.</param>
    /// <param name="maxCalibRange">The maximum scalar value in the form of float32 obtained through calibration. If present, it will be used to requantize the int32 data into int8.</param>
    static member ContribRequantize(data : NDArray, 
                                    minRange : NDArray, 
                                    maxRange : NDArray, 
                                    [<Optional>] outType : ContribRequantizeOutType, 
                                    [<Optional>] minCalibRange : float Nullable, 
                                    [<Optional>] maxCalibRange : float Nullable) =
        let creator = AtomicSymbolCreator.FromName "_contrib_requantize"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; minRange.NDArrayHandle; maxRange.NDArrayHandle|]
                                                 [|"out_type"; "min_calib_range"; "max_calib_range"|]
                                                 [|(if isNull (outType :> obj) then "int8" else outType.ToString()); minCalibRange.ToString(); maxCalibRange.ToString()|]
        outputs

    /// <summary>Given data that is quantized in int32 and the corresponding thresholds,
    /// requantize the data into int8 using min and max thresholds either calculated at runtime
    /// or from calibration. It&#39;s highly recommended to pre-calucate the min and max thresholds
    /// through calibration since it is able to save the runtime of the operator and improve the
    /// inference accuracy.
    /// 
    /// .. Note::
    ///     This operator only supports forward propogation. DO NOT use it in training.
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\quantization\requantize.cc:L60</summary>
    /// <param name="data">A ndarray/symbol of type `int32`</param>
    /// <param name="minRange">The original minimum scalar value in the form of float32 used for quantizing data into int32.</param>
    /// <param name="maxRange">The original maximum scalar value in the form of float32 used for quantizing data into int32.</param>
    /// <param name="outType">Output data type. `auto` can be specified to automatically determine output type according to min_calib_range.</param>
    /// <param name="minCalibRange">The minimum scalar value in the form of float32 obtained through calibration. If present, it will be used to requantize the int32 data into int8.</param>
    /// <param name="maxCalibRange">The maximum scalar value in the form of float32 obtained through calibration. If present, it will be used to requantize the int32 data into int8.</param>
    static member ContribRequantize(data : NDArray, 
                                    minRange : NDArray, 
                                    maxRange : NDArray, 
                                    ?outType : ContribRequantizeOutType, 
                                    ?minCalibRange : float, 
                                    ?maxCalibRange : float) =
        let creator = AtomicSymbolCreator.FromName "_contrib_requantize"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; minRange.NDArrayHandle; maxRange.NDArrayHandle|]
                                                 [|"out_type"; "min_calib_range"; "max_calib_range"|]
                                                 [|(match outType with None -> "int8" | _ -> outType.ToString()); (match minCalibRange with None -> "None" | _ -> minCalibRange.ToString()); (match maxCalibRange with None -> "None" | _ -> maxCalibRange.ToString())|]
        outputs

    /// <summary>Concurrent sampling from multiple
    /// uniform distributions on the intervals given by *[low,high)*.
    /// 
    /// The parameters of the distributions are provided as input arrays.
    /// Let *[s]* be the shape of the input arrays, *n* be the dimension of *[s]*, *[t]*
    /// be the shape specified as the parameter of the operator, and *m* be the dimension
    /// of *[t]*. Then the output will be a *(n+m)*-dimensional array with shape *[s]x[t]*.
    /// 
    /// For any valid *n*-dimensional index *i* with respect to the input arrays, *output[i]*
    /// will be an *m*-dimensional array that holds randomly drawn samples from the distribution
    /// which is parameterized by the input values at index *i*. If the shape parameter of the
    /// operator is not set, then one sample will be drawn per distribution and the output array
    /// has the same shape as the input arrays.
    /// 
    /// Examples::
    /// 
    ///    low = [ 0.0, 2.5 ]
    ///    high = [ 1.0, 3.7 ]
    /// 
    ///    // Draw a single sample for each distribution
    ///    sample_uniform(low, high) = [ 0.40451524,  3.18687344]
    /// 
    ///    // Draw a vector containing two samples for each distribution
    ///    sample_uniform(low, high, shape=(2)) = [[ 0.40451524,  0.18017688],
    ///                                            [ 3.18687344,  3.68352246]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\random\multisample_op.cc:L277</summary>
    /// <param name="low">Lower bounds of the distributions.</param>
    /// <param name="shape">Shape to be sampled from each random distribution.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    /// <param name="high">Upper bounds of the distributions.</param>
    static member SampleUniform(low : NDArray, [<Optional>] shape : int seq, [<Optional>] dtype : SampleUniformDtype, high : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_sample_uniform"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|low.NDArrayHandle; high.NDArrayHandle|]
                                                 [|"shape"; "dtype"|]
                                                 [|(if isNull (shape :> obj) then "None" else shape.ToString()); (if isNull (dtype :> obj) then "None" else dtype.ToString())|]
        outputs

    /// <summary>Concurrent sampling from multiple
    /// normal distributions with parameters *mu* (mean) and *sigma* (standard deviation).
    /// 
    /// The parameters of the distributions are provided as input arrays.
    /// Let *[s]* be the shape of the input arrays, *n* be the dimension of *[s]*, *[t]*
    /// be the shape specified as the parameter of the operator, and *m* be the dimension
    /// of *[t]*. Then the output will be a *(n+m)*-dimensional array with shape *[s]x[t]*.
    /// 
    /// For any valid *n*-dimensional index *i* with respect to the input arrays, *output[i]*
    /// will be an *m*-dimensional array that holds randomly drawn samples from the distribution
    /// which is parameterized by the input values at index *i*. If the shape parameter of the
    /// operator is not set, then one sample will be drawn per distribution and the output array
    /// has the same shape as the input arrays.
    /// 
    /// Examples::
    /// 
    ///    mu = [ 0.0, 2.5 ]
    ///    sigma = [ 1.0, 3.7 ]
    /// 
    ///    // Draw a single sample for each distribution
    ///    sample_normal(mu, sigma) = [-0.56410581,  0.95934606]
    /// 
    ///    // Draw a vector containing two samples for each distribution
    ///    sample_normal(mu, sigma, shape=(2)) = [[-0.56410581,  0.2928229 ],
    ///                                           [ 0.95934606,  4.48287058]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\random\multisample_op.cc:L279</summary>
    /// <param name="mu">Means of the distributions.</param>
    /// <param name="shape">Shape to be sampled from each random distribution.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    /// <param name="sigma">Standard deviations of the distributions.</param>
    static member SampleNormal(mu : NDArray, [<Optional>] shape : int seq, [<Optional>] dtype : SampleNormalDtype, sigma : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_sample_normal"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|mu.NDArrayHandle; sigma.NDArrayHandle|]
                                                 [|"shape"; "dtype"|]
                                                 [|(if isNull (shape :> obj) then "None" else shape.ToString()); (if isNull (dtype :> obj) then "None" else dtype.ToString())|]
        outputs

    /// <summary>Concurrent sampling from multiple
    /// gamma distributions with parameters *alpha* (shape) and *beta* (scale).
    /// 
    /// The parameters of the distributions are provided as input arrays.
    /// Let *[s]* be the shape of the input arrays, *n* be the dimension of *[s]*, *[t]*
    /// be the shape specified as the parameter of the operator, and *m* be the dimension
    /// of *[t]*. Then the output will be a *(n+m)*-dimensional array with shape *[s]x[t]*.
    /// 
    /// For any valid *n*-dimensional index *i* with respect to the input arrays, *output[i]*
    /// will be an *m*-dimensional array that holds randomly drawn samples from the distribution
    /// which is parameterized by the input values at index *i*. If the shape parameter of the
    /// operator is not set, then one sample will be drawn per distribution and the output array
    /// has the same shape as the input arrays.
    /// 
    /// Examples::
    /// 
    ///    alpha = [ 0.0, 2.5 ]
    ///    beta = [ 1.0, 0.7 ]
    /// 
    ///    // Draw a single sample for each distribution
    ///    sample_gamma(alpha, beta) = [ 0.        ,  2.25797319]
    /// 
    ///    // Draw a vector containing two samples for each distribution
    ///    sample_gamma(alpha, beta, shape=(2)) = [[ 0.        ,  0.        ],
    ///                                            [ 2.25797319,  1.70734084]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\random\multisample_op.cc:L282</summary>
    /// <param name="alpha">Alpha (shape) parameters of the distributions.</param>
    /// <param name="shape">Shape to be sampled from each random distribution.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    /// <param name="beta">Beta (scale) parameters of the distributions.</param>
    static member SampleGamma(alpha : NDArray, [<Optional>] shape : int seq, [<Optional>] dtype : SampleGammaDtype, beta : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_sample_gamma"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|alpha.NDArrayHandle; beta.NDArrayHandle|]
                                                 [|"shape"; "dtype"|]
                                                 [|(if isNull (shape :> obj) then "None" else shape.ToString()); (if isNull (dtype :> obj) then "None" else dtype.ToString())|]
        outputs

    /// <summary>Concurrent sampling from multiple
    /// exponential distributions with parameters lambda (rate).
    /// 
    /// The parameters of the distributions are provided as an input array.
    /// Let *[s]* be the shape of the input array, *n* be the dimension of *[s]*, *[t]*
    /// be the shape specified as the parameter of the operator, and *m* be the dimension
    /// of *[t]*. Then the output will be a *(n+m)*-dimensional array with shape *[s]x[t]*.
    /// 
    /// For any valid *n*-dimensional index *i* with respect to the input array, *output[i]*
    /// will be an *m*-dimensional array that holds randomly drawn samples from the distribution
    /// which is parameterized by the input value at index *i*. If the shape parameter of the
    /// operator is not set, then one sample will be drawn per distribution and the output array
    /// has the same shape as the input array.
    /// 
    /// Examples::
    /// 
    ///    lam = [ 1.0, 8.5 ]
    /// 
    ///    // Draw a single sample for each distribution
    ///    sample_exponential(lam) = [ 0.51837951,  0.09994757]
    /// 
    ///    // Draw a vector containing two samples for each distribution
    ///    sample_exponential(lam, shape=(2)) = [[ 0.51837951,  0.19866663],
    ///                                          [ 0.09994757,  0.50447971]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\random\multisample_op.cc:L284</summary>
    /// <param name="lam">Lambda (rate) parameters of the distributions.</param>
    /// <param name="shape">Shape to be sampled from each random distribution.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    static member SampleExponential(lam : NDArray, [<Optional>] shape : int seq, [<Optional>] dtype : SampleExponentialDtype) =
        let creator = AtomicSymbolCreator.FromName "_sample_exponential"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lam.NDArrayHandle|]
                                                 [|"shape"; "dtype"|]
                                                 [|(if isNull (shape :> obj) then "None" else shape.ToString()); (if isNull (dtype :> obj) then "None" else dtype.ToString())|]
        outputs

    /// <summary>Concurrent sampling from multiple
    /// Poisson distributions with parameters lambda (rate).
    /// 
    /// The parameters of the distributions are provided as an input array.
    /// Let *[s]* be the shape of the input array, *n* be the dimension of *[s]*, *[t]*
    /// be the shape specified as the parameter of the operator, and *m* be the dimension
    /// of *[t]*. Then the output will be a *(n+m)*-dimensional array with shape *[s]x[t]*.
    /// 
    /// For any valid *n*-dimensional index *i* with respect to the input array, *output[i]*
    /// will be an *m*-dimensional array that holds randomly drawn samples from the distribution
    /// which is parameterized by the input value at index *i*. If the shape parameter of the
    /// operator is not set, then one sample will be drawn per distribution and the output array
    /// has the same shape as the input array.
    /// 
    /// Samples will always be returned as a floating point data type.
    /// 
    /// Examples::
    /// 
    ///    lam = [ 1.0, 8.5 ]
    /// 
    ///    // Draw a single sample for each distribution
    ///    sample_poisson(lam) = [  0.,  13.]
    /// 
    ///    // Draw a vector containing two samples for each distribution
    ///    sample_poisson(lam, shape=(2)) = [[  0.,   4.],
    ///                                      [ 13.,   8.]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\random\multisample_op.cc:L286</summary>
    /// <param name="lam">Lambda (rate) parameters of the distributions.</param>
    /// <param name="shape">Shape to be sampled from each random distribution.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    static member SamplePoisson(lam : NDArray, [<Optional>] shape : int seq, [<Optional>] dtype : SamplePoissonDtype) =
        let creator = AtomicSymbolCreator.FromName "_sample_poisson"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lam.NDArrayHandle|]
                                                 [|"shape"; "dtype"|]
                                                 [|(if isNull (shape :> obj) then "None" else shape.ToString()); (if isNull (dtype :> obj) then "None" else dtype.ToString())|]
        outputs

    /// <summary>Concurrent sampling from multiple
    /// negative binomial distributions with parameters *k* (failure limit) and *p* (failure probability).
    /// 
    /// The parameters of the distributions are provided as input arrays.
    /// Let *[s]* be the shape of the input arrays, *n* be the dimension of *[s]*, *[t]*
    /// be the shape specified as the parameter of the operator, and *m* be the dimension
    /// of *[t]*. Then the output will be a *(n+m)*-dimensional array with shape *[s]x[t]*.
    /// 
    /// For any valid *n*-dimensional index *i* with respect to the input arrays, *output[i]*
    /// will be an *m*-dimensional array that holds randomly drawn samples from the distribution
    /// which is parameterized by the input values at index *i*. If the shape parameter of the
    /// operator is not set, then one sample will be drawn per distribution and the output array
    /// has the same shape as the input arrays.
    /// 
    /// Samples will always be returned as a floating point data type.
    /// 
    /// Examples::
    /// 
    ///    k = [ 20, 49 ]
    ///    p = [ 0.4 , 0.77 ]
    /// 
    ///    // Draw a single sample for each distribution
    ///    sample_negative_binomial(k, p) = [ 15.,  16.]
    /// 
    ///    // Draw a vector containing two samples for each distribution
    ///    sample_negative_binomial(k, p, shape=(2)) = [[ 15.,  50.],
    ///                                                 [ 16.,  12.]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\random\multisample_op.cc:L289</summary>
    /// <param name="k">Limits of unsuccessful experiments.</param>
    /// <param name="shape">Shape to be sampled from each random distribution.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    /// <param name="p">Failure probabilities in each experiment.</param>
    static member SampleNegativeBinomial(k : NDArray, [<Optional>] shape : int seq, [<Optional>] dtype : SampleNegativeBinomialDtype, p : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_sample_negative_binomial"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|k.NDArrayHandle; p.NDArrayHandle|]
                                                 [|"shape"; "dtype"|]
                                                 [|(if isNull (shape :> obj) then "None" else shape.ToString()); (if isNull (dtype :> obj) then "None" else dtype.ToString())|]
        outputs

    /// <summary>Concurrent sampling from multiple
    /// generalized negative binomial distributions with parameters *mu* (mean) and *alpha* (dispersion).
    /// 
    /// The parameters of the distributions are provided as input arrays.
    /// Let *[s]* be the shape of the input arrays, *n* be the dimension of *[s]*, *[t]*
    /// be the shape specified as the parameter of the operator, and *m* be the dimension
    /// of *[t]*. Then the output will be a *(n+m)*-dimensional array with shape *[s]x[t]*.
    /// 
    /// For any valid *n*-dimensional index *i* with respect to the input arrays, *output[i]*
    /// will be an *m*-dimensional array that holds randomly drawn samples from the distribution
    /// which is parameterized by the input values at index *i*. If the shape parameter of the
    /// operator is not set, then one sample will be drawn per distribution and the output array
    /// has the same shape as the input arrays.
    /// 
    /// Samples will always be returned as a floating point data type.
    /// 
    /// Examples::
    /// 
    ///    mu = [ 2.0, 2.5 ]
    ///    alpha = [ 1.0, 0.1 ]
    /// 
    ///    // Draw a single sample for each distribution
    ///    sample_generalized_negative_binomial(mu, alpha) = [ 0.,  3.]
    /// 
    ///    // Draw a vector containing two samples for each distribution
    ///    sample_generalized_negative_binomial(mu, alpha, shape=(2)) = [[ 0.,  3.],
    ///                                                                  [ 3.,  1.]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\random\multisample_op.cc:L293</summary>
    /// <param name="mu">Means of the distributions.</param>
    /// <param name="shape">Shape to be sampled from each random distribution.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    /// <param name="alpha">Alpha (dispersion) parameters of the distributions.</param>
    static member SampleGeneralizedNegativeBinomial(mu : NDArray, [<Optional>] shape : int seq, [<Optional>] dtype : SampleGeneralizedNegativeBinomialDtype, alpha : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_sample_generalized_negative_binomial"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|mu.NDArrayHandle; alpha.NDArrayHandle|]
                                                 [|"shape"; "dtype"|]
                                                 [|(if isNull (shape :> obj) then "None" else shape.ToString()); (if isNull (dtype :> obj) then "None" else dtype.ToString())|]
        outputs

    /// <summary>Concurrent sampling from multiple multinomial distributions.
    /// 
    /// *data* is an *n* dimensional array whose last dimension has length *k*, where
    /// *k* is the number of possible outcomes of each multinomial distribution. This
    /// operator will draw *shape* samples from each distribution. If shape is empty
    /// one sample will be drawn from each distribution.
    /// 
    /// If *get_prob* is true, a second array containing log likelihood of the drawn
    /// samples will also be returned. This is usually used for reinforcement learning
    /// where you can provide reward as head gradient for this array to estimate
    /// gradient.
    /// 
    /// Note that the input distribution must be normalized, i.e. *data* must sum to
    /// 1 along its last axis.
    /// 
    /// Examples::
    /// 
    ///    probs = [[0, 0.1, 0.2, 0.3, 0.4], [0.4, 0.3, 0.2, 0.1, 0]]
    /// 
    ///    // Draw a single sample for each distribution
    ///    sample_multinomial(probs) = [3, 0]
    /// 
    ///    // Draw a vector containing two samples for each distribution
    ///    sample_multinomial(probs, shape=(2)) = [[4, 2],
    ///                                            [0, 0]]
    /// 
    ///    // requests log likelihood
    ///    sample_multinomial(probs, get_prob=True) = [2, 1], [0.2, 0.3]
    /// </summary>
    /// <param name="data">Distribution probabilities. Must sum to one on the last axis.</param>
    /// <param name="shape">Shape to be sampled from each random distribution.</param>
    /// <param name="getProb">Whether to also return the log probability of sampled result. This is usually used for differentiating through stochastic variables, e.g. in reinforcement learning.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred.</param>
    static member SampleMultinomial(data : NDArray, [<Optional>] shape : int seq, [<Optional; DefaultParameterValue(false)>] getProb : bool, [<Optional>] dtype : SampleMultinomialDtype) =
        let creator = AtomicSymbolCreator.FromName "_sample_multinomial"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"shape"; "get_prob"; "dtype"|]
                                                 [|(if isNull (shape :> obj) then "[]" else shape.ToString()); getProb.ToString(); (if isNull (dtype :> obj) then "int32" else dtype.ToString())|]
        outputs

    /// <summary>Draw random samples from a uniform distribution according to the input array shape.
    /// 
    /// Samples are uniformly distributed over the half-open interval *[low, high)*
    /// (includes *low*, but excludes *high*).
    /// 
    /// Example::
    /// 
    ///    uniform(low=0, high=1, data=ones(2,2)) = [[ 0.60276335,  0.85794562],
    ///                                              [ 0.54488319,  0.84725171]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\random\sample_op.cc:L208</summary>
    /// <param name="low">Lower bound of the distribution.</param>
    /// <param name="high">Upper bound of the distribution.</param>
    /// <param name="data">The input</param>
    static member RandomUniformLike([<Optional; DefaultParameterValue(0.0)>] low : float, [<Optional; DefaultParameterValue(1.0)>] high : float, data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_random_uniform_like"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"low"; "high"|]
                                                 [|low.ToString(); high.ToString()|]
        outputs

    /// <summary>Draw random samples from a normal (Gaussian) distribution according to the input array shape.
    /// 
    /// Samples are distributed according to a normal distribution parametrized by *loc* (mean) and *scale*
    /// (standard deviation).
    /// 
    /// Example::
    /// 
    ///    normal(loc=0, scale=1, data=ones(2,2)) = [[ 1.89171135, -1.16881478],
    ///                                              [-1.23474145,  1.55807114]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\random\sample_op.cc:L220</summary>
    /// <param name="loc">Mean of the distribution.</param>
    /// <param name="scale">Standard deviation of the distribution.</param>
    /// <param name="data">The input</param>
    static member RandomNormalLike([<Optional; DefaultParameterValue(0.0)>] loc : float, [<Optional; DefaultParameterValue(1.0)>] scale : float, data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_random_normal_like"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"loc"; "scale"|]
                                                 [|loc.ToString(); scale.ToString()|]
        outputs

    /// <summary>Draw random samples from a gamma distribution according to the input array shape.
    /// 
    /// Samples are distributed according to a gamma distribution parametrized by *alpha* (shape) and *beta* (scale).
    /// 
    /// Example::
    /// 
    ///    gamma(alpha=9, beta=0.5, data=ones(2,2)) = [[ 7.10486984,  3.37695289],
    ///                                                [ 3.91697288,  3.65933681]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\random\sample_op.cc:L231</summary>
    /// <param name="alpha">Alpha parameter (shape) of the gamma distribution.</param>
    /// <param name="beta">Beta parameter (scale) of the gamma distribution.</param>
    /// <param name="data">The input</param>
    static member RandomGammaLike([<Optional; DefaultParameterValue(1.0)>] alpha : float, [<Optional; DefaultParameterValue(1.0)>] beta : float, data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_random_gamma_like"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"alpha"; "beta"|]
                                                 [|alpha.ToString(); beta.ToString()|]
        outputs

    /// <summary>Draw random samples from an exponential distribution according to the input array shape.
    /// 
    /// Samples are distributed according to an exponential distribution parametrized by *lambda* (rate).
    /// 
    /// Example::
    /// 
    ///    exponential(lam=4, data=ones(2,2)) = [[ 0.0097189 ,  0.08999364],
    ///                                          [ 0.04146638,  0.31715935]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\random\sample_op.cc:L242</summary>
    /// <param name="lam">Lambda parameter (rate) of the exponential distribution.</param>
    /// <param name="data">The input</param>
    static member RandomExponentialLike([<Optional; DefaultParameterValue(1.0)>] lam : float, data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_random_exponential_like"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"lam"|]
                                                 [|lam.ToString()|]
        outputs

    /// <summary>Draw random samples from a Poisson distribution according to the input array shape.
    /// 
    /// Samples are distributed according to a Poisson distribution parametrized by *lambda* (rate).
    /// Samples will always be returned as a floating point data type.
    /// 
    /// Example::
    /// 
    ///    poisson(lam=4, data=ones(2,2)) = [[ 5.,  2.],
    ///                                      [ 4.,  6.]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\random\sample_op.cc:L254</summary>
    /// <param name="lam">Lambda parameter (rate) of the Poisson distribution.</param>
    /// <param name="data">The input</param>
    static member RandomPoissonLike([<Optional; DefaultParameterValue(1.0)>] lam : float, data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_random_poisson_like"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"lam"|]
                                                 [|lam.ToString()|]
        outputs

    /// <summary>Draw random samples from a negative binomial distribution according to the input array shape.
    /// 
    /// Samples are distributed according to a negative binomial distribution parametrized by
    /// *k* (limit of unsuccessful experiments) and *p* (failure probability in each experiment).
    /// Samples will always be returned as a floating point data type.
    /// 
    /// Example::
    /// 
    ///    negative_binomial(k=3, p=0.4, data=ones(2,2)) = [[ 4.,  7.],
    ///                                                     [ 2.,  5.]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\random\sample_op.cc:L267</summary>
    /// <param name="k">Limit of unsuccessful experiments.</param>
    /// <param name="p">Failure probability in each experiment.</param>
    /// <param name="data">The input</param>
    static member RandomNegativeBinomialLike([<Optional; DefaultParameterValue(1)>] k : int, [<Optional; DefaultParameterValue(1.0)>] p : float, data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_random_negative_binomial_like"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"k"; "p"|]
                                                 [|k.ToString(); p.ToString()|]
        outputs

    /// <summary>Draw random samples from a generalized negative binomial distribution according to the
    /// input array shape.
    /// 
    /// Samples are distributed according to a generalized negative binomial distribution parametrized by
    /// *mu* (mean) and *alpha* (dispersion). *alpha* is defined as *1/k* where *k* is the failure limit of the
    /// number of unsuccessful experiments (generalized to real numbers).
    /// Samples will always be returned as a floating point data type.
    /// 
    /// Example::
    /// 
    ///    generalized_negative_binomial(mu=2.0, alpha=0.3, data=ones(2,2)) = [[ 2.,  1.],
    ///                                                                        [ 6.,  4.]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\random\sample_op.cc:L283</summary>
    /// <param name="mu">Mean of the negative binomial distribution.</param>
    /// <param name="alpha">Alpha (dispersion) parameter of the negative binomial distribution.</param>
    /// <param name="data">The input</param>
    static member RandomGeneralizedNegativeBinomialLike([<Optional; DefaultParameterValue(1.0)>] mu : float, [<Optional; DefaultParameterValue(1.0)>] alpha : float, data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_random_generalized_negative_binomial_like"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"mu"; "alpha"|]
                                                 [|mu.ToString(); alpha.ToString()|]
        outputs

    /// <summary>Randomly shuffle the elements.
    /// 
    /// This shuffles the array along the first axis.
    /// The order of the elements in each subarray does not change.
    /// For example, if a 2D array is given, the order of the rows randomly changes,
    /// but the order of the elements in each row does not change.
    /// </summary>
    /// <param name="data">Data to be shuffled.</param>
    static member Shuffle(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_shuffle"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Computes and optimizes for squared loss during backward propagation.
    /// Just outputs ``data`` during forward propagation.
    /// 
    /// If :math:`\hat{y}_i` is the predicted value of the i-th sample, and :math:`y_i` is the corresponding target value,
    /// then the squared loss estimated over :math:`n` samples is defined as
    /// 
    /// :math:`\text{SquaredLoss}(\textbf{Y}, \hat{\textbf{Y}} ) = \frac{1}{n} \sum_{i=0}^{n-1} \lVert  \textbf{y}_i - \hat{\textbf{y}}_i  \rVert_2`
    /// 
    /// .. note::
    ///    Use the LinearRegressionOutput as the final output layer of a net.
    /// 
    /// The storage type of ``label`` can be ``default`` or ``csr``
    /// 
    /// - LinearRegressionOutput(default, default) = default
    /// - LinearRegressionOutput(default, csr) = default
    /// 
    /// By default, gradients of this loss function are scaled by factor `1/m`, where m is the number of regression outputs of a training example.
    /// The parameter `grad_scale` can be used to change this scale to `grad_scale/m`.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\regression_output.cc:L92</summary>
    /// <param name="data">Input data to the function.</param>
    /// <param name="label">Input label to the function.</param>
    /// <param name="gradScale">Scale the gradient by a float factor</param>
    static member LinearRegressionOutput(data : NDArray, label : NDArray, [<Optional; DefaultParameterValue(1.0)>] gradScale : float) =
        let creator = AtomicSymbolCreator.FromName "LinearRegressionOutput"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; label.NDArrayHandle|]
                                                 [|"grad_scale"|]
                                                 [|gradScale.ToString()|]
        outputs

    /// <summary>Computes mean absolute error of the input.
    /// 
    /// MAE is a risk metric corresponding to the expected value of the absolute error.
    /// 
    /// If :math:`\hat{y}_i` is the predicted value of the i-th sample, and :math:`y_i` is the corresponding target value,
    /// then the mean absolute error (MAE) estimated over :math:`n` samples is defined as
    /// 
    /// :math:`\text{MAE}(\textbf{Y}, \hat{\textbf{Y}} ) = \frac{1}{n} \sum_{i=0}^{n-1} \lVert \textbf{y}_i - \hat{\textbf{y}}_i \rVert_1`
    /// 
    /// .. note::
    ///    Use the MAERegressionOutput as the final output layer of a net.
    /// 
    /// The storage type of ``label`` can be ``default`` or ``csr``
    /// 
    /// - MAERegressionOutput(default, default) = default
    /// - MAERegressionOutput(default, csr) = default
    /// 
    /// By default, gradients of this loss function are scaled by factor `1/m`, where m is the number of regression outputs of a training example.
    /// The parameter `grad_scale` can be used to change this scale to `grad_scale/m`.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\regression_output.cc:L120</summary>
    /// <param name="data">Input data to the function.</param>
    /// <param name="label">Input label to the function.</param>
    /// <param name="gradScale">Scale the gradient by a float factor</param>
    static member MAERegressionOutput(data : NDArray, label : NDArray, [<Optional; DefaultParameterValue(1.0)>] gradScale : float) =
        let creator = AtomicSymbolCreator.FromName "MAERegressionOutput"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; label.NDArrayHandle|]
                                                 [|"grad_scale"|]
                                                 [|gradScale.ToString()|]
        outputs

    /// <summary>Applies a logistic function to the input.
    /// 
    /// The logistic function, also known as the sigmoid function, is computed as
    /// :math:`\frac{1}{1+exp(-\textbf{x})}`.
    /// 
    /// Commonly, the sigmoid is used to squash the real-valued output of a linear model
    /// :math:`wTx+b` into the [0,1] range so that it can be interpreted as a probability.
    /// It is suitable for binary classification or probability prediction tasks.
    /// 
    /// .. note::
    ///    Use the LogisticRegressionOutput as the final output layer of a net.
    /// 
    /// The storage type of ``label`` can be ``default`` or ``csr``
    /// 
    /// - LogisticRegressionOutput(default, default) = default
    /// - LogisticRegressionOutput(default, csr) = default
    /// 
    /// The loss function used is the Binary Cross Entropy Loss:
    /// 
    /// :math:`-{(y\log(p) + (1 - y)\log(1 - p))}`
    /// 
    /// Where `y` is the ground truth probability of positive outcome for a given example, and `p` the probability predicted by the model. By default, gradients of this loss function are scaled by factor `1/m`, where m is the number of regression outputs of a training example.
    /// The parameter `grad_scale` can be used to change this scale to `grad_scale/m`.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\regression_output.cc:L152</summary>
    /// <param name="data">Input data to the function.</param>
    /// <param name="label">Input label to the function.</param>
    /// <param name="gradScale">Scale the gradient by a float factor</param>
    static member LogisticRegressionOutput(data : NDArray, label : NDArray, [<Optional; DefaultParameterValue(1.0)>] gradScale : float) =
        let creator = AtomicSymbolCreator.FromName "LogisticRegressionOutput"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; label.NDArrayHandle|]
                                                 [|"grad_scale"|]
                                                 [|gradScale.ToString()|]
        outputs

    /// <summary>Applies recurrent layers to input data. Currently, vanilla RNN, LSTM and GRU are
    /// implemented, with both multi-layer and bidirectional support.
    /// 
    /// When the input data is of type float32 and the environment variables MXNET_CUDA_ALLOW_TENSOR_CORE
    /// and MXNET_CUDA_TENSOR_OP_MATH_ALLOW_CONVERSION are set to 1, this operator will try to use
    /// pseudo-float16 precision (float32 math with float16 I/O) precision in order to use
    /// Tensor Cores on suitable NVIDIA GPUs. This can sometimes give significant speedups.
    /// 
    /// **Vanilla RNN**
    /// 
    /// Applies a single-gate recurrent layer to input X. Two kinds of activation function are supported:
    /// ReLU and Tanh.
    /// 
    /// With ReLU activation function:
    /// 
    /// .. math::
    ///     h_t = relu(W_{ih} * x_t + b_{ih}  +  W_{hh} * h_{(t-1)} + b_{hh})
    /// 
    /// With Tanh activtion function:
    /// 
    /// .. math::
    ///     h_t = \tanh(W_{ih} * x_t + b_{ih}  +  W_{hh} * h_{(t-1)} + b_{hh})
    /// 
    /// Reference paper: Finding structure in time - Elman, 1988.
    /// https://crl.ucsd.edu/~elman/Papers/fsit.pdf
    /// 
    /// **LSTM**
    /// 
    /// Long Short-Term Memory - Hochreiter, 1997. http://www.bioinf.jku.at/publications/older/2604.pdf
    /// 
    /// .. math::
    ///   \begin{array}{ll}
    ///             i_t = \mathrm{sigmoid}(W_{ii} x_t + b_{ii} + W_{hi} h_{(t-1)} + b_{hi}) \\
    ///             f_t = \mathrm{sigmoid}(W_{if} x_t + b_{if} + W_{hf} h_{(t-1)} + b_{hf}) \\
    ///             g_t = \tanh(W_{ig} x_t + b_{ig} + W_{hc} h_{(t-1)} + b_{hg}) \\
    ///             o_t = \mathrm{sigmoid}(W_{io} x_t + b_{io} + W_{ho} h_{(t-1)} + b_{ho}) \\
    ///             c_t = f_t * c_{(t-1)} + i_t * g_t \\
    ///             h_t = o_t * \tanh(c_t)
    ///             \end{array}
    /// 
    /// **GRU**
    /// 
    /// Gated Recurrent Unit - Cho et al. 2014. http://arxiv.org/abs/1406.1078
    /// 
    /// The definition of GRU here is slightly different from paper but compatible with CUDNN.
    /// 
    /// .. math::
    ///   \begin{array}{ll}
    ///             r_t = \mathrm{sigmoid}(W_{ir} x_t + b_{ir} + W_{hr} h_{(t-1)} + b_{hr}) \\
    ///             z_t = \mathrm{sigmoid}(W_{iz} x_t + b_{iz} + W_{hz} h_{(t-1)} + b_{hz}) \\
    ///             n_t = \tanh(W_{in} x_t + b_{in} + r_t * (W_{hn} h_{(t-1)}+ b_{hn})) \\
    ///             h_t = (1 - z_t) * n_t + z_t * h_{(t-1)} \\
    ///             \end{array}
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\rnn.cc:L690</summary>
    /// <param name="data">Input data to RNN</param>
    /// <param name="parameters">Vector of all RNN trainable parameters concatenated</param>
    /// <param name="state">initial hidden state of the RNN</param>
    /// <param name="stateCell">initial cell state for LSTM networks (only for LSTM)</param>
    /// <param name="sequenceLength">Vector of valid sequence lengths for each element in batch. (Only used if use_sequence_length kwarg is True)</param>
    /// <param name="stateSize">size of the state for each layer</param>
    /// <param name="numLayers">number of stacked layers</param>
    /// <param name="mode">the type of RNN to compute</param>
    /// <param name="projectionSize">size of project size</param>
    /// <param name="lstmStateClipMin">Minimum clip value of LSTM states. This option must be used together with lstm_state_clip_max.</param>
    /// <param name="lstmStateClipMax">Maximum clip value of LSTM states. This option must be used together with lstm_state_clip_min.</param>
    /// <param name="bidirectional">whether to use bidirectional recurrent layers</param>
    /// <param name="p">drop rate of the dropout on the outputs of each RNN layer, except the last layer.</param>
    /// <param name="stateOutputs">Whether to have the states as symbol outputs.</param>
    /// <param name="lstmStateClipNan">Whether to stop NaN from propagating in state by clipping it to min/max. If clipping range is not specified, this option is ignored.</param>
    /// <param name="useSequenceLength">If set to true, this layer takes in an extra input parameter `sequence_length` to specify variable length sequence</param>
    static member RNN(data : NDArray, 
                      parameters : NDArray, 
                      state : NDArray, 
                      stateCell : NDArray, 
                      sequenceLength : NDArray, 
                      stateSize : int, 
                      numLayers : int, 
                      mode : RNNMode, 
                      [<Optional>] projectionSize : int Nullable, 
                      [<Optional>] lstmStateClipMin : float Nullable, 
                      [<Optional>] lstmStateClipMax : float Nullable, 
                      [<Optional; DefaultParameterValue(false)>] bidirectional : bool, 
                      [<Optional; DefaultParameterValue(0.0)>] p : float, 
                      [<Optional; DefaultParameterValue(false)>] stateOutputs : bool, 
                      [<Optional; DefaultParameterValue(false)>] lstmStateClipNan : bool, 
                      [<Optional; DefaultParameterValue(false)>] useSequenceLength : bool) =
        let creator = AtomicSymbolCreator.FromName "RNN"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; parameters.NDArrayHandle; state.NDArrayHandle; stateCell.NDArrayHandle; sequenceLength.NDArrayHandle|]
                                                 [|"state_size"; "num_layers"; "mode"; "projection_size"; "lstm_state_clip_min"; "lstm_state_clip_max"; "bidirectional"; "p"; "state_outputs"; "lstm_state_clip_nan"; "use_sequence_length"|]
                                                 [|stateSize.ToString(); numLayers.ToString(); mode.ToString(); projectionSize.ToString(); lstmStateClipMin.ToString(); lstmStateClipMax.ToString(); bidirectional.ToString(); p.ToString(); stateOutputs.ToString(); lstmStateClipNan.ToString(); useSequenceLength.ToString()|]
        outputs

    /// <summary>Applies recurrent layers to input data. Currently, vanilla RNN, LSTM and GRU are
    /// implemented, with both multi-layer and bidirectional support.
    /// 
    /// When the input data is of type float32 and the environment variables MXNET_CUDA_ALLOW_TENSOR_CORE
    /// and MXNET_CUDA_TENSOR_OP_MATH_ALLOW_CONVERSION are set to 1, this operator will try to use
    /// pseudo-float16 precision (float32 math with float16 I/O) precision in order to use
    /// Tensor Cores on suitable NVIDIA GPUs. This can sometimes give significant speedups.
    /// 
    /// **Vanilla RNN**
    /// 
    /// Applies a single-gate recurrent layer to input X. Two kinds of activation function are supported:
    /// ReLU and Tanh.
    /// 
    /// With ReLU activation function:
    /// 
    /// .. math::
    ///     h_t = relu(W_{ih} * x_t + b_{ih}  +  W_{hh} * h_{(t-1)} + b_{hh})
    /// 
    /// With Tanh activtion function:
    /// 
    /// .. math::
    ///     h_t = \tanh(W_{ih} * x_t + b_{ih}  +  W_{hh} * h_{(t-1)} + b_{hh})
    /// 
    /// Reference paper: Finding structure in time - Elman, 1988.
    /// https://crl.ucsd.edu/~elman/Papers/fsit.pdf
    /// 
    /// **LSTM**
    /// 
    /// Long Short-Term Memory - Hochreiter, 1997. http://www.bioinf.jku.at/publications/older/2604.pdf
    /// 
    /// .. math::
    ///   \begin{array}{ll}
    ///             i_t = \mathrm{sigmoid}(W_{ii} x_t + b_{ii} + W_{hi} h_{(t-1)} + b_{hi}) \\
    ///             f_t = \mathrm{sigmoid}(W_{if} x_t + b_{if} + W_{hf} h_{(t-1)} + b_{hf}) \\
    ///             g_t = \tanh(W_{ig} x_t + b_{ig} + W_{hc} h_{(t-1)} + b_{hg}) \\
    ///             o_t = \mathrm{sigmoid}(W_{io} x_t + b_{io} + W_{ho} h_{(t-1)} + b_{ho}) \\
    ///             c_t = f_t * c_{(t-1)} + i_t * g_t \\
    ///             h_t = o_t * \tanh(c_t)
    ///             \end{array}
    /// 
    /// **GRU**
    /// 
    /// Gated Recurrent Unit - Cho et al. 2014. http://arxiv.org/abs/1406.1078
    /// 
    /// The definition of GRU here is slightly different from paper but compatible with CUDNN.
    /// 
    /// .. math::
    ///   \begin{array}{ll}
    ///             r_t = \mathrm{sigmoid}(W_{ir} x_t + b_{ir} + W_{hr} h_{(t-1)} + b_{hr}) \\
    ///             z_t = \mathrm{sigmoid}(W_{iz} x_t + b_{iz} + W_{hz} h_{(t-1)} + b_{hz}) \\
    ///             n_t = \tanh(W_{in} x_t + b_{in} + r_t * (W_{hn} h_{(t-1)}+ b_{hn})) \\
    ///             h_t = (1 - z_t) * n_t + z_t * h_{(t-1)} \\
    ///             \end{array}
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\rnn.cc:L690</summary>
    /// <param name="data">Input data to RNN</param>
    /// <param name="parameters">Vector of all RNN trainable parameters concatenated</param>
    /// <param name="state">initial hidden state of the RNN</param>
    /// <param name="stateCell">initial cell state for LSTM networks (only for LSTM)</param>
    /// <param name="sequenceLength">Vector of valid sequence lengths for each element in batch. (Only used if use_sequence_length kwarg is True)</param>
    /// <param name="stateSize">size of the state for each layer</param>
    /// <param name="numLayers">number of stacked layers</param>
    /// <param name="mode">the type of RNN to compute</param>
    /// <param name="projectionSize">size of project size</param>
    /// <param name="lstmStateClipMin">Minimum clip value of LSTM states. This option must be used together with lstm_state_clip_max.</param>
    /// <param name="lstmStateClipMax">Maximum clip value of LSTM states. This option must be used together with lstm_state_clip_min.</param>
    /// <param name="bidirectional">whether to use bidirectional recurrent layers</param>
    /// <param name="p">drop rate of the dropout on the outputs of each RNN layer, except the last layer.</param>
    /// <param name="stateOutputs">Whether to have the states as symbol outputs.</param>
    /// <param name="lstmStateClipNan">Whether to stop NaN from propagating in state by clipping it to min/max. If clipping range is not specified, this option is ignored.</param>
    /// <param name="useSequenceLength">If set to true, this layer takes in an extra input parameter `sequence_length` to specify variable length sequence</param>
    static member RNN(data : NDArray, 
                      parameters : NDArray, 
                      state : NDArray, 
                      stateCell : NDArray, 
                      sequenceLength : NDArray, 
                      stateSize : int, 
                      numLayers : int, 
                      mode : RNNMode, 
                      ?projectionSize : int, 
                      ?lstmStateClipMin : float, 
                      ?lstmStateClipMax : float, 
                      ?bidirectional : bool, 
                      ?p : float, 
                      ?stateOutputs : bool, 
                      ?lstmStateClipNan : bool, 
                      ?useSequenceLength : bool) =
        let creator = AtomicSymbolCreator.FromName "RNN"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; parameters.NDArrayHandle; state.NDArrayHandle; stateCell.NDArrayHandle; sequenceLength.NDArrayHandle|]
                                                 [|"state_size"; "num_layers"; "mode"; "projection_size"; "lstm_state_clip_min"; "lstm_state_clip_max"; "bidirectional"; "p"; "state_outputs"; "lstm_state_clip_nan"; "use_sequence_length"|]
                                                 [|stateSize.ToString(); numLayers.ToString(); mode.ToString(); (match projectionSize with None -> "None" | _ -> projectionSize.ToString()); (match lstmStateClipMin with None -> "None" | _ -> lstmStateClipMin.ToString()); (match lstmStateClipMax with None -> "None" | _ -> lstmStateClipMax.ToString()); (match bidirectional with None -> "false" | _ -> bidirectional.ToString()); (match p with None -> "0.0" | _ -> p.ToString()); (match stateOutputs with None -> "false" | _ -> stateOutputs.ToString()); (match lstmStateClipNan with None -> "false" | _ -> lstmStateClipNan.ToString()); (match useSequenceLength with None -> "false" | _ -> useSequenceLength.ToString())|]
        outputs

    /// <summary>Splits an array along a particular axis into multiple sub-arrays.
    /// 
    /// .. note:: ``SliceChannel`` is deprecated. Use ``split`` instead.
    /// 
    /// **Note** that `num_outputs` should evenly divide the length of the axis
    /// along which to split the array.
    /// 
    /// Example::
    /// 
    ///    x  = [[[ 1.]
    ///           [ 2.]]
    ///          [[ 3.]
    ///           [ 4.]]
    ///          [[ 5.]
    ///           [ 6.]]]
    ///    x.shape = (3, 2, 1)
    /// 
    ///    y = split(x, axis=1, num_outputs=2) // a list of 2 arrays with shape (3, 1, 1)
    ///    y = [[[ 1.]]
    ///         [[ 3.]]
    ///         [[ 5.]]]
    /// 
    ///        [[[ 2.]]
    ///         [[ 4.]]
    ///         [[ 6.]]]
    /// 
    ///    y[0].shape = (3, 1, 1)
    /// 
    ///    z = split(x, axis=0, num_outputs=3) // a list of 3 arrays with shape (1, 2, 1)
    ///    z = [[[ 1.]
    ///          [ 2.]]]
    /// 
    ///        [[[ 3.]
    ///          [ 4.]]]
    /// 
    ///        [[[ 5.]
    ///          [ 6.]]]
    /// 
    ///    z[0].shape = (1, 2, 1)
    /// 
    /// `squeeze_axis=1` removes the axis with length 1 from the shapes of the output arrays.
    /// **Note** that setting `squeeze_axis` to ``1`` removes axis with length 1 only
    /// along the `axis` which it is split.
    /// Also `squeeze_axis` can be set to true only if ``input.shape[axis] == num_outputs``.
    /// 
    /// Example::
    /// 
    ///    z = split(x, axis=0, num_outputs=3, squeeze_axis=1) // a list of 3 arrays with shape (2, 1)
    ///    z = [[ 1.]
    ///         [ 2.]]
    /// 
    ///        [[ 3.]
    ///         [ 4.]]
    /// 
    ///        [[ 5.]
    ///         [ 6.]]
    ///    z[0].shape = (2 ,1 )
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\slice_channel.cc:L107</summary>
    /// <param name="data">The input</param>
    /// <param name="numOutputs">Number of splits. Note that this should evenly divide the length of the `axis`.</param>
    /// <param name="axis">Axis along which to split.</param>
    /// <param name="squeezeAxis">If true, Removes the axis with length 1 from the shapes of the output arrays. **Note** that setting `squeeze_axis` to ``true`` removes axis with length 1 only along the `axis` which it is split. Also `squeeze_axis` can be set to ``true`` only if ``input.shape[axis] == num_outputs``.</param>
    static member SliceChannel(data : NDArray, numOutputs : int, [<Optional; DefaultParameterValue(1)>] axis : int, [<Optional; DefaultParameterValue(false)>] squeezeAxis : bool) =
        let creator = AtomicSymbolCreator.FromName "SliceChannel"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"num_outputs"; "axis"; "squeeze_axis"|]
                                                 [|numOutputs.ToString(); axis.ToString(); squeezeAxis.ToString()|]
        outputs

    /// <summary>Computes the gradient of cross entropy loss with respect to softmax output.
    /// 
    /// - This operator computes the gradient in two steps.
    ///   The cross entropy loss does not actually need to be computed.
    /// 
    ///   - Applies softmax function on the input array.
    ///   - Computes and returns the gradient of cross entropy loss w.r.t. the softmax output.
    /// 
    /// - The softmax function, cross entropy loss and gradient is given by:
    /// 
    ///   - Softmax Function:
    /// 
    ///     .. math:: \text{softmax}(x)_i = \frac{exp(x_i)}{\sum_j exp(x_j)}
    /// 
    ///   - Cross Entropy Function:
    /// 
    ///     .. math:: \text{CE(label, output)} = - \sum_i \text{label}_i \log(\text{output}_i)
    /// 
    ///   - The gradient of cross entropy loss w.r.t softmax output:
    /// 
    ///     .. math:: \text{gradient} = \text{output} - \text{label}
    /// 
    /// - During forward propagation, the softmax function is computed for each instance in the input array.
    /// 
    ///   For general *N*-D input arrays with shape :math:`(d_1, d_2, ..., d_n)`. The size is
    ///   :math:`s=d_1 \cdot d_2 \cdot \cdot \cdot d_n`. We can use the parameters `preserve_shape`
    ///   and `multi_output` to specify the way to compute softmax:
    /// 
    ///   - By default, `preserve_shape` is ``false``. This operator will reshape the input array
    ///     into a 2-D array with shape :math:`(d_1, \frac{s}{d_1})` and then compute the softmax function for
    ///     each row in the reshaped array, and afterwards reshape it back to the original shape
    ///     :math:`(d_1, d_2, ..., d_n)`.
    ///   - If `preserve_shape` is ``true``, the softmax function will be computed along
    ///     the last axis (`axis` = ``-1``).
    ///   - If `multi_output` is ``true``, the softmax function will be computed along
    ///     the second axis (`axis` = ``1``).
    /// 
    /// - During backward propagation, the gradient of cross-entropy loss w.r.t softmax output array is computed.
    ///   The provided label can be a one-hot label array or a probability label array.
    /// 
    ///   - If the parameter `use_ignore` is ``true``, `ignore_label` can specify input instances
    ///     with a particular label to be ignored during backward propagation. **This has no effect when
    ///     softmax `output` has same shape as `label`**.
    /// 
    ///     Example::
    /// 
    ///       data = [[1,2,3,4],[2,2,2,2],[3,3,3,3],[4,4,4,4]]
    ///       label = [1,0,2,3]
    ///       ignore_label = 1
    ///       SoftmaxOutput(data=data, label = label,\
    ///                     multi_output=true, use_ignore=true,\
    ///                     ignore_label=ignore_label)
    ///       ## forward softmax output
    ///       [[ 0.0320586   0.08714432  0.23688284  0.64391428]
    ///        [ 0.25        0.25        0.25        0.25      ]
    ///        [ 0.25        0.25        0.25        0.25      ]
    ///        [ 0.25        0.25        0.25        0.25      ]]
    ///       ## backward gradient output
    ///       [[ 0.    0.    0.    0.  ]
    ///        [-0.75  0.25  0.25  0.25]
    ///        [ 0.25  0.25 -0.75  0.25]
    ///        [ 0.25  0.25  0.25 -0.75]]
    ///       ## notice that the first row is all 0 because label[0] is 1, which is equal to ignore_label.
    /// 
    ///   - The parameter `grad_scale` can be used to rescale the gradient, which is often used to
    ///     give each loss function different weights.
    /// 
    ///   - This operator also supports various ways to normalize the gradient by `normalization`,
    ///     The `normalization` is applied if softmax output has different shape than the labels.
    ///     The `normalization` mode can be set to the followings:
    /// 
    ///     - ``&#39;null&#39;``: do nothing.
    ///     - ``&#39;batch&#39;``: divide the gradient by the batch size.
    ///     - ``&#39;valid&#39;``: divide the gradient by the number of instances which are not ignored.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\softmax_output.cc:L230</summary>
    /// <param name="data">Input array.</param>
    /// <param name="label">Ground truth label.</param>
    /// <param name="gradScale">Scales the gradient by a float factor.</param>
    /// <param name="ignoreLabel">The instances whose `labels` == `ignore_label` will be ignored during backward, if `use_ignore` is set to ``true``).</param>
    /// <param name="multiOutput">If set to ``true``, the softmax function will be computed along axis ``1``. This is applied when the shape of input array differs from the shape of label array.</param>
    /// <param name="useIgnore">If set to ``true``, the `ignore_label` value will not contribute to the backward gradient.</param>
    /// <param name="preserveShape">If set to ``true``, the softmax function will be computed along the last axis (``-1``).</param>
    /// <param name="normalization">Normalizes the gradient.</param>
    /// <param name="outGrad">Multiplies gradient with output gradient element-wise.</param>
    /// <param name="smoothAlpha">Constant for computing a label smoothed version of cross-entropyfor the backwards pass.  This constant gets subtracted from theone-hot encoding of the gold label and distributed uniformly toall other labels.</param>
    static member SoftmaxOutput(data : NDArray, 
                                label : NDArray, 
                                [<Optional; DefaultParameterValue(1.0)>] gradScale : float, 
                                [<Optional; DefaultParameterValue(-1.0)>] ignoreLabel : float, 
                                [<Optional; DefaultParameterValue(false)>] multiOutput : bool, 
                                [<Optional; DefaultParameterValue(false)>] useIgnore : bool, 
                                [<Optional; DefaultParameterValue(false)>] preserveShape : bool, 
                                [<Optional>] normalization : Normalization, 
                                [<Optional; DefaultParameterValue(false)>] outGrad : bool, 
                                [<Optional; DefaultParameterValue(0.0)>] smoothAlpha : float) =
        let creator = AtomicSymbolCreator.FromName "SoftmaxOutput"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; label.NDArrayHandle|]
                                                 [|"grad_scale"; "ignore_label"; "multi_output"; "use_ignore"; "preserve_shape"; "normalization"; "out_grad"; "smooth_alpha"|]
                                                 [|gradScale.ToString(); ignoreLabel.ToString(); multiOutput.ToString(); useIgnore.ToString(); preserveShape.ToString(); (if isNull (normalization :> obj) then "null" else normalization.ToString()); outGrad.ToString(); smoothAlpha.ToString()|]
        outputs

    /// <summary>Interchanges two axes of an array.
    /// 
    /// Examples::
    /// 
    ///   x = [[1, 2, 3]])
    ///   swapaxes(x, 0, 1) = [[ 1],
    ///                        [ 2],
    ///                        [ 3]]
    /// 
    ///   x = [[[ 0, 1],
    ///         [ 2, 3]],
    ///        [[ 4, 5],
    ///         [ 6, 7]]]  // (2,2,2) array
    /// 
    ///  swapaxes(x, 0, 2) = [[[ 0, 4],
    ///                        [ 2, 6]],
    ///                       [[ 1, 5],
    ///                        [ 3, 7]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\swapaxis.cc:L70</summary>
    /// <param name="data">Input array.</param>
    /// <param name="dim1">the first axis to be swapped.</param>
    /// <param name="dim2">the second axis to be swapped.</param>
    static member SwapAxis(data : NDArray, dim1 : int, dim2 : int) =
        let creator = AtomicSymbolCreator.FromName "SwapAxis"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"dim1"; "dim2"|]
                                                 [|dim1.ToString(); dim2.ToString()|]
        outputs

    /// <summary>Cast function between low precision float/FP32 used by AMP.
    /// 
    /// It casts only between low precision float/FP32 and does not do anything for other types.
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\amp_cast.cc:L37</summary>
    /// <param name="data">The input.</param>
    /// <param name="dtype">Output data type.</param>
    static member AmpCast(data : NDArray, dtype : AmpCastDtype) =
        let creator = AtomicSymbolCreator.FromName "amp_cast"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"dtype"|]
                                                 [|dtype.ToString()|]
        outputs

    /// <summary>Cast function used by AMP, that casts its inputs to the common widest type.
    /// 
    /// It casts only between low precision float/FP32 and does not do anything for other types.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\amp_cast.cc:L71</summary>
    /// <param name="data">Weights</param>
    /// <param name="numOutputs">Number of input/output pairs to be casted to the widest type.</param>
    static member AmpMulticast([<ParamArray>] data : NDArray[], numOutputs : int) =
        let creator = AtomicSymbolCreator.FromName "amp_multicast"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 [|"num_outputs"|]
                                                 [|numOutputs.ToString()|]
        outputs

    /// <param name="grad">Gradients</param>
    /// <param name="numOutputs">Number of input/output pairs to be casted to the widest type.</param>
    static member BackwardAmpMulticast([<ParamArray>] grad : NDArray[], numOutputs : int) =
        let creator = AtomicSymbolCreator.FromName "_backward_amp_multicast"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 [|"num_outputs"|]
                                                 [|numOutputs.ToString()|]
        outputs

    /// <summary>Returns indices of the maximum values along an axis.
    /// 
    /// In the case of multiple occurrences of maximum values, the indices corresponding to the first occurrence
    /// are returned.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 0.,  1.,  2.],
    ///        [ 3.,  4.,  5.]]
    /// 
    ///   // argmax along axis 0
    ///   argmax(x, axis=0) = [ 1.,  1.,  1.]
    /// 
    ///   // argmax along axis 1
    ///   argmax(x, axis=1) = [ 2.,  2.]
    /// 
    ///   // argmax along axis 1 keeping same dims as an input array
    ///   argmax(x, axis=1, keepdims=True) = [[ 2.],
    ///                                       [ 2.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_index.cc:L52</summary>
    /// <param name="data">The input</param>
    /// <param name="axis">The axis along which to perform the reduction. Negative values means indexing from right to left. ``Requires axis to be set as int, because global reduction is not supported yet.``</param>
    /// <param name="keepdims">If this is set to `True`, the reduced axis is left in the result as dimension with size one.</param>
    static member Argmax(data : NDArray, [<Optional>] axis : int Nullable, [<Optional; DefaultParameterValue(false)>] keepdims : bool) =
        let creator = AtomicSymbolCreator.FromName "argmax"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "keepdims"|]
                                                 [|axis.ToString(); keepdims.ToString()|]
        outputs

    /// <summary>Returns indices of the maximum values along an axis.
    /// 
    /// In the case of multiple occurrences of maximum values, the indices corresponding to the first occurrence
    /// are returned.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 0.,  1.,  2.],
    ///        [ 3.,  4.,  5.]]
    /// 
    ///   // argmax along axis 0
    ///   argmax(x, axis=0) = [ 1.,  1.,  1.]
    /// 
    ///   // argmax along axis 1
    ///   argmax(x, axis=1) = [ 2.,  2.]
    /// 
    ///   // argmax along axis 1 keeping same dims as an input array
    ///   argmax(x, axis=1, keepdims=True) = [[ 2.],
    ///                                       [ 2.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_index.cc:L52</summary>
    /// <param name="data">The input</param>
    /// <param name="axis">The axis along which to perform the reduction. Negative values means indexing from right to left. ``Requires axis to be set as int, because global reduction is not supported yet.``</param>
    /// <param name="keepdims">If this is set to `True`, the reduced axis is left in the result as dimension with size one.</param>
    static member Argmax(data : NDArray, ?axis : int, ?keepdims : bool) =
        let creator = AtomicSymbolCreator.FromName "argmax"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "keepdims"|]
                                                 [|(match axis with None -> "None" | _ -> axis.ToString()); (match keepdims with None -> "false" | _ -> keepdims.ToString())|]
        outputs

    /// <summary>Returns indices of the minimum values along an axis.
    /// 
    /// In the case of multiple occurrences of minimum values, the indices corresponding to the first occurrence
    /// are returned.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 0.,  1.,  2.],
    ///        [ 3.,  4.,  5.]]
    /// 
    ///   // argmin along axis 0
    ///   argmin(x, axis=0) = [ 0.,  0.,  0.]
    /// 
    ///   // argmin along axis 1
    ///   argmin(x, axis=1) = [ 0.,  0.]
    /// 
    ///   // argmin along axis 1 keeping same dims as an input array
    ///   argmin(x, axis=1, keepdims=True) = [[ 0.],
    ///                                       [ 0.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_index.cc:L77</summary>
    /// <param name="data">The input</param>
    /// <param name="axis">The axis along which to perform the reduction. Negative values means indexing from right to left. ``Requires axis to be set as int, because global reduction is not supported yet.``</param>
    /// <param name="keepdims">If this is set to `True`, the reduced axis is left in the result as dimension with size one.</param>
    static member Argmin(data : NDArray, [<Optional>] axis : int Nullable, [<Optional; DefaultParameterValue(false)>] keepdims : bool) =
        let creator = AtomicSymbolCreator.FromName "argmin"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "keepdims"|]
                                                 [|axis.ToString(); keepdims.ToString()|]
        outputs

    /// <summary>Returns indices of the minimum values along an axis.
    /// 
    /// In the case of multiple occurrences of minimum values, the indices corresponding to the first occurrence
    /// are returned.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 0.,  1.,  2.],
    ///        [ 3.,  4.,  5.]]
    /// 
    ///   // argmin along axis 0
    ///   argmin(x, axis=0) = [ 0.,  0.,  0.]
    /// 
    ///   // argmin along axis 1
    ///   argmin(x, axis=1) = [ 0.,  0.]
    /// 
    ///   // argmin along axis 1 keeping same dims as an input array
    ///   argmin(x, axis=1, keepdims=True) = [[ 0.],
    ///                                       [ 0.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_index.cc:L77</summary>
    /// <param name="data">The input</param>
    /// <param name="axis">The axis along which to perform the reduction. Negative values means indexing from right to left. ``Requires axis to be set as int, because global reduction is not supported yet.``</param>
    /// <param name="keepdims">If this is set to `True`, the reduced axis is left in the result as dimension with size one.</param>
    static member Argmin(data : NDArray, ?axis : int, ?keepdims : bool) =
        let creator = AtomicSymbolCreator.FromName "argmin"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "keepdims"|]
                                                 [|(match axis with None -> "None" | _ -> axis.ToString()); (match keepdims with None -> "false" | _ -> keepdims.ToString())|]
        outputs

    /// <summary>Returns argmax indices of each channel from the input array.
    /// 
    /// The result will be an NDArray of shape (num_channel,).
    /// 
    /// In case of multiple occurrences of the maximum values, the indices corresponding to the first occurrence
    /// are returned.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 0.,  1.,  2.],
    ///        [ 3.,  4.,  5.]]
    /// 
    ///   argmax_channel(x) = [ 2.,  2.]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_index.cc:L97</summary>
    /// <param name="data">The input array</param>
    static member ArgmaxChannel(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "argmax_channel"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Picks elements from an input array according to the input indices along the given axis.
    /// 
    /// Given an input array of shape ``(d0, d1)`` and indices of shape ``(i0,)``, the result will be
    /// an output array of shape ``(i0,)`` with::
    /// 
    ///   output[i] = input[i, indices[i]]
    /// 
    /// By default, if any index mentioned is too large, it is replaced by the index that addresses
    /// the last element along an axis (the `clip` mode).
    /// 
    /// This function supports n-dimensional input and (n-1)-dimensional indices arrays.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 1.,  2.],
    ///        [ 3.,  4.],
    ///        [ 5.,  6.]]
    /// 
    ///   // picks elements with specified indices along axis 0
    ///   pick(x, y=[0,1], 0) = [ 1.,  4.]
    /// 
    ///   // picks elements with specified indices along axis 1
    ///   pick(x, y=[0,1,0], 1) = [ 1.,  4.,  5.]
    /// 
    ///   y = [[ 1.],
    ///        [ 0.],
    ///        [ 2.]]
    /// 
    ///   // picks elements with specified indices along axis 1 using &#39;wrap&#39; mode
    ///   // to place indicies that would normally be out of bounds
    ///   pick(x, y=[2,-1,-2], 1, mode=&#39;wrap&#39;) = [ 1.,  4.,  5.]
    /// 
    ///   y = [[ 1.],
    ///        [ 0.],
    ///        [ 2.]]
    /// 
    ///   // picks elements with specified indices along axis 1 and dims are maintained
    ///   pick(x,y, 1, keepdims=True) = [[ 2.],
    ///                                  [ 3.],
    ///                                  [ 6.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_index.cc:L154</summary>
    /// <param name="data">The input array</param>
    /// <param name="index">The index array</param>
    /// <param name="axis">int or None. The axis to picking the elements. Negative values means indexing from right to left. If is `None`, the elements in the index w.r.t the flattened input will be picked.</param>
    /// <param name="keepdims">If true, the axis where we pick the elements is left in the result as dimension with size one.</param>
    /// <param name="mode">Specify how out-of-bound indices behave. Default is &quot;clip&quot;. &quot;clip&quot; means clip to the range. So, if all indices mentioned are too large, they are replaced by the index that addresses the last element along an axis.  &quot;wrap&quot; means to wrap around.</param>
    static member Pick(data : NDArray, 
                       index : NDArray, 
                       [<Optional>] axis : int Nullable, 
                       [<Optional; DefaultParameterValue(false)>] keepdims : bool, 
                       [<Optional>] mode : PickMode) =
        let creator = AtomicSymbolCreator.FromName "pick"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; index.NDArrayHandle|]
                                                 [|"axis"; "keepdims"; "mode"|]
                                                 [|axis.ToString(); keepdims.ToString(); (if isNull (mode :> obj) then "clip" else mode.ToString())|]
        outputs

    /// <summary>Picks elements from an input array according to the input indices along the given axis.
    /// 
    /// Given an input array of shape ``(d0, d1)`` and indices of shape ``(i0,)``, the result will be
    /// an output array of shape ``(i0,)`` with::
    /// 
    ///   output[i] = input[i, indices[i]]
    /// 
    /// By default, if any index mentioned is too large, it is replaced by the index that addresses
    /// the last element along an axis (the `clip` mode).
    /// 
    /// This function supports n-dimensional input and (n-1)-dimensional indices arrays.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 1.,  2.],
    ///        [ 3.,  4.],
    ///        [ 5.,  6.]]
    /// 
    ///   // picks elements with specified indices along axis 0
    ///   pick(x, y=[0,1], 0) = [ 1.,  4.]
    /// 
    ///   // picks elements with specified indices along axis 1
    ///   pick(x, y=[0,1,0], 1) = [ 1.,  4.,  5.]
    /// 
    ///   y = [[ 1.],
    ///        [ 0.],
    ///        [ 2.]]
    /// 
    ///   // picks elements with specified indices along axis 1 using &#39;wrap&#39; mode
    ///   // to place indicies that would normally be out of bounds
    ///   pick(x, y=[2,-1,-2], 1, mode=&#39;wrap&#39;) = [ 1.,  4.,  5.]
    /// 
    ///   y = [[ 1.],
    ///        [ 0.],
    ///        [ 2.]]
    /// 
    ///   // picks elements with specified indices along axis 1 and dims are maintained
    ///   pick(x,y, 1, keepdims=True) = [[ 2.],
    ///                                  [ 3.],
    ///                                  [ 6.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_index.cc:L154</summary>
    /// <param name="data">The input array</param>
    /// <param name="index">The index array</param>
    /// <param name="axis">int or None. The axis to picking the elements. Negative values means indexing from right to left. If is `None`, the elements in the index w.r.t the flattened input will be picked.</param>
    /// <param name="keepdims">If true, the axis where we pick the elements is left in the result as dimension with size one.</param>
    /// <param name="mode">Specify how out-of-bound indices behave. Default is &quot;clip&quot;. &quot;clip&quot; means clip to the range. So, if all indices mentioned are too large, they are replaced by the index that addresses the last element along an axis.  &quot;wrap&quot; means to wrap around.</param>
    static member Pick(data : NDArray, 
                       index : NDArray, 
                       ?axis : int, 
                       ?keepdims : bool, 
                       ?mode : PickMode) =
        let creator = AtomicSymbolCreator.FromName "pick"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; index.NDArrayHandle|]
                                                 [|"axis"; "keepdims"; "mode"|]
                                                 [|(match axis with None -> "None" | _ -> axis.ToString()); (match keepdims with None -> "false" | _ -> keepdims.ToString()); (match mode with None -> "clip" | _ -> mode.ToString())|]
        outputs

    /// <summary>Computes the sum of array elements over given axes.
    /// 
    /// .. Note::
    /// 
    ///   `sum` and `sum_axis` are equivalent.
    ///   For ndarray of csr storage type summation along axis 0 and axis 1 is supported.
    ///   Setting keepdims or exclude to True will cause a fallback to dense operator.
    /// 
    /// Example::
    /// 
    ///   data = [[[1, 2], [2, 3], [1, 3]],
    ///           [[1, 4], [4, 3], [5, 2]],
    ///           [[7, 1], [7, 2], [7, 3]]]
    /// 
    ///   sum(data, axis=1)
    ///   [[  4.   8.]
    ///    [ 10.   9.]
    ///    [ 21.   6.]]
    /// 
    ///   sum(data, axis=[1,2])
    ///   [ 12.  19.  27.]
    /// 
    ///   data = [[1, 2, 0],
    ///           [3, 0, 1],
    ///           [4, 1, 0]]
    /// 
    ///   csr = cast_storage(data, &#39;csr&#39;)
    /// 
    ///   sum(csr, axis=0)
    ///   [ 8.  3.  1.]
    /// 
    ///   sum(csr, axis=1)
    ///   [ 3.  4.  5.]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_value.cc:L116</summary>
    /// <param name="data">The input</param>
    /// <param name="axis">The axis or axes along which to perform the reduction.
    /// 
    ///       The default, `axis=()`, will compute over all elements into a
    ///       scalar array with shape `(1,)`.
    /// 
    ///       If `axis` is int, a reduction is performed on a particular axis.
    /// 
    ///       If `axis` is a tuple of ints, a reduction is performed on all the axes
    ///       specified in the tuple.
    /// 
    ///       If `exclude` is true, reduction will be performed on the axes that are
    ///       NOT in axis instead.
    /// 
    ///       Negative values means indexing from right to left.</param>
    /// <param name="keepdims">If this is set to `True`, the reduced axes are left in the result as dimension with size one.</param>
    /// <param name="exclude">Whether to perform reduction on axis that are NOT in axis instead.</param>
    static member Sum(data : NDArray, [<Optional>] axis : int seq, [<Optional; DefaultParameterValue(false)>] keepdims : bool, [<Optional; DefaultParameterValue(false)>] exclude : bool) =
        let creator = AtomicSymbolCreator.FromName "sum"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "keepdims"; "exclude"|]
                                                 [|axis.ToString(); keepdims.ToString(); exclude.ToString()|]
        outputs

    /// <summary>Computes the mean of array elements over given axes.
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_value.cc:L132</summary>
    /// <param name="data">The input</param>
    /// <param name="axis">The axis or axes along which to perform the reduction.
    /// 
    ///       The default, `axis=()`, will compute over all elements into a
    ///       scalar array with shape `(1,)`.
    /// 
    ///       If `axis` is int, a reduction is performed on a particular axis.
    /// 
    ///       If `axis` is a tuple of ints, a reduction is performed on all the axes
    ///       specified in the tuple.
    /// 
    ///       If `exclude` is true, reduction will be performed on the axes that are
    ///       NOT in axis instead.
    /// 
    ///       Negative values means indexing from right to left.</param>
    /// <param name="keepdims">If this is set to `True`, the reduced axes are left in the result as dimension with size one.</param>
    /// <param name="exclude">Whether to perform reduction on axis that are NOT in axis instead.</param>
    static member Mean(data : NDArray, [<Optional>] axis : int seq, [<Optional; DefaultParameterValue(false)>] keepdims : bool, [<Optional; DefaultParameterValue(false)>] exclude : bool) =
        let creator = AtomicSymbolCreator.FromName "mean"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "keepdims"; "exclude"|]
                                                 [|axis.ToString(); keepdims.ToString(); exclude.ToString()|]
        outputs

    /// <summary>Computes the product of array elements over given axes.
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_value.cc:L147</summary>
    /// <param name="data">The input</param>
    /// <param name="axis">The axis or axes along which to perform the reduction.
    /// 
    ///       The default, `axis=()`, will compute over all elements into a
    ///       scalar array with shape `(1,)`.
    /// 
    ///       If `axis` is int, a reduction is performed on a particular axis.
    /// 
    ///       If `axis` is a tuple of ints, a reduction is performed on all the axes
    ///       specified in the tuple.
    /// 
    ///       If `exclude` is true, reduction will be performed on the axes that are
    ///       NOT in axis instead.
    /// 
    ///       Negative values means indexing from right to left.</param>
    /// <param name="keepdims">If this is set to `True`, the reduced axes are left in the result as dimension with size one.</param>
    /// <param name="exclude">Whether to perform reduction on axis that are NOT in axis instead.</param>
    static member Prod(data : NDArray, [<Optional>] axis : int seq, [<Optional; DefaultParameterValue(false)>] keepdims : bool, [<Optional; DefaultParameterValue(false)>] exclude : bool) =
        let creator = AtomicSymbolCreator.FromName "prod"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "keepdims"; "exclude"|]
                                                 [|axis.ToString(); keepdims.ToString(); exclude.ToString()|]
        outputs

    /// <summary>Computes the sum of array elements over given axes treating Not a Numbers (``NaN``) as zero.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_value.cc:L162</summary>
    /// <param name="data">The input</param>
    /// <param name="axis">The axis or axes along which to perform the reduction.
    /// 
    ///       The default, `axis=()`, will compute over all elements into a
    ///       scalar array with shape `(1,)`.
    /// 
    ///       If `axis` is int, a reduction is performed on a particular axis.
    /// 
    ///       If `axis` is a tuple of ints, a reduction is performed on all the axes
    ///       specified in the tuple.
    /// 
    ///       If `exclude` is true, reduction will be performed on the axes that are
    ///       NOT in axis instead.
    /// 
    ///       Negative values means indexing from right to left.</param>
    /// <param name="keepdims">If this is set to `True`, the reduced axes are left in the result as dimension with size one.</param>
    /// <param name="exclude">Whether to perform reduction on axis that are NOT in axis instead.</param>
    static member Nansum(data : NDArray, [<Optional>] axis : int seq, [<Optional; DefaultParameterValue(false)>] keepdims : bool, [<Optional; DefaultParameterValue(false)>] exclude : bool) =
        let creator = AtomicSymbolCreator.FromName "nansum"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "keepdims"; "exclude"|]
                                                 [|axis.ToString(); keepdims.ToString(); exclude.ToString()|]
        outputs

    /// <summary>Computes the product of array elements over given axes treating Not a Numbers (``NaN``) as one.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_value.cc:L177</summary>
    /// <param name="data">The input</param>
    /// <param name="axis">The axis or axes along which to perform the reduction.
    /// 
    ///       The default, `axis=()`, will compute over all elements into a
    ///       scalar array with shape `(1,)`.
    /// 
    ///       If `axis` is int, a reduction is performed on a particular axis.
    /// 
    ///       If `axis` is a tuple of ints, a reduction is performed on all the axes
    ///       specified in the tuple.
    /// 
    ///       If `exclude` is true, reduction will be performed on the axes that are
    ///       NOT in axis instead.
    /// 
    ///       Negative values means indexing from right to left.</param>
    /// <param name="keepdims">If this is set to `True`, the reduced axes are left in the result as dimension with size one.</param>
    /// <param name="exclude">Whether to perform reduction on axis that are NOT in axis instead.</param>
    static member Nanprod(data : NDArray, [<Optional>] axis : int seq, [<Optional; DefaultParameterValue(false)>] keepdims : bool, [<Optional; DefaultParameterValue(false)>] exclude : bool) =
        let creator = AtomicSymbolCreator.FromName "nanprod"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "keepdims"; "exclude"|]
                                                 [|axis.ToString(); keepdims.ToString(); exclude.ToString()|]
        outputs

    /// <summary>Computes the max of array elements over given axes.
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_value.cc:L191</summary>
    /// <param name="data">The input</param>
    /// <param name="axis">The axis or axes along which to perform the reduction.
    /// 
    ///       The default, `axis=()`, will compute over all elements into a
    ///       scalar array with shape `(1,)`.
    /// 
    ///       If `axis` is int, a reduction is performed on a particular axis.
    /// 
    ///       If `axis` is a tuple of ints, a reduction is performed on all the axes
    ///       specified in the tuple.
    /// 
    ///       If `exclude` is true, reduction will be performed on the axes that are
    ///       NOT in axis instead.
    /// 
    ///       Negative values means indexing from right to left.</param>
    /// <param name="keepdims">If this is set to `True`, the reduced axes are left in the result as dimension with size one.</param>
    /// <param name="exclude">Whether to perform reduction on axis that are NOT in axis instead.</param>
    static member Max(data : NDArray, [<Optional>] axis : int seq, [<Optional; DefaultParameterValue(false)>] keepdims : bool, [<Optional; DefaultParameterValue(false)>] exclude : bool) =
        let creator = AtomicSymbolCreator.FromName "max"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "keepdims"; "exclude"|]
                                                 [|axis.ToString(); keepdims.ToString(); exclude.ToString()|]
        outputs

    /// <summary>Computes the min of array elements over given axes.
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_value.cc:L205</summary>
    /// <param name="data">The input</param>
    /// <param name="axis">The axis or axes along which to perform the reduction.
    /// 
    ///       The default, `axis=()`, will compute over all elements into a
    ///       scalar array with shape `(1,)`.
    /// 
    ///       If `axis` is int, a reduction is performed on a particular axis.
    /// 
    ///       If `axis` is a tuple of ints, a reduction is performed on all the axes
    ///       specified in the tuple.
    /// 
    ///       If `exclude` is true, reduction will be performed on the axes that are
    ///       NOT in axis instead.
    /// 
    ///       Negative values means indexing from right to left.</param>
    /// <param name="keepdims">If this is set to `True`, the reduced axes are left in the result as dimension with size one.</param>
    /// <param name="exclude">Whether to perform reduction on axis that are NOT in axis instead.</param>
    static member Min(data : NDArray, [<Optional>] axis : int seq, [<Optional; DefaultParameterValue(false)>] keepdims : bool, [<Optional; DefaultParameterValue(false)>] exclude : bool) =
        let creator = AtomicSymbolCreator.FromName "min"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "keepdims"; "exclude"|]
                                                 [|axis.ToString(); keepdims.ToString(); exclude.ToString()|]
        outputs

    /// <summary>Broadcasts the input array over particular axes.
    /// 
    /// Broadcasting is allowed on axes with size 1, such as from `(2,1,3,1)` to
    /// `(2,8,3,9)`. Elements will be duplicated on the broadcasted axes.
    /// 
    /// Example::
    /// 
    ///    // given x of shape (1,2,1)
    ///    x = [[[ 1.],
    ///          [ 2.]]]
    /// 
    ///    // broadcast x on on axis 2
    ///    broadcast_axis(x, axis=2, size=3) = [[[ 1.,  1.,  1.],
    ///                                          [ 2.,  2.,  2.]]]
    ///    // broadcast x on on axes 0 and 2
    ///    broadcast_axis(x, axis=(0,2), size=(2,3)) = [[[ 1.,  1.,  1.],
    ///                                                  [ 2.,  2.,  2.]],
    ///                                                 [[ 1.,  1.,  1.],
    ///                                                  [ 2.,  2.,  2.]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_value.cc:L238</summary>
    /// <param name="data">The input</param>
    /// <param name="axis">The axes to perform the broadcasting.</param>
    /// <param name="size">Target sizes of the broadcasting axes.</param>
    static member BroadcastAxis(data : NDArray, [<Optional>] axis : int seq, [<Optional>] size : int seq) =
        let creator = AtomicSymbolCreator.FromName "broadcast_axis"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "size"|]
                                                 [|(if isNull (axis :> obj) then "[]" else axis.ToString()); (if isNull (size :> obj) then "[]" else size.ToString())|]
        outputs

    /// <summary>Broadcasts the input array to a new shape.
    /// 
    /// Broadcasting is a mechanism that allows NDArrays to perform arithmetic operations
    /// with arrays of different shapes efficiently without creating multiple copies of arrays.
    /// Also see, `Broadcasting &lt;https://docs.scipy.org/doc/numpy/user/basics.broadcasting.html&gt;`_ for more explanation.
    /// 
    /// Broadcasting is allowed on axes with size 1, such as from `(2,1,3,1)` to
    /// `(2,8,3,9)`. Elements will be duplicated on the broadcasted axes.
    /// 
    /// For example::
    /// 
    ///    broadcast_to([[1,2,3]], shape=(2,3)) = [[ 1.,  2.,  3.],
    ///                                            [ 1.,  2.,  3.]])
    /// 
    /// The dimension which you do not want to change can also be kept as `0` which means copy the original value.
    /// So with `shape=(2,0)`, we will obtain the same result as in the above example.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_value.cc:L262</summary>
    /// <param name="data">The input</param>
    /// <param name="shape">The shape of the desired array. We can set the dim to zero if it&#39;s same as the original. E.g `A = broadcast_to(B, shape=(10, 0, 0))` has the same meaning as `A = broadcast_axis(B, axis=0, size=10)`.</param>
    static member BroadcastTo(data : NDArray, [<Optional>] shape : int seq) =
        let creator = AtomicSymbolCreator.FromName "broadcast_to"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"shape"|]
                                                 [|(if isNull (shape :> obj) then "[]" else shape.ToString())|]
        outputs

    /// <summary>Broadcasts lhs to have the same shape as rhs.
    /// 
    /// Broadcasting is a mechanism that allows NDArrays to perform arithmetic operations
    /// with arrays of different shapes efficiently without creating multiple copies of arrays.
    /// Also see, `Broadcasting &lt;https://docs.scipy.org/doc/numpy/user/basics.broadcasting.html&gt;`_ for more explanation.
    /// 
    /// Broadcasting is allowed on axes with size 1, such as from `(2,1,3,1)` to
    /// `(2,8,3,9)`. Elements will be duplicated on the broadcasted axes.
    /// 
    /// For example::
    /// 
    ///    broadcast_like([[1,2,3]], [[5,6,7],[7,8,9]]) = [[ 1.,  2.,  3.],
    ///                                                    [ 1.,  2.,  3.]])
    /// 
    ///    broadcast_like([9], [1,2,3,4,5], lhs_axes=(0,), rhs_axes=(-1,)) = [9,9,9,9,9]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_value.cc:L315</summary>
    /// <param name="lhs">First input.</param>
    /// <param name="rhs">Second input.</param>
    /// <param name="lhsAxes">Axes to perform broadcast on in the first input array</param>
    /// <param name="rhsAxes">Axes to copy from the second input array</param>
    static member BroadcastLike(lhs : NDArray, rhs : NDArray, [<Optional>] lhsAxes : int seq, [<Optional>] rhsAxes : int seq) =
        let creator = AtomicSymbolCreator.FromName "broadcast_like"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 [|"lhs_axes"; "rhs_axes"|]
                                                 [|lhsAxes.ToString(); rhsAxes.ToString()|]
        outputs

    /// <summary>Computes the norm on an NDArray.
    /// 
    /// This operator computes the norm on an NDArray with the specified axis, depending
    /// on the value of the ord parameter. By default, it computes the L2 norm on the entire
    /// array. Currently only ord=2 supports sparse ndarrays.
    /// 
    /// Examples::
    /// 
    ///   x = [[[1, 2],
    ///         [3, 4]],
    ///        [[2, 2],
    ///         [5, 6]]]
    /// 
    ///   norm(x, ord=2, axis=1) = [[3.1622777 4.472136 ]
    ///                             [5.3851647 6.3245554]]
    /// 
    ///   norm(x, ord=1, axis=1) = [[4., 6.],
    ///                             [7., 8.]]
    /// 
    ///   rsp = x.cast_storage(&#39;row_sparse&#39;)
    /// 
    ///   norm(rsp) = [5.47722578]
    /// 
    ///   csr = x.cast_storage(&#39;csr&#39;)
    /// 
    ///   norm(csr) = [5.47722578]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\broadcast_reduce_op_value.cc:L350</summary>
    /// <param name="data">The input</param>
    /// <param name="ord">Order of the norm. Currently ord=1 and ord=2 is supported.</param>
    /// <param name="axis">The axis or axes along which to perform the reduction.
    ///       The default, `axis=()`, will compute over all elements into a
    ///       scalar array with shape `(1,)`.
    ///       If `axis` is int, a reduction is performed on a particular axis.
    ///       If `axis` is a 2-tuple, it specifies the axes that hold 2-D matrices,
    ///       and the matrix norms of these matrices are computed.</param>
    /// <param name="outDtype">The data type of the output.</param>
    /// <param name="keepdims">If this is set to `True`, the reduced axis is left in the result as dimension with size one.</param>
    static member Norm(data : NDArray, 
                       [<Optional; DefaultParameterValue(2)>] ord : int, 
                       [<Optional>] axis : int seq, 
                       [<Optional>] outDtype : OutDtype, 
                       [<Optional; DefaultParameterValue(false)>] keepdims : bool) =
        let creator = AtomicSymbolCreator.FromName "norm"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"ord"; "axis"; "out_dtype"; "keepdims"|]
                                                 [|ord.ToString(); axis.ToString(); (if isNull (outDtype :> obj) then "None" else outDtype.ToString()); keepdims.ToString()|]
        outputs

    /// <summary>Casts tensor storage type to the new type.
    /// 
    /// When an NDArray with default storage type is cast to csr or row_sparse storage,
    /// the result is compact, which means:
    /// 
    /// - for csr, zero values will not be retained
    /// - for row_sparse, row slices of all zeros will not be retained
    /// 
    /// The storage type of ``cast_storage`` output depends on stype parameter:
    /// 
    /// - cast_storage(csr, &#39;default&#39;) = default
    /// - cast_storage(row_sparse, &#39;default&#39;) = default
    /// - cast_storage(default, &#39;csr&#39;) = csr
    /// - cast_storage(default, &#39;row_sparse&#39;) = row_sparse
    /// - cast_storage(csr, &#39;csr&#39;) = csr
    /// - cast_storage(row_sparse, &#39;row_sparse&#39;) = row_sparse
    /// 
    /// Example::
    /// 
    ///     dense = [[ 0.,  1.,  0.],
    ///              [ 2.,  0.,  3.],
    ///              [ 0.,  0.,  0.],
    ///              [ 0.,  0.,  0.]]
    /// 
    ///     # cast to row_sparse storage type
    ///     rsp = cast_storage(dense, &#39;row_sparse&#39;)
    ///     rsp.indices = [0, 1]
    ///     rsp.values = [[ 0.,  1.,  0.],
    ///                   [ 2.,  0.,  3.]]
    /// 
    ///     # cast to csr storage type
    ///     csr = cast_storage(dense, &#39;csr&#39;)
    ///     csr.indices = [1, 0, 2]
    ///     csr.values = [ 1.,  2.,  3.]
    ///     csr.indptr = [0, 1, 3, 3, 3]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\cast_storage.cc:L71</summary>
    /// <param name="data">The input.</param>
    /// <param name="stype">Output storage type.</param>
    static member CastStorage(data : NDArray, stype : Stype) =
        let creator = AtomicSymbolCreator.FromName "cast_storage"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"stype"|]
                                                 [|stype.ToString()|]
        outputs

    /// <summary>Return the elements, either from x or y, depending on the condition.
    /// 
    /// Given three ndarrays, condition, x, and y, return an ndarray with the elements from x or y,
    /// depending on the elements from condition are true or false. x and y must have the same shape.
    /// If condition has the same shape as x, each element in the output array is from x if the
    /// corresponding element in the condition is true, and from y if false.
    /// 
    /// If condition does not have the same shape as x, it must be a 1D array whose size is
    /// the same as x&#39;s first dimension size. Each row of the output array is from x&#39;s row
    /// if the corresponding element from condition is true, and from y&#39;s row if false.
    /// 
    /// Note that all non-zero values are interpreted as ``True`` in condition.
    /// 
    /// Examples::
    /// 
    ///   x = [[1, 2], [3, 4]]
    ///   y = [[5, 6], [7, 8]]
    ///   cond = [[0, 1], [-1, 0]]
    /// 
    ///   where(cond, x, y) = [[5, 2], [3, 8]]
    /// 
    ///   csr_cond = cast_storage(cond, &#39;csr&#39;)
    /// 
    ///   where(csr_cond, x, y) = [[5, 2], [3, 8]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\control_flow_op.cc:L57</summary>
    /// <param name="condition">condition array</param>
    /// <param name="x"></param>
    /// <param name="y"></param>
    static member Where(condition : NDArray, x : NDArray, y : NDArray) =
        let creator = AtomicSymbolCreator.FromName "where"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|condition.NDArrayHandle; x.NDArrayHandle; y.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Extracts a diagonal or constructs a diagonal array.
    /// 
    /// ``diag``&#39;s behavior depends on the input array dimensions:
    /// 
    /// - 1-D arrays: constructs a 2-D array with the input as its diagonal, all other elements are zero.
    /// - N-D arrays: extracts the diagonals of the sub-arrays with axes specified by ``axis1`` and ``axis2``.
    ///   The output shape would be decided by removing the axes numbered ``axis1`` and ``axis2`` from the
    ///   input shape and appending to the result a new axis with the size of the diagonals in question.
    /// 
    ///   For example, when the input shape is `(2, 3, 4, 5)`, ``axis1`` and ``axis2`` are 0 and 2
    ///   respectively and ``k`` is 0, the resulting shape would be `(3, 5, 2)`.
    /// 
    /// Examples::
    /// 
    ///   x = [[1, 2, 3],
    ///        [4, 5, 6]]
    /// 
    ///   diag(x) = [1, 5]
    /// 
    ///   diag(x, k=1) = [2, 6]
    /// 
    ///   diag(x, k=-1) = [4]
    /// 
    ///   x = [1, 2, 3]
    /// 
    ///   diag(x) = [[1, 0, 0],
    ///              [0, 2, 0],
    ///              [0, 0, 3]]
    /// 
    ///   diag(x, k=1) = [[0, 1, 0],
    ///                   [0, 0, 2],
    ///                   [0, 0, 0]]
    /// 
    ///   diag(x, k=-1) = [[0, 0, 0],
    ///                    [1, 0, 0],
    ///                    [0, 2, 0]]
    /// 
    ///   x = [[[1, 2],
    ///         [3, 4]],
    /// 
    ///        [[5, 6],
    ///         [7, 8]]]
    /// 
    ///   diag(x) = [[1, 7],
    ///              [2, 8]]
    /// 
    ///   diag(x, k=1) = [[3],
    ///                   [4]]
    /// 
    ///   diag(x, axis1=-2, axis2=-1) = [[1, 4],
    ///                                  [5, 8]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\diag_op.cc:L87</summary>
    /// <param name="data">Input ndarray</param>
    /// <param name="k">Diagonal in question. The default is 0. Use k&gt;0 for diagonals above the main diagonal, and k&lt;0 for diagonals below the main diagonal. If input has shape (S0 S1) k must be between -S0 and S1</param>
    /// <param name="axis1">The first axis of the sub-arrays of interest. Ignored when the input is a 1-D array.</param>
    /// <param name="axis2">The second axis of the sub-arrays of interest. Ignored when the input is a 1-D array.</param>
    static member Diag(data : NDArray, [<Optional; DefaultParameterValue(0)>] k : int, [<Optional; DefaultParameterValue(0)>] axis1 : int, [<Optional; DefaultParameterValue(1)>] axis2 : int) =
        let creator = AtomicSymbolCreator.FromName "diag"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"k"; "axis1"; "axis2"|]
                                                 [|k.ToString(); axis1.ToString(); axis2.ToString()|]
        outputs

    /// <summary>Dot product of two arrays.
    /// 
    /// ``dot``&#39;s behavior depends on the input array dimensions:
    /// 
    /// - 1-D arrays: inner product of vectors
    /// - 2-D arrays: matrix multiplication
    /// - N-D arrays: a sum product over the last axis of the first input and the first
    ///   axis of the second input
    /// 
    ///   For example, given 3-D ``x`` with shape `(n,m,k)` and ``y`` with shape `(k,r,s)`, the
    ///   result array will have shape `(n,m,r,s)`. It is computed by::
    /// 
    ///     dot(x,y)[i,j,a,b] = sum(x[i,j,:]*y[:,a,b])
    /// 
    ///   Example::
    /// 
    ///     x = reshape([0,1,2,3,4,5,6,7], shape=(2,2,2))
    ///     y = reshape([7,6,5,4,3,2,1,0], shape=(2,2,2))
    ///     dot(x,y)[0,0,1,1] = 0
    ///     sum(x[0,0,:]*y[:,1,1]) = 0
    /// 
    /// The storage type of ``dot`` output depends on storage types of inputs, transpose option and
    /// forward_stype option for output storage type. Implemented sparse operations include:
    /// 
    /// - dot(default, default, transpose_a=True/False, transpose_b=True/False) = default
    /// - dot(csr, default, transpose_a=True) = default
    /// - dot(csr, default, transpose_a=True) = row_sparse
    /// - dot(csr, default) = default
    /// - dot(csr, row_sparse) = default
    /// - dot(default, csr) = csr (CPU only)
    /// - dot(default, csr, forward_stype=&#39;default&#39;) = default
    /// - dot(default, csr, transpose_b=True, forward_stype=&#39;default&#39;) = default
    /// 
    /// If the combination of input storage types and forward_stype does not match any of the
    /// above patterns, ``dot`` will fallback and generate output with default storage.
    /// 
    /// .. Note::
    /// 
    ///     If the storage type of the lhs is &quot;csr&quot;, the storage type of gradient w.r.t rhs will be
    ///     &quot;row_sparse&quot;. Only a subset of optimizers support sparse gradients, including SGD, AdaGrad
    ///     and Adam. Note that by default lazy updates is turned on, which may perform differently
    ///     from standard updates. For more details, please check the Optimization API at:
    ///     https://mxnet.incubator.apache.org/api/python/optimization/optimization.html
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\dot.cc:L77</summary>
    /// <param name="lhs">The first input</param>
    /// <param name="rhs">The second input</param>
    /// <param name="transposeA">If true then transpose the first input before dot.</param>
    /// <param name="transposeB">If true then transpose the second input before dot.</param>
    /// <param name="forwardStype">The desired storage type of the forward output given by user, if thecombination of input storage types and this hint does not matchany implemented ones, the dot operator will perform fallback operationand still produce an output of the desired storage type.</param>
    static member Dot(lhs : NDArray, 
                      rhs : NDArray, 
                      [<Optional; DefaultParameterValue(false)>] transposeA : bool, 
                      [<Optional; DefaultParameterValue(false)>] transposeB : bool, 
                      [<Optional>] forwardStype : ForwardStype) =
        let creator = AtomicSymbolCreator.FromName "dot"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 [|"transpose_a"; "transpose_b"; "forward_stype"|]
                                                 [|transposeA.ToString(); transposeB.ToString(); (if isNull (forwardStype :> obj) then "None" else forwardStype.ToString())|]
        outputs

    /// <summary>Batchwise dot product.
    /// 
    /// ``batch_dot`` is used to compute dot product of ``x`` and ``y`` when ``x`` and
    /// ``y`` are data in batch, namely 3D arrays in shape of `(batch_size, :, :)`.
    /// 
    /// For example, given ``x`` with shape `(batch_size, n, m)` and ``y`` with shape
    /// `(batch_size, m, k)`, the result array will have shape `(batch_size, n, k)`,
    /// which is computed by::
    /// 
    ///    batch_dot(x,y)[i,:,:] = dot(x[i,:,:], y[i,:,:])
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\dot.cc:L125</summary>
    /// <param name="lhs">The first input</param>
    /// <param name="rhs">The second input</param>
    /// <param name="transposeA">If true then transpose the first input before dot.</param>
    /// <param name="transposeB">If true then transpose the second input before dot.</param>
    /// <param name="forwardStype">The desired storage type of the forward output given by user, if thecombination of input storage types and this hint does not matchany implemented ones, the dot operator will perform fallback operationand still produce an output of the desired storage type.</param>
    static member BatchDot(lhs : NDArray, 
                           rhs : NDArray, 
                           [<Optional; DefaultParameterValue(false)>] transposeA : bool, 
                           [<Optional; DefaultParameterValue(false)>] transposeB : bool, 
                           [<Optional>] forwardStype : ForwardStype) =
        let creator = AtomicSymbolCreator.FromName "batch_dot"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 [|"transpose_a"; "transpose_b"; "forward_stype"|]
                                                 [|transposeA.ToString(); transposeB.ToString(); (if isNull (forwardStype :> obj) then "None" else forwardStype.ToString())|]
        outputs

    /// <summary>Returns element-wise sum of the input arrays with broadcasting.
    /// 
    /// `broadcast_plus` is an alias to the function `broadcast_add`.
    /// 
    /// Example::
    /// 
    ///    x = [[ 1.,  1.,  1.],
    ///         [ 1.,  1.,  1.]]
    /// 
    ///    y = [[ 0.],
    ///         [ 1.]]
    /// 
    ///    broadcast_add(x, y) = [[ 1.,  1.,  1.],
    ///                           [ 2.,  2.,  2.]]
    /// 
    ///    broadcast_plus(x, y) = [[ 1.,  1.,  1.],
    ///                            [ 2.,  2.,  2.]]
    /// 
    /// Supported sparse operations:
    /// 
    ///    broadcast_add(csr, dense(1D)) = dense
    ///    broadcast_add(dense(1D), csr) = dense
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_basic.cc:L58</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastAdd(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_add"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise difference of the input arrays with broadcasting.
    /// 
    /// `broadcast_minus` is an alias to the function `broadcast_sub`.
    /// 
    /// Example::
    /// 
    ///    x = [[ 1.,  1.,  1.],
    ///         [ 1.,  1.,  1.]]
    /// 
    ///    y = [[ 0.],
    ///         [ 1.]]
    /// 
    ///    broadcast_sub(x, y) = [[ 1.,  1.,  1.],
    ///                           [ 0.,  0.,  0.]]
    /// 
    ///    broadcast_minus(x, y) = [[ 1.,  1.,  1.],
    ///                             [ 0.,  0.,  0.]]
    /// 
    /// Supported sparse operations:
    /// 
    ///    broadcast_sub/minus(csr, dense(1D)) = dense
    ///    broadcast_sub/minus(dense(1D), csr) = dense
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_basic.cc:L106</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastSub(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_sub"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise product of the input arrays with broadcasting.
    /// 
    /// Example::
    /// 
    ///    x = [[ 1.,  1.,  1.],
    ///         [ 1.,  1.,  1.]]
    /// 
    ///    y = [[ 0.],
    ///         [ 1.]]
    /// 
    ///    broadcast_mul(x, y) = [[ 0.,  0.,  0.],
    ///                           [ 1.,  1.,  1.]]
    /// 
    /// Supported sparse operations:
    /// 
    ///    broadcast_mul(csr, dense(1D)) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_basic.cc:L146</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastMul(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_mul"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise division of the input arrays with broadcasting.
    /// 
    /// Example::
    /// 
    ///    x = [[ 6.,  6.,  6.],
    ///         [ 6.,  6.,  6.]]
    /// 
    ///    y = [[ 2.],
    ///         [ 3.]]
    /// 
    ///    broadcast_div(x, y) = [[ 3.,  3.,  3.],
    ///                           [ 2.,  2.,  2.]]
    /// 
    /// Supported sparse operations:
    /// 
    ///    broadcast_div(csr, dense(1D)) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_basic.cc:L187</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastDiv(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_div"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise modulo of the input arrays with broadcasting.
    /// 
    /// Example::
    /// 
    ///    x = [[ 8.,  8.,  8.],
    ///         [ 8.,  8.,  8.]]
    /// 
    ///    y = [[ 2.],
    ///         [ 3.]]
    /// 
    ///    broadcast_mod(x, y) = [[ 0.,  0.,  0.],
    ///                           [ 2.,  2.,  2.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_basic.cc:L222</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastMod(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_mod"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns result of first array elements raised to powers from second array, element-wise with broadcasting.
    /// 
    /// Example::
    /// 
    ///    x = [[ 1.,  1.,  1.],
    ///         [ 1.,  1.,  1.]]
    /// 
    ///    y = [[ 0.],
    ///         [ 1.]]
    /// 
    ///    broadcast_power(x, y) = [[ 2.,  2.,  2.],
    ///                             [ 4.,  4.,  4.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_extended.cc:L45</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastPower(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_power"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise maximum of the input arrays with broadcasting.
    /// 
    /// This function compares two input arrays and returns a new array having the element-wise maxima.
    /// 
    /// Example::
    /// 
    ///    x = [[ 1.,  1.,  1.],
    ///         [ 1.,  1.,  1.]]
    /// 
    ///    y = [[ 0.],
    ///         [ 1.]]
    /// 
    ///    broadcast_maximum(x, y) = [[ 1.,  1.,  1.],
    ///                               [ 1.,  1.,  1.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_extended.cc:L80</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastMaximum(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_maximum"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise minimum of the input arrays with broadcasting.
    /// 
    /// This function compares two input arrays and returns a new array having the element-wise minima.
    /// 
    /// Example::
    /// 
    ///    x = [[ 1.,  1.,  1.],
    ///         [ 1.,  1.,  1.]]
    /// 
    ///    y = [[ 0.],
    ///         [ 1.]]
    /// 
    ///    broadcast_maximum(x, y) = [[ 0.,  0.,  0.],
    ///                               [ 1.,  1.,  1.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_extended.cc:L115</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastMinimum(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_minimum"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary> Returns the hypotenuse of a right angled triangle, given its &quot;legs&quot;
    /// with broadcasting.
    /// 
    /// It is equivalent to doing :math:`sqrt(x_1^2 + x_2^2)`.
    /// 
    /// Example::
    /// 
    ///    x = [[ 3.,  3.,  3.]]
    /// 
    ///    y = [[ 4.],
    ///         [ 4.]]
    /// 
    ///    broadcast_hypot(x, y) = [[ 5.,  5.,  5.],
    ///                             [ 5.,  5.,  5.]]
    /// 
    ///    z = [[ 0.],
    ///         [ 4.]]
    /// 
    ///    broadcast_hypot(x, z) = [[ 3.,  3.,  3.],
    ///                             [ 5.,  5.,  5.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_extended.cc:L156</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastHypot(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_hypot"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the result of element-wise **equal to** (==) comparison operation with broadcasting.
    /// 
    /// Example::
    /// 
    ///    x = [[ 1.,  1.,  1.],
    ///         [ 1.,  1.,  1.]]
    /// 
    ///    y = [[ 0.],
    ///         [ 1.]]
    /// 
    ///    broadcast_equal(x, y) = [[ 0.,  0.,  0.],
    ///                             [ 1.,  1.,  1.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_logic.cc:L46</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastEqual(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_equal"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the result of element-wise **not equal to** (!=) comparison operation with broadcasting.
    /// 
    /// Example::
    /// 
    ///    x = [[ 1.,  1.,  1.],
    ///         [ 1.,  1.,  1.]]
    /// 
    ///    y = [[ 0.],
    ///         [ 1.]]
    /// 
    ///    broadcast_not_equal(x, y) = [[ 1.,  1.,  1.],
    ///                                 [ 0.,  0.,  0.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_logic.cc:L64</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastNotEqual(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_not_equal"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the result of element-wise **greater than** (&gt;) comparison operation with broadcasting.
    /// 
    /// Example::
    /// 
    ///    x = [[ 1.,  1.,  1.],
    ///         [ 1.,  1.,  1.]]
    /// 
    ///    y = [[ 0.],
    ///         [ 1.]]
    /// 
    ///    broadcast_greater(x, y) = [[ 1.,  1.,  1.],
    ///                               [ 0.,  0.,  0.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_logic.cc:L82</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastGreater(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_greater"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the result of element-wise **greater than or equal to** (&gt;=) comparison operation with broadcasting.
    /// 
    /// Example::
    /// 
    ///    x = [[ 1.,  1.,  1.],
    ///         [ 1.,  1.,  1.]]
    /// 
    ///    y = [[ 0.],
    ///         [ 1.]]
    /// 
    ///    broadcast_greater_equal(x, y) = [[ 1.,  1.,  1.],
    ///                                     [ 1.,  1.,  1.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_logic.cc:L100</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastGreaterEqual(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_greater_equal"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the result of element-wise **lesser than** (&lt;) comparison operation with broadcasting.
    /// 
    /// Example::
    /// 
    ///    x = [[ 1.,  1.,  1.],
    ///         [ 1.,  1.,  1.]]
    /// 
    ///    y = [[ 0.],
    ///         [ 1.]]
    /// 
    ///    broadcast_lesser(x, y) = [[ 0.,  0.,  0.],
    ///                              [ 0.,  0.,  0.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_logic.cc:L118</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastLesser(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_lesser"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the result of element-wise **lesser than or equal to** (&lt;=) comparison operation with broadcasting.
    /// 
    /// Example::
    /// 
    ///    x = [[ 1.,  1.,  1.],
    ///         [ 1.,  1.,  1.]]
    /// 
    ///    y = [[ 0.],
    ///         [ 1.]]
    /// 
    ///    broadcast_lesser_equal(x, y) = [[ 0.,  0.,  0.],
    ///                                    [ 1.,  1.,  1.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_logic.cc:L136</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastLesserEqual(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_lesser_equal"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the result of element-wise **logical and** with broadcasting.
    /// 
    /// Example::
    /// 
    ///    x = [[ 1.,  1.,  1.],
    ///         [ 1.,  1.,  1.]]
    /// 
    ///    y = [[ 0.],
    ///         [ 1.]]
    /// 
    ///    broadcast_logical_and(x, y) = [[ 0.,  0.,  0.],
    ///                                   [ 1.,  1.,  1.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_logic.cc:L154</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastLogicalAnd(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_logical_and"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the result of element-wise **logical or** with broadcasting.
    /// 
    /// Example::
    /// 
    ///    x = [[ 1.,  1.,  0.],
    ///         [ 1.,  1.,  0.]]
    /// 
    ///    y = [[ 1.],
    ///         [ 0.]]
    /// 
    ///    broadcast_logical_or(x, y) = [[ 1.,  1.,  1.],
    ///                                  [ 1.,  1.,  0.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_logic.cc:L172</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastLogicalOr(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_logical_or"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the result of element-wise **logical xor** with broadcasting.
    /// 
    /// Example::
    /// 
    ///    x = [[ 1.,  1.,  0.],
    ///         [ 1.,  1.,  0.]]
    /// 
    ///    y = [[ 1.],
    ///         [ 0.]]
    /// 
    ///    broadcast_logical_xor(x, y) = [[ 0.,  0.,  1.],
    ///                                   [ 1.,  1.,  0.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_broadcast_op_logic.cc:L190</summary>
    /// <param name="lhs">First input to the function</param>
    /// <param name="rhs">Second input to the function</param>
    static member BroadcastLogicalXor(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "broadcast_logical_xor"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Adds arguments element-wise.
    /// 
    /// The storage type of ``elemwise_add`` output depends on storage types of inputs
    /// 
    ///    - elemwise_add(row_sparse, row_sparse) = row_sparse
    ///    - elemwise_add(csr, csr) = csr
    ///    - elemwise_add(default, csr) = default
    ///    - elemwise_add(csr, default) = default
    ///    - elemwise_add(default, rsp) = default
    ///    - elemwise_add(rsp, default) = default
    ///    - otherwise, ``elemwise_add`` generates output with default storage
    /// 
    /// </summary>
    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member ElemwiseAdd(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "elemwise_add"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member GradAdd(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_grad_add"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Subtracts arguments element-wise.
    /// 
    /// The storage type of ``elemwise_sub`` output depends on storage types of inputs
    /// 
    ///    - elemwise_sub(row_sparse, row_sparse) = row_sparse
    ///    - elemwise_sub(csr, csr) = csr
    ///    - elemwise_sub(default, csr) = default
    ///    - elemwise_sub(csr, default) = default
    ///    - elemwise_sub(default, rsp) = default
    ///    - elemwise_sub(rsp, default) = default
    ///    - otherwise, ``elemwise_sub`` generates output with default storage
    /// 
    /// </summary>
    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member ElemwiseSub(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "elemwise_sub"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Multiplies arguments element-wise.
    /// 
    /// The storage type of ``elemwise_mul`` output depends on storage types of inputs
    /// 
    ///    - elemwise_mul(default, default) = default
    ///    - elemwise_mul(row_sparse, row_sparse) = row_sparse
    ///    - elemwise_mul(default, row_sparse) = row_sparse
    ///    - elemwise_mul(row_sparse, default) = row_sparse
    ///    - elemwise_mul(csr, csr) = csr
    ///    - otherwise, ``elemwise_mul`` generates output with default storage
    /// 
    /// </summary>
    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member ElemwiseMul(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "elemwise_mul"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Divides arguments element-wise.
    /// 
    /// The storage type of ``elemwise_div`` output is always dense
    /// 
    /// </summary>
    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member ElemwiseDiv(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "elemwise_div"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member Mod(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_mod"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member Power(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_power"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member Maximum(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_maximum"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member Minimum(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_minimum"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Given the &quot;legs&quot; of a right triangle, return its hypotenuse.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_op_extended.cc:L79</summary>
    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member Hypot(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_hypot"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member Equal(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_equal"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member NotEqual(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_not_equal"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member Greater(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_greater"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member GreaterEqual(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_greater_equal"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member Lesser(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_lesser"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member LesserEqual(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_lesser_equal"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member LogicalAnd(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_logical_and"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member LogicalOr(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_logical_or"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member LogicalXor(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_logical_xor"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member PlusScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_plus_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member MinusScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_minus_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member RminusScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_rminus_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <summary>Multiply an array with a scalar.
    /// 
    /// ``_mul_scalar`` only operates on data array of input if input is sparse.
    /// 
    /// For example, if input of shape (100, 100) has only 2 non zero elements,
    /// i.e. input.data = [5, 6], scalar = nan,
    /// it will result output.data = [nan, nan] instead of 10000 nans.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_scalar_op_basic.cc:L149</summary>
    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member MulScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_mul_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member BackwardMulScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_backward_mul_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <summary>Divide an array with a scalar.
    /// 
    /// ``_div_scalar`` only operates on data array of input if input is sparse.
    /// 
    /// For example, if input of shape (100, 100) has only 2 non zero elements,
    /// i.e. input.data = [5, 6], scalar = nan,
    /// it will result output.data = [nan, nan] instead of 10000 nans.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_scalar_op_basic.cc:L171</summary>
    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member DivScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_div_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member BackwardDivScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_backward_div_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member RdivScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_rdiv_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    /// <param name="scalar">scalar value</param>
    static member BackwardRdivScalar(lhs : NDArray, rhs : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_backward_rdiv_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member ModScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_mod_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    /// <param name="scalar">scalar value</param>
    static member BackwardModScalar(lhs : NDArray, rhs : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_backward_mod_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member RmodScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_rmod_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    /// <param name="scalar">scalar value</param>
    static member BackwardRmodScalar(lhs : NDArray, rhs : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_backward_rmod_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member MaximumScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_maximum_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    /// <param name="scalar">scalar value</param>
    static member BackwardMaximumScalar(lhs : NDArray, rhs : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_backward_maximum_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member MinimumScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_minimum_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    /// <param name="scalar">scalar value</param>
    static member BackwardMinimumScalar(lhs : NDArray, rhs : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_backward_minimum_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member PowerScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_power_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    /// <param name="scalar">scalar value</param>
    static member BackwardPowerScalar(lhs : NDArray, rhs : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_backward_power_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member RpowerScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_rpower_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    /// <param name="scalar">scalar value</param>
    static member BackwardRpowerScalar(lhs : NDArray, rhs : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_backward_rpower_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member HypotScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_hypot_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    /// <param name="scalar">scalar value</param>
    static member BackwardHypotScalar(lhs : NDArray, rhs : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_backward_hypot_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <summary>Calculate Smooth L1 Loss(lhs, scalar) by summing
    /// 
    /// .. math::
    /// 
    ///     f(x) =
    ///     \begin{cases}
    ///     (\sigma x)^2/2,&amp; \text{if }x &lt; 1/\sigma^2\\
    ///     |x|-0.5/\sigma^2,&amp; \text{otherwise}
    ///     \end{cases}
    /// 
    /// where :math:`x` is an element of the tensor *lhs* and :math:`\sigma` is the scalar.
    /// 
    /// Example::
    /// 
    ///   smooth_l1([1, 2, 3, 4]) = [0.5, 1.5, 2.5, 3.5]
    ///   smooth_l1([1, 2, 3, 4], scalar=1) = [0.5, 1.5, 2.5, 3.5]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_binary_scalar_op_extended.cc:L104</summary>
    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member SmoothL1(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "smooth_l1"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardSmoothL1(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_smooth_l1"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member EqualScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_equal_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member NotEqualScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_not_equal_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member GreaterScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_greater_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member GreaterEqualScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_greater_equal_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member LesserScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_lesser_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member LesserEqualScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_lesser_equal_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member LogicalAndScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_logical_and_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member LogicalOrScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_logical_or_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member LogicalXorScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_logical_xor_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <summary>Divides arguments element-wise.  If the left-hand-side input is &#39;row_sparse&#39;, then
    /// only the values which exist in the left-hand sparse array are computed.  The &#39;missing&#39; values
    /// are ignored.
    /// 
    /// The storage type of ``_scatter_elemwise_div`` output depends on storage types of inputs
    /// 
    /// - _scatter_elemwise_div(row_sparse, row_sparse) = row_sparse
    /// - _scatter_elemwise_div(row_sparse, dense) = row_sparse
    /// - _scatter_elemwise_div(row_sparse, csr) = row_sparse
    /// - otherwise, ``_scatter_elemwise_div`` behaves exactly like elemwise_div and generates output
    /// with default storage
    /// 
    /// </summary>
    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member ScatterElemwiseDiv(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_scatter_elemwise_div"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Adds a scalar to a tensor element-wise.  If the left-hand-side input is
    /// &#39;row_sparse&#39; or &#39;csr&#39;, then only the values which exist in the left-hand sparse array are computed.
    /// The &#39;missing&#39; values are ignored.
    /// 
    /// The storage type of ``_scatter_plus_scalar`` output depends on storage types of inputs
    /// 
    /// - _scatter_plus_scalar(row_sparse, scalar) = row_sparse
    /// - _scatter_plus_scalar(csr, scalar) = csr
    /// - otherwise, ``_scatter_plus_scalar`` behaves exactly like _plus_scalar and generates output
    /// with default storage
    /// 
    /// </summary>
    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member ScatterPlusScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_scatter_plus_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <summary>Subtracts a scalar to a tensor element-wise.  If the left-hand-side input is
    /// &#39;row_sparse&#39; or &#39;csr&#39;, then only the values which exist in the left-hand sparse array are computed.
    /// The &#39;missing&#39; values are ignored.
    /// 
    /// The storage type of ``_scatter_minus_scalar`` output depends on storage types of inputs
    /// 
    /// - _scatter_minus_scalar(row_sparse, scalar) = row_sparse
    /// - _scatter_minus_scalar(csr, scalar) = csr
    /// - otherwise, ``_scatter_minus_scalar`` behaves exactly like _minus_scalar and generates output
    /// with default storage
    /// 
    /// </summary>
    /// <param name="data">source input</param>
    /// <param name="scalar">scalar input</param>
    static member ScatterMinusScalar(data : NDArray, scalar : float) =
        let creator = AtomicSymbolCreator.FromName "_scatter_minus_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"|]
                                                 [|scalar.ToString()|]
        outputs

    /// <summary>Adds all input arguments element-wise.
    /// 
    /// .. math::
    ///    add\_n(a_1, a_2, ..., a_n) = a_1 + a_2 + ... + a_n
    /// 
    /// ``add_n`` is potentially more efficient than calling ``add`` by `n` times.
    /// 
    /// The storage type of ``add_n`` output depends on storage types of inputs
    /// 
    /// - add_n(row_sparse, row_sparse, ..) = row_sparse
    /// - add_n(default, csr, default) = default
    /// - add_n(any input combinations longer than 4 (&gt;4) with at least one default type) = default
    /// - otherwise, ``add_n`` falls all inputs back to default storage and generates default storage
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_sum.cc:L155</summary>
    /// <param name="args">Positional input arguments</param>
    static member AddN([<ParamArray>] args : NDArray[]) =
        let creator = AtomicSymbolCreator.FromName "add_n"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Computes rectified linear activation.
    /// 
    /// .. math::
    ///    max(features, 0)
    /// 
    /// The storage type of ``relu`` output depends upon the input storage type:
    /// 
    ///    - relu(default) = default
    ///    - relu(row_sparse) = row_sparse
    ///    - relu(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L85</summary>
    /// <param name="data">The input array.</param>
    static member Relu(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "relu"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardRelu(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_relu"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Computes sigmoid of x element-wise.
    /// 
    /// .. math::
    ///    y = 1 / (1 + exp(-x))
    /// 
    /// The storage type of ``sigmoid`` output is always dense
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L119</summary>
    /// <param name="data">The input array.</param>
    static member Sigmoid(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "sigmoid"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardSigmoid(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_sigmoid"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Computes hard sigmoid of x element-wise.
    /// 
    /// .. math::
    ///    y = max(0, min(1, alpha * x + beta))
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L133</summary>
    /// <param name="data">The input array.</param>
    /// <param name="alpha">Slope of hard sigmoid</param>
    /// <param name="beta">Bias of hard sigmoid.</param>
    static member HardSigmoid(data : NDArray, [<Optional; DefaultParameterValue(0.200000003)>] alpha : float, [<Optional; DefaultParameterValue(0.5)>] beta : float) =
        let creator = AtomicSymbolCreator.FromName "hard_sigmoid"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"alpha"; "beta"|]
                                                 [|alpha.ToString(); beta.ToString()|]
        outputs

    /// <summary>Computes softsign of x element-wise.
    /// 
    /// .. math::
    ///    y = x / (1 + abs(x))
    /// 
    /// The storage type of ``softsign`` output is always dense
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L163</summary>
    /// <param name="data">The input array.</param>
    static member Softsign(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "softsign"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardSoftsign(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_softsign"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns a copy of the input.
    /// 
    /// From:C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:218</summary>
    /// <param name="data">The input array.</param>
    static member Copy(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_copy"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Stops gradient computation.
    /// 
    /// Stops the accumulated gradient of the inputs from flowing through this operator
    /// in the backward direction. In other words, this operator prevents the contribution
    /// of its inputs to be taken into account for computing gradients.
    /// 
    /// Example::
    /// 
    ///   v1 = [1, 2]
    ///   v2 = [0, 1]
    ///   a = Variable(&#39;a&#39;)
    ///   b = Variable(&#39;b&#39;)
    ///   b_stop_grad = stop_gradient(3 * b)
    ///   loss = MakeLoss(b_stop_grad + a)
    /// 
    ///   executor = loss.simple_bind(ctx=cpu(), a=(1,2), b=(1,2))
    ///   executor.forward(is_train=True, a=v1, b=v2)
    ///   executor.outputs
    ///   [ 1.  5.]
    /// 
    ///   executor.backward()
    ///   executor.grad_arrays
    ///   [ 0.  0.]
    ///   [ 1.  1.]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L299</summary>
    /// <param name="data">The input array.</param>
    static member BlockGrad(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "BlockGrad"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Make your own loss function in network construction.
    /// 
    /// This operator accepts a customized loss function symbol as a terminal loss and
    /// the symbol should be an operator with no backward dependency.
    /// The output of this function is the gradient of loss with respect to the input data.
    /// 
    /// For example, if you are a making a cross entropy loss function. Assume ``out`` is the
    /// predicted output and ``label`` is the true label, then the cross entropy can be defined as::
    /// 
    ///   cross_entropy = label * log(out) + (1 - label) * log(1 - out)
    ///   loss = make_loss(cross_entropy)
    /// 
    /// We will need to use ``make_loss`` when we are creating our own loss function or we want to
    /// combine multiple loss functions. Also we may want to stop some variables&#39; gradients
    /// from backpropagation. See more detail in ``BlockGrad`` or ``stop_gradient``.
    /// 
    /// The storage type of ``make_loss`` output depends upon the input storage type:
    /// 
    ///    - make_loss(default) = default
    ///    - make_loss(row_sparse) = row_sparse
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L332</summary>
    /// <param name="data">The input array.</param>
    static member MakeLoss(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "make_loss"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">First input.</param>
    /// <param name="rhs">Second input.</param>
    static member IdentityWithAttrLikeRhs(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_identity_with_attr_like_rhs"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Reshape some or all dimensions of `lhs` to have the same shape as some or all dimensions of `rhs`.
    /// 
    /// Returns a **view** of the `lhs` array with a new shape without altering any data.
    /// 
    /// Example::
    /// 
    ///   x = [1, 2, 3, 4, 5, 6]
    ///   y = [[0, -4], [3, 2], [2, 2]]
    ///   reshape_like(x, y) = [[1, 2], [3, 4], [5, 6]]
    /// 
    /// More precise control over how dimensions are inherited is achieved by specifying \
    /// slices over the `lhs` and `rhs` array dimensions. Only the sliced `lhs` dimensions \
    /// are reshaped to the `rhs` sliced dimensions, with the non-sliced `lhs` dimensions staying the same.
    /// 
    ///   Examples::
    /// 
    ///   - lhs shape = (30,7), rhs shape = (15,2,4), lhs_begin=0, lhs_end=1, rhs_begin=0, rhs_end=2, output shape = (15,2,7)
    ///   - lhs shape = (3, 5), rhs shape = (1,15,4), lhs_begin=0, lhs_end=2, rhs_begin=1, rhs_end=2, output shape = (15)
    /// 
    /// Negative indices are supported, and `None` can be used for either `lhs_end` or `rhs_end` to indicate the end of the range.
    /// 
    ///   Example::
    /// 
    ///   - lhs shape = (30, 12), rhs shape = (4, 2, 2, 3), lhs_begin=-1, lhs_end=None, rhs_begin=1, rhs_end=None, output shape = (30, 2, 2, 3)
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L485</summary>
    /// <param name="lhs">First input.</param>
    /// <param name="rhs">Second input.</param>
    static member ReshapeLike(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "reshape_like"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns a 1D int64 array containing the shape of data.
    /// 
    /// Example::
    /// 
    ///   shape_array([[1,2,3,4], [5,6,7,8]]) = [2,4]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L544</summary>
    /// <param name="data">Input Array.</param>
    /// <param name="lhsBegin">Defaults to 0. The beginning index along which the lhs dimensions are to be reshaped. Supports negative indices.</param>
    /// <param name="lhsEnd">Defaults to None. The ending index along which the lhs dimensions are to be used for reshaping. Supports negative indices.</param>
    /// <param name="rhsBegin">Defaults to 0. The beginning index along which the rhs dimensions are to be used for reshaping. Supports negative indices.</param>
    /// <param name="rhsEnd">Defaults to None. The ending index along which the rhs dimensions are to be used for reshaping. Supports negative indices.</param>
    static member ShapeArray(data : NDArray, 
                             [<Optional>] lhsBegin : int Nullable, 
                             [<Optional>] lhsEnd : int Nullable, 
                             [<Optional>] rhsBegin : int Nullable, 
                             [<Optional>] rhsEnd : int Nullable) =
        let creator = AtomicSymbolCreator.FromName "shape_array"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"lhs_begin"; "lhs_end"; "rhs_begin"; "rhs_end"|]
                                                 [|lhsBegin.ToString(); lhsEnd.ToString(); rhsBegin.ToString(); rhsEnd.ToString()|]
        outputs

    /// <summary>Returns a 1D int64 array containing the shape of data.
    /// 
    /// Example::
    /// 
    ///   shape_array([[1,2,3,4], [5,6,7,8]]) = [2,4]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L544</summary>
    /// <param name="data">Input Array.</param>
    /// <param name="lhsBegin">Defaults to 0. The beginning index along which the lhs dimensions are to be reshaped. Supports negative indices.</param>
    /// <param name="lhsEnd">Defaults to None. The ending index along which the lhs dimensions are to be used for reshaping. Supports negative indices.</param>
    /// <param name="rhsBegin">Defaults to 0. The beginning index along which the rhs dimensions are to be used for reshaping. Supports negative indices.</param>
    /// <param name="rhsEnd">Defaults to None. The ending index along which the rhs dimensions are to be used for reshaping. Supports negative indices.</param>
    static member ShapeArray(data : NDArray, 
                             ?lhsBegin : int, 
                             ?lhsEnd : int, 
                             ?rhsBegin : int, 
                             ?rhsEnd : int) =
        let creator = AtomicSymbolCreator.FromName "shape_array"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"lhs_begin"; "lhs_end"; "rhs_begin"; "rhs_end"|]
                                                 [|(match lhsBegin with None -> "None" | _ -> lhsBegin.ToString()); (match lhsEnd with None -> "None" | _ -> lhsEnd.ToString()); (match rhsBegin with None -> "None" | _ -> rhsBegin.ToString()); (match rhsEnd with None -> "None" | _ -> rhsEnd.ToString())|]
        outputs

    /// <summary>Returns a 1D int64 array containing the size of data.
    /// 
    /// Example::
    /// 
    ///   size_array([[1,2,3,4], [5,6,7,8]]) = [8]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L596</summary>
    /// <param name="data">Input Array.</param>
    static member SizeArray(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "size_array"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Casts all elements of the input to a new type.
    /// 
    /// .. note:: ``Cast`` is deprecated. Use ``cast`` instead.
    /// 
    /// Example::
    /// 
    ///    cast([0.9, 1.3], dtype=&#39;int32&#39;) = [0, 1]
    ///    cast([1e20, 11.1], dtype=&#39;float16&#39;) = [inf, 11.09375]
    ///    cast([300, 11.1, 10.9, -1, -3], dtype=&#39;uint8&#39;) = [44, 11, 10, 255, 253]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L634</summary>
    /// <param name="data">The input.</param>
    /// <param name="dtype">Output data type.</param>
    static member Cast(data : NDArray, dtype : CastDtype) =
        let creator = AtomicSymbolCreator.FromName "Cast"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"dtype"|]
                                                 [|dtype.ToString()|]
        outputs

    /// <summary>Numerical negative of the argument, element-wise.
    /// 
    /// The storage type of ``negative`` output depends upon the input storage type:
    /// 
    ///    - negative(default) = default
    ///    - negative(row_sparse) = row_sparse
    ///    - negative(csr) = csr
    /// 
    /// </summary>
    /// <param name="data">The input array.</param>
    static member Negative(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "negative"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the reciprocal of the argument, element-wise.
    /// 
    /// Calculates 1/x.
    /// 
    /// Example::
    /// 
    ///     reciprocal([-2, 1, 3, 1.6, 0.2]) = [-0.5, 1.0, 0.33333334, 0.625, 5.0]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L686</summary>
    /// <param name="data">The input array.</param>
    static member Reciprocal(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "reciprocal"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardReciprocal(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_reciprocal"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise absolute value of the input.
    /// 
    /// Example::
    /// 
    ///    abs([-2, 0, 3]) = [2, 0, 3]
    /// 
    /// The storage type of ``abs`` output depends upon the input storage type:
    /// 
    ///    - abs(default) = default
    ///    - abs(row_sparse) = row_sparse
    ///    - abs(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L708</summary>
    /// <param name="data">The input array.</param>
    static member Abs(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "abs"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardAbs(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_abs"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise sign of the input.
    /// 
    /// Example::
    /// 
    ///    sign([-2, 0, 3]) = [-1, 0, 1]
    /// 
    /// The storage type of ``sign`` output depends upon the input storage type:
    /// 
    ///    - sign(default) = default
    ///    - sign(row_sparse) = row_sparse
    ///    - sign(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L727</summary>
    /// <param name="data">The input array.</param>
    static member Sign(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "sign"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardSign(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_sign"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise rounded value to the nearest integer of the input.
    /// 
    /// Example::
    /// 
    ///    round([-1.5, 1.5, -1.9, 1.9, 2.1]) = [-2.,  2., -2.,  2.,  2.]
    /// 
    /// The storage type of ``round`` output depends upon the input storage type:
    /// 
    ///   - round(default) = default
    ///   - round(row_sparse) = row_sparse
    ///   - round(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L746</summary>
    /// <param name="data">The input array.</param>
    static member Round(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "round"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise rounded value to the nearest integer of the input.
    /// 
    /// .. note::
    ///    - For input ``n.5`` ``rint`` returns ``n`` while ``round`` returns ``n+1``.
    ///    - For input ``-n.5`` both ``rint`` and ``round`` returns ``-n-1``.
    /// 
    /// Example::
    /// 
    ///    rint([-1.5, 1.5, -1.9, 1.9, 2.1]) = [-2.,  1., -2.,  2.,  2.]
    /// 
    /// The storage type of ``rint`` output depends upon the input storage type:
    /// 
    ///    - rint(default) = default
    ///    - rint(row_sparse) = row_sparse
    ///    - rint(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L767</summary>
    /// <param name="data">The input array.</param>
    static member Rint(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "rint"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise ceiling of the input.
    /// 
    /// The ceil of the scalar x is the smallest integer i, such that i &gt;= x.
    /// 
    /// Example::
    /// 
    ///    ceil([-2.1, -1.9, 1.5, 1.9, 2.1]) = [-2., -1.,  2.,  2.,  3.]
    /// 
    /// The storage type of ``ceil`` output depends upon the input storage type:
    /// 
    ///    - ceil(default) = default
    ///    - ceil(row_sparse) = row_sparse
    ///    - ceil(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L786</summary>
    /// <param name="data">The input array.</param>
    static member Ceil(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "ceil"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise floor of the input.
    /// 
    /// The floor of the scalar x is the largest integer i, such that i &lt;= x.
    /// 
    /// Example::
    /// 
    ///    floor([-2.1, -1.9, 1.5, 1.9, 2.1]) = [-3., -2.,  1.,  1.,  2.]
    /// 
    /// The storage type of ``floor`` output depends upon the input storage type:
    /// 
    ///    - floor(default) = default
    ///    - floor(row_sparse) = row_sparse
    ///    - floor(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L805</summary>
    /// <param name="data">The input array.</param>
    static member Floor(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "floor"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Return the element-wise truncated value of the input.
    /// 
    /// The truncated value of the scalar x is the nearest integer i which is closer to
    /// zero than x is. In short, the fractional part of the signed number x is discarded.
    /// 
    /// Example::
    /// 
    ///    trunc([-2.1, -1.9, 1.5, 1.9, 2.1]) = [-2., -1.,  1.,  1.,  2.]
    /// 
    /// The storage type of ``trunc`` output depends upon the input storage type:
    /// 
    ///    - trunc(default) = default
    ///    - trunc(row_sparse) = row_sparse
    ///    - trunc(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L825</summary>
    /// <param name="data">The input array.</param>
    static member Trunc(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "trunc"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise rounded value to the nearest \
    /// integer towards zero of the input.
    /// 
    /// Example::
    /// 
    ///    fix([-2.1, -1.9, 1.9, 2.1]) = [-2., -1.,  1., 2.]
    /// 
    /// The storage type of ``fix`` output depends upon the input storage type:
    /// 
    ///    - fix(default) = default
    ///    - fix(row_sparse) = row_sparse
    ///    - fix(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L843</summary>
    /// <param name="data">The input array.</param>
    static member Fix(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "fix"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise squared value of the input.
    /// 
    /// .. math::
    ///    square(x) = x^2
    /// 
    /// Example::
    /// 
    ///    square([2, 3, 4]) = [4, 9, 16]
    /// 
    /// The storage type of ``square`` output depends upon the input storage type:
    /// 
    ///    - square(default) = default
    ///    - square(row_sparse) = row_sparse
    ///    - square(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L883</summary>
    /// <param name="data">The input array.</param>
    static member Square(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "square"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardSquare(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_square"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise square-root value of the input.
    /// 
    /// .. math::
    ///    \textrm{sqrt}(x) = \sqrt{x}
    /// 
    /// Example::
    /// 
    ///    sqrt([4, 9, 16]) = [2, 3, 4]
    /// 
    /// The storage type of ``sqrt`` output depends upon the input storage type:
    /// 
    ///    - sqrt(default) = default
    ///    - sqrt(row_sparse) = row_sparse
    ///    - sqrt(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L907</summary>
    /// <param name="data">The input array.</param>
    static member Sqrt(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "sqrt"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardSqrt(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_sqrt"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise inverse square-root value of the input.
    /// 
    /// .. math::
    ///    rsqrt(x) = 1/\sqrt{x}
    /// 
    /// Example::
    /// 
    ///    rsqrt([4,9,16]) = [0.5, 0.33333334, 0.25]
    /// 
    /// The storage type of ``rsqrt`` output is always dense
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L927</summary>
    /// <param name="data">The input array.</param>
    static member Rsqrt(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "rsqrt"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardRsqrt(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_rsqrt"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise cube-root value of the input.
    /// 
    /// .. math::
    ///    cbrt(x) = \sqrt[3]{x}
    /// 
    /// Example::
    /// 
    ///    cbrt([1, 8, -125]) = [1, 2, -5]
    /// 
    /// The storage type of ``cbrt`` output depends upon the input storage type:
    /// 
    ///    - cbrt(default) = default
    ///    - cbrt(row_sparse) = row_sparse
    ///    - cbrt(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L950</summary>
    /// <param name="data">The input array.</param>
    static member Cbrt(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "cbrt"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardCbrt(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_cbrt"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise gauss error function of the input.
    /// 
    /// Example::
    /// 
    ///    erf([0, -1., 10.]) = [0., -0.8427, 1.]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L964</summary>
    /// <param name="data">The input array.</param>
    static member Erf(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "erf"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardErf(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_erf"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise inverse gauss error function of the input.
    /// 
    /// Example::
    /// 
    ///    erfinv([0, 0.5., -1.]) = [0., 0.4769, -inf]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L985</summary>
    /// <param name="data">The input array.</param>
    static member Erfinv(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "erfinv"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardErfinv(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_erfinv"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise inverse cube-root value of the input.
    /// 
    /// .. math::
    ///    rcbrt(x) = 1/\sqrt[3]{x}
    /// 
    /// Example::
    /// 
    ///    rcbrt([1,8,-125]) = [1.0, 0.5, -0.2]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L1004</summary>
    /// <param name="data">The input array.</param>
    static member Rcbrt(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "rcbrt"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardRcbrt(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_rcbrt"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise exponential value of the input.
    /// 
    /// .. math::
    ///    exp(x) = e^x \approx 2.718^x
    /// 
    /// Example::
    /// 
    ///    exp([0, 1, 2]) = [1., 2.71828175, 7.38905621]
    /// 
    /// The storage type of ``exp`` output is always dense
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L1044</summary>
    /// <param name="data">The input array.</param>
    static member Exp(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "exp"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise Natural logarithmic value of the input.
    /// 
    /// The natural logarithm is logarithm in base *e*, so that ``log(exp(x)) = x``
    /// 
    /// The storage type of ``log`` output is always dense
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L1057</summary>
    /// <param name="data">The input array.</param>
    static member Log(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "log"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise Base-10 logarithmic value of the input.
    /// 
    /// ``10**log10(x) = x``
    /// 
    /// The storage type of ``log10`` output is always dense
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L1074</summary>
    /// <param name="data">The input array.</param>
    static member Log10(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "log10"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise Base-2 logarithmic value of the input.
    /// 
    /// ``2**log2(x) = x``
    /// 
    /// The storage type of ``log2`` output is always dense
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L1086</summary>
    /// <param name="data">The input array.</param>
    static member Log2(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "log2"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardLog(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_log"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardLog10(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_log10"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardLog2(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_log2"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise ``log(1 + x)`` value of the input.
    /// 
    /// This function is more accurate than ``log(1 + x)``  for small ``x`` so that
    /// :math:`1+x\approx 1`
    /// 
    /// The storage type of ``log1p`` output depends upon the input storage type:
    /// 
    ///    - log1p(default) = default
    ///    - log1p(row_sparse) = row_sparse
    ///    - log1p(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L1171</summary>
    /// <param name="data">The input array.</param>
    static member Log1p(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "log1p"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardLog1p(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_log1p"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns ``exp(x) - 1`` computed element-wise on the input.
    /// 
    /// This function provides greater precision than ``exp(x) - 1`` for small values of ``x``.
    /// 
    /// The storage type of ``expm1`` output depends upon the input storage type:
    /// 
    ///    - expm1(default) = default
    ///    - expm1(row_sparse) = row_sparse
    ///    - expm1(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_basic.cc:L1189</summary>
    /// <param name="data">The input array.</param>
    static member Expm1(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "expm1"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardExpm1(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_expm1"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the gamma function (extension of the factorial function \
    /// to the reals), computed element-wise on the input array.
    /// 
    /// The storage type of ``gamma`` output is always dense
    /// 
    /// </summary>
    /// <param name="data">The input array.</param>
    static member Gamma(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "gamma"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardGamma(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_gamma"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise log of the absolute value of the gamma function \
    /// of the input.
    /// 
    /// The storage type of ``gammaln`` output is always dense
    /// 
    /// </summary>
    /// <param name="data">The input array.</param>
    static member Gammaln(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "gammaln"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardGammaln(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_gammaln"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the result of logical NOT (!) function
    /// 
    /// Example:
    ///   logical_not([-2., 0., 1.]) = [0., 1., 0.]
    /// 
    /// </summary>
    /// <param name="data">The input array.</param>
    static member LogicalNot(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "logical_not"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Computes the element-wise sine of the input array.
    /// 
    /// The input should be in radians (:math:`2\pi` rad equals 360 degrees).
    /// 
    /// .. math::
    ///    sin([0, \pi/4, \pi/2]) = [0, 0.707, 1]
    /// 
    /// The storage type of ``sin`` output depends upon the input storage type:
    /// 
    ///    - sin(default) = default
    ///    - sin(row_sparse) = row_sparse
    ///    - sin(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_trig.cc:L46</summary>
    /// <param name="data">The input array.</param>
    static member Sin(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "sin"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardSin(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_sin"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Computes the element-wise cosine of the input array.
    /// 
    /// The input should be in radians (:math:`2\pi` rad equals 360 degrees).
    /// 
    /// .. math::
    ///    cos([0, \pi/4, \pi/2]) = [1, 0.707, 0]
    /// 
    /// The storage type of ``cos`` output is always dense
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_trig.cc:L89</summary>
    /// <param name="data">The input array.</param>
    static member Cos(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "cos"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardCos(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_cos"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Computes the element-wise tangent of the input array.
    /// 
    /// The input should be in radians (:math:`2\pi` rad equals 360 degrees).
    /// 
    /// .. math::
    ///    tan([0, \pi/4, \pi/2]) = [0, 1, -inf]
    /// 
    /// The storage type of ``tan`` output depends upon the input storage type:
    /// 
    ///    - tan(default) = default
    ///    - tan(row_sparse) = row_sparse
    ///    - tan(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_trig.cc:L139</summary>
    /// <param name="data">The input array.</param>
    static member Tan(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "tan"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardTan(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_tan"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise inverse sine of the input array.
    /// 
    /// The input should be in the range `[-1, 1]`.
    /// The output is in the closed interval of [:math:`-\pi/2`, :math:`\pi/2`].
    /// 
    /// .. math::
    ///    arcsin([-1, -.707, 0, .707, 1]) = [-\pi/2, -\pi/4, 0, \pi/4, \pi/2]
    /// 
    /// The storage type of ``arcsin`` output depends upon the input storage type:
    /// 
    ///    - arcsin(default) = default
    ///    - arcsin(row_sparse) = row_sparse
    ///    - arcsin(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_trig.cc:L160</summary>
    /// <param name="data">The input array.</param>
    static member Arcsin(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "arcsin"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardArcsin(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_arcsin"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise inverse cosine of the input array.
    /// 
    /// The input should be in range `[-1, 1]`.
    /// The output is in the closed interval :math:`[0, \pi]`
    /// 
    /// .. math::
    ///    arccos([-1, -.707, 0, .707, 1]) = [\pi, 3\pi/4, \pi/2, \pi/4, 0]
    /// 
    /// The storage type of ``arccos`` output is always dense
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_trig.cc:L179</summary>
    /// <param name="data">The input array.</param>
    static member Arccos(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "arccos"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardArccos(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_arccos"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns element-wise inverse tangent of the input array.
    /// 
    /// The output is in the closed interval :math:`[-\pi/2, \pi/2]`
    /// 
    /// .. math::
    ///    arctan([-1, 0, 1]) = [-\pi/4, 0, \pi/4]
    /// 
    /// The storage type of ``arctan`` output depends upon the input storage type:
    /// 
    ///    - arctan(default) = default
    ///    - arctan(row_sparse) = row_sparse
    ///    - arctan(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_trig.cc:L200</summary>
    /// <param name="data">The input array.</param>
    static member Arctan(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "arctan"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardArctan(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_arctan"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Converts each element of the input array from radians to degrees.
    /// 
    /// .. math::
    ///    degrees([0, \pi/2, \pi, 3\pi/2, 2\pi]) = [0, 90, 180, 270, 360]
    /// 
    /// The storage type of ``degrees`` output depends upon the input storage type:
    /// 
    ///    - degrees(default) = default
    ///    - degrees(row_sparse) = row_sparse
    ///    - degrees(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_trig.cc:L219</summary>
    /// <param name="data">The input array.</param>
    static member Degrees(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "degrees"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardDegrees(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_degrees"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Converts each element of the input array from degrees to radians.
    /// 
    /// .. math::
    ///    radians([0, 90, 180, 270, 360]) = [0, \pi/2, \pi, 3\pi/2, 2\pi]
    /// 
    /// The storage type of ``radians`` output depends upon the input storage type:
    /// 
    ///    - radians(default) = default
    ///    - radians(row_sparse) = row_sparse
    ///    - radians(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_trig.cc:L238</summary>
    /// <param name="data">The input array.</param>
    static member Radians(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "radians"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardRadians(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_radians"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the hyperbolic sine of the input array, computed element-wise.
    /// 
    /// .. math::
    ///    sinh(x) = 0.5\times(exp(x) - exp(-x))
    /// 
    /// The storage type of ``sinh`` output depends upon the input storage type:
    /// 
    ///    - sinh(default) = default
    ///    - sinh(row_sparse) = row_sparse
    ///    - sinh(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_trig.cc:L257</summary>
    /// <param name="data">The input array.</param>
    static member Sinh(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "sinh"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardSinh(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_sinh"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the hyperbolic cosine  of the input array, computed element-wise.
    /// 
    /// .. math::
    ///    cosh(x) = 0.5\times(exp(x) + exp(-x))
    /// 
    /// The storage type of ``cosh`` output is always dense
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_trig.cc:L272</summary>
    /// <param name="data">The input array.</param>
    static member Cosh(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "cosh"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardCosh(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_cosh"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the hyperbolic tangent of the input array, computed element-wise.
    /// 
    /// .. math::
    ///    tanh(x) = sinh(x) / cosh(x)
    /// 
    /// The storage type of ``tanh`` output depends upon the input storage type:
    /// 
    ///    - tanh(default) = default
    ///    - tanh(row_sparse) = row_sparse
    ///    - tanh(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_trig.cc:L290</summary>
    /// <param name="data">The input array.</param>
    static member Tanh(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "tanh"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardTanh(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_tanh"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the element-wise inverse hyperbolic sine of the input array, \
    /// computed element-wise.
    /// 
    /// The storage type of ``arcsinh`` output depends upon the input storage type:
    /// 
    ///    - arcsinh(default) = default
    ///    - arcsinh(row_sparse) = row_sparse
    ///    - arcsinh(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_trig.cc:L306</summary>
    /// <param name="data">The input array.</param>
    static member Arcsinh(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "arcsinh"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardArcsinh(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_arcsinh"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the element-wise inverse hyperbolic cosine of the input array, \
    /// computed element-wise.
    /// 
    /// The storage type of ``arccosh`` output is always dense
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_trig.cc:L320</summary>
    /// <param name="data">The input array.</param>
    static member Arccosh(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "arccosh"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardArccosh(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_arccosh"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns the element-wise inverse hyperbolic tangent of the input array, \
    /// computed element-wise.
    /// 
    /// The storage type of ``arctanh`` output depends upon the input storage type:
    /// 
    ///    - arctanh(default) = default
    ///    - arctanh(row_sparse) = row_sparse
    ///    - arctanh(csr) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\elemwise_unary_op_trig.cc:L337</summary>
    /// <param name="data">The input array.</param>
    static member Arctanh(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "arctanh"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <param name="lhs">first input</param>
    /// <param name="rhs">second input</param>
    static member BackwardArctanh(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_backward_arctanh"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>This operators implements the histogram function.
    /// 
    /// Example::
    ///   x = [[0, 1], [2, 2], [3, 4]]
    ///   histo, bin_edges = histogram(data=x, bin_bounds=[], bin_cnt=5, range=(0,5))
    ///   histo = [1, 1, 2, 1, 1]
    ///   bin_edges = [0., 1., 2., 3., 4.]
    ///   histo, bin_edges = histogram(data=x, bin_bounds=[0., 2.1, 3.])
    ///   histo = [4, 1]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\histogram.cc:L136</summary>
    /// <param name="data">Input ndarray</param>
    /// <param name="bins">Input ndarray</param>
    /// <param name="binCnt">Number of bins for uniform case</param>
    /// <param name="range">The lower and upper range of the bins. if not provided, range is simply (a.min(), a.max()). values outside the range are ignored. the first element of the range must be less than or equal to the second. range affects the automatic bin computation as well. while bin width is computed to be optimal based on the actual data within range, the bin count will fill the entire range including portions containing no data.</param>
    static member Histogram(data : NDArray, bins : NDArray, [<Optional>] binCnt : int Nullable, [<Optional>] range : struct(float*float)) =
        let creator = AtomicSymbolCreator.FromName "_histogram"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; bins.NDArrayHandle|]
                                                 [|"bin_cnt"; "range"|]
                                                 [|binCnt.ToString(); (if isNull (range :> obj) then "None" else range.ToString())|]
        outputs

    /// <summary>This operators implements the histogram function.
    /// 
    /// Example::
    ///   x = [[0, 1], [2, 2], [3, 4]]
    ///   histo, bin_edges = histogram(data=x, bin_bounds=[], bin_cnt=5, range=(0,5))
    ///   histo = [1, 1, 2, 1, 1]
    ///   bin_edges = [0., 1., 2., 3., 4.]
    ///   histo, bin_edges = histogram(data=x, bin_bounds=[0., 2.1, 3.])
    ///   histo = [4, 1]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\histogram.cc:L136</summary>
    /// <param name="data">Input ndarray</param>
    /// <param name="bins">Input ndarray</param>
    /// <param name="binCnt">Number of bins for uniform case</param>
    /// <param name="range">The lower and upper range of the bins. if not provided, range is simply (a.min(), a.max()). values outside the range are ignored. the first element of the range must be less than or equal to the second. range affects the automatic bin computation as well. while bin width is computed to be optimal based on the actual data within range, the bin count will fill the entire range including portions containing no data.</param>
    static member Histogram(data : NDArray, bins : NDArray, ?binCnt : int, ?range : struct(float*float)) =
        let creator = AtomicSymbolCreator.FromName "_histogram"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; bins.NDArrayHandle|]
                                                 [|"bin_cnt"; "range"|]
                                                 [|(match binCnt with None -> "None" | _ -> binCnt.ToString()); (match range with None -> "None" | _ -> range.ToString())|]
        outputs

    /// <summary>This operators implements the histogram function.
    /// 
    /// Example::
    ///   x = [[0, 1], [2, 2], [3, 4]]
    ///   histo, bin_edges = histogram(data=x, bin_bounds=[], bin_cnt=5, range=(0,5))
    ///   histo = [1, 1, 2, 1, 1]
    ///   bin_edges = [0., 1., 2., 3., 4.]
    ///   histo, bin_edges = histogram(data=x, bin_bounds=[0., 2.1, 3.])
    ///   histo = [4, 1]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\histogram.cc:L136</summary>
    /// <param name="data">Input ndarray</param>
    /// <param name="bins">Input ndarray</param>
    /// <param name="binCnt">Number of bins for uniform case</param>
    /// <param name="range">The lower and upper range of the bins. if not provided, range is simply (a.min(), a.max()). values outside the range are ignored. the first element of the range must be less than or equal to the second. range affects the automatic bin computation as well. while bin width is computed to be optimal based on the actual data within range, the bin count will fill the entire range including portions containing no data.</param>
    static member Histogram(data : NDArray, bins : NDArray, [<Optional>] binCnt : int Nullable, [<Optional>] range : float*float) =
        let creator = AtomicSymbolCreator.FromName "_histogram"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; bins.NDArrayHandle|]
                                                 [|"bin_cnt"; "range"|]
                                                 [|binCnt.ToString(); (if isNull (range :> obj) then "None" else range.ToString())|]
        outputs

    /// <summary>This operators implements the histogram function.
    /// 
    /// Example::
    ///   x = [[0, 1], [2, 2], [3, 4]]
    ///   histo, bin_edges = histogram(data=x, bin_bounds=[], bin_cnt=5, range=(0,5))
    ///   histo = [1, 1, 2, 1, 1]
    ///   bin_edges = [0., 1., 2., 3., 4.]
    ///   histo, bin_edges = histogram(data=x, bin_bounds=[0., 2.1, 3.])
    ///   histo = [4, 1]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\histogram.cc:L136</summary>
    /// <param name="data">Input ndarray</param>
    /// <param name="bins">Input ndarray</param>
    /// <param name="binCnt">Number of bins for uniform case</param>
    /// <param name="range">The lower and upper range of the bins. if not provided, range is simply (a.min(), a.max()). values outside the range are ignored. the first element of the range must be less than or equal to the second. range affects the automatic bin computation as well. while bin width is computed to be optimal based on the actual data within range, the bin count will fill the entire range including portions containing no data.</param>
    static member Histogram(data : NDArray, bins : NDArray, ?binCnt : int, ?range : float*float) =
        let creator = AtomicSymbolCreator.FromName "_histogram"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; bins.NDArrayHandle|]
                                                 [|"bin_cnt"; "range"|]
                                                 [|(match binCnt with None -> "None" | _ -> binCnt.ToString()); (match range with None -> "None" | _ -> range.ToString())|]
        outputs

    /// <summary>Maps integer indices to vector representations (embeddings).
    /// 
    /// This operator maps words to real-valued vectors in a high-dimensional space,
    /// called word embeddings. These embeddings can capture semantic and syntactic properties of the words.
    /// For example, it has been noted that in the learned embedding spaces, similar words tend
    /// to be close to each other and dissimilar words far apart.
    /// 
    /// For an input array of shape (d1, ..., dK),
    /// the shape of an output array is (d1, ..., dK, output_dim).
    /// All the input values should be integers in the range [0, input_dim).
    /// 
    /// If the input_dim is ip0 and output_dim is op0, then shape of the embedding weight matrix must be
    /// (ip0, op0).
    /// 
    /// By default, if any index mentioned is too large, it is replaced by the index that addresses
    /// the last vector in an embedding matrix.
    /// 
    /// Examples::
    /// 
    ///   input_dim = 4
    ///   output_dim = 5
    /// 
    ///   // Each row in weight matrix y represents a word. So, y = (w0,w1,w2,w3)
    ///   y = [[  0.,   1.,   2.,   3.,   4.],
    ///        [  5.,   6.,   7.,   8.,   9.],
    ///        [ 10.,  11.,  12.,  13.,  14.],
    ///        [ 15.,  16.,  17.,  18.,  19.]]
    /// 
    ///   // Input array x represents n-grams(2-gram). So, x = [(w1,w3), (w0,w2)]
    ///   x = [[ 1.,  3.],
    ///        [ 0.,  2.]]
    /// 
    ///   // Mapped input x to its vector representation y.
    ///   Embedding(x, y, 4, 5) = [[[  5.,   6.,   7.,   8.,   9.],
    ///                             [ 15.,  16.,  17.,  18.,  19.]],
    /// 
    ///                            [[  0.,   1.,   2.,   3.,   4.],
    ///                             [ 10.,  11.,  12.,  13.,  14.]]]
    /// 
    /// 
    /// The storage type of weight can be either row_sparse or default.
    /// 
    /// .. Note::
    /// 
    ///     If &quot;sparse_grad&quot; is set to True, the storage type of gradient w.r.t weights will be
    ///     &quot;row_sparse&quot;. Only a subset of optimizers support sparse gradients, including SGD, AdaGrad
    ///     and Adam. Note that by default lazy updates is turned on, which may perform differently
    ///     from standard updates. For more details, please check the Optimization API at:
    ///     https://mxnet.incubator.apache.org/api/python/optimization/optimization.html
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\indexing_op.cc:L519</summary>
    /// <param name="data">The input array to the embedding operator.</param>
    /// <param name="weight">The embedding weight matrix.</param>
    /// <param name="inputDim">Vocabulary size of the input indices.</param>
    /// <param name="outputDim">Dimension of the embedding vectors.</param>
    /// <param name="dtype">Data type of weight.</param>
    /// <param name="sparseGrad">Compute row sparse gradient in the backward calculation. If set to True, the grad&#39;s storage type is row_sparse.</param>
    static member Embedding(data : NDArray, 
                            weight : NDArray, 
                            inputDim : int, 
                            outputDim : int, 
                            [<Optional>] dtype : EmbeddingDtype, 
                            [<Optional; DefaultParameterValue(false)>] sparseGrad : bool) =
        let creator = AtomicSymbolCreator.FromName "Embedding"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; weight.NDArrayHandle|]
                                                 [|"input_dim"; "output_dim"; "dtype"; "sparse_grad"|]
                                                 [|inputDim.ToString(); outputDim.ToString(); (if isNull (dtype :> obj) then "float32" else dtype.ToString()); sparseGrad.ToString()|]
        outputs

    /// <summary>Maps integer indices to vector representations (embeddings).
    /// 
    /// note:: ``contrib.SparseEmbedding`` is deprecated, use ``Embedding`` instead.
    /// 
    /// This operator maps words to real-valued vectors in a high-dimensional space,
    /// called word embeddings. These embeddings can capture semantic and syntactic properties of the words.
    /// For example, it has been noted that in the learned embedding spaces, similar words tend
    /// to be close to each other and dissimilar words far apart.
    /// 
    /// For an input array of shape (d1, ..., dK),
    /// the shape of an output array is (d1, ..., dK, output_dim).
    /// All the input values should be integers in the range [0, input_dim).
    /// 
    /// If the input_dim is ip0 and output_dim is op0, then shape of the embedding weight matrix must be
    /// (ip0, op0).
    /// 
    /// The storage type of the gradient will be `row_sparse`.
    /// 
    /// .. Note::
    /// 
    ///     `SparseEmbedding` is designed for the use case where `input_dim` is very large (e.g. 100k).
    ///     The operator is available on both CPU and GPU.
    ///     When `deterministic` is set to `True`, the accumulation of gradients follows a
    ///     deterministic order if a feature appears multiple times in the input. However, the
    ///     accumulation is usually slower when the order is enforced on GPU.
    ///     When the operator is used on the GPU, the recommended value for `deterministic` is `True`.
    /// 
    /// Examples::
    /// 
    ///   input_dim = 4
    ///   output_dim = 5
    /// 
    ///   // Each row in weight matrix y represents a word. So, y = (w0,w1,w2,w3)
    ///   y = [[  0.,   1.,   2.,   3.,   4.],
    ///        [  5.,   6.,   7.,   8.,   9.],
    ///        [ 10.,  11.,  12.,  13.,  14.],
    ///        [ 15.,  16.,  17.,  18.,  19.]]
    /// 
    ///   // Input array x represents n-grams(2-gram). So, x = [(w1,w3), (w0,w2)]
    ///   x = [[ 1.,  3.],
    ///        [ 0.,  2.]]
    /// 
    ///   // Mapped input x to its vector representation y.
    ///   SparseEmbedding(x, y, 4, 5) = [[[  5.,   6.,   7.,   8.,   9.],
    ///                                  [ 15.,  16.,  17.,  18.,  19.]],
    /// 
    ///                                 [[  0.,   1.,   2.,   3.,   4.],
    ///                                  [ 10.,  11.,  12.,  13.,  14.]]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\indexing_op.cc:L595</summary>
    /// <param name="data">The input array to the embedding operator.</param>
    /// <param name="weight">The embedding weight matrix.</param>
    /// <param name="inputDim">Vocabulary size of the input indices.</param>
    /// <param name="outputDim">Dimension of the embedding vectors.</param>
    /// <param name="dtype">Data type of weight.</param>
    /// <param name="sparseGrad">Compute row sparse gradient in the backward calculation. If set to True, the grad&#39;s storage type is row_sparse.</param>
    static member ContribSparseEmbedding(data : NDArray, 
                                         weight : NDArray, 
                                         inputDim : int, 
                                         outputDim : int, 
                                         [<Optional>] dtype : ContribSparseEmbeddingDtype, 
                                         [<Optional; DefaultParameterValue(false)>] sparseGrad : bool) =
        let creator = AtomicSymbolCreator.FromName "_contrib_SparseEmbedding"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; weight.NDArrayHandle|]
                                                 [|"input_dim"; "output_dim"; "dtype"; "sparse_grad"|]
                                                 [|inputDim.ToString(); outputDim.ToString(); (if isNull (dtype :> obj) then "float32" else dtype.ToString()); sparseGrad.ToString()|]
        outputs

    /// <summary>Takes elements from an input array along the given axis.
    /// 
    /// This function slices the input array along a particular axis with the provided indices.
    /// 
    /// Given data tensor of rank r &gt;= 1, and indices tensor of rank q, gather entries of the axis
    /// dimension of data (by default outer-most one as axis=0) indexed by indices, and concatenates them
    /// in an output tensor of rank q + (r - 1).
    /// 
    /// Examples::
    /// 
    ///   x = [4.  5.  6.]
    /// 
    ///   // Trivial case, take the second element along the first axis.
    /// 
    ///   take(x, [1]) = [ 5. ]
    /// 
    ///   // The other trivial case, axis=-1, take the third element along the first axis
    /// 
    ///   take(x, [3], axis=-1, mode=&#39;clip&#39;) = [ 6. ]
    /// 
    ///   x = [[ 1.,  2.],
    ///        [ 3.,  4.],
    ///        [ 5.,  6.]]
    /// 
    ///   // In this case we will get rows 0 and 1, then 1 and 2. Along axis 0
    /// 
    ///   take(x, [[0,1],[1,2]]) = [[[ 1.,  2.],
    ///                              [ 3.,  4.]],
    /// 
    ///                             [[ 3.,  4.],
    ///                              [ 5.,  6.]]]
    /// 
    ///   // In this case we will get rows 0 and 1, then 1 and 2 (calculated by wrapping around).
    ///   // Along axis 1
    /// 
    ///   take(x, [[0, 3], [-1, -2]], axis=1, mode=&#39;wrap&#39;) = [[[ 1.  2.]
    ///                                                        [ 2.  1.]]
    /// 
    ///                                                       [[ 3.  4.]
    ///                                                        [ 4.  3.]]
    /// 
    ///                                                       [[ 5.  6.]
    ///                                                        [ 6.  5.]]]
    /// 
    /// The storage type of ``take`` output depends upon the input storage type:
    /// 
    ///    - take(default, default) = default
    ///    - take(csr, default, axis=0) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\indexing_op.cc:L695</summary>
    /// <param name="a">The input array.</param>
    /// <param name="indices">The indices of the values to be extracted.</param>
    /// <param name="axis">The axis of input array to be taken.For input tensor of rank r, it could be in the range of [-r, r-1]</param>
    /// <param name="mode">Specify how out-of-bound indices bahave. Default is &quot;clip&quot;. &quot;clip&quot; means clip to the range. So, if all indices mentioned are too large, they are replaced by the index that addresses the last element along an axis.  &quot;wrap&quot; means to wrap around.  &quot;raise&quot; means to raise an error, not supported yet.</param>
    static member Take(a : NDArray, indices : NDArray, [<Optional; DefaultParameterValue(0)>] axis : int, [<Optional>] mode : TakeMode) =
        let creator = AtomicSymbolCreator.FromName "take"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|a.NDArrayHandle; indices.NDArrayHandle|]
                                                 [|"axis"; "mode"|]
                                                 [|axis.ToString(); (if isNull (mode :> obj) then "clip" else mode.ToString())|]
        outputs

    /// <summary>Takes elements from a data batch.
    /// 
    /// .. note::
    ///   `batch_take` is deprecated. Use `pick` instead.
    /// 
    /// Given an input array of shape ``(d0, d1)`` and indices of shape ``(i0,)``, the result will be
    /// an output array of shape ``(i0,)`` with::
    /// 
    ///   output[i] = input[i, indices[i]]
    /// 
    /// Examples::
    /// 
    ///   x = [[ 1.,  2.],
    ///        [ 3.,  4.],
    ///        [ 5.,  6.]]
    /// 
    ///   // takes elements with specified indices
    ///   batch_take(x, [0,1,0]) = [ 1.  4.  5.]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\indexing_op.cc:L753</summary>
    /// <param name="a">The input array</param>
    /// <param name="indices">The index array</param>
    static member BatchTake(a : NDArray, indices : NDArray) =
        let creator = AtomicSymbolCreator.FromName "batch_take"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|a.NDArrayHandle; indices.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Returns a one-hot array.
    /// 
    /// The locations represented by `indices` take value `on_value`, while all
    /// other locations take value `off_value`.
    /// 
    /// `one_hot` operation with `indices` of shape ``(i0, i1)`` and `depth`  of ``d`` would result
    /// in an output array of shape ``(i0, i1, d)`` with::
    /// 
    ///   output[i,j,:] = off_value
    ///   output[i,j,indices[i,j]] = on_value
    /// 
    /// Examples::
    /// 
    ///   one_hot([1,0,2,0], 3) = [[ 0.  1.  0.]
    ///                            [ 1.  0.  0.]
    ///                            [ 0.  0.  1.]
    ///                            [ 1.  0.  0.]]
    /// 
    ///   one_hot([1,0,2,0], 3, on_value=8, off_value=1,
    ///           dtype=&#39;int32&#39;) = [[1 8 1]
    ///                             [8 1 1]
    ///                             [1 1 8]
    ///                             [8 1 1]]
    /// 
    ///   one_hot([[1,0],[1,0],[2,0]], 3) = [[[ 0.  1.  0.]
    ///                                       [ 1.  0.  0.]]
    /// 
    ///                                      [[ 0.  1.  0.]
    ///                                       [ 1.  0.  0.]]
    /// 
    ///                                      [[ 0.  0.  1.]
    ///                                       [ 1.  0.  0.]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\indexing_op.cc:L799</summary>
    /// <param name="indices">array of locations where to set on_value</param>
    /// <param name="depth">Depth of the one hot dimension.</param>
    /// <param name="onValue">The value assigned to the locations represented by indices.</param>
    /// <param name="offValue">The value assigned to the locations not represented by indices.</param>
    /// <param name="dtype">DType of the output</param>
    static member OneHot(indices : NDArray, 
                         depth : int, 
                         onValue : double, 
                         offValue : double, 
                         [<Optional>] dtype : OneHotDtype) =
        let creator = AtomicSymbolCreator.FromName "one_hot"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|indices.NDArrayHandle|]
                                                 [|"depth"; "on_value"; "off_value"; "dtype"|]
                                                 [|depth.ToString(); onValue.ToString(); offValue.ToString(); (if isNull (dtype :> obj) then "float32" else dtype.ToString())|]
        outputs

    /// <summary>Gather elements or slices from `data` and store to a tensor whose
    /// shape is defined by `indices`.
    /// 
    /// Given `data` with shape `(X_0, X_1, ..., X_{N-1})` and indices with shape
    /// `(M, Y_0, ..., Y_{K-1})`, the output will have shape `(Y_0, ..., Y_{K-1}, X_M, ..., X_{N-1})`,
    /// where `M &lt;= N`. If `M == N`, output shape will simply be `(Y_0, ..., Y_{K-1})`.
    /// 
    /// The elements in output is defined as follows::
    /// 
    ///   output[y_0, ..., y_{K-1}, x_M, ..., x_{N-1}] = data[indices[0, y_0, ..., y_{K-1}],
    ///                                                       ...,
    ///                                                       indices[M-1, y_0, ..., y_{K-1}],
    ///                                                       x_M, ..., x_{N-1}]
    /// 
    /// Examples::
    /// 
    ///   data = [[0, 1], [2, 3]]
    ///   indices = [[1, 1, 0], [0, 1, 0]]
    ///   gather_nd(data, indices) = [2, 3, 0]
    /// 
    ///   data = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]]
    ///   indices = [[0, 1], [1, 0]]
    ///   gather_nd(data, indices) = [[3, 4], [5, 6]]
    /// 
    /// </summary>
    /// <param name="data">data</param>
    /// <param name="indices">indices</param>
    static member GatherNd(data : NDArray, indices : NDArray) =
        let creator = AtomicSymbolCreator.FromName "gather_nd"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; indices.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Scatters data into a new tensor according to indices.
    /// 
    /// Given `data` with shape `(Y_0, ..., Y_{K-1}, X_M, ..., X_{N-1})` and indices with shape
    /// `(M, Y_0, ..., Y_{K-1})`, the output will have shape `(X_0, X_1, ..., X_{N-1})`,
    /// where `M &lt;= N`. If `M == N`, data shape should simply be `(Y_0, ..., Y_{K-1})`.
    /// 
    /// The elements in output is defined as follows::
    /// 
    ///   output[indices[0, y_0, ..., y_{K-1}],
    ///          ...,
    ///          indices[M-1, y_0, ..., y_{K-1}],
    ///          x_M, ..., x_{N-1}] = data[y_0, ..., y_{K-1}, x_M, ..., x_{N-1}]
    /// 
    /// all other entries in output are 0.
    /// 
    /// .. warning::
    /// 
    ///     If the indices have duplicates, the result will be non-deterministic and
    ///     the gradient of `scatter_nd` will not be correct!!
    /// 
    /// 
    /// Examples::
    /// 
    ///   data = [2, 3, 0]
    ///   indices = [[1, 1, 0], [0, 1, 0]]
    ///   shape = (2, 2)
    ///   scatter_nd(data, indices, shape) = [[0, 0], [2, 3]]
    /// 
    ///   data = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]]
    ///   indices = [[0, 1], [1, 1]]
    ///   shape = (2, 2, 2, 2)
    ///   scatter_nd(data, indices, shape) = [[[[0, 0],
    ///                                         [0, 0]],
    /// 
    ///                                        [[1, 2],
    ///                                         [3, 4]]],
    /// 
    ///                                       [[[0, 0],
    ///                                         [0, 0]],
    /// 
    ///                                        [[5, 6],
    ///                                         [7, 8]]]]
    /// 
    /// </summary>
    /// <param name="data">data</param>
    /// <param name="indices">indices</param>
    /// <param name="shape">Shape of output.</param>
    static member ScatterNd(data : NDArray, indices : NDArray, shape : int seq) =
        let creator = AtomicSymbolCreator.FromName "scatter_nd"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; indices.NDArrayHandle|]
                                                 [|"shape"|]
                                                 [|shape.ToString()|]
        outputs

    /// <summary>Accumulates data according to indices and get the result. It&#39;s the backward of
    /// `gather_nd`.
    /// 
    /// Given `data` with shape `(Y_0, ..., Y_{K-1}, X_M, ..., X_{N-1})` and indices with shape
    /// `(M, Y_0, ..., Y_{K-1})`, the output will have shape `(X_0, X_1, ..., X_{N-1})`,
    /// where `M &lt;= N`. If `M == N`, data shape should simply be `(Y_0, ..., Y_{K-1})`.
    /// 
    /// The elements in output is defined as follows::
    /// 
    ///   output[indices[0, y_0, ..., y_{K-1}],
    ///          ...,
    ///          indices[M-1, y_0, ..., y_{K-1}],
    ///          x_M, ..., x_{N-1}] += data[y_0, ..., y_{K-1}, x_M, ..., x_{N-1}]
    /// 
    /// all other entries in output are 0 or the original value if AddTo is triggered.
    /// 
    /// Examples::
    /// 
    ///   data = [2, 3, 0]
    ///   indices = [[1, 1, 0], [0, 1, 0]]
    ///   shape = (2, 2)
    ///   _backward_gather_nd(data, indices, shape) = [[0, 0], [2, 3]] # Same as scatter_nd
    /// 
    ///   # The difference between scatter_nd and scatter_nd_acc is the latter will accumulate
    ///   #  the values that point to the same index.
    /// 
    ///   data = [2, 3, 0]
    ///   indices = [[1, 1, 0], [1, 1, 0]]
    ///   shape = (2, 2)
    ///   _backward_gather_nd(data, indices, shape) = [[0, 0], [0, 5]]
    /// 
    /// </summary>
    /// <param name="data">data</param>
    /// <param name="indices">indices</param>
    /// <param name="shape">Shape of output.</param>
    static member BackwardGatherNd(data : NDArray, indices : NDArray, shape : int seq) =
        let creator = AtomicSymbolCreator.FromName "_backward_gather_nd"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; indices.NDArrayHandle|]
                                                 [|"shape"|]
                                                 [|shape.ToString()|]
        outputs

    /// <summary>This operator has the same functionality as scatter_nd
    /// except that it does not reset the elements not indexed by the input
    /// index `NDArray` in the input data `NDArray`. output should be explicitly
    /// given and be the same as lhs.
    /// 
    /// .. note:: This operator is for internal use only.
    /// 
    /// Examples::
    /// 
    ///   data = [2, 3, 0]
    ///   indices = [[1, 1, 0], [0, 1, 0]]
    ///   out = [[1, 1], [1, 1]]
    ///   _scatter_set_nd(lhs=out, rhs=data, indices=indices, out=out)
    ///   out = [[0, 1], [2, 3]]
    /// 
    /// </summary>
    /// <param name="lhs">source input</param>
    /// <param name="rhs">value to assign</param>
    /// <param name="indices">indices</param>
    /// <param name="shape">Shape of output.</param>
    static member ScatterSetNd(lhs : NDArray, rhs : NDArray, indices : NDArray, shape : int seq) =
        let creator = AtomicSymbolCreator.FromName "_scatter_set_nd"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle; indices.NDArrayHandle|]
                                                 [|"shape"|]
                                                 [|shape.ToString()|]
        outputs

    /// <summary>Return an array of zeros with the same shape, type and storage type
    /// as the input array.
    /// 
    /// The storage type of ``zeros_like`` output depends on the storage type of the input
    /// 
    /// - zeros_like(row_sparse) = row_sparse
    /// - zeros_like(csr) = csr
    /// - zeros_like(default) = default
    /// 
    /// Examples::
    /// 
    ///   x = [[ 1.,  1.,  1.],
    ///        [ 1.,  1.,  1.]]
    /// 
    ///   zeros_like(x) = [[ 0.,  0.,  0.],
    ///                    [ 0.,  0.,  0.]]
    /// 
    /// </summary>
    /// <param name="data">The input</param>
    static member ZerosLike(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "zeros_like"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Return an array of ones with the same shape and type
    /// as the input array.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 0.,  0.,  0.],
    ///        [ 0.,  0.,  0.]]
    /// 
    ///   ones_like(x) = [[ 1.,  1.,  1.],
    ///                   [ 1.,  1.,  1.]]
    /// 
    /// </summary>
    /// <param name="data">The input</param>
    static member OnesLike(data : NDArray) =
        let creator = AtomicSymbolCreator.FromName "ones_like"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Performs general matrix multiplication and accumulation.
    /// Input are tensors *A*, *B*, *C*, each of dimension *n &gt;= 2* and having the same shape
    /// on the leading *n-2* dimensions.
    /// 
    /// If *n=2*, the BLAS3 function *gemm* is performed:
    /// 
    ///    *out* = *alpha* \* *op*\ (*A*) \* *op*\ (*B*) + *beta* \* *C*
    /// 
    /// Here, *alpha* and *beta* are scalar parameters, and *op()* is either the identity or
    /// matrix transposition (depending on *transpose_a*, *transpose_b*).
    /// 
    /// If *n&gt;2*, *gemm* is performed separately for a batch of matrices. The column indices of the matrices
    /// are given by the last dimensions of the tensors, the row indices by the axis specified with the *axis*
    /// parameter. By default, the trailing two dimensions will be used for matrix encoding.
    /// 
    /// For a non-default axis parameter, the operation performed is equivalent to a series of swapaxes/gemm/swapaxes
    /// calls. For example let *A*, *B*, *C* be 5 dimensional tensors. Then gemm(*A*, *B*, *C*, axis=1) is equivalent
    /// to the following without the overhead of the additional swapaxis operations::
    /// 
    ///     A1 = swapaxes(A, dim1=1, dim2=3)
    ///     B1 = swapaxes(B, dim1=1, dim2=3)
    ///     C = swapaxes(C, dim1=1, dim2=3)
    ///     C = gemm(A1, B1, C)
    ///     C = swapaxis(C, dim1=1, dim2=3)
    /// 
    /// When the input data is of type float32 and the environment variables MXNET_CUDA_ALLOW_TENSOR_CORE
    /// and MXNET_CUDA_TENSOR_OP_MATH_ALLOW_CONVERSION are set to 1, this operator will try to use
    /// pseudo-float16 precision (float32 math with float16 I/O) precision in order to use
    /// Tensor Cores on suitable NVIDIA GPUs. This can sometimes give significant speedups.
    /// 
    /// .. note:: The operator supports float32 and float64 data types only.
    /// 
    /// Examples::
    /// 
    ///    // Single matrix multiply-add
    ///    A = [[1.0, 1.0], [1.0, 1.0]]
    ///    B = [[1.0, 1.0], [1.0, 1.0], [1.0, 1.0]]
    ///    C = [[1.0, 1.0, 1.0], [1.0, 1.0, 1.0]]
    ///    gemm(A, B, C, transpose_b=True, alpha=2.0, beta=10.0)
    ///            = [[14.0, 14.0, 14.0], [14.0, 14.0, 14.0]]
    /// 
    ///    // Batch matrix multiply-add
    ///    A = [[[1.0, 1.0]], [[0.1, 0.1]]]
    ///    B = [[[1.0, 1.0]], [[0.1, 0.1]]]
    ///    C = [[[10.0]], [[0.01]]]
    ///    gemm(A, B, C, transpose_b=True, alpha=2.0 , beta=10.0)
    ///            = [[[104.0]], [[0.14]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\la_op.cc:L89</summary>
    /// <param name="A">Tensor of input matrices</param>
    /// <param name="B">Tensor of input matrices</param>
    /// <param name="C">Tensor of input matrices</param>
    /// <param name="transposeA">Multiply with transposed of first input (A).</param>
    /// <param name="transposeB">Multiply with transposed of second input (B).</param>
    /// <param name="alpha">Scalar factor multiplied with A*B.</param>
    /// <param name="beta">Scalar factor multiplied with C.</param>
    /// <param name="axis">Axis corresponding to the matrix rows.</param>
    static member LinalgGemm(A : NDArray, 
                             B : NDArray, 
                             C : NDArray, 
                             [<Optional; DefaultParameterValue(false)>] transposeA : bool, 
                             [<Optional; DefaultParameterValue(false)>] transposeB : bool, 
                             alpha : double, 
                             beta : double, 
                             [<Optional; DefaultParameterValue(-2)>] axis : int) =
        let creator = AtomicSymbolCreator.FromName "_linalg_gemm"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|A.NDArrayHandle; B.NDArrayHandle; C.NDArrayHandle|]
                                                 [|"transpose_a"; "transpose_b"; "alpha"; "beta"; "axis"|]
                                                 [|transposeA.ToString(); transposeB.ToString(); alpha.ToString(); beta.ToString(); axis.ToString()|]
        outputs

    /// <summary>Performs general matrix multiplication.
    /// Input are tensors *A*, *B*, each of dimension *n &gt;= 2* and having the same shape
    /// on the leading *n-2* dimensions.
    /// 
    /// If *n=2*, the BLAS3 function *gemm* is performed:
    /// 
    ///    *out* = *alpha* \* *op*\ (*A*) \* *op*\ (*B*)
    /// 
    /// Here *alpha* is a scalar parameter and *op()* is either the identity or the matrix
    /// transposition (depending on *transpose_a*, *transpose_b*).
    /// 
    /// If *n&gt;2*, *gemm* is performed separately for a batch of matrices. The column indices of the matrices
    /// are given by the last dimensions of the tensors, the row indices by the axis specified with the *axis*
    /// parameter. By default, the trailing two dimensions will be used for matrix encoding.
    /// 
    /// For a non-default axis parameter, the operation performed is equivalent to a series of swapaxes/gemm/swapaxes
    /// calls. For example let *A*, *B* be 5 dimensional tensors. Then gemm(*A*, *B*, axis=1) is equivalent to
    /// the following without the overhead of the additional swapaxis operations::
    /// 
    ///     A1 = swapaxes(A, dim1=1, dim2=3)
    ///     B1 = swapaxes(B, dim1=1, dim2=3)
    ///     C = gemm2(A1, B1)
    ///     C = swapaxis(C, dim1=1, dim2=3)
    /// 
    /// When the input data is of type float32 and the environment variables MXNET_CUDA_ALLOW_TENSOR_CORE
    /// and MXNET_CUDA_TENSOR_OP_MATH_ALLOW_CONVERSION are set to 1, this operator will try to use
    /// pseudo-float16 precision (float32 math with float16 I/O) precision in order to use
    /// Tensor Cores on suitable NVIDIA GPUs. This can sometimes give significant speedups.
    /// 
    /// .. note:: The operator supports float32 and float64 data types only.
    /// 
    /// Examples::
    /// 
    ///    // Single matrix multiply
    ///    A = [[1.0, 1.0], [1.0, 1.0]]
    ///    B = [[1.0, 1.0], [1.0, 1.0], [1.0, 1.0]]
    ///    gemm2(A, B, transpose_b=True, alpha=2.0)
    ///             = [[4.0, 4.0, 4.0], [4.0, 4.0, 4.0]]
    /// 
    ///    // Batch matrix multiply
    ///    A = [[[1.0, 1.0]], [[0.1, 0.1]]]
    ///    B = [[[1.0, 1.0]], [[0.1, 0.1]]]
    ///    gemm2(A, B, transpose_b=True, alpha=2.0)
    ///            = [[[4.0]], [[0.04 ]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\la_op.cc:L163</summary>
    /// <param name="A">Tensor of input matrices</param>
    /// <param name="B">Tensor of input matrices</param>
    /// <param name="transposeA">Multiply with transposed of first input (A).</param>
    /// <param name="transposeB">Multiply with transposed of second input (B).</param>
    /// <param name="alpha">Scalar factor multiplied with A*B.</param>
    /// <param name="axis">Axis corresponding to the matrix row indices.</param>
    static member LinalgGemm2(A : NDArray, 
                              B : NDArray, 
                              [<Optional; DefaultParameterValue(false)>] transposeA : bool, 
                              [<Optional; DefaultParameterValue(false)>] transposeB : bool, 
                              alpha : double, 
                              [<Optional; DefaultParameterValue(-2)>] axis : int) =
        let creator = AtomicSymbolCreator.FromName "_linalg_gemm2"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|A.NDArrayHandle; B.NDArrayHandle|]
                                                 [|"transpose_a"; "transpose_b"; "alpha"; "axis"|]
                                                 [|transposeA.ToString(); transposeB.ToString(); alpha.ToString(); axis.ToString()|]
        outputs

    /// <summary>Performs Cholesky factorization of a symmetric positive-definite matrix.
    /// Input is a tensor *A* of dimension *n &gt;= 2*.
    /// 
    /// If *n=2*, the Cholesky factor *B* of the symmetric, positive definite matrix *A* is
    /// computed. *B* is triangular (entries of upper or lower triangle are all zero), has
    /// positive diagonal entries, and:
    /// 
    ///   *A* = *B* \* *B*\ :sup:`T`  if *lower* = *true*
    ///   *A* = *B*\ :sup:`T` \* *B*  if *lower* = *false*
    /// 
    /// If *n&gt;2*, *potrf* is performed separately on the trailing two dimensions for all inputs
    /// (batch mode).
    /// 
    /// .. note:: The operator supports float32 and float64 data types only.
    /// 
    /// Examples::
    /// 
    ///    // Single matrix factorization
    ///    A = [[4.0, 1.0], [1.0, 4.25]]
    ///    potrf(A) = [[2.0, 0], [0.5, 2.0]]
    /// 
    ///    // Batch matrix factorization
    ///    A = [[[4.0, 1.0], [1.0, 4.25]], [[16.0, 4.0], [4.0, 17.0]]]
    ///    potrf(A) = [[[2.0, 0], [0.5, 2.0]], [[4.0, 0], [1.0, 4.0]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\la_op.cc:L214</summary>
    /// <param name="A">Tensor of input matrices to be decomposed</param>
    static member LinalgPotrf(A : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_linalg_potrf"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|A.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Performs matrix inversion from a Cholesky factorization.
    /// Input is a tensor *A* of dimension *n &gt;= 2*.
    /// 
    /// If *n=2*, *A* is a triangular matrix (entries of upper or lower triangle are all zero)
    /// with positive diagonal. We compute:
    /// 
    ///   *out* = *A*\ :sup:`-T` \* *A*\ :sup:`-1` if *lower* = *true*
    ///   *out* = *A*\ :sup:`-1` \* *A*\ :sup:`-T` if *lower* = *false*
    /// 
    /// In other words, if *A* is the Cholesky factor of a symmetric positive definite matrix
    /// *B* (obtained by *potrf*), then
    /// 
    ///   *out* = *B*\ :sup:`-1`
    /// 
    /// If *n&gt;2*, *potri* is performed separately on the trailing two dimensions for all inputs
    /// (batch mode).
    /// 
    /// .. note:: The operator supports float32 and float64 data types only.
    /// 
    /// .. note:: Use this operator only if you are certain you need the inverse of *B*, and
    ///           cannot use the Cholesky factor *A* (*potrf*), together with backsubstitution
    ///           (*trsm*). The latter is numerically much safer, and also cheaper.
    /// 
    /// Examples::
    /// 
    ///    // Single matrix inverse
    ///    A = [[2.0, 0], [0.5, 2.0]]
    ///    potri(A) = [[0.26563, -0.0625], [-0.0625, 0.25]]
    /// 
    ///    // Batch matrix inverse
    ///    A = [[[2.0, 0], [0.5, 2.0]], [[4.0, 0], [1.0, 4.0]]]
    ///    potri(A) = [[[0.26563, -0.0625], [-0.0625, 0.25]],
    ///                [[0.06641, -0.01562], [-0.01562, 0,0625]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\la_op.cc:L275</summary>
    /// <param name="A">Tensor of lower triangular matrices</param>
    static member LinalgPotri(A : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_linalg_potri"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|A.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Performs multiplication with a lower triangular matrix.
    /// Input are tensors *A*, *B*, each of dimension *n &gt;= 2* and having the same shape
    /// on the leading *n-2* dimensions.
    /// 
    /// If *n=2*, *A* must be triangular. The operator performs the BLAS3 function
    /// *trmm*:
    /// 
    ///    *out* = *alpha* \* *op*\ (*A*) \* *B*
    /// 
    /// if *rightside=False*, or
    /// 
    ///    *out* = *alpha* \* *B* \* *op*\ (*A*)
    /// 
    /// if *rightside=True*. Here, *alpha* is a scalar parameter, and *op()* is either the
    /// identity or the matrix transposition (depending on *transpose*).
    /// 
    /// If *n&gt;2*, *trmm* is performed separately on the trailing two dimensions for all inputs
    /// (batch mode).
    /// 
    /// .. note:: The operator supports float32 and float64 data types only.
    /// 
    /// Examples::
    /// 
    ///    // Single triangular matrix multiply
    ///    A = [[1.0, 0], [1.0, 1.0]]
    ///    B = [[1.0, 1.0, 1.0], [1.0, 1.0, 1.0]]
    ///    trmm(A, B, alpha=2.0) = [[2.0, 2.0, 2.0], [4.0, 4.0, 4.0]]
    /// 
    ///    // Batch triangular matrix multiply
    ///    A = [[[1.0, 0], [1.0, 1.0]], [[1.0, 0], [1.0, 1.0]]]
    ///    B = [[[1.0, 1.0, 1.0], [1.0, 1.0, 1.0]], [[0.5, 0.5, 0.5], [0.5, 0.5, 0.5]]]
    ///    trmm(A, B, alpha=2.0) = [[[2.0, 2.0, 2.0], [4.0, 4.0, 4.0]],
    ///                             [[1.0, 1.0, 1.0], [2.0, 2.0, 2.0]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\la_op.cc:L333</summary>
    /// <param name="A">Tensor of lower triangular matrices</param>
    /// <param name="B">Tensor of matrices</param>
    /// <param name="transpose">Use transposed of the triangular matrix</param>
    /// <param name="rightside">Multiply triangular matrix from the right to non-triangular one.</param>
    /// <param name="lower">True if the triangular matrix is lower triangular, false if it is upper triangular.</param>
    /// <param name="alpha">Scalar factor to be applied to the result.</param>
    static member LinalgTrmm(A : NDArray, 
                             B : NDArray, 
                             [<Optional; DefaultParameterValue(false)>] transpose : bool, 
                             [<Optional; DefaultParameterValue(false)>] rightside : bool, 
                             [<Optional; DefaultParameterValue(true)>] lower : bool, 
                             alpha : double) =
        let creator = AtomicSymbolCreator.FromName "_linalg_trmm"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|A.NDArrayHandle; B.NDArrayHandle|]
                                                 [|"transpose"; "rightside"; "lower"; "alpha"|]
                                                 [|transpose.ToString(); rightside.ToString(); lower.ToString(); alpha.ToString()|]
        outputs

    /// <summary>Solves matrix equation involving a lower triangular matrix.
    /// Input are tensors *A*, *B*, each of dimension *n &gt;= 2* and having the same shape
    /// on the leading *n-2* dimensions.
    /// 
    /// If *n=2*, *A* must be triangular. The operator performs the BLAS3 function
    /// *trsm*, solving for *out* in:
    /// 
    ///    *op*\ (*A*) \* *out* = *alpha* \* *B*
    /// 
    /// if *rightside=False*, or
    /// 
    ///    *out* \* *op*\ (*A*) = *alpha* \* *B*
    /// 
    /// if *rightside=True*. Here, *alpha* is a scalar parameter, and *op()* is either the
    /// identity or the matrix transposition (depending on *transpose*).
    /// 
    /// If *n&gt;2*, *trsm* is performed separately on the trailing two dimensions for all inputs
    /// (batch mode).
    /// 
    /// .. note:: The operator supports float32 and float64 data types only.
    /// 
    /// Examples::
    /// 
    ///    // Single matrix solve
    ///    A = [[1.0, 0], [1.0, 1.0]]
    ///    B = [[2.0, 2.0, 2.0], [4.0, 4.0, 4.0]]
    ///    trsm(A, B, alpha=0.5) = [[1.0, 1.0, 1.0], [1.0, 1.0, 1.0]]
    /// 
    ///    // Batch matrix solve
    ///    A = [[[1.0, 0], [1.0, 1.0]], [[1.0, 0], [1.0, 1.0]]]
    ///    B = [[[2.0, 2.0, 2.0], [4.0, 4.0, 4.0]],
    ///         [[4.0, 4.0, 4.0], [8.0, 8.0, 8.0]]]
    ///    trsm(A, B, alpha=0.5) = [[[1.0, 1.0, 1.0], [1.0, 1.0, 1.0]],
    ///                             [[2.0, 2.0, 2.0], [2.0, 2.0, 2.0]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\la_op.cc:L396</summary>
    /// <param name="A">Tensor of lower triangular matrices</param>
    /// <param name="B">Tensor of matrices</param>
    /// <param name="transpose">Use transposed of the triangular matrix</param>
    /// <param name="rightside">Multiply triangular matrix from the right to non-triangular one.</param>
    /// <param name="lower">True if the triangular matrix is lower triangular, false if it is upper triangular.</param>
    /// <param name="alpha">Scalar factor to be applied to the result.</param>
    static member LinalgTrsm(A : NDArray, 
                             B : NDArray, 
                             [<Optional; DefaultParameterValue(false)>] transpose : bool, 
                             [<Optional; DefaultParameterValue(false)>] rightside : bool, 
                             [<Optional; DefaultParameterValue(true)>] lower : bool, 
                             alpha : double) =
        let creator = AtomicSymbolCreator.FromName "_linalg_trsm"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|A.NDArrayHandle; B.NDArrayHandle|]
                                                 [|"transpose"; "rightside"; "lower"; "alpha"|]
                                                 [|transpose.ToString(); rightside.ToString(); lower.ToString(); alpha.ToString()|]
        outputs

    /// <summary>Computes the sum of the logarithms of the diagonal elements of a square matrix.
    /// Input is a tensor *A* of dimension *n &gt;= 2*.
    /// 
    /// If *n=2*, *A* must be square with positive diagonal entries. We sum the natural
    /// logarithms of the diagonal elements, the result has shape (1,).
    /// 
    /// If *n&gt;2*, *sumlogdiag* is performed separately on the trailing two dimensions for all
    /// inputs (batch mode).
    /// 
    /// .. note:: The operator supports float32 and float64 data types only.
    /// 
    /// Examples::
    /// 
    ///    // Single matrix reduction
    ///    A = [[1.0, 1.0], [1.0, 7.0]]
    ///    sumlogdiag(A) = [1.9459]
    /// 
    ///    // Batch matrix reduction
    ///    A = [[[1.0, 1.0], [1.0, 7.0]], [[3.0, 0], [0, 17.0]]]
    ///    sumlogdiag(A) = [1.9459, 3.9318]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\la_op.cc:L445</summary>
    /// <param name="A">Tensor of square matrices</param>
    static member LinalgSumlogdiag(A : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_linalg_sumlogdiag"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|A.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Extracts the diagonal entries of a square matrix.
    /// Input is a tensor *A* of dimension *n &gt;= 2*.
    /// 
    /// If *n=2*, then *A* represents a single square matrix which diagonal elements get extracted as a 1-dimensional tensor.
    /// 
    /// If *n&gt;2*, then *A* represents a batch of square matrices on the trailing two dimensions. The extracted diagonals are returned as an *n-1*-dimensional tensor.
    /// 
    /// .. note:: The operator supports float32 and float64 data types only.
    /// 
    /// Examples::
    /// 
    ///     // Single matrix diagonal extraction
    ///     A = [[1.0, 2.0],
    ///          [3.0, 4.0]]
    /// 
    ///     extractdiag(A) = [1.0, 4.0]
    /// 
    ///     extractdiag(A, 1) = [2.0]
    /// 
    ///     // Batch matrix diagonal extraction
    ///     A = [[[1.0, 2.0],
    ///           [3.0, 4.0]],
    ///          [[5.0, 6.0],
    ///           [7.0, 8.0]]]
    /// 
    ///     extractdiag(A) = [[1.0, 4.0],
    ///                       [5.0, 8.0]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\la_op.cc:L495</summary>
    /// <param name="A">Tensor of square matrices</param>
    /// <param name="offset">Offset of the diagonal versus the main diagonal. 0 corresponds to the main diagonal, a negative/positive value to diagonals below/above the main diagonal.</param>
    static member LinalgExtractdiag(A : NDArray, [<Optional; DefaultParameterValue(0)>] offset : int) =
        let creator = AtomicSymbolCreator.FromName "_linalg_extractdiag"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|A.NDArrayHandle|]
                                                 [|"offset"|]
                                                 [|offset.ToString()|]
        outputs

    /// <summary>Constructs a square matrix with the input as diagonal.
    /// Input is a tensor *A* of dimension *n &gt;= 1*.
    /// 
    /// If *n=1*, then *A* represents the diagonal entries of a single square matrix. This matrix will be returned as a 2-dimensional tensor.
    /// If *n&gt;1*, then *A* represents a batch of diagonals of square matrices. The batch of diagonal matrices will be returned as an *n+1*-dimensional tensor.
    /// 
    /// .. note:: The operator supports float32 and float64 data types only.
    /// 
    /// Examples::
    /// 
    ///     // Single diagonal matrix construction
    ///     A = [1.0, 2.0]
    /// 
    ///     makediag(A)    = [[1.0, 0.0],
    ///                       [0.0, 2.0]]
    /// 
    ///     makediag(A, 1) = [[0.0, 1.0, 0.0],
    ///                       [0.0, 0.0, 2.0],
    ///                       [0.0, 0.0, 0.0]]
    /// 
    ///     // Batch diagonal matrix construction
    ///     A = [[1.0, 2.0],
    ///          [3.0, 4.0]]
    /// 
    ///     makediag(A) = [[[1.0, 0.0],
    ///                     [0.0, 2.0]],
    ///                    [[3.0, 0.0],
    ///                     [0.0, 4.0]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\la_op.cc:L547</summary>
    /// <param name="A">Tensor of diagonal entries</param>
    /// <param name="offset">Offset of the diagonal versus the main diagonal. 0 corresponds to the main diagonal, a negative/positive value to diagonals below/above the main diagonal.</param>
    static member LinalgMakediag(A : NDArray, [<Optional; DefaultParameterValue(0)>] offset : int) =
        let creator = AtomicSymbolCreator.FromName "_linalg_makediag"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|A.NDArrayHandle|]
                                                 [|"offset"|]
                                                 [|offset.ToString()|]
        outputs

    /// <summary>Extracts a triangular sub-matrix from a square matrix.
    /// Input is a tensor *A* of dimension *n &gt;= 2*.
    /// 
    /// If *n=2*, then *A* represents a single square matrix from which a triangular sub-matrix is extracted as a 1-dimensional tensor.
    /// 
    /// If *n&gt;2*, then *A* represents a batch of square matrices on the trailing two dimensions. The extracted triangular sub-matrices are returned as an *n-1*-dimensional tensor.
    /// 
    /// The *offset* and *lower* parameters determine the triangle to be extracted:
    /// 
    /// - When *offset = 0* either the lower or upper triangle with respect to the main diagonal is extracted depending on the value of parameter *lower*.
    /// - When *offset = k &gt; 0* the upper triangle with respect to the k-th diagonal above the main diagonal is extracted. 
    /// - When *offset = k &lt; 0* the lower triangle with respect to the k-th diagonal below the main diagonal is extracted. 
    /// 
    /// .. note:: The operator supports float32 and float64 data types only.
    /// 
    /// Examples::
    /// 
    ///     // Single triagonal extraction
    ///     A = [[1.0, 2.0],
    ///          [3.0, 4.0]]
    /// 
    ///     extracttrian(A) = [1.0, 3.0, 4.0]
    ///     extracttrian(A, lower=False) = [1.0, 2.0, 4.0]
    ///     extracttrian(A, 1) = [2.0]
    ///     extracttrian(A, -1) = [3.0]
    /// 
    ///     // Batch triagonal extraction
    ///     A = [[[1.0, 2.0],
    ///           [3.0, 4.0]],
    ///          [[5.0, 6.0],
    ///           [7.0, 8.0]]]
    /// 
    ///     extracttrian(A) = [[1.0, 3.0, 4.0],
    ///                        [5.0, 7.0, 8.0]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\la_op.cc:L605</summary>
    /// <param name="A">Tensor of square matrices</param>
    /// <param name="offset">Offset of the diagonal versus the main diagonal. 0 corresponds to the main diagonal, a negative/positive value to diagonals below/above the main diagonal.</param>
    /// <param name="lower">Refer to the lower triangular matrix if lower=true, refer to the upper otherwise. Only relevant when offset=0</param>
    static member LinalgExtracttrian(A : NDArray, [<Optional; DefaultParameterValue(0)>] offset : int, [<Optional; DefaultParameterValue(true)>] lower : bool) =
        let creator = AtomicSymbolCreator.FromName "_linalg_extracttrian"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|A.NDArrayHandle|]
                                                 [|"offset"; "lower"|]
                                                 [|offset.ToString(); lower.ToString()|]
        outputs

    /// <summary>Constructs a square matrix with the input representing a specific triangular sub-matrix.
    /// This is basically the inverse of *linalg.extracttrian*. Input is a tensor *A* of dimension *n &gt;= 1*.
    /// 
    /// If *n=1*, then *A* represents the entries of a triangular matrix which is lower triangular if *offset&lt;0* or *offset=0*, *lower=true*. The resulting matrix is derived by first constructing the square
    /// matrix with the entries outside the triangle set to zero and then adding *offset*-times an additional 
    /// diagonal with zero entries to the square matrix. 
    /// 
    /// If *n&gt;1*, then *A* represents a batch of triangular sub-matrices. The batch of corresponding square matrices is returned as an *n+1*-dimensional tensor.
    /// 
    /// .. note:: The operator supports float32 and float64 data types only.
    /// 
    /// Examples::
    /// 
    ///     // Single  matrix construction
    ///     A = [1.0, 2.0, 3.0]
    /// 
    ///     maketrian(A)              = [[1.0, 0.0],
    ///                                  [2.0, 3.0]]
    /// 
    ///     maketrian(A, lower=false) = [[1.0, 2.0],
    ///                                  [0.0, 3.0]]
    /// 
    ///     maketrian(A, offset=1)    = [[0.0, 1.0, 2.0],
    ///                                  [0.0, 0.0, 3.0],
    ///                                  [0.0, 0.0, 0.0]]
    ///     maketrian(A, offset=-1)   = [[0.0, 0.0, 0.0],
    ///                                  [1.0, 0.0, 0.0],
    ///                                  [2.0, 3.0, 0.0]]
    /// 
    ///     // Batch matrix construction
    ///     A = [[1.0, 2.0, 3.0],
    ///          [4.0, 5.0, 6.0]]
    /// 
    ///     maketrian(A)           = [[[1.0, 0.0],
    ///                                [2.0, 3.0]],
    ///                               [[4.0, 0.0],
    ///                                [5.0, 6.0]]]
    /// 
    ///     maketrian(A, offset=1) = [[[0.0, 1.0, 2.0],
    ///                                [0.0, 0.0, 3.0],
    ///                                [0.0, 0.0, 0.0]],
    ///                               [[0.0, 4.0, 5.0],
    ///                                [0.0, 0.0, 6.0],
    ///                                [0.0, 0.0, 0.0]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\la_op.cc:L673</summary>
    /// <param name="A">Tensor of triangular matrices stored as vectors</param>
    /// <param name="offset">Offset of the diagonal versus the main diagonal. 0 corresponds to the main diagonal, a negative/positive value to diagonals below/above the main diagonal.</param>
    /// <param name="lower">Refer to the lower triangular matrix if lower=true, refer to the upper otherwise. Only relevant when offset=0</param>
    static member LinalgMaketrian(A : NDArray, [<Optional; DefaultParameterValue(0)>] offset : int, [<Optional; DefaultParameterValue(true)>] lower : bool) =
        let creator = AtomicSymbolCreator.FromName "_linalg_maketrian"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|A.NDArrayHandle|]
                                                 [|"offset"; "lower"|]
                                                 [|offset.ToString(); lower.ToString()|]
        outputs

    /// <summary>Multiplication of matrix with its transpose.
    /// Input is a tensor *A* of dimension *n &gt;= 2*.
    /// 
    /// If *n=2*, the operator performs the BLAS3 function *syrk*:
    /// 
    ///   *out* = *alpha* \* *A* \* *A*\ :sup:`T`
    /// 
    /// if *transpose=False*, or
    /// 
    ///   *out* = *alpha* \* *A*\ :sup:`T` \ \* *A*
    /// 
    /// if *transpose=True*.
    /// 
    /// If *n&gt;2*, *syrk* is performed separately on the trailing two dimensions for all
    /// inputs (batch mode).
    /// 
    /// .. note:: The operator supports float32 and float64 data types only.
    /// 
    /// Examples::
    /// 
    ///    // Single matrix multiply
    ///    A = [[1., 2., 3.], [4., 5., 6.]]
    ///    syrk(A, alpha=1., transpose=False)
    ///             = [[14., 32.],
    ///                [32., 77.]]
    ///    syrk(A, alpha=1., transpose=True)
    ///             = [[17., 22., 27.],
    ///                [22., 29., 36.],
    ///                [27., 36., 45.]]
    /// 
    ///    // Batch matrix multiply
    ///    A = [[[1., 1.]], [[0.1, 0.1]]]
    ///    syrk(A, alpha=2., transpose=False) = [[[4.]], [[0.04]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\la_op.cc:L730</summary>
    /// <param name="A">Tensor of input matrices</param>
    /// <param name="transpose">Use transpose of input matrix.</param>
    /// <param name="alpha">Scalar factor to be applied to the result.</param>
    static member LinalgSyrk(A : NDArray, [<Optional; DefaultParameterValue(false)>] transpose : bool, alpha : double) =
        let creator = AtomicSymbolCreator.FromName "_linalg_syrk"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|A.NDArrayHandle|]
                                                 [|"transpose"; "alpha"|]
                                                 [|transpose.ToString(); alpha.ToString()|]
        outputs

    /// <summary>LQ factorization for general matrix.
    /// Input is a tensor *A* of dimension *n &gt;= 2*.
    /// 
    /// If *n=2*, we compute the LQ factorization (LAPACK *gelqf*, followed by *orglq*). *A*
    /// must have shape *(x, y)* with *x &lt;= y*, and must have full rank *=x*. The LQ
    /// factorization consists of *L* with shape *(x, x)* and *Q* with shape *(x, y)*, so
    /// that:
    /// 
    ///    *A* = *L* \* *Q*
    /// 
    /// Here, *L* is lower triangular (upper triangle equal to zero) with nonzero diagonal,
    /// and *Q* is row-orthonormal, meaning that
    /// 
    ///    *Q* \* *Q*\ :sup:`T`
    /// 
    /// is equal to the identity matrix of shape *(x, x)*.
    /// 
    /// If *n&gt;2*, *gelqf* is performed separately on the trailing two dimensions for all
    /// inputs (batch mode).
    /// 
    /// .. note:: The operator supports float32 and float64 data types only.
    /// 
    /// Examples::
    /// 
    ///    // Single LQ factorization
    ///    A = [[1., 2., 3.], [4., 5., 6.]]
    ///    Q, L = gelqf(A)
    ///    Q = [[-0.26726124, -0.53452248, -0.80178373],
    ///         [0.87287156, 0.21821789, -0.43643578]]
    ///    L = [[-3.74165739, 0.],
    ///         [-8.55235974, 1.96396101]]
    /// 
    ///    // Batch LQ factorization
    ///    A = [[[1., 2., 3.], [4., 5., 6.]],
    ///         [[7., 8., 9.], [10., 11., 12.]]]
    ///    Q, L = gelqf(A)
    ///    Q = [[[-0.26726124, -0.53452248, -0.80178373],
    ///          [0.87287156, 0.21821789, -0.43643578]],
    ///         [[-0.50257071, -0.57436653, -0.64616234],
    ///          [0.7620735, 0.05862104, -0.64483142]]]
    ///    L = [[[-3.74165739, 0.],
    ///          [-8.55235974, 1.96396101]],
    ///         [[-13.92838828, 0.],
    ///          [-19.09768702, 0.52758934]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\la_op.cc:L798</summary>
    /// <param name="A">Tensor of input matrices to be factorized</param>
    static member LinalgGelqf(A : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_linalg_gelqf"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|A.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Eigendecomposition for symmetric matrix.
    /// Input is a tensor *A* of dimension *n &gt;= 2*.
    /// 
    /// If *n=2*, *A* must be symmetric, of shape *(x, x)*. We compute the eigendecomposition,
    /// resulting in the orthonormal matrix *U* of eigenvectors, shape *(x, x)*, and the
    /// vector *L* of eigenvalues, shape *(x,)*, so that:
    /// 
    ///    *U* \* *A* = *diag(L)* \* *U*
    /// 
    /// Here:
    /// 
    ///    *U* \* *U*\ :sup:`T` = *U*\ :sup:`T` \* *U* = *I*
    /// 
    /// where *I* is the identity matrix. Also, *L(0) &lt;= L(1) &lt;= L(2) &lt;= ...* (ascending order).
    /// 
    /// If *n&gt;2*, *syevd* is performed separately on the trailing two dimensions of *A* (batch
    /// mode). In this case, *U* has *n* dimensions like *A*, and *L* has *n-1* dimensions.
    /// 
    /// .. note:: The operator supports float32 and float64 data types only.
    /// 
    /// .. note:: Derivatives for this operator are defined only if *A* is such that all its
    ///           eigenvalues are distinct, and the eigengaps are not too small. If you need
    ///           gradients, do not apply this operator to matrices with multiple eigenvalues.
    /// 
    /// Examples::
    /// 
    ///    // Single symmetric eigendecomposition
    ///    A = [[1., 2.], [2., 4.]]
    ///    U, L = syevd(A)
    ///    U = [[0.89442719, -0.4472136],
    ///         [0.4472136, 0.89442719]]
    ///    L = [0., 5.]
    /// 
    ///    // Batch symmetric eigendecomposition
    ///    A = [[[1., 2.], [2., 4.]],
    ///         [[1., 2.], [2., 5.]]]
    ///    U, L = syevd(A)
    ///    U = [[[0.89442719, -0.4472136],
    ///          [0.4472136, 0.89442719]],
    ///         [[0.92387953, -0.38268343],
    ///          [0.38268343, 0.92387953]]]
    ///    L = [[0., 5.],
    ///         [0.17157288, 5.82842712]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\la_op.cc:L867</summary>
    /// <param name="A">Tensor of input matrices to be factorized</param>
    static member LinalgSyevd(A : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_linalg_syevd"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|A.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Compute the inverse of a matrix.
    /// Input is a tensor *A* of dimension *n &gt;= 2*.
    /// 
    /// If *n=2*, *A* is a square matrix. We compute:
    /// 
    ///   *out* = *A*\ :sup:`-1`
    /// 
    /// If *n&gt;2*, *inverse* is performed separately on the trailing two dimensions
    /// for all inputs (batch mode).
    /// 
    /// .. note:: The operator supports float32 and float64 data types only.
    /// 
    /// Examples::
    /// 
    ///    // Single matrix inversion
    ///    A = [[1., 4.], [2., 3.]]
    ///    inverse(A) = [[-0.6, 0.8], [0.4, -0.2]]
    /// 
    ///    // Batch matrix inversion
    ///    A = [[[1., 4.], [2., 3.]],
    ///         [[1., 3.], [2., 4.]]]
    ///    inverse(A) = [[[-0.6, 0.8], [0.4, -0.2]],
    ///                  [[-2., 1.5], [1., -0.5]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\la_op.cc:L917</summary>
    /// <param name="A">Tensor of square matrix</param>
    static member LinalgInverse(A : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_linalg_inverse"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|A.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Reshapes the input array.
    /// 
    /// .. note:: ``Reshape`` is deprecated, use ``reshape``
    /// 
    /// Given an array and a shape, this function returns a copy of the array in the new shape.
    /// The shape is a tuple of integers such as (2,3,4). The size of the new shape should be same as the size of the input array.
    /// 
    /// Example::
    /// 
    ///   reshape([1,2,3,4], shape=(2,2)) = [[1,2], [3,4]]
    /// 
    /// Some dimensions of the shape can take special values from the set {0, -1, -2, -3, -4}. The significance of each is explained below:
    /// 
    /// - ``0``  copy this dimension from the input to the output shape.
    /// 
    ///   Example::
    /// 
    ///   - input shape = (2,3,4), shape = (4,0,2), output shape = (4,3,2)
    ///   - input shape = (2,3,4), shape = (2,0,0), output shape = (2,3,4)
    /// 
    /// - ``-1`` infers the dimension of the output shape by using the remainder of the input dimensions
    ///   keeping the size of the new array same as that of the input array.
    ///   At most one dimension of shape can be -1.
    /// 
    ///   Example::
    /// 
    ///   - input shape = (2,3,4), shape = (6,1,-1), output shape = (6,1,4)
    ///   - input shape = (2,3,4), shape = (3,-1,8), output shape = (3,1,8)
    ///   - input shape = (2,3,4), shape=(-1,), output shape = (24,)
    /// 
    /// - ``-2`` copy all/remainder of the input dimensions to the output shape.
    /// 
    ///   Example::
    /// 
    ///   - input shape = (2,3,4), shape = (-2,), output shape = (2,3,4)
    ///   - input shape = (2,3,4), shape = (2,-2), output shape = (2,3,4)
    ///   - input shape = (2,3,4), shape = (-2,1,1), output shape = (2,3,4,1,1)
    /// 
    /// - ``-3`` use the product of two consecutive dimensions of the input shape as the output dimension.
    /// 
    ///   Example::
    /// 
    ///   - input shape = (2,3,4), shape = (-3,4), output shape = (6,4)
    ///   - input shape = (2,3,4,5), shape = (-3,-3), output shape = (6,20)
    ///   - input shape = (2,3,4), shape = (0,-3), output shape = (2,12)
    ///   - input shape = (2,3,4), shape = (-3,-2), output shape = (6,4)
    /// 
    /// - ``-4`` split one dimension of the input into two dimensions passed subsequent to -4 in shape (can contain -1).
    /// 
    ///   Example::
    /// 
    ///   - input shape = (2,3,4), shape = (-4,1,2,-2), output shape =(1,2,3,4)
    ///   - input shape = (2,3,4), shape = (2,-4,-1,3,-2), output shape = (2,1,3,4)
    /// 
    /// If the argument `reverse` is set to 1, then the special values are inferred from right to left.
    /// 
    ///   Example::
    /// 
    ///   - without reverse=1, for input shape = (10,5,4), shape = (-1,0), output shape would be (40,5)
    ///   - with reverse=1, output shape will be (50,4).
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L201</summary>
    /// <param name="data">Input data to reshape.</param>
    /// <param name="shape">The target shape</param>
    /// <param name="reverse">If true then the special values are inferred from right to left</param>
    /// <param name="targetShape">(Deprecated! Use ``shape`` instead.) Target new shape. One and only one dim can be 0, in which case it will be inferred from the rest of dims</param>
    /// <param name="keepHighest">(Deprecated! Use ``shape`` instead.) Whether keep the highest dim unchanged.If set to true, then the first dim in target_shape is ignored,and always fixed as input</param>
    static member Reshape(data : NDArray, 
                          [<Optional>] shape : int seq, 
                          [<Optional; DefaultParameterValue(false)>] reverse : bool, 
                          [<Optional>] targetShape : int seq, 
                          [<Optional; DefaultParameterValue(false)>] keepHighest : bool) =
        let creator = AtomicSymbolCreator.FromName "Reshape"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"shape"; "reverse"; "target_shape"; "keep_highest"|]
                                                 [|(if isNull (shape :> obj) then "[]" else shape.ToString()); reverse.ToString(); (if isNull (targetShape :> obj) then "[]" else targetShape.ToString()); keepHighest.ToString()|]
        outputs

    /// <summary>Permutes the dimensions of an array.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 1, 2],
    ///        [ 3, 4]]
    /// 
    ///   transpose(x) = [[ 1.,  3.],
    ///                   [ 2.,  4.]]
    /// 
    ///   x = [[[ 1.,  2.],
    ///         [ 3.,  4.]],
    /// 
    ///        [[ 5.,  6.],
    ///         [ 7.,  8.]]]
    /// 
    ///   transpose(x) = [[[ 1.,  5.],
    ///                    [ 3.,  7.]],
    /// 
    ///                   [[ 2.,  6.],
    ///                    [ 4.,  8.]]]
    /// 
    ///   transpose(x, axes=(1,0,2)) = [[[ 1.,  2.],
    ///                                  [ 5.,  6.]],
    /// 
    ///                                 [[ 3.,  4.],
    ///                                  [ 7.,  8.]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L377</summary>
    /// <param name="data">Source input</param>
    /// <param name="axes">Target axis order. By default the axes will be inverted.</param>
    static member Transpose(data : NDArray, [<Optional>] axes : int seq) =
        let creator = AtomicSymbolCreator.FromName "transpose"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axes"|]
                                                 [|(if isNull (axes :> obj) then "[]" else axes.ToString())|]
        outputs

    /// <summary>Inserts a new axis of size 1 into the array shape
    /// 
    /// For example, given ``x`` with shape ``(2,3,4)``, then ``expand_dims(x, axis=1)``
    /// will return a new array with shape ``(2,1,3,4)``.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L418</summary>
    /// <param name="data">Source input</param>
    /// <param name="axis">Position where new axis is to be inserted. Suppose that the input `NDArray`&#39;s dimension is `ndim`, the range of the inserted axis is `[-ndim, ndim]`</param>
    static member ExpandDims(data : NDArray, axis : int) =
        let creator = AtomicSymbolCreator.FromName "expand_dims"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"|]
                                                 [|axis.ToString()|]
        outputs

    /// <summary>Slices a region of the array.
    /// 
    /// .. note:: ``crop`` is deprecated. Use ``slice`` instead.
    /// 
    /// This function returns a sliced array between the indices given
    /// by `begin` and `end` with the corresponding `step`.
    /// 
    /// For an input array of ``shape=(d_0, d_1, ..., d_n-1)``,
    /// slice operation with ``begin=(b_0, b_1...b_m-1)``,
    /// ``end=(e_0, e_1, ..., e_m-1)``, and ``step=(s_0, s_1, ..., s_m-1)``,
    /// where m &lt;= n, results in an array with the shape
    /// ``(|e_0-b_0|/|s_0|, ..., |e_m-1-b_m-1|/|s_m-1|, d_m, ..., d_n-1)``.
    /// 
    /// The resulting array&#39;s *k*-th dimension contains elements
    /// from the *k*-th dimension of the input array starting
    /// from index ``b_k`` (inclusive) with step ``s_k``
    /// until reaching ``e_k`` (exclusive).
    /// 
    /// If the *k*-th elements are `None` in the sequence of `begin`, `end`,
    /// and `step`, the following rule will be used to set default values.
    /// If `s_k` is `None`, set `s_k=1`. If `s_k &gt; 0`, set `b_k=0`, `e_k=d_k`;
    /// else, set `b_k=d_k-1`, `e_k=-1`.
    /// 
    /// The storage type of ``slice`` output depends on storage types of inputs
    /// 
    /// - slice(csr) = csr
    /// - otherwise, ``slice`` generates output with default storage
    /// 
    /// .. note:: When input data storage type is csr, it only supports
    ///    step=(), or step=(None,), or step=(1,) to generate a csr output.
    ///    For other step parameter values, it falls back to slicing
    ///    a dense tensor.
    /// 
    /// Example::
    /// 
    ///   x = [[  1.,   2.,   3.,   4.],
    ///        [  5.,   6.,   7.,   8.],
    ///        [  9.,  10.,  11.,  12.]]
    /// 
    ///   slice(x, begin=(0,1), end=(2,4)) = [[ 2.,  3.,  4.],
    ///                                      [ 6.,  7.,  8.]]
    ///   slice(x, begin=(None, 0), end=(None, 3), step=(-1, 2)) = [[9., 11.],
    ///                                                             [5.,  7.],
    ///                                                             [1.,  3.]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L508</summary>
    /// <param name="data">Source input</param>
    /// <param name="sliceBegin">starting indices for the slice operation, supports negative indices.</param>
    /// <param name="sliceEnd">ending indices for the slice operation, supports negative indices.</param>
    /// <param name="step">step for the slice operation, supports negative values.</param>
    static member Slice(data : NDArray, sliceBegin : int seq, sliceEnd : int seq, [<Optional>] step : int seq) =
        let creator = AtomicSymbolCreator.FromName "slice"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"begin"; "end"; "step"|]
                                                 [|sliceBegin.ToString(); sliceEnd.ToString(); (if isNull (step :> obj) then "[]" else step.ToString())|]
        outputs

    /// <summary>Assign the rhs to a cropped subset of lhs.
    /// 
    /// Requirements
    /// ------------
    /// - output should be explicitly given and be the same as lhs.
    /// - lhs and rhs are of the same data type, and on the same device.
    /// 
    /// 
    /// From:C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:537</summary>
    /// <param name="lhs">Source input</param>
    /// <param name="rhs">value to assign</param>
    /// <param name="sliceBegin">starting indices for the slice operation, supports negative indices.</param>
    /// <param name="sliceEnd">ending indices for the slice operation, supports negative indices.</param>
    /// <param name="step">step for the slice operation, supports negative values.</param>
    static member SliceAssign(lhs : NDArray, 
                              rhs : NDArray, 
                              sliceBegin : int seq, 
                              sliceEnd : int seq, 
                              [<Optional>] step : int seq) =
        let creator = AtomicSymbolCreator.FromName "_slice_assign"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 [|"begin"; "end"; "step"|]
                                                 [|sliceBegin.ToString(); sliceEnd.ToString(); (if isNull (step :> obj) then "[]" else step.ToString())|]
        outputs

    /// <summary>(Assign the scalar to a cropped subset of the input.
    /// 
    /// Requirements
    /// ------------
    /// - output should be explicitly given and be the same as input
    /// )
    /// 
    /// From:C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:562</summary>
    /// <param name="data">Source input</param>
    /// <param name="scalar">The scalar value for assignment.</param>
    /// <param name="sliceBegin">starting indices for the slice operation, supports negative indices.</param>
    /// <param name="sliceEnd">ending indices for the slice operation, supports negative indices.</param>
    /// <param name="step">step for the slice operation, supports negative values.</param>
    static member SliceAssignScalar(data : NDArray, 
                                    scalar : double, 
                                    sliceBegin : int seq, 
                                    sliceEnd : int seq, 
                                    [<Optional>] step : int seq) =
        let creator = AtomicSymbolCreator.FromName "_slice_assign_scalar"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"scalar"; "begin"; "end"; "step"|]
                                                 [|scalar.ToString(); sliceBegin.ToString(); sliceEnd.ToString(); (if isNull (step :> obj) then "[]" else step.ToString())|]
        outputs

    /// <summary>Slices along a given axis.
    /// 
    /// Returns an array slice along a given `axis` starting from the `begin` index
    /// to the `end` index.
    /// 
    /// Examples::
    /// 
    ///   x = [[  1.,   2.,   3.,   4.],
    ///        [  5.,   6.,   7.,   8.],
    ///        [  9.,  10.,  11.,  12.]]
    /// 
    ///   slice_axis(x, axis=0, begin=1, end=3) = [[  5.,   6.,   7.,   8.],
    ///                                            [  9.,  10.,  11.,  12.]]
    /// 
    ///   slice_axis(x, axis=1, begin=0, end=2) = [[  1.,   2.],
    ///                                            [  5.,   6.],
    ///                                            [  9.,  10.]]
    /// 
    ///   slice_axis(x, axis=1, begin=-3, end=-1) = [[  2.,   3.],
    ///                                              [  6.,   7.],
    ///                                              [ 10.,  11.]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L598</summary>
    /// <param name="data">Source input</param>
    /// <param name="axis">Axis along which to be sliced, supports negative indexes.</param>
    /// <param name="sliceBegin">The beginning index along the axis to be sliced,  supports negative indexes.</param>
    /// <param name="sliceEnd">The ending index along the axis to be sliced,  supports negative indexes.</param>
    static member SliceAxis(data : NDArray, axis : int, sliceBegin : int, [<Optional>] sliceEnd : int Nullable) =
        let creator = AtomicSymbolCreator.FromName "slice_axis"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "begin"; "end"|]
                                                 [|axis.ToString(); sliceBegin.ToString(); sliceEnd.ToString()|]
        outputs

    /// <summary>Slices along a given axis.
    /// 
    /// Returns an array slice along a given `axis` starting from the `begin` index
    /// to the `end` index.
    /// 
    /// Examples::
    /// 
    ///   x = [[  1.,   2.,   3.,   4.],
    ///        [  5.,   6.,   7.,   8.],
    ///        [  9.,  10.,  11.,  12.]]
    /// 
    ///   slice_axis(x, axis=0, begin=1, end=3) = [[  5.,   6.,   7.,   8.],
    ///                                            [  9.,  10.,  11.,  12.]]
    /// 
    ///   slice_axis(x, axis=1, begin=0, end=2) = [[  1.,   2.],
    ///                                            [  5.,   6.],
    ///                                            [  9.,  10.]]
    /// 
    ///   slice_axis(x, axis=1, begin=-3, end=-1) = [[  2.,   3.],
    ///                                              [  6.,   7.],
    ///                                              [ 10.,  11.]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L598</summary>
    /// <param name="data">Source input</param>
    /// <param name="axis">Axis along which to be sliced, supports negative indexes.</param>
    /// <param name="sliceBegin">The beginning index along the axis to be sliced,  supports negative indexes.</param>
    /// <param name="sliceEnd">The ending index along the axis to be sliced,  supports negative indexes.</param>
    static member SliceAxis(data : NDArray, axis : int, sliceBegin : int, ?sliceEnd : int) =
        let creator = AtomicSymbolCreator.FromName "slice_axis"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "begin"; "end"|]
                                                 [|axis.ToString(); sliceBegin.ToString(); (match sliceEnd with None -> "None" | _ -> sliceEnd.ToString())|]
        outputs

    /// <summary>Slices a region of the array like the shape of another array.
    /// 
    /// This function is similar to ``slice``, however, the `begin` are always `0`s
    /// and `end` of specific axes are inferred from the second input `shape_like`.
    /// 
    /// Given the second `shape_like` input of ``shape=(d_0, d_1, ..., d_n-1)``,
    /// a ``slice_like`` operator with default empty `axes`, it performs the
    /// following operation:
    /// 
    /// `` out = slice(input, begin=(0, 0, ..., 0), end=(d_0, d_1, ..., d_n-1))``.
    /// 
    /// When `axes` is not empty, it is used to speficy which axes are being sliced.
    /// 
    /// Given a 4-d input data, ``slice_like`` operator with ``axes=(0, 2, -1)``
    /// will perform the following operation:
    /// 
    /// `` out = slice(input, begin=(0, 0, 0, 0), end=(d_0, None, d_2, d_3))``.
    /// 
    /// Note that it is allowed to have first and second input with different dimensions,
    /// however, you have to make sure the `axes` are specified and not exceeding the
    /// dimension limits.
    /// 
    /// For example, given `input_1` with ``shape=(2,3,4,5)`` and `input_2` with
    /// ``shape=(1,2,3)``, it is not allowed to use:
    /// 
    /// `` out = slice_like(a, b)`` because ndim of `input_1` is 4, and ndim of `input_2`
    /// is 3.
    /// 
    /// The following is allowed in this situation:
    /// 
    /// `` out = slice_like(a, b, axes=(0, 2))``
    /// 
    /// Example::
    /// 
    ///   x = [[  1.,   2.,   3.,   4.],
    ///        [  5.,   6.,   7.,   8.],
    ///        [  9.,  10.,  11.,  12.]]
    /// 
    ///   y = [[  0.,   0.,   0.],
    ///        [  0.,   0.,   0.]]
    /// 
    ///   slice_like(x, y) = [[ 1.,  2.,  3.]
    ///                       [ 5.,  6.,  7.]]
    ///   slice_like(x, y, axes=(0, 1)) = [[ 1.,  2.,  3.]
    ///                                    [ 5.,  6.,  7.]]
    ///   slice_like(x, y, axes=(0)) = [[ 1.,  2.,  3.,  4.]
    ///                                 [ 5.,  6.,  7.,  8.]]
    ///   slice_like(x, y, axes=(-1)) = [[  1.,   2.,   3.]
    ///                                  [  5.,   6.,   7.]
    ///                                  [  9.,  10.,  11.]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L667</summary>
    /// <param name="data">Source input</param>
    /// <param name="shapeLike">Shape like input</param>
    /// <param name="axes">List of axes on which input data will be sliced according to the corresponding size of the second input. By default will slice on all axes. Negative axes are supported.</param>
    static member SliceLike(data : NDArray, shapeLike : NDArray, [<Optional>] axes : int seq) =
        let creator = AtomicSymbolCreator.FromName "slice_like"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; shapeLike.NDArrayHandle|]
                                                 [|"axes"|]
                                                 [|(if isNull (axes :> obj) then "[]" else axes.ToString())|]
        outputs

    /// <summary>Clips (limits) the values in an array.
    /// 
    /// Given an interval, values outside the interval are clipped to the interval edges.
    /// Clipping ``x`` between `a_min` and `a_x` would be::
    /// 
    ///    clip(x, a_min, a_max) = max(min(x, a_max), a_min))
    /// 
    /// Example::
    /// 
    ///     x = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    /// 
    ///     clip(x,1,8) = [ 1.,  1.,  2.,  3.,  4.,  5.,  6.,  7.,  8.,  8.]
    /// 
    /// The storage type of ``clip`` output depends on storage types of inputs and the a_min, a_max \
    /// parameter values:
    /// 
    ///    - clip(default) = default
    ///    - clip(row_sparse, a_min &lt;= 0, a_max &gt;= 0) = row_sparse
    ///    - clip(csr, a_min &lt;= 0, a_max &gt;= 0) = csr
    ///    - clip(row_sparse, a_min &lt; 0, a_max &lt; 0) = default
    ///    - clip(row_sparse, a_min &gt; 0, a_max &gt; 0) = default
    ///    - clip(csr, a_min &lt; 0, a_max &lt; 0) = csr
    ///    - clip(csr, a_min &gt; 0, a_max &gt; 0) = csr
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L725</summary>
    /// <param name="data">Input array.</param>
    /// <param name="aMin">Minimum value</param>
    /// <param name="aMax">Maximum value</param>
    static member Clip(data : NDArray, aMin : float, aMax : float) =
        let creator = AtomicSymbolCreator.FromName "clip"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"a_min"; "a_max"|]
                                                 [|aMin.ToString(); aMax.ToString()|]
        outputs

    /// <summary>Repeats elements of an array.
    /// 
    /// By default, ``repeat`` flattens the input array into 1-D and then repeats the
    /// elements::
    /// 
    ///   x = [[ 1, 2],
    ///        [ 3, 4]]
    /// 
    ///   repeat(x, repeats=2) = [ 1.,  1.,  2.,  2.,  3.,  3.,  4.,  4.]
    /// 
    /// The parameter ``axis`` specifies the axis along which to perform repeat::
    /// 
    ///   repeat(x, repeats=2, axis=1) = [[ 1.,  1.,  2.,  2.],
    ///                                   [ 3.,  3.,  4.,  4.]]
    /// 
    ///   repeat(x, repeats=2, axis=0) = [[ 1.,  2.],
    ///                                   [ 1.,  2.],
    ///                                   [ 3.,  4.],
    ///                                   [ 3.,  4.]]
    /// 
    ///   repeat(x, repeats=2, axis=-1) = [[ 1.,  1.,  2.,  2.],
    ///                                    [ 3.,  3.,  4.,  4.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L798</summary>
    /// <param name="data">Input data array</param>
    /// <param name="repeats">The number of repetitions for each element.</param>
    /// <param name="axis">The axis along which to repeat values. The negative numbers are interpreted counting from the backward. By default, use the flattened input array, and return a flat output array.</param>
    static member Repeat(data : NDArray, repeats : int, [<Optional>] axis : int Nullable) =
        let creator = AtomicSymbolCreator.FromName "repeat"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"repeats"; "axis"|]
                                                 [|repeats.ToString(); axis.ToString()|]
        outputs

    /// <summary>Repeats elements of an array.
    /// 
    /// By default, ``repeat`` flattens the input array into 1-D and then repeats the
    /// elements::
    /// 
    ///   x = [[ 1, 2],
    ///        [ 3, 4]]
    /// 
    ///   repeat(x, repeats=2) = [ 1.,  1.,  2.,  2.,  3.,  3.,  4.,  4.]
    /// 
    /// The parameter ``axis`` specifies the axis along which to perform repeat::
    /// 
    ///   repeat(x, repeats=2, axis=1) = [[ 1.,  1.,  2.,  2.],
    ///                                   [ 3.,  3.,  4.,  4.]]
    /// 
    ///   repeat(x, repeats=2, axis=0) = [[ 1.,  2.],
    ///                                   [ 1.,  2.],
    ///                                   [ 3.,  4.],
    ///                                   [ 3.,  4.]]
    /// 
    ///   repeat(x, repeats=2, axis=-1) = [[ 1.,  1.,  2.,  2.],
    ///                                    [ 3.,  3.,  4.,  4.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L798</summary>
    /// <param name="data">Input data array</param>
    /// <param name="repeats">The number of repetitions for each element.</param>
    /// <param name="axis">The axis along which to repeat values. The negative numbers are interpreted counting from the backward. By default, use the flattened input array, and return a flat output array.</param>
    static member Repeat(data : NDArray, repeats : int, ?axis : int) =
        let creator = AtomicSymbolCreator.FromName "repeat"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"repeats"; "axis"|]
                                                 [|repeats.ToString(); (match axis with None -> "None" | _ -> axis.ToString())|]
        outputs

    /// <summary>Repeats the whole array multiple times.
    /// 
    /// If ``reps`` has length *d*, and input array has dimension of *n*. There are
    /// three cases:
    /// 
    /// - **n=d**. Repeat *i*-th dimension of the input by ``reps[i]`` times::
    /// 
    ///     x = [[1, 2],
    ///          [3, 4]]
    /// 
    ///     tile(x, reps=(2,3)) = [[ 1.,  2.,  1.,  2.,  1.,  2.],
    ///                            [ 3.,  4.,  3.,  4.,  3.,  4.],
    ///                            [ 1.,  2.,  1.,  2.,  1.,  2.],
    ///                            [ 3.,  4.,  3.,  4.,  3.,  4.]]
    /// 
    /// - **n&gt;d**. ``reps`` is promoted to length *n* by pre-pending 1&#39;s to it. Thus for
    ///   an input shape ``(2,3)``, ``repos=(2,)`` is treated as ``(1,2)``::
    /// 
    /// 
    ///     tile(x, reps=(2,)) = [[ 1.,  2.,  1.,  2.],
    ///                           [ 3.,  4.,  3.,  4.]]
    /// 
    /// - **n&lt;d**. The input is promoted to be d-dimensional by prepending new axes. So a
    ///   shape ``(2,2)`` array is promoted to ``(1,2,2)`` for 3-D replication::
    /// 
    ///     tile(x, reps=(2,2,3)) = [[[ 1.,  2.,  1.,  2.,  1.,  2.],
    ///                               [ 3.,  4.,  3.,  4.,  3.,  4.],
    ///                               [ 1.,  2.,  1.,  2.,  1.,  2.],
    ///                               [ 3.,  4.,  3.,  4.,  3.,  4.]],
    /// 
    ///                              [[ 1.,  2.,  1.,  2.,  1.,  2.],
    ///                               [ 3.,  4.,  3.,  4.,  3.,  4.],
    ///                               [ 1.,  2.,  1.,  2.,  1.,  2.],
    ///                               [ 3.,  4.,  3.,  4.,  3.,  4.]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L859</summary>
    /// <param name="data">Input data array</param>
    /// <param name="reps">The number of times for repeating the tensor a. Each dim size of reps must be a positive integer. If reps has length d, the result will have dimension of max(d, a.ndim); If a.ndim &lt; d, a is promoted to be d-dimensional by prepending new axes. If a.ndim &gt; d, reps is promoted to a.ndim by pre-pending 1&#39;s to it.</param>
    static member Tile(data : NDArray, reps : int seq) =
        let creator = AtomicSymbolCreator.FromName "tile"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"reps"|]
                                                 [|reps.ToString()|]
        outputs

    /// <summary>Reverses the order of elements along given axis while preserving array shape.
    /// 
    /// Note: reverse and flip are equivalent. We use reverse in the following examples.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 0.,  1.,  2.,  3.,  4.],
    ///        [ 5.,  6.,  7.,  8.,  9.]]
    /// 
    ///   reverse(x, axis=0) = [[ 5.,  6.,  7.,  8.,  9.],
    ///                         [ 0.,  1.,  2.,  3.,  4.]]
    /// 
    ///   reverse(x, axis=1) = [[ 4.,  3.,  2.,  1.,  0.],
    ///                         [ 9.,  8.,  7.,  6.,  5.]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L900</summary>
    /// <param name="data">Input data array</param>
    /// <param name="axis">The axis which to reverse elements.</param>
    static member Reverse(data : NDArray, axis : int seq) =
        let creator = AtomicSymbolCreator.FromName "reverse"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"|]
                                                 [|axis.ToString()|]
        outputs

    /// <summary>Join a sequence of arrays along a new axis.
    /// 
    /// The axis parameter specifies the index of the new axis in the dimensions of the
    /// result. For example, if axis=0 it will be the first dimension and if axis=-1 it
    /// will be the last dimension.
    /// 
    /// Examples::
    /// 
    ///   x = [1, 2]
    ///   y = [3, 4]
    /// 
    ///   stack(x, y) = [[1, 2],
    ///                  [3, 4]]
    ///   stack(x, y, axis=1) = [[1, 3],
    ///                          [2, 4]]
    /// </summary>
    /// <param name="data">List of arrays to stack</param>
    /// <param name="axis">The axis in the result array along which the input arrays are stacked.</param>
    /// <param name="numArgs">Number of inputs to be stacked.</param>
    static member Stack([<ParamArray>] data : NDArray[], [<Optional; DefaultParameterValue(0)>] axis : int, numArgs : int) =
        let creator = AtomicSymbolCreator.FromName "stack"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 [|"axis"; "num_args"|]
                                                 [|axis.ToString(); numArgs.ToString()|]
        outputs

    /// <summary>Remove single-dimensional entries from the shape of an array.
    /// Same behavior of defining the output tensor shape as numpy.squeeze for the most of cases.
    /// See the following note for exception.
    /// 
    /// Examples::
    /// 
    ///   data = [[[0], [1], [2]]]
    ///   squeeze(data) = [0, 1, 2]
    ///   squeeze(data, axis=0) = [[0], [1], [2]]
    ///   squeeze(data, axis=2) = [[0, 1, 2]]
    ///   squeeze(data, axis=(0, 2)) = [0, 1, 2]
    /// 
    /// .. Note::
    ///   The output of this operator will keep at least one dimension not removed. For example,
    ///   squeeze([[[4]]]) = [4], while in numpy.squeeze, the output will become a scalar.
    /// </summary>
    /// <param name="data">data to squeeze</param>
    /// <param name="axis">Selects a subset of the single-dimensional entries in the shape. If an axis is selected with shape entry greater than one, an error is raised.</param>
    static member Squeeze([<ParamArray>] data : NDArray[], [<Optional>] axis : int seq) =
        let creator = AtomicSymbolCreator.FromName "squeeze"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 Array.empty
                                                 [|"axis"|]
                                                 [|axis.ToString()|]
        outputs

    /// <summary>Rearranges(permutes) data from depth into blocks of spatial data.
    /// Similar to ONNX DepthToSpace operator:
    /// https://github.com/onnx/onnx/blob/master/docs/Operators.md#DepthToSpace.
    /// The output is a new tensor where the values from depth dimension are moved in spatial blocks 
    /// to height and width dimension. The reverse of this operation is ``space_to_depth``.
    /// 
    /// .. math::
    /// 
    ///     \begin{gather*}
    ///     x \prime = reshape(x, [N, block\_size, block\_size, C / (block\_size ^ 2), H * block\_size, W * block\_size]) \\
    ///     x \prime \prime = transpose(x \prime, [0, 3, 4, 1, 5, 2]) \\
    ///     y = reshape(x \prime \prime, [N, C / (block\_size ^ 2), H * block\_size, W * block\_size])
    ///     \end{gather*}
    /// 
    /// where :math:`x` is an input tensor with default layout as :math:`[N, C, H, W]`: [batch, channels, height, width] 
    /// and :math:`y` is the output tensor of layout :math:`[N, C / (block\_size ^ 2), H * block\_size, W * block\_size]`
    /// 
    /// Example::
    /// 
    ///   x = [[[[0, 1, 2],
    ///          [3, 4, 5]],
    ///         [[6, 7, 8],
    ///          [9, 10, 11]],
    ///         [[12, 13, 14],
    ///          [15, 16, 17]],
    ///         [[18, 19, 20],
    ///          [21, 22, 23]]]]
    /// 
    ///   depth_to_space(x, 2) = [[[[0, 6, 1, 7, 2, 8],
    ///                             [12, 18, 13, 19, 14, 20],
    ///                             [3, 9, 4, 10, 5, 11],
    ///                             [15, 21, 16, 22, 17, 23]]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L1052</summary>
    /// <param name="data">Input ndarray</param>
    /// <param name="blockSize">Blocks of [block_size. block_size] are moved</param>
    static member DepthToSpace(data : NDArray, blockSize : int) =
        let creator = AtomicSymbolCreator.FromName "depth_to_space"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"block_size"|]
                                                 [|blockSize.ToString()|]
        outputs

    /// <summary>Rearranges(permutes) blocks of spatial data into depth.
    /// Similar to ONNX SpaceToDepth operator:
    /// https://github.com/onnx/onnx/blob/master/docs/Operators.md#SpaceToDepth 
    /// 
    /// The output is a new tensor where the values from height and width dimension are 
    /// moved to the depth dimension. The reverse of this operation is ``depth_to_space``.
    /// 
    /// .. math::
    /// 
    ///     \begin{gather*}
    ///     x \prime = reshape(x, [N, C, H / block\_size, block\_size, W / block\_size, block\_size]) \\
    ///     x \prime \prime = transpose(x \prime, [0, 3, 5, 1, 2, 4]) \\
    ///     y = reshape(x \prime \prime, [N, C * (block\_size ^ 2), H / block\_size, W / block\_size])
    ///     \end{gather*}
    /// 
    /// where :math:`x` is an input tensor with default layout as :math:`[N, C, H, W]`: [batch, channels, height, width] 
    /// and :math:`y` is the output tensor of layout :math:`[N, C * (block\_size ^ 2), H / block\_size, W / block\_size]`
    /// 
    /// Example::
    /// 
    ///   x = [[[[0, 6, 1, 7, 2, 8],
    ///          [12, 18, 13, 19, 14, 20],
    ///          [3, 9, 4, 10, 5, 11],
    ///          [15, 21, 16, 22, 17, 23]]]]
    /// 
    /// 
    ///   space_to_depth(x, 2) = [[[[0, 1, 2],
    ///                             [3, 4, 5]],
    ///                            [[6, 7, 8],
    ///                             [9, 10, 11]],
    ///                            [[12, 13, 14],
    ///                             [15, 16, 17]],
    ///                            [[18, 19, 20],
    ///                             [21, 22, 23]]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L1106</summary>
    /// <param name="data">Input ndarray</param>
    /// <param name="blockSize">Blocks of [block_size. block_size] are moved</param>
    static member SpaceToDepth(data : NDArray, blockSize : int) =
        let creator = AtomicSymbolCreator.FromName "space_to_depth"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"block_size"|]
                                                 [|blockSize.ToString()|]
        outputs

    /// <summary>Splits an array along a particular axis into multiple sub-arrays.
    /// 
    /// Example::
    /// 
    ///    x  = [[[ 1.]
    ///           [ 2.]]
    ///          [[ 3.]
    ///           [ 4.]]
    ///          [[ 5.]
    ///           [ 6.]]]
    ///    x.shape = (3, 2, 1)
    /// 
    ///    y = split_v2(x, axis=1, indices_or_sections=2) // a list of 2 arrays with shape (3, 1, 1)
    ///    y = [[[ 1.]]
    ///         [[ 3.]]
    ///         [[ 5.]]]
    /// 
    ///        [[[ 2.]]
    ///         [[ 4.]]
    ///         [[ 6.]]]
    /// 
    ///    y[0].shape = (3, 1, 1)
    /// 
    ///    z = split_v2(x, axis=0, indices_or_sections=3) // a list of 3 arrays with shape (1, 2, 1)
    ///    z = [[[ 1.]
    ///          [ 2.]]]
    /// 
    ///        [[[ 3.]
    ///          [ 4.]]]
    /// 
    ///        [[[ 5.]
    ///          [ 6.]]]
    /// 
    ///    z[0].shape = (1, 2, 1)
    /// 
    ///    w = split_v2(x, axis=0, indices_or_sections=(1,)) // a list of 2 arrays with shape [(1, 2, 1), (2, 2, 1)]
    ///    w = [[[ 1.]
    ///          [ 2.]]]
    /// 
    ///        [[[3.]
    ///          [4.]]
    /// 
    ///         [[5.]
    ///          [6.]]]
    /// 
    ///   w[0].shape = (1, 2, 1)
    ///   w[1].shape = (2, 2, 1)
    /// 
    /// `squeeze_axis=True` removes the axis with length 1 from the shapes of the output arrays.
    /// **Note** that setting `squeeze_axis` to ``1`` removes axis with length 1 only
    /// along the `axis` which it is split.
    /// Also `squeeze_axis` can be set to true only if ``input.shape[axis] == indices_or_sections``.
    /// 
    /// Example::
    /// 
    ///    z = split_v2(x, axis=0, indices_or_sections=3, squeeze_axis=1) // a list of 3 arrays with shape (2, 1)
    ///    z = [[ 1.]
    ///         [ 2.]]
    /// 
    ///        [[ 3.]
    ///         [ 4.]]
    /// 
    ///        [[ 5.]
    ///         [ 6.]]
    ///    z[0].shape = (2, 1)
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\matrix_op.cc:L1192</summary>
    /// <param name="data">The input</param>
    /// <param name="indices">Indices of splits. The elements should denote the boundaries of at which split is performed along the `axis`.</param>
    /// <param name="axis">Axis along which to split.</param>
    /// <param name="squeezeAxis">If true, Removes the axis with length 1 from the shapes of the output arrays. **Note** that setting `squeeze_axis` to ``true`` removes axis with length 1 only along the `axis` which it is split. Also `squeeze_axis` can be set to ``true`` only if ``input.shape[axis] == num_outputs``.</param>
    /// <param name="sections">Number of sections if equally splitted. Default to 0 which means split by indices.</param>
    static member SplitV2(data : NDArray, 
                          indices : int seq, 
                          [<Optional; DefaultParameterValue(1)>] axis : int, 
                          [<Optional; DefaultParameterValue(false)>] squeezeAxis : bool, 
                          [<Optional; DefaultParameterValue(0)>] sections : int) =
        let creator = AtomicSymbolCreator.FromName "_split_v2"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"indices"; "axis"; "squeeze_axis"; "sections"|]
                                                 [|indices.ToString(); axis.ToString(); squeezeAxis.ToString(); sections.ToString()|]
        outputs

    /// <summary>Returns the top *k* elements in an input array along the given axis.
    ///  The returned elements will be sorted.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 0.3,  0.2,  0.4],
    ///        [ 0.1,  0.3,  0.2]]
    /// 
    ///   // returns an index of the largest element on last axis
    ///   topk(x) = [[ 2.],
    ///              [ 1.]]
    /// 
    ///   // returns the value of top-2 largest elements on last axis
    ///   topk(x, ret_typ=&#39;value&#39;, k=2) = [[ 0.4,  0.3],
    ///                                    [ 0.3,  0.2]]
    /// 
    ///   // returns the value of top-2 smallest elements on last axis
    ///   topk(x, ret_typ=&#39;value&#39;, k=2, is_ascend=1) = [[ 0.2 ,  0.3],
    ///                                                [ 0.1 ,  0.2]]
    /// 
    ///   // returns the value of top-2 largest elements on axis 0
    ///   topk(x, axis=0, ret_typ=&#39;value&#39;, k=2) = [[ 0.3,  0.3,  0.4],
    ///                                            [ 0.1,  0.2,  0.2]]
    /// 
    ///   // flattens and then returns list of both values and indices
    ///   topk(x, ret_typ=&#39;both&#39;, k=2) = [[[ 0.4,  0.3], [ 0.3,  0.2]] ,  [[ 2.,  0.], [ 1.,  2.]]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\ordering_op.cc:L64</summary>
    /// <param name="data">The input array</param>
    /// <param name="axis">Axis along which to choose the top k indices. If not given, the flattened array is used. Default is -1.</param>
    /// <param name="k">Number of top elements to select, should be always smaller than or equal to the element number in the given axis. A global sort is performed if set k &lt; 1.</param>
    /// <param name="retTyp">The return type.
    ///  &quot;value&quot; means to return the top k values, &quot;indices&quot; means to return the indices of the top k values, &quot;mask&quot; means to return a mask array containing 0 and 1. 1 means the top k values. &quot;both&quot; means to return a list of both values and indices of top k elements.</param>
    /// <param name="isAscend">Whether to choose k largest or k smallest elements. Top K largest elements will be chosen if set to false.</param>
    /// <param name="dtype">DType of the output indices when ret_typ is &quot;indices&quot; or &quot;both&quot;. An error will be raised if the selected data type cannot precisely represent the indices.</param>
    static member Topk(data : NDArray, 
                       [<Optional>] axis : int Nullable, 
                       [<Optional; DefaultParameterValue(1)>] k : int, 
                       [<Optional>] retTyp : RetTyp, 
                       [<Optional; DefaultParameterValue(false)>] isAscend : bool, 
                       [<Optional>] dtype : TopkDtype) =
        let creator = AtomicSymbolCreator.FromName "topk"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "k"; "ret_typ"; "is_ascend"; "dtype"|]
                                                 [|axis.ToString(); k.ToString(); (if isNull (retTyp :> obj) then "indices" else retTyp.ToString()); isAscend.ToString(); (if isNull (dtype :> obj) then "float32" else dtype.ToString())|]
        outputs

    /// <summary>Returns the top *k* elements in an input array along the given axis.
    ///  The returned elements will be sorted.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 0.3,  0.2,  0.4],
    ///        [ 0.1,  0.3,  0.2]]
    /// 
    ///   // returns an index of the largest element on last axis
    ///   topk(x) = [[ 2.],
    ///              [ 1.]]
    /// 
    ///   // returns the value of top-2 largest elements on last axis
    ///   topk(x, ret_typ=&#39;value&#39;, k=2) = [[ 0.4,  0.3],
    ///                                    [ 0.3,  0.2]]
    /// 
    ///   // returns the value of top-2 smallest elements on last axis
    ///   topk(x, ret_typ=&#39;value&#39;, k=2, is_ascend=1) = [[ 0.2 ,  0.3],
    ///                                                [ 0.1 ,  0.2]]
    /// 
    ///   // returns the value of top-2 largest elements on axis 0
    ///   topk(x, axis=0, ret_typ=&#39;value&#39;, k=2) = [[ 0.3,  0.3,  0.4],
    ///                                            [ 0.1,  0.2,  0.2]]
    /// 
    ///   // flattens and then returns list of both values and indices
    ///   topk(x, ret_typ=&#39;both&#39;, k=2) = [[[ 0.4,  0.3], [ 0.3,  0.2]] ,  [[ 2.,  0.], [ 1.,  2.]]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\ordering_op.cc:L64</summary>
    /// <param name="data">The input array</param>
    /// <param name="axis">Axis along which to choose the top k indices. If not given, the flattened array is used. Default is -1.</param>
    /// <param name="k">Number of top elements to select, should be always smaller than or equal to the element number in the given axis. A global sort is performed if set k &lt; 1.</param>
    /// <param name="retTyp">The return type.
    ///  &quot;value&quot; means to return the top k values, &quot;indices&quot; means to return the indices of the top k values, &quot;mask&quot; means to return a mask array containing 0 and 1. 1 means the top k values. &quot;both&quot; means to return a list of both values and indices of top k elements.</param>
    /// <param name="isAscend">Whether to choose k largest or k smallest elements. Top K largest elements will be chosen if set to false.</param>
    /// <param name="dtype">DType of the output indices when ret_typ is &quot;indices&quot; or &quot;both&quot;. An error will be raised if the selected data type cannot precisely represent the indices.</param>
    static member Topk(data : NDArray, 
                       ?axis : int, 
                       ?k : int, 
                       ?retTyp : RetTyp, 
                       ?isAscend : bool, 
                       ?dtype : TopkDtype) =
        let creator = AtomicSymbolCreator.FromName "topk"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "k"; "ret_typ"; "is_ascend"; "dtype"|]
                                                 [|(match axis with None -> "None" | _ -> axis.ToString()); (match k with None -> "1" | _ -> k.ToString()); (match retTyp with None -> "indices" | _ -> retTyp.ToString()); (match isAscend with None -> "false" | _ -> isAscend.ToString()); (match dtype with None -> "float32" | _ -> dtype.ToString())|]
        outputs

    /// <summary>Returns a sorted copy of an input array along the given axis.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 1, 4],
    ///        [ 3, 1]]
    /// 
    ///   // sorts along the last axis
    ///   sort(x) = [[ 1.,  4.],
    ///              [ 1.,  3.]]
    /// 
    ///   // flattens and then sorts
    ///   sort(x) = [ 1.,  1.,  3.,  4.]
    /// 
    ///   // sorts along the first axis
    ///   sort(x, axis=0) = [[ 1.,  1.],
    ///                      [ 3.,  4.]]
    /// 
    ///   // in a descend order
    ///   sort(x, is_ascend=0) = [[ 4.,  1.],
    ///                           [ 3.,  1.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\ordering_op.cc:L127</summary>
    /// <param name="data">The input array</param>
    /// <param name="axis">Axis along which to choose sort the input tensor. If not given, the flattened array is used. Default is -1.</param>
    /// <param name="isAscend">Whether to sort in ascending or descending order.</param>
    static member Sort(data : NDArray, [<Optional>] axis : int Nullable, [<Optional; DefaultParameterValue(true)>] isAscend : bool) =
        let creator = AtomicSymbolCreator.FromName "sort"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "is_ascend"|]
                                                 [|axis.ToString(); isAscend.ToString()|]
        outputs

    /// <summary>Returns a sorted copy of an input array along the given axis.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 1, 4],
    ///        [ 3, 1]]
    /// 
    ///   // sorts along the last axis
    ///   sort(x) = [[ 1.,  4.],
    ///              [ 1.,  3.]]
    /// 
    ///   // flattens and then sorts
    ///   sort(x) = [ 1.,  1.,  3.,  4.]
    /// 
    ///   // sorts along the first axis
    ///   sort(x, axis=0) = [[ 1.,  1.],
    ///                      [ 3.,  4.]]
    /// 
    ///   // in a descend order
    ///   sort(x, is_ascend=0) = [[ 4.,  1.],
    ///                           [ 3.,  1.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\ordering_op.cc:L127</summary>
    /// <param name="data">The input array</param>
    /// <param name="axis">Axis along which to choose sort the input tensor. If not given, the flattened array is used. Default is -1.</param>
    /// <param name="isAscend">Whether to sort in ascending or descending order.</param>
    static member Sort(data : NDArray, ?axis : int, ?isAscend : bool) =
        let creator = AtomicSymbolCreator.FromName "sort"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "is_ascend"|]
                                                 [|(match axis with None -> "None" | _ -> axis.ToString()); (match isAscend with None -> "true" | _ -> isAscend.ToString())|]
        outputs

    /// <summary>Returns the indices that would sort an input array along the given axis.
    /// 
    /// This function performs sorting along the given axis and returns an array of indices having same shape
    /// as an input array that index data in sorted order.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 0.3,  0.2,  0.4],
    ///        [ 0.1,  0.3,  0.2]]
    /// 
    ///   // sort along axis -1
    ///   argsort(x) = [[ 1.,  0.,  2.],
    ///                 [ 0.,  2.,  1.]]
    /// 
    ///   // sort along axis 0
    ///   argsort(x, axis=0) = [[ 1.,  0.,  1.]
    ///                         [ 0.,  1.,  0.]]
    /// 
    ///   // flatten and then sort
    ///   argsort(x) = [ 3.,  1.,  5.,  0.,  4.,  2.]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\ordering_op.cc:L177</summary>
    /// <param name="data">The input array</param>
    /// <param name="axis">Axis along which to sort the input tensor. If not given, the flattened array is used. Default is -1.</param>
    /// <param name="isAscend">Whether to sort in ascending or descending order.</param>
    /// <param name="dtype">DType of the output indices. It is only valid when ret_typ is &quot;indices&quot; or &quot;both&quot;. An error will be raised if the selected data type cannot precisely represent the indices.</param>
    static member Argsort(data : NDArray, [<Optional>] axis : int Nullable, [<Optional; DefaultParameterValue(true)>] isAscend : bool, [<Optional>] dtype : ArgsortDtype) =
        let creator = AtomicSymbolCreator.FromName "argsort"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "is_ascend"; "dtype"|]
                                                 [|axis.ToString(); isAscend.ToString(); (if isNull (dtype :> obj) then "float32" else dtype.ToString())|]
        outputs

    /// <summary>Returns the indices that would sort an input array along the given axis.
    /// 
    /// This function performs sorting along the given axis and returns an array of indices having same shape
    /// as an input array that index data in sorted order.
    /// 
    /// Examples::
    /// 
    ///   x = [[ 0.3,  0.2,  0.4],
    ///        [ 0.1,  0.3,  0.2]]
    /// 
    ///   // sort along axis -1
    ///   argsort(x) = [[ 1.,  0.,  2.],
    ///                 [ 0.,  2.,  1.]]
    /// 
    ///   // sort along axis 0
    ///   argsort(x, axis=0) = [[ 1.,  0.,  1.]
    ///                         [ 0.,  1.,  0.]]
    /// 
    ///   // flatten and then sort
    ///   argsort(x) = [ 3.,  1.,  5.,  0.,  4.,  2.]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\ordering_op.cc:L177</summary>
    /// <param name="data">The input array</param>
    /// <param name="axis">Axis along which to sort the input tensor. If not given, the flattened array is used. Default is -1.</param>
    /// <param name="isAscend">Whether to sort in ascending or descending order.</param>
    /// <param name="dtype">DType of the output indices. It is only valid when ret_typ is &quot;indices&quot; or &quot;both&quot;. An error will be raised if the selected data type cannot precisely represent the indices.</param>
    static member Argsort(data : NDArray, ?axis : int, ?isAscend : bool, ?dtype : ArgsortDtype) =
        let creator = AtomicSymbolCreator.FromName "argsort"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "is_ascend"; "dtype"|]
                                                 [|(match axis with None -> "None" | _ -> axis.ToString()); (match isAscend with None -> "true" | _ -> isAscend.ToString()); (match dtype with None -> "float32" | _ -> dtype.ToString())|]
        outputs

    /// <summary>Converts a batch of index arrays into an array of flat indices. The operator follows numpy conventions so a single multi index is given by a column of the input matrix. The leading dimension may be left unspecified by using -1 as placeholder.  
    /// 
    /// Examples::
    ///    
    ///    A = [[3,6,6],[4,5,1]]
    ///    ravel(A, shape=(7,6)) = [22,41,37]
    ///    ravel(A, shape=(-1,6)) = [22,41,37]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\ravel.cc:L42</summary>
    /// <param name="data">Batch of multi-indices</param>
    /// <param name="shape">Shape of the array into which the multi-indices apply.</param>
    static member RavelMultiIndex(data : NDArray, [<Optional>] shape : int seq) =
        let creator = AtomicSymbolCreator.FromName "_ravel_multi_index"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"shape"|]
                                                 [|(if isNull (shape :> obj) then "None" else shape.ToString())|]
        outputs

    /// <summary>Converts an array of flat indices into a batch of index arrays. The operator follows numpy conventions so a single multi index is given by a column of the output matrix. The leading dimension may be left unspecified by using -1 as placeholder.  
    /// 
    /// Examples::
    /// 
    ///    A = [22,41,37]
    ///    unravel(A, shape=(7,6)) = [[3,6,6],[4,5,1]]
    ///    unravel(A, shape=(-1,6)) = [[3,6,6],[4,5,1]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\ravel.cc:L67</summary>
    /// <param name="data">Array of flat indices</param>
    /// <param name="shape">Shape of the array into which the multi-indices apply.</param>
    static member UnravelIndex(data : NDArray, [<Optional>] shape : int seq) =
        let creator = AtomicSymbolCreator.FromName "_unravel_index"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"shape"|]
                                                 [|(if isNull (shape :> obj) then "None" else shape.ToString())|]
        outputs

    /// <summary>pick rows specified by user input index array from a row sparse matrix
    /// and save them in the output sparse matrix.
    /// 
    /// Example::
    /// 
    ///   data = [[1, 2], [3, 4], [5, 6]]
    ///   indices = [0, 1, 3]
    ///   shape = (4, 2)
    ///   rsp_in = row_sparse(data, indices)
    ///   to_retain = [0, 3]
    ///   rsp_out = retain(rsp_in, to_retain)
    ///   rsp_out.values = [[1, 2], [5, 6]]
    ///   rsp_out.indices = [0, 3]
    /// 
    /// The storage type of ``retain`` output depends on storage types of inputs
    /// 
    /// - retain(row_sparse, default) = row_sparse
    /// - otherwise, ``retain`` is not supported
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\sparse_retain.cc:L53</summary>
    /// <param name="data">The input array for sparse_retain operator.</param>
    /// <param name="indices">The index array of rows ids that will be retained.</param>
    static member SparseRetain(data : NDArray, indices : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_sparse_retain"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; indices.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Computes the square sum of array elements over a given axis
    /// for row-sparse matrix. This is a temporary solution for fusing ops square and
    /// sum together for row-sparse matrix to save memory for storing gradients.
    /// It will become deprecated once the functionality of fusing operators is finished
    /// in the future.
    /// 
    /// Example::
    /// 
    ///   dns = mx.nd.array([[0, 0], [1, 2], [0, 0], [3, 4], [0, 0]])
    ///   rsp = dns.tostype(&#39;row_sparse&#39;)
    ///   sum = mx.nd._internal._square_sum(rsp, axis=1)
    ///   sum = [0, 5, 0, 25, 0]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\tensor\square_sum.cc:L63</summary>
    /// <param name="data">The input</param>
    /// <param name="axis">The axis or axes along which to perform the reduction.
    /// 
    ///       The default, `axis=()`, will compute over all elements into a
    ///       scalar array with shape `(1,)`.
    /// 
    ///       If `axis` is int, a reduction is performed on a particular axis.
    /// 
    ///       If `axis` is a tuple of ints, a reduction is performed on all the axes
    ///       specified in the tuple.
    /// 
    ///       If `exclude` is true, reduction will be performed on the axes that are
    ///       NOT in axis instead.
    /// 
    ///       Negative values means indexing from right to left.</param>
    /// <param name="keepdims">If this is set to `True`, the reduced axes are left in the result as dimension with size one.</param>
    /// <param name="exclude">Whether to perform reduction on axis that are NOT in axis instead.</param>
    static member SquareSum(data : NDArray, [<Optional>] axis : int seq, [<Optional; DefaultParameterValue(false)>] keepdims : bool, [<Optional; DefaultParameterValue(false)>] exclude : bool) =
        let creator = AtomicSymbolCreator.FromName "_square_sum"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"axis"; "keepdims"; "exclude"|]
                                                 [|axis.ToString(); keepdims.ToString(); exclude.ToString()|]
        outputs

    /// <summary>Applies bilinear sampling to input feature map.
    /// 
    /// Bilinear Sampling is the key of  [NIPS2015] \&quot;Spatial Transformer Networks\&quot;. The usage of the operator is very similar to remap function in OpenCV,
    /// except that the operator has the backward pass.
    /// 
    /// Given :math:`data` and :math:`grid`, then the output is computed by
    /// 
    /// .. math::
    ///   x_{src} = grid[batch, 0, y_{dst}, x_{dst}] \\
    ///   y_{src} = grid[batch, 1, y_{dst}, x_{dst}] \\
    ///   output[batch, channel, y_{dst}, x_{dst}] = G(data[batch, channel, y_{src}, x_{src})
    /// 
    /// :math:`x_{dst}`, :math:`y_{dst}` enumerate all spatial locations in :math:`output`, and :math:`G()` denotes the bilinear interpolation kernel.
    /// The out-boundary points will be padded with zeros.The shape of the output will be (data.shape[0], data.shape[1], grid.shape[2], grid.shape[3]).
    /// 
    /// The operator assumes that :math:`data` has &#39;NCHW&#39; layout and :math:`grid` has been normalized to [-1, 1].
    /// 
    /// BilinearSampler often cooperates with GridGenerator which generates sampling grids for BilinearSampler.
    /// GridGenerator supports two kinds of transformation: ``affine`` and ``warp``.
    /// If users want to design a CustomOp to manipulate :math:`grid`, please firstly refer to the code of GridGenerator.
    /// 
    /// Example 1::
    /// 
    ///   ## Zoom out data two times
    ///   data = array([[[[1, 4, 3, 6],
    ///                   [1, 8, 8, 9],
    ///                   [0, 4, 1, 5],
    ///                   [1, 0, 1, 3]]]])
    /// 
    ///   affine_matrix = array([[2, 0, 0],
    ///                          [0, 2, 0]])
    /// 
    ///   affine_matrix = reshape(affine_matrix, shape=(1, 6))
    /// 
    ///   grid = GridGenerator(data=affine_matrix, transform_type=&#39;affine&#39;, target_shape=(4, 4))
    /// 
    ///   out = BilinearSampler(data, grid)
    /// 
    ///   out
    ///   [[[[ 0,   0,     0,   0],
    ///      [ 0,   3.5,   6.5, 0],
    ///      [ 0,   1.25,  2.5, 0],
    ///      [ 0,   0,     0,   0]]]
    /// 
    /// 
    /// Example 2::
    /// 
    ///   ## shift data horizontally by -1 pixel
    /// 
    ///   data = array([[[[1, 4, 3, 6],
    ///                   [1, 8, 8, 9],
    ///                   [0, 4, 1, 5],
    ///                   [1, 0, 1, 3]]]])
    /// 
    ///   warp_maxtrix = array([[[[1, 1, 1, 1],
    ///                           [1, 1, 1, 1],
    ///                           [1, 1, 1, 1],
    ///                           [1, 1, 1, 1]],
    ///                          [[0, 0, 0, 0],
    ///                           [0, 0, 0, 0],
    ///                           [0, 0, 0, 0],
    ///                           [0, 0, 0, 0]]]])
    /// 
    ///   grid = GridGenerator(data=warp_matrix, transform_type=&#39;warp&#39;)
    ///   out = BilinearSampler(data, grid)
    /// 
    ///   out
    ///   [[[[ 4,  3,  6,  0],
    ///      [ 8,  8,  9,  0],
    ///      [ 4,  1,  5,  0],
    ///      [ 0,  1,  3,  0]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\bilinear_sampler.cc:L256</summary>
    /// <param name="data">Input data to the BilinearsamplerOp.</param>
    /// <param name="grid">Input grid to the BilinearsamplerOp.grid has two channels: x_src, y_src</param>
    /// <param name="cudnnOff">whether to turn cudnn off</param>
    static member BilinearSampler(data : NDArray, grid : NDArray, [<Optional>] cudnnOff : bool Nullable) =
        let creator = AtomicSymbolCreator.FromName "BilinearSampler"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; grid.NDArrayHandle|]
                                                 [|"cudnn_off"|]
                                                 [|cudnnOff.ToString()|]
        outputs

    /// <summary>Applies bilinear sampling to input feature map.
    /// 
    /// Bilinear Sampling is the key of  [NIPS2015] \&quot;Spatial Transformer Networks\&quot;. The usage of the operator is very similar to remap function in OpenCV,
    /// except that the operator has the backward pass.
    /// 
    /// Given :math:`data` and :math:`grid`, then the output is computed by
    /// 
    /// .. math::
    ///   x_{src} = grid[batch, 0, y_{dst}, x_{dst}] \\
    ///   y_{src} = grid[batch, 1, y_{dst}, x_{dst}] \\
    ///   output[batch, channel, y_{dst}, x_{dst}] = G(data[batch, channel, y_{src}, x_{src})
    /// 
    /// :math:`x_{dst}`, :math:`y_{dst}` enumerate all spatial locations in :math:`output`, and :math:`G()` denotes the bilinear interpolation kernel.
    /// The out-boundary points will be padded with zeros.The shape of the output will be (data.shape[0], data.shape[1], grid.shape[2], grid.shape[3]).
    /// 
    /// The operator assumes that :math:`data` has &#39;NCHW&#39; layout and :math:`grid` has been normalized to [-1, 1].
    /// 
    /// BilinearSampler often cooperates with GridGenerator which generates sampling grids for BilinearSampler.
    /// GridGenerator supports two kinds of transformation: ``affine`` and ``warp``.
    /// If users want to design a CustomOp to manipulate :math:`grid`, please firstly refer to the code of GridGenerator.
    /// 
    /// Example 1::
    /// 
    ///   ## Zoom out data two times
    ///   data = array([[[[1, 4, 3, 6],
    ///                   [1, 8, 8, 9],
    ///                   [0, 4, 1, 5],
    ///                   [1, 0, 1, 3]]]])
    /// 
    ///   affine_matrix = array([[2, 0, 0],
    ///                          [0, 2, 0]])
    /// 
    ///   affine_matrix = reshape(affine_matrix, shape=(1, 6))
    /// 
    ///   grid = GridGenerator(data=affine_matrix, transform_type=&#39;affine&#39;, target_shape=(4, 4))
    /// 
    ///   out = BilinearSampler(data, grid)
    /// 
    ///   out
    ///   [[[[ 0,   0,     0,   0],
    ///      [ 0,   3.5,   6.5, 0],
    ///      [ 0,   1.25,  2.5, 0],
    ///      [ 0,   0,     0,   0]]]
    /// 
    /// 
    /// Example 2::
    /// 
    ///   ## shift data horizontally by -1 pixel
    /// 
    ///   data = array([[[[1, 4, 3, 6],
    ///                   [1, 8, 8, 9],
    ///                   [0, 4, 1, 5],
    ///                   [1, 0, 1, 3]]]])
    /// 
    ///   warp_maxtrix = array([[[[1, 1, 1, 1],
    ///                           [1, 1, 1, 1],
    ///                           [1, 1, 1, 1],
    ///                           [1, 1, 1, 1]],
    ///                          [[0, 0, 0, 0],
    ///                           [0, 0, 0, 0],
    ///                           [0, 0, 0, 0],
    ///                           [0, 0, 0, 0]]]])
    /// 
    ///   grid = GridGenerator(data=warp_matrix, transform_type=&#39;warp&#39;)
    ///   out = BilinearSampler(data, grid)
    /// 
    ///   out
    ///   [[[[ 4,  3,  6,  0],
    ///      [ 8,  8,  9,  0],
    ///      [ 4,  1,  5,  0],
    ///      [ 0,  1,  3,  0]]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\bilinear_sampler.cc:L256</summary>
    /// <param name="data">Input data to the BilinearsamplerOp.</param>
    /// <param name="grid">Input grid to the BilinearsamplerOp.grid has two channels: x_src, y_src</param>
    /// <param name="cudnnOff">whether to turn cudnn off</param>
    static member BilinearSampler(data : NDArray, grid : NDArray, ?cudnnOff : bool) =
        let creator = AtomicSymbolCreator.FromName "BilinearSampler"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; grid.NDArrayHandle|]
                                                 [|"cudnn_off"|]
                                                 [|(match cudnnOff with None -> "None" | _ -> cudnnOff.ToString())|]
        outputs

    /// <summary>Apply CountSketch to input: map a d-dimension data to k-dimension data&quot;
    /// 
    /// .. note:: `count_sketch` is only available on GPU.
    /// 
    /// Assume input data has shape (N, d), sign hash table s has shape (N, d),
    /// index hash table h has shape (N, d) and mapping dimension out_dim = k,
    /// each element in s is either +1 or -1, each element in h is random integer from 0 to k-1.
    /// Then the operator computs:
    /// 
    /// .. math::
    ///    out[h[i]] += data[i] * s[i]
    /// 
    /// Example::
    /// 
    ///    out_dim = 5
    ///    x = [[1.2, 2.5, 3.4],[3.2, 5.7, 6.6]]
    ///    h = [[0, 3, 4]]
    ///    s = [[1, -1, 1]]
    ///    mx.contrib.ndarray.count_sketch(data=x, h=h, s=s, out_dim = 5) = [[1.2, 0, 0, -2.5, 3.4],
    ///                                                                      [3.2, 0, 0, -5.7, 6.6]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\count_sketch.cc:L67</summary>
    /// <param name="data">Input data to the CountSketchOp.</param>
    /// <param name="h">The index vector</param>
    /// <param name="s">The sign vector</param>
    /// <param name="outDim">The output dimension.</param>
    /// <param name="processingBatchSize">How many sketch vectors to process at one time.</param>
    static member ContribCountSketch(data : NDArray, 
                                     h : NDArray, 
                                     s : NDArray, 
                                     outDim : int, 
                                     [<Optional; DefaultParameterValue(32)>] processingBatchSize : int) =
        let creator = AtomicSymbolCreator.FromName "_contrib_count_sketch"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; h.NDArrayHandle; s.NDArrayHandle|]
                                                 [|"out_dim"; "processing_batch_size"|]
                                                 [|outDim.ToString(); processingBatchSize.ToString()|]
        outputs

    /// <summary>Compute 2-D deformable convolution on 4-D input.
    /// 
    /// The deformable convolution operation is described in https://arxiv.org/abs/1703.06211
    /// 
    /// For 2-D deformable convolution, the shapes are
    /// 
    /// - **data**: *(batch_size, channel, height, width)*
    /// - **offset**: *(batch_size, num_deformable_group * kernel[0] * kernel[1] * 2, height, width)*
    /// - **weight**: *(num_filter, channel, kernel[0], kernel[1])*
    /// - **bias**: *(num_filter,)*
    /// - **out**: *(batch_size, num_filter, out_height, out_width)*.
    /// 
    /// Define::
    /// 
    ///   f(x,k,p,s,d) = floor((x+2*p-d*(k-1)-1)/s)+1
    /// 
    /// then we have::
    /// 
    ///   out_height=f(height, kernel[0], pad[0], stride[0], dilate[0])
    ///   out_width=f(width, kernel[1], pad[1], stride[1], dilate[1])
    /// 
    /// If ``no_bias`` is set to be true, then the ``bias`` term is ignored.
    /// 
    /// The default data ``layout`` is *NCHW*, namely *(batch_size, channle, height,
    /// width)*.
    /// 
    /// If ``num_group`` is larger than 1, denoted by *g*, then split the input ``data``
    /// evenly into *g* parts along the channel axis, and also evenly split ``weight``
    /// along the first dimension. Next compute the convolution on the *i*-th part of
    /// the data with the *i*-th weight part. The output is obtained by concating all
    /// the *g* results.
    /// 
    /// If ``num_deformable_group`` is larger than 1, denoted by *dg*, then split the
    /// input ``offset`` evenly into *dg* parts along the channel axis, and also evenly
    /// split ``data`` into *dg* parts along the channel axis. Next compute the
    /// deformable convolution, apply the *i*-th part of the offset on the *i*-th part
    /// of the data.
    /// 
    /// 
    /// Both ``weight`` and ``bias`` are learnable parameters.
    /// 
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\deformable_convolution.cc:L100</summary>
    /// <param name="data">Input data to the DeformableConvolutionOp.</param>
    /// <param name="offset">Input offset to the DeformableConvolutionOp.</param>
    /// <param name="weight">Weight matrix.</param>
    /// <param name="bias">Bias parameter.</param>
    /// <param name="kernel">Convolution kernel size: (h, w) or (d, h, w)</param>
    /// <param name="stride">Convolution stride: (h, w) or (d, h, w). Defaults to 1 for each dimension.</param>
    /// <param name="dilate">Convolution dilate: (h, w) or (d, h, w). Defaults to 1 for each dimension.</param>
    /// <param name="pad">Zero pad for convolution: (h, w) or (d, h, w). Defaults to no padding.</param>
    /// <param name="numFilter">Convolution filter(channel) number</param>
    /// <param name="numGroup">Number of group partitions.</param>
    /// <param name="numDeformableGroup">Number of deformable group partitions.</param>
    /// <param name="workspace">Maximum temperal workspace allowed for convolution (MB).</param>
    /// <param name="noBias">Whether to disable bias parameter.</param>
    /// <param name="layout">Set layout for input, output and weight. Empty for
    ///     default layout: NCW for 1d, NCHW for 2d and NCDHW for 3d.</param>
    static member ContribDeformableConvolution(data : NDArray, 
                                               offset : NDArray, 
                                               weight : NDArray, 
                                               bias : NDArray, 
                                               kernel : int seq, 
                                               [<Optional>] stride : int seq, 
                                               [<Optional>] dilate : int seq, 
                                               [<Optional>] pad : int seq, 
                                               numFilter : int, 
                                               [<Optional; DefaultParameterValue(1)>] numGroup : int, 
                                               [<Optional; DefaultParameterValue(1)>] numDeformableGroup : int, 
                                               workspace : int64, 
                                               [<Optional; DefaultParameterValue(false)>] noBias : bool, 
                                               [<Optional>] layout : ContribDeformableConvolutionLayout) =
        let creator = AtomicSymbolCreator.FromName "_contrib_DeformableConvolution"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; offset.NDArrayHandle; weight.NDArrayHandle; bias.NDArrayHandle|]
                                                 [|"kernel"; "stride"; "dilate"; "pad"; "num_filter"; "num_group"; "num_deformable_group"; "workspace"; "no_bias"; "layout"|]
                                                 [|kernel.ToString(); (if isNull (stride :> obj) then "[]" else stride.ToString()); (if isNull (dilate :> obj) then "[]" else dilate.ToString()); (if isNull (pad :> obj) then "[]" else pad.ToString()); numFilter.ToString(); numGroup.ToString(); numDeformableGroup.ToString(); workspace.ToString(); noBias.ToString(); (if isNull (layout :> obj) then "None" else layout.ToString())|]
        outputs

    /// <summary>Apply 1D FFT to input&quot;
    /// 
    /// .. note:: `fft` is only available on GPU.
    /// 
    /// Currently accept 2 input data shapes: (N, d) or (N1, N2, N3, d), data can only be real numbers.
    /// The output data has shape: (N, 2*d) or (N1, N2, N3, 2*d). The format is: [real0, imag0, real1, imag1, ...].
    /// 
    /// Example::
    /// 
    ///    data = np.random.normal(0,1,(3,4))
    ///    out = mx.contrib.ndarray.fft(data = mx.nd.array(data,ctx = mx.gpu(0)))
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\fft.cc:L56</summary>
    /// <param name="data">Input data to the FFTOp.</param>
    /// <param name="computeSize">Maximum size of sub-batch to be forwarded at one time</param>
    static member ContribFft(data : NDArray, [<Optional; DefaultParameterValue(128)>] computeSize : int) =
        let creator = AtomicSymbolCreator.FromName "_contrib_fft"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"compute_size"|]
                                                 [|computeSize.ToString()|]
        outputs

    /// <summary>Apply 1D ifft to input&quot;
    /// 
    /// .. note:: `ifft` is only available on GPU.
    /// 
    /// Currently accept 2 input data shapes: (N, d) or (N1, N2, N3, d). Data is in format: [real0, imag0, real1, imag1, ...].
    /// Last dimension must be an even number.
    /// The output data has shape: (N, d/2) or (N1, N2, N3, d/2). It is only the real part of the result.
    /// 
    /// Example::
    /// 
    ///    data = np.random.normal(0,1,(3,4))
    ///    out = mx.contrib.ndarray.ifft(data = mx.nd.array(data,ctx = mx.gpu(0)))
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\contrib\ifft.cc:L58</summary>
    /// <param name="data">Input data to the IFFTOp.</param>
    /// <param name="computeSize">Maximum size of sub-batch to be forwarded at one time</param>
    static member ContribIfft(data : NDArray, [<Optional; DefaultParameterValue(128)>] computeSize : int) =
        let creator = AtomicSymbolCreator.FromName "_contrib_ifft"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"compute_size"|]
                                                 [|computeSize.ToString()|]
        outputs

    /// <summary>This operator is DEPRECATED. Apply convolution to input then add a bias.</summary>
    /// <param name="data">Input data to the ConvolutionV1Op.</param>
    /// <param name="weight">Weight matrix.</param>
    /// <param name="bias">Bias parameter.</param>
    /// <param name="kernel">convolution kernel size: (h, w) or (d, h, w)</param>
    /// <param name="stride">convolution stride: (h, w) or (d, h, w)</param>
    /// <param name="dilate">convolution dilate: (h, w) or (d, h, w)</param>
    /// <param name="pad">pad for convolution: (h, w) or (d, h, w)</param>
    /// <param name="numFilter">convolution filter(channel) number</param>
    /// <param name="numGroup">Number of group partitions. Equivalent to slicing input into num_group
    ///     partitions, apply convolution on each, then concatenate the results</param>
    /// <param name="workspace">Maximum temporary workspace allowed for convolution (MB).This parameter determines the effective batch size of the convolution kernel, which may be smaller than the given batch size. Also, the workspace will be automatically enlarged to make sure that we can run the kernel with batch_size=1</param>
    /// <param name="noBias">Whether to disable bias parameter.</param>
    /// <param name="cudnnTune">Whether to pick convolution algo by running performance test.
    ///     Leads to higher startup time but may give faster speed. Options are:
    ///     &#39;off&#39;: no tuning
    ///     &#39;limited_workspace&#39;: run test and pick the fastest algorithm that doesn&#39;t exceed workspace limit.
    ///     &#39;fastest&#39;: pick the fastest algorithm and ignore workspace limit.
    ///     If set to None (default), behavior is determined by environment
    ///     variable MXNET_CUDNN_AUTOTUNE_DEFAULT: 0 for off,
    ///     1 for limited workspace (default), 2 for fastest.</param>
    /// <param name="cudnnOff">Turn off cudnn for this layer.</param>
    /// <param name="layout">Set layout for input, output and weight. Empty for
    ///     default layout: NCHW for 2d and NCDHW for 3d.</param>
    static member ConvolutionV1(data : NDArray, 
                                weight : NDArray, 
                                bias : NDArray, 
                                kernel : int seq, 
                                [<Optional>] stride : int seq, 
                                [<Optional>] dilate : int seq, 
                                [<Optional>] pad : int seq, 
                                numFilter : int, 
                                numGroup : int, 
                                workspace : int64, 
                                [<Optional; DefaultParameterValue(false)>] noBias : bool, 
                                [<Optional>] cudnnTune : CudnnTune, 
                                [<Optional; DefaultParameterValue(false)>] cudnnOff : bool, 
                                [<Optional>] layout : ConvolutionV1Layout) =
        let creator = AtomicSymbolCreator.FromName "Convolution_v1"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; weight.NDArrayHandle; bias.NDArrayHandle|]
                                                 [|"kernel"; "stride"; "dilate"; "pad"; "num_filter"; "num_group"; "workspace"; "no_bias"; "cudnn_tune"; "cudnn_off"; "layout"|]
                                                 [|kernel.ToString(); (if isNull (stride :> obj) then "[]" else stride.ToString()); (if isNull (dilate :> obj) then "[]" else dilate.ToString()); (if isNull (pad :> obj) then "[]" else pad.ToString()); numFilter.ToString(); numGroup.ToString(); workspace.ToString(); noBias.ToString(); (if isNull (cudnnTune :> obj) then "None" else cudnnTune.ToString()); cudnnOff.ToString(); (if isNull (layout :> obj) then "None" else layout.ToString())|]
        outputs

    /// <summary>Applies correlation to inputs.
    /// 
    /// The correlation layer performs multiplicative patch comparisons between two feature maps.
    /// 
    /// Given two multi-channel feature maps :math:`f_{1}, f_{2}`, with :math:`w`, :math:`h`, and :math:`c` being their width, height, and number of channels,
    /// the correlation layer lets the network compare each patch from :math:`f_{1}` with each patch from :math:`f_{2}`.
    /// 
    /// For now we consider only a single comparison of two patches. The &#39;correlation&#39; of two patches centered at :math:`x_{1}` in the first map and
    /// :math:`x_{2}` in the second map is then defined as:
    /// 
    /// .. math::
    /// 
    ///    c(x_{1}, x_{2}) = \sum_{o \in [-k,k] \times [-k,k]} &lt;f_{1}(x_{1} + o), f_{2}(x_{2} + o)&gt;
    /// 
    /// for a square patch of size :math:`K:=2k+1`.
    /// 
    /// Note that the equation above is identical to one step of a convolution in neural networks, but instead of convolving data with a filter, it convolves data with other
    /// data. For this reason, it has no training weights.
    /// 
    /// Computing :math:`c(x_{1}, x_{2})` involves :math:`c * K^{2}` multiplications. Comparing all patch combinations involves :math:`w^{2}*h^{2}` such computations.
    /// 
    /// Given a maximum displacement :math:`d`, for each location :math:`x_{1}` it computes correlations :math:`c(x_{1}, x_{2})` only in a neighborhood of size :math:`D:=2d+1`,
    /// by limiting the range of :math:`x_{2}`. We use strides :math:`s_{1}, s_{2}`, to quantize :math:`x_{1}` globally and to quantize :math:`x_{2}` within the neighborhood
    /// centered around :math:`x_{1}`.
    /// 
    /// The final output is defined by the following expression:
    /// 
    /// .. math::
    ///   out[n, q, i, j] = c(x_{i, j}, x_{q})
    /// 
    /// where :math:`i` and :math:`j` enumerate spatial locations in :math:`f_{1}`, and :math:`q` denotes the :math:`q^{th}` neighborhood of :math:`x_{i,j}`.
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\correlation.cc:L198</summary>
    /// <param name="data1">Input data1 to the correlation.</param>
    /// <param name="data2">Input data2 to the correlation.</param>
    /// <param name="kernelSize">kernel size for Correlation must be an odd number</param>
    /// <param name="maxDisplacement">Max displacement of Correlation </param>
    /// <param name="stride1">stride1 quantize data1 globally</param>
    /// <param name="stride2">stride2 quantize data2 within the neighborhood centered around data1</param>
    /// <param name="padSize">pad for Correlation</param>
    /// <param name="isMultiply">operation type is either multiplication or subduction</param>
    static member Correlation(data1 : NDArray, 
                              data2 : NDArray, 
                              kernelSize : int, 
                              maxDisplacement : int, 
                              stride1 : int, 
                              stride2 : int, 
                              padSize : int, 
                              [<Optional; DefaultParameterValue(true)>] isMultiply : bool) =
        let creator = AtomicSymbolCreator.FromName "Correlation"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data1.NDArrayHandle; data2.NDArrayHandle|]
                                                 [|"kernel_size"; "max_displacement"; "stride1"; "stride2"; "pad_size"; "is_multiply"|]
                                                 [|kernelSize.ToString(); maxDisplacement.ToString(); stride1.ToString(); stride2.ToString(); padSize.ToString(); isMultiply.ToString()|]
        outputs

    /// <summary>Generates 2D sampling grid for bilinear sampling.</summary>
    /// <param name="data">Input data to the function.</param>
    /// <param name="transformType">The type of transformation. For `affine`, input data should be an affine matrix of size (batch, 6). For `warp`, input data should be an optical flow of size (batch, 2, h, w).</param>
    /// <param name="targetShape">Specifies the output shape (H, W). This is required if transformation type is `affine`. If transformation type is `warp`, this parameter is ignored.</param>
    static member GridGenerator(data : NDArray, transformType : GridGeneratorTransformType, [<Optional>] targetShape : int seq) =
        let creator = AtomicSymbolCreator.FromName "GridGenerator"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"transform_type"; "target_shape"|]
                                                 [|transformType.ToString(); (if isNull (targetShape :> obj) then "[0,0]" else targetShape.ToString())|]
        outputs

    /// <summary>Applies instance normalization to the n-dimensional input array.
    /// 
    /// This operator takes an n-dimensional input array where (n&gt;2) and normalizes
    /// the input using the following formula:
    /// 
    /// .. math::
    /// 
    ///   out = \frac{x - mean[data]}{ \sqrt{Var[data]} + \epsilon} * gamma + beta
    /// 
    /// This layer is similar to batch normalization layer (`BatchNorm`)
    /// with two differences: first, the normalization is
    /// carried out per example (instance), not over a batch. Second, the
    /// same normalization is applied both at test and train time. This
    /// operation is also known as `contrast normalization`.
    /// 
    /// If the input data is of shape [batch, channel, spacial_dim1, spacial_dim2, ...],
    /// `gamma` and `beta` parameters must be vectors of shape [channel].
    /// 
    /// This implementation is based on paper:
    /// 
    /// .. [1] Instance Normalization: The Missing Ingredient for Fast Stylization,
    ///    D. Ulyanov, A. Vedaldi, V. Lempitsky, 2016 (arXiv:1607.08022v2).
    /// 
    /// Examples::
    /// 
    ///   // Input of shape (2,1,2)
    ///   x = [[[ 1.1,  2.2]],
    ///        [[ 3.3,  4.4]]]
    /// 
    ///   // gamma parameter of length 1
    ///   gamma = [1.5]
    /// 
    ///   // beta parameter of length 1
    ///   beta = [0.5]
    /// 
    ///   // Instance normalization is calculated with the above formula
    ///   InstanceNorm(x,gamma,beta) = [[[-0.997527  ,  1.99752665]],
    ///                                 [[-0.99752653,  1.99752724]]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\instance_norm.cc:L95</summary>
    /// <param name="data">An n-dimensional input array (n &gt; 2) of the form [batch, channel, spatial_dim1, spatial_dim2, ...].</param>
    /// <param name="gamma">A vector of length &#39;channel&#39;, which multiplies the normalized input.</param>
    /// <param name="beta">A vector of length &#39;channel&#39;, which is added to the product of the normalized input and the weight.</param>
    /// <param name="eps">An `epsilon` parameter to prevent division by 0.</param>
    static member InstanceNorm(data : NDArray, gamma : NDArray, beta : NDArray, [<Optional; DefaultParameterValue(0.00100000005)>] eps : float) =
        let creator = AtomicSymbolCreator.FromName "InstanceNorm"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; gamma.NDArrayHandle; beta.NDArrayHandle|]
                                                 [|"eps"|]
                                                 [|eps.ToString()|]
        outputs

    /// <summary>Normalize the input array using the L2 norm.
    /// 
    /// For 1-D NDArray, it computes::
    /// 
    ///   out = data / sqrt(sum(data ** 2) + eps)
    /// 
    /// For N-D NDArray, if the input array has shape (N, N, ..., N),
    /// 
    /// with ``mode`` = ``instance``, it normalizes each instance in the multidimensional
    /// array by its L2 norm.::
    /// 
    ///   for i in 0...N
    ///     out[i,:,:,...,:] = data[i,:,:,...,:] / sqrt(sum(data[i,:,:,...,:] ** 2) + eps)
    /// 
    /// with ``mode`` = ``channel``, it normalizes each channel in the array by its L2 norm.::
    /// 
    ///   for i in 0...N
    ///     out[:,i,:,...,:] = data[:,i,:,...,:] / sqrt(sum(data[:,i,:,...,:] ** 2) + eps)
    /// 
    /// with ``mode`` = ``spatial``, it normalizes the cross channel norm for each position
    /// in the array by its L2 norm.::
    /// 
    ///   for dim in 2...N
    ///     for i in 0...N
    ///       out[.....,i,...] = take(out, indices=i, axis=dim) / sqrt(sum(take(out, indices=i, axis=dim) ** 2) + eps)
    ///           -dim-
    /// 
    /// Example::
    /// 
    ///   x = [[[1,2],
    ///         [3,4]],
    ///        [[2,2],
    ///         [5,6]]]
    /// 
    ///   L2Normalization(x, mode=&#39;instance&#39;)
    ///   =[[[ 0.18257418  0.36514837]
    ///      [ 0.54772252  0.73029673]]
    ///     [[ 0.24077171  0.24077171]
    ///      [ 0.60192931  0.72231513]]]
    /// 
    ///   L2Normalization(x, mode=&#39;channel&#39;)
    ///   =[[[ 0.31622776  0.44721359]
    ///      [ 0.94868326  0.89442718]]
    ///     [[ 0.37139067  0.31622776]
    ///      [ 0.92847669  0.94868326]]]
    /// 
    ///   L2Normalization(x, mode=&#39;spatial&#39;)
    ///   =[[[ 0.44721359  0.89442718]
    ///      [ 0.60000002  0.80000001]]
    ///     [[ 0.70710677  0.70710677]
    ///      [ 0.6401844   0.76822126]]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\l2_normalization.cc:L196</summary>
    /// <param name="data">Input array to normalize.</param>
    /// <param name="eps">A small constant for numerical stability.</param>
    /// <param name="mode">Specify the dimension along which to compute L2 norm.</param>
    static member L2Normalization(data : NDArray, [<Optional; DefaultParameterValue(1.00000001E-10)>] eps : float, [<Optional>] mode : L2NormalizationMode) =
        let creator = AtomicSymbolCreator.FromName "L2Normalization"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"eps"; "mode"|]
                                                 [|eps.ToString(); (if isNull (mode :> obj) then "instance" else mode.ToString())|]
        outputs

    /// <summary>Make your own loss function in network construction.
    /// 
    /// This operator accepts a customized loss function symbol as a terminal loss and
    /// the symbol should be an operator with no backward dependency.
    /// The output of this function is the gradient of loss with respect to the input data.
    /// 
    /// For example, if you are a making a cross entropy loss function. Assume ``out`` is the
    /// predicted output and ``label`` is the true label, then the cross entropy can be defined as::
    /// 
    ///   cross_entropy = label * log(out) + (1 - label) * log(1 - out)
    ///   loss = MakeLoss(cross_entropy)
    /// 
    /// We will need to use ``MakeLoss`` when we are creating our own loss function or we want to
    /// combine multiple loss functions. Also we may want to stop some variables&#39; gradients
    /// from backpropagation. See more detail in ``BlockGrad`` or ``stop_gradient``.
    /// 
    /// In addition, we can give a scale to the loss by setting ``grad_scale``,
    /// so that the gradient of the loss will be rescaled in the backpropagation.
    /// 
    /// .. note:: This operator should be used as a Symbol instead of NDArray.
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\make_loss.cc:L71</summary>
    /// <param name="data">Input array.</param>
    /// <param name="gradScale">Gradient scale as a supplement to unary and binary operators</param>
    /// <param name="validThresh">clip each element in the array to 0 when it is less than ``valid_thresh``. This is used when ``normalization`` is set to ``&#39;valid&#39;``.</param>
    /// <param name="normalization">If this is set to null, the output gradient will not be normalized. If this is set to batch, the output gradient will be divided by the batch size. If this is set to valid, the output gradient will be divided by the number of valid input elements.</param>
    static member MakeLoss(data : NDArray, [<Optional; DefaultParameterValue(1.0)>] gradScale : float, [<Optional; DefaultParameterValue(0.0)>] validThresh : float, [<Optional>] normalization : Normalization) =
        let creator = AtomicSymbolCreator.FromName "MakeLoss"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle|]
                                                 [|"grad_scale"; "valid_thresh"; "normalization"|]
                                                 [|gradScale.ToString(); validThresh.ToString(); (if isNull (normalization :> obj) then "null" else normalization.ToString())|]
        outputs

    /// <summary>Performs region of interest(ROI) pooling on the input array.
    /// 
    /// ROI pooling is a variant of a max pooling layer, in which the output size is fixed and
    /// region of interest is a parameter. Its purpose is to perform max pooling on the inputs
    /// of non-uniform sizes to obtain fixed-size feature maps. ROI pooling is a neural-net
    /// layer mostly used in training a `Fast R-CNN` network for object detection.
    /// 
    /// This operator takes a 4D feature map as an input array and region proposals as `rois`,
    /// then it pools over sub-regions of input and produces a fixed-sized output array
    /// regardless of the ROI size.
    /// 
    /// To crop the feature map accordingly, you can resize the bounding box coordinates
    /// by changing the parameters `rois` and `spatial_scale`.
    /// 
    /// The cropped feature maps are pooled by standard max pooling operation to a fixed size output
    /// indicated by a `pooled_size` parameter. batch_size will change to the number of region
    /// bounding boxes after `ROIPooling`.
    /// 
    /// The size of each region of interest doesn&#39;t have to be perfectly divisible by
    /// the number of pooling sections(`pooled_size`).
    /// 
    /// Example::
    /// 
    ///   x = [[[[  0.,   1.,   2.,   3.,   4.,   5.],
    ///          [  6.,   7.,   8.,   9.,  10.,  11.],
    ///          [ 12.,  13.,  14.,  15.,  16.,  17.],
    ///          [ 18.,  19.,  20.,  21.,  22.,  23.],
    ///          [ 24.,  25.,  26.,  27.,  28.,  29.],
    ///          [ 30.,  31.,  32.,  33.,  34.,  35.],
    ///          [ 36.,  37.,  38.,  39.,  40.,  41.],
    ///          [ 42.,  43.,  44.,  45.,  46.,  47.]]]]
    /// 
    ///   // region of interest i.e. bounding box coordinates.
    ///   y = [[0,0,0,4,4]]
    /// 
    ///   // returns array of shape (2,2) according to the given roi with max pooling.
    ///   ROIPooling(x, y, (2,2), 1.0) = [[[[ 14.,  16.],
    ///                                     [ 26.,  28.]]]]
    /// 
    ///   // region of interest is changed due to the change in `spacial_scale` parameter.
    ///   ROIPooling(x, y, (2,2), 0.7) = [[[[  7.,   9.],
    ///                                     [ 19.,  21.]]]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\roi_pooling.cc:L295</summary>
    /// <param name="data">The input array to the pooling operator,  a 4D Feature maps </param>
    /// <param name="rois">Bounding box coordinates, a 2D array of [[batch_index, x1, y1, x2, y2]], where (x1, y1) and (x2, y2) are top left and bottom right corners of designated region of interest. `batch_index` indicates the index of corresponding image in the input array</param>
    /// <param name="pooledSize">ROI pooling output shape (h,w) </param>
    /// <param name="spatialScale">Ratio of input feature map height (or w) to raw image height (or w). Equals the reciprocal of total stride in convolutional layers</param>
    static member ROIPooling(data : NDArray, rois : NDArray, pooledSize : int seq, spatialScale : float) =
        let creator = AtomicSymbolCreator.FromName "ROIPooling"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; rois.NDArrayHandle|]
                                                 [|"pooled_size"; "spatial_scale"|]
                                                 [|pooledSize.ToString(); spatialScale.ToString()|]
        outputs

    /// <summary>Takes the last element of a sequence.
    /// 
    /// This function takes an n-dimensional input array of the form
    /// [max_sequence_length, batch_size, other_feature_dims] and returns a (n-1)-dimensional array
    /// of the form [batch_size, other_feature_dims].
    /// 
    /// Parameter `sequence_length` is used to handle variable-length sequences. `sequence_length` should be
    /// an input array of positive ints of dimension [batch_size]. To use this parameter,
    /// set `use_sequence_length` to `True`, otherwise each example in the batch is assumed
    /// to have the max sequence length.
    /// 
    /// .. note:: Alternatively, you can also use `take` operator.
    /// 
    /// Example::
    /// 
    ///    x = [[[  1.,   2.,   3.],
    ///          [  4.,   5.,   6.],
    ///          [  7.,   8.,   9.]],
    /// 
    ///         [[ 10.,   11.,   12.],
    ///          [ 13.,   14.,   15.],
    ///          [ 16.,   17.,   18.]],
    /// 
    ///         [[  19.,   20.,   21.],
    ///          [  22.,   23.,   24.],
    ///          [  25.,   26.,   27.]]]
    /// 
    ///    // returns last sequence when sequence_length parameter is not used
    ///    SequenceLast(x) = [[  19.,   20.,   21.],
    ///                       [  22.,   23.,   24.],
    ///                       [  25.,   26.,   27.]]
    /// 
    ///    // sequence_length is used
    ///    SequenceLast(x, sequence_length=[1,1,1], use_sequence_length=True) =
    ///             [[  1.,   2.,   3.],
    ///              [  4.,   5.,   6.],
    ///              [  7.,   8.,   9.]]
    /// 
    ///    // sequence_length is used
    ///    SequenceLast(x, sequence_length=[1,2,3], use_sequence_length=True) =
    ///             [[  1.,    2.,   3.],
    ///              [  13.,  14.,  15.],
    ///              [  25.,  26.,  27.]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\sequence_last.cc:L100</summary>
    /// <param name="data">n-dimensional input array of the form [max_sequence_length, batch_size, other_feature_dims] where n&gt;2</param>
    /// <param name="sequenceLength">vector of sequence lengths of the form [batch_size]</param>
    /// <param name="useSequenceLength">If set to true, this layer takes in an extra input parameter `sequence_length` to specify variable length sequence</param>
    /// <param name="axis">The sequence axis. Only values of 0 and 1 are currently supported.</param>
    static member SequenceLast(data : NDArray, sequenceLength : NDArray, [<Optional; DefaultParameterValue(false)>] useSequenceLength : bool, [<Optional; DefaultParameterValue(0)>] axis : int) =
        let creator = AtomicSymbolCreator.FromName "SequenceLast"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; sequenceLength.NDArrayHandle|]
                                                 [|"use_sequence_length"; "axis"|]
                                                 [|useSequenceLength.ToString(); axis.ToString()|]
        outputs

    /// <summary>Sets all elements outside the sequence to a constant value.
    /// 
    /// This function takes an n-dimensional input array of the form
    /// [max_sequence_length, batch_size, other_feature_dims] and returns an array of the same shape.
    /// 
    /// Parameter `sequence_length` is used to handle variable-length sequences. `sequence_length`
    /// should be an input array of positive ints of dimension [batch_size].
    /// To use this parameter, set `use_sequence_length` to `True`,
    /// otherwise each example in the batch is assumed to have the max sequence length and
    /// this operator works as the `identity` operator.
    /// 
    /// Example::
    /// 
    ///    x = [[[  1.,   2.,   3.],
    ///          [  4.,   5.,   6.]],
    /// 
    ///         [[  7.,   8.,   9.],
    ///          [ 10.,  11.,  12.]],
    /// 
    ///         [[ 13.,  14.,   15.],
    ///          [ 16.,  17.,   18.]]]
    /// 
    ///    // Batch 1
    ///    B1 = [[  1.,   2.,   3.],
    ///          [  7.,   8.,   9.],
    ///          [ 13.,  14.,  15.]]
    /// 
    ///    // Batch 2
    ///    B2 = [[  4.,   5.,   6.],
    ///          [ 10.,  11.,  12.],
    ///          [ 16.,  17.,  18.]]
    /// 
    ///    // works as identity operator when sequence_length parameter is not used
    ///    SequenceMask(x) = [[[  1.,   2.,   3.],
    ///                        [  4.,   5.,   6.]],
    /// 
    ///                       [[  7.,   8.,   9.],
    ///                        [ 10.,  11.,  12.]],
    /// 
    ///                       [[ 13.,  14.,   15.],
    ///                        [ 16.,  17.,   18.]]]
    /// 
    ///    // sequence_length [1,1] means 1 of each batch will be kept
    ///    // and other rows are masked with default mask value = 0
    ///    SequenceMask(x, sequence_length=[1,1], use_sequence_length=True) =
    ///                 [[[  1.,   2.,   3.],
    ///                   [  4.,   5.,   6.]],
    /// 
    ///                  [[  0.,   0.,   0.],
    ///                   [  0.,   0.,   0.]],
    /// 
    ///                  [[  0.,   0.,   0.],
    ///                   [  0.,   0.,   0.]]]
    /// 
    ///    // sequence_length [2,3] means 2 of batch B1 and 3 of batch B2 will be kept
    ///    // and other rows are masked with value = 1
    ///    SequenceMask(x, sequence_length=[2,3], use_sequence_length=True, value=1) =
    ///                 [[[  1.,   2.,   3.],
    ///                   [  4.,   5.,   6.]],
    /// 
    ///                  [[  7.,   8.,   9.],
    ///                   [  10.,  11.,  12.]],
    /// 
    ///                  [[   1.,   1.,   1.],
    ///                   [  16.,  17.,  18.]]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\sequence_mask.cc:L186</summary>
    /// <param name="data">n-dimensional input array of the form [max_sequence_length, batch_size, other_feature_dims] where n&gt;2</param>
    /// <param name="sequenceLength">vector of sequence lengths of the form [batch_size]</param>
    /// <param name="useSequenceLength">If set to true, this layer takes in an extra input parameter `sequence_length` to specify variable length sequence</param>
    /// <param name="value">The value to be used as a mask.</param>
    /// <param name="axis">The sequence axis. Only values of 0 and 1 are currently supported.</param>
    static member SequenceMask(data : NDArray, 
                               sequenceLength : NDArray, 
                               [<Optional; DefaultParameterValue(false)>] useSequenceLength : bool, 
                               [<Optional; DefaultParameterValue(0.0)>] value : float, 
                               [<Optional; DefaultParameterValue(0)>] axis : int) =
        let creator = AtomicSymbolCreator.FromName "SequenceMask"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; sequenceLength.NDArrayHandle|]
                                                 [|"use_sequence_length"; "value"; "axis"|]
                                                 [|useSequenceLength.ToString(); value.ToString(); axis.ToString()|]
        outputs

    /// <summary>Reverses the elements of each sequence.
    /// 
    /// This function takes an n-dimensional input array of the form [max_sequence_length, batch_size, other_feature_dims]
    /// and returns an array of the same shape.
    /// 
    /// Parameter `sequence_length` is used to handle variable-length sequences.
    /// `sequence_length` should be an input array of positive ints of dimension [batch_size].
    /// To use this parameter, set `use_sequence_length` to `True`,
    /// otherwise each example in the batch is assumed to have the max sequence length.
    /// 
    /// Example::
    /// 
    ///    x = [[[  1.,   2.,   3.],
    ///          [  4.,   5.,   6.]],
    /// 
    ///         [[  7.,   8.,   9.],
    ///          [ 10.,  11.,  12.]],
    /// 
    ///         [[ 13.,  14.,   15.],
    ///          [ 16.,  17.,   18.]]]
    /// 
    ///    // Batch 1
    ///    B1 = [[  1.,   2.,   3.],
    ///          [  7.,   8.,   9.],
    ///          [ 13.,  14.,  15.]]
    /// 
    ///    // Batch 2
    ///    B2 = [[  4.,   5.,   6.],
    ///          [ 10.,  11.,  12.],
    ///          [ 16.,  17.,  18.]]
    /// 
    ///    // returns reverse sequence when sequence_length parameter is not used
    ///    SequenceReverse(x) = [[[ 13.,  14.,   15.],
    ///                           [ 16.,  17.,   18.]],
    /// 
    ///                          [[  7.,   8.,   9.],
    ///                           [ 10.,  11.,  12.]],
    /// 
    ///                          [[  1.,   2.,   3.],
    ///                           [  4.,   5.,   6.]]]
    /// 
    ///    // sequence_length [2,2] means 2 rows of
    ///    // both batch B1 and B2 will be reversed.
    ///    SequenceReverse(x, sequence_length=[2,2], use_sequence_length=True) =
    ///                      [[[  7.,   8.,   9.],
    ///                        [ 10.,  11.,  12.]],
    /// 
    ///                       [[  1.,   2.,   3.],
    ///                        [  4.,   5.,   6.]],
    /// 
    ///                       [[ 13.,  14.,   15.],
    ///                        [ 16.,  17.,   18.]]]
    /// 
    ///    // sequence_length [2,3] means 2 of batch B2 and 3 of batch B3
    ///    // will be reversed.
    ///    SequenceReverse(x, sequence_length=[2,3], use_sequence_length=True) =
    ///                     [[[  7.,   8.,   9.],
    ///                       [ 16.,  17.,  18.]],
    /// 
    ///                      [[  1.,   2.,   3.],
    ///                       [ 10.,  11.,  12.]],
    /// 
    ///                      [[ 13.,  14,   15.],
    ///                       [  4.,   5.,   6.]]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\sequence_reverse.cc:L122</summary>
    /// <param name="data">n-dimensional input array of the form [max_sequence_length, batch_size, other dims] where n&gt;2 </param>
    /// <param name="sequenceLength">vector of sequence lengths of the form [batch_size]</param>
    /// <param name="useSequenceLength">If set to true, this layer takes in an extra input parameter `sequence_length` to specify variable length sequence</param>
    /// <param name="axis">The sequence axis. Only 0 is currently supported.</param>
    static member SequenceReverse(data : NDArray, sequenceLength : NDArray, [<Optional; DefaultParameterValue(false)>] useSequenceLength : bool, [<Optional; DefaultParameterValue(0)>] axis : int) =
        let creator = AtomicSymbolCreator.FromName "SequenceReverse"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; sequenceLength.NDArrayHandle|]
                                                 [|"use_sequence_length"; "axis"|]
                                                 [|useSequenceLength.ToString(); axis.ToString()|]
        outputs

    /// <summary>Applies a spatial transformer to input feature map.</summary>
    /// <param name="data">Input data to the SpatialTransformerOp.</param>
    /// <param name="loc">localisation net, the output dim should be 6 when transform_type is affine. You shold initialize the weight and bias with identity tranform.</param>
    /// <param name="targetShape">output shape(h, w) of spatial transformer: (y, x)</param>
    /// <param name="transformType">transformation type</param>
    /// <param name="samplerType">sampling type</param>
    /// <param name="cudnnOff">whether to turn cudnn off</param>
    static member SpatialTransformer(data : NDArray, loc : NDArray, [<Optional>] targetShape : int seq, [<Optional>] cudnnOff : bool Nullable) =
        let creator = AtomicSymbolCreator.FromName "SpatialTransformer"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; loc.NDArrayHandle|]
                                                 [|"target_shape"; "transform_type"; "sampler_type"; "cudnn_off"|]
                                                 [|(if isNull (targetShape :> obj) then "[0,0]" else targetShape.ToString()); "affine"; "bilinear"; cudnnOff.ToString()|]
        outputs

    /// <summary>Applies a spatial transformer to input feature map.</summary>
    /// <param name="data">Input data to the SpatialTransformerOp.</param>
    /// <param name="loc">localisation net, the output dim should be 6 when transform_type is affine. You shold initialize the weight and bias with identity tranform.</param>
    /// <param name="targetShape">output shape(h, w) of spatial transformer: (y, x)</param>
    /// <param name="transformType">transformation type</param>
    /// <param name="samplerType">sampling type</param>
    /// <param name="cudnnOff">whether to turn cudnn off</param>
    static member SpatialTransformer(data : NDArray, loc : NDArray, ?targetShape : int seq, ?cudnnOff : bool) =
        let creator = AtomicSymbolCreator.FromName "SpatialTransformer"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; loc.NDArrayHandle|]
                                                 [|"target_shape"; "transform_type"; "sampler_type"; "cudnn_off"|]
                                                 [|(match targetShape with None -> "[0,0]" | _ -> targetShape.ToString()); "affine"; "bilinear"; (match cudnnOff with None -> "None" | _ -> cudnnOff.ToString())|]
        outputs

    /// <summary>Computes support vector machine based transformation of the input.
    /// 
    /// This tutorial demonstrates using SVM as output layer for classification instead of softmax:
    /// https://github.com/dmlc/mxnet/tree/master/example/svm_mnist.
    /// 
    /// </summary>
    /// <param name="data">Input data for SVM transformation.</param>
    /// <param name="label">Class label for the input data.</param>
    /// <param name="margin">The loss function penalizes outputs that lie outside this margin. Default margin is 1.</param>
    /// <param name="regularizationCoefficient">Regularization parameter for the SVM. This balances the tradeoff between coefficient size and error.</param>
    /// <param name="useLinear">Whether to use L1-SVM objective. L2-SVM objective is used by default.</param>
    static member SVMOutput(data : NDArray, 
                            label : NDArray, 
                            [<Optional; DefaultParameterValue(1.0)>] margin : float, 
                            [<Optional; DefaultParameterValue(1.0)>] regularizationCoefficient : float, 
                            [<Optional; DefaultParameterValue(false)>] useLinear : bool) =
        let creator = AtomicSymbolCreator.FromName "SVMOutput"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|data.NDArrayHandle; label.NDArrayHandle|]
                                                 [|"margin"; "regularization_coefficient"; "use_linear"|]
                                                 [|margin.ToString(); regularizationCoefficient.ToString(); useLinear.ToString()|]
        outputs

    /// <param name="lhs">Left operand to the function.</param>
    /// <param name="rhs">Right operand to the function.</param>
    static member OnehotEncode(lhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "_onehot_encode"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Fill one element of each line(row for python, column for R/Julia) in lhs according to index indicated by rhs and values indicated by mhs. This function assume rhs uses 0-based index.</summary>
    /// <param name="lhs">Left operand to the function.</param>
    /// <param name="mhs">Middle operand to the function.</param>
    /// <param name="rhs">Right operand to the function.</param>
    static member FillElement0index(lhs : NDArray, mhs : NDArray, rhs : NDArray) =
        let creator = AtomicSymbolCreator.FromName "fill_element_0index"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|lhs.NDArrayHandle; mhs.NDArrayHandle; rhs.NDArrayHandle|]
                                                 Array.empty
                                                 Array.empty
        outputs

    /// <summary>Decode an image, clip to (x0, y0, x1, y1), subtract mean, and write to buffer</summary>
    /// <param name="mean">image mean</param>
    /// <param name="index">buffer position for output</param>
    /// <param name="x0">x0</param>
    /// <param name="y0">y0</param>
    /// <param name="x1">x1</param>
    /// <param name="y1">y1</param>
    /// <param name="c">channel</param>
    /// <param name="size">length of str_img</param>
    static member Imdecode(mean : NDArray, 
                           index : int, 
                           x0 : int, 
                           y0 : int, 
                           x1 : int, 
                           y1 : int, 
                           c : int, 
                           size : int) =
        let creator = AtomicSymbolCreator.FromName "_imdecode"
        let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
                                                 [|mean.NDArrayHandle|]
                                                 [|"index"; "x0"; "y0"; "x1"; "y1"; "c"; "size"|]
                                                 [|index.ToString(); x0.ToString(); y0.ToString(); x1.ToString(); y1.ToString(); c.ToString(); size.ToString()|]
        outputs

// ********************************************************************************************************
// Exception Exception
// Unexpected SymbolOrNDArray [|Symbol; ManySymbolOrNDArray|]
// { Name = "_foreach"
//   Description =
//                "Run a for loop over an NDArray with user-defined computation
// 
// From:C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\control_flow.cc:1090"
//   Arguments =
//              [|{ Name = "fn"
//                  Description = "Input graph."
//                  TypeInfo = "Symbol" };
//                { Name = "data"
//                  Description =
//                               "The input arrays that include data arrays and states."
//                  TypeInfo = "NDArray-or-Symbol[]" };
//                { Name = "num_args"
//                  Description = "Number of inputs."
//                  TypeInfo = "int, required" };
//                { Name = "num_outputs"
//                  Description = "The number of outputs of the subgraph."
//                  TypeInfo = "int, required" };
//                { Name = "num_out_data"
//                  Description = "The number of output data of the subgraph."
//                  TypeInfo = "int, required" };
//                { Name = "in_state_locs"
//                  Description = "The locations of loop states among the inputs."
//                  TypeInfo = ", required" };
//                { Name = "in_data_locs"
//                  Description = "The locations of input data among the inputs."
//                  TypeInfo = ", required" };
//                { Name = "remain_locs"
//                  Description =
//                               "The locations of remaining data among the inputs."
//                  TypeInfo = ", required" }|]
//   ReturnTypeInfo = ""
//   KeyVarNumArgs = 8315740999501313390n }
// ********************************************************************************************************



// ********************************************************************************************************
// Exception Exception
// Unexpected SymbolOrNDArray [|Symbol; ManySymbolOrNDArray|]
// { Name = "_while_loop"
//   Description =
//                "Run a while loop over with user-defined condition and computation
// 
// From:C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\control_flow.cc:1151"
//   Arguments =
//              [|{ Name = "cond"
//                  Description = "Input graph for the loop condition."
//                  TypeInfo = "Symbol" };
//                { Name = "func"
//                  Description = "Input graph for the loop body."
//                  TypeInfo = "Symbol" };
//                { Name = "data"
//                  Description =
//                               "The input arrays that include data arrays and states."
//                  TypeInfo = "NDArray-or-Symbol[]" };
//                { Name = "num_args"
//                  Description =
//                               "Number of input arguments, including cond and func as two symbol inputs."
//                  TypeInfo = "int, required" };
//                { Name = "num_outputs"
//                  Description = "The number of outputs of the subgraph."
//                  TypeInfo = "int, required" };
//                { Name = "num_out_data"
//                  Description = "The number of outputs from the function body."
//                  TypeInfo = "int, required" };
//                { Name = "max_iterations"
//                  Description = "Maximum number of iterations."
//                  TypeInfo = "int, required" };
//                { Name = "cond_input_locs"
//                  Description =
//                               "The locations of cond's inputs in the given inputs."
//                  TypeInfo = ", required" };
//                { Name = "func_input_locs"
//                  Description =
//                               "The locations of func's inputs in the given inputs."
//                  TypeInfo = ", required" };
//                { Name = "func_var_locs"
//                  Description = "The locations of loop_vars among func's inputs."
//                  TypeInfo = ", required" }|]
//   ReturnTypeInfo = ""
//   KeyVarNumArgs = 8315740999501313390n }
// ********************************************************************************************************



// ********************************************************************************************************
// Exception Exception
// Unexpected SymbolOrNDArray [|Symbol; ManySymbolOrNDArray|]
// { Name = "_cond"
//   Description =
//                "Run a if-then-else using user-defined condition and computation
// 
// From:C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\control_flow.cc:1212"
//   Arguments =
//              [|{ Name = "cond"
//                  Description = "Input graph for the condition."
//                  TypeInfo = "Symbol" };
//                { Name = "then_branch"
//                  Description = "Input graph for the then branch."
//                  TypeInfo = "Symbol" };
//                { Name = "else_branch"
//                  Description = "Input graph for the else branch."
//                  TypeInfo = "Symbol" };
//                { Name = "data"
//                  Description =
//                               "The input arrays that include data arrays and states."
//                  TypeInfo = "NDArray-or-Symbol[]" };
//                { Name = "num_args"
//                  Description =
//                               "Number of input arguments, including cond, then and else as three symbol inputs."
//                  TypeInfo = "int, required" };
//                { Name = "num_outputs"
//                  Description = "The number of outputs of the subgraph."
//                  TypeInfo = "int, required" };
//                { Name = "cond_input_locs"
//                  Description =
//                               "The locations of cond's inputs in the given inputs."
//                  TypeInfo = ", required" };
//                { Name = "then_input_locs"
//                  Description =
//                               "The locations of then's inputs in the given inputs."
//                  TypeInfo = ", required" };
//                { Name = "else_input_locs"
//                  Description =
//                               "The locations of else's inputs in the given inputs."
//                  TypeInfo = ", required" }|]
//   ReturnTypeInfo = ""
//   KeyVarNumArgs = 8315740999501313390n }
// ********************************************************************************************************


//     /// <summary>Adjust the lighting level of the input. Follow the AlexNet style.
//     /// 
//     /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\image\image_random.cc:L242</summary>
//     /// <param name="data">The input.</param>
//     /// <param name="alpha">The lighting alphas for the R, G, B channels.</param>
//     static member ImageAdjustLighting(data : NDArray, alpha : ) =
//         let creator = AtomicSymbolCreator.FromName "_image_adjust_lighting"
//         let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
//                                                  [|data.NDArrayHandle|]
//                                                  [|"alpha"|]
//                                                  [|alpha.ToString()|]
//         outputs

//     /// <summary>Update function for Stochastic Gradient Descent (SDG) optimizer.
//     /// 
//     /// It updates the weights using::
//     /// 
//     ///  weight = weight - learning_rate * (gradient + wd * weight)
//     /// 
//     /// 
//     /// 
//     /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L327</summary>
//     /// <param name="data">Weights</param>
//     /// <param name="lrs">Learning rates.</param>
//     /// <param name="wds">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
//     /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
//     /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
//     /// <param name="numWeights">Number of updated weights.</param>
//     static member MultiSgdUpdate([<ParamArray>] data : NDArray[], 
//                                  lrs : , 
//                                  wds : , 
//                                  [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
//                                  [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float, 
//                                  [<Optional; DefaultParameterValue(1)>] numWeights : int) =
//         let creator = AtomicSymbolCreator.FromName "multi_sgd_update"
//         let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
//                                                  Array.empty
//                                                  [|"lrs"; "wds"; "rescale_grad"; "clip_gradient"; "num_weights"|]
//                                                  [|lrs.ToString(); wds.ToString(); rescaleGrad.ToString(); clipGradient.ToString(); numWeights.ToString()|]
//         outputs

//     /// <summary>Momentum update function for Stochastic Gradient Descent (SGD) optimizer.
//     /// 
//     /// Momentum update has better convergence rates on neural networks. Mathematically it looks
//     /// like below:
//     /// 
//     /// .. math::
//     /// 
//     ///   v_1 = \alpha * \nabla J(W_0)\\
//     ///   v_t = \gamma v_{t-1} - \alpha * \nabla J(W_{t-1})\\
//     ///   W_t = W_{t-1} + v_t
//     /// 
//     /// It updates the weights using::
//     /// 
//     ///   v = momentum * v - learning_rate * gradient
//     ///   weight += v
//     /// 
//     /// Where the parameter ``momentum`` is the decay rate of momentum estimates at each epoch.
//     /// 
//     /// 
//     /// 
//     /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L372</summary>
//     /// <param name="data">Weights, gradients and momentum</param>
//     /// <param name="lrs">Learning rates.</param>
//     /// <param name="wds">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
//     /// <param name="momentum">The decay rate of momentum estimates at each epoch.</param>
//     /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
//     /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
//     /// <param name="numWeights">Number of updated weights.</param>
//     static member MultiSgdMomUpdate([<ParamArray>] data : NDArray[], 
//                                     lrs : , 
//                                     wds : , 
//                                     [<Optional; DefaultParameterValue(0.0)>] momentum : float, 
//                                     [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
//                                     [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float, 
//                                     [<Optional; DefaultParameterValue(1)>] numWeights : int) =
//         let creator = AtomicSymbolCreator.FromName "multi_sgd_mom_update"
//         let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
//                                                  Array.empty
//                                                  [|"lrs"; "wds"; "momentum"; "rescale_grad"; "clip_gradient"; "num_weights"|]
//                                                  [|lrs.ToString(); wds.ToString(); momentum.ToString(); rescaleGrad.ToString(); clipGradient.ToString(); numWeights.ToString()|]
//         outputs

//     /// <summary>Update function for multi-precision Stochastic Gradient Descent (SDG) optimizer.
//     /// 
//     /// It updates the weights using::
//     /// 
//     ///  weight = weight - learning_rate * (gradient + wd * weight)
//     /// 
//     /// 
//     /// 
//     /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L415</summary>
//     /// <param name="data">Weights</param>
//     /// <param name="lrs">Learning rates.</param>
//     /// <param name="wds">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
//     /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
//     /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
//     /// <param name="numWeights">Number of updated weights.</param>
//     static member MultiMpSgdUpdate([<ParamArray>] data : NDArray[], 
//                                    lrs : , 
//                                    wds : , 
//                                    [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
//                                    [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float, 
//                                    [<Optional; DefaultParameterValue(1)>] numWeights : int) =
//         let creator = AtomicSymbolCreator.FromName "multi_mp_sgd_update"
//         let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
//                                                  Array.empty
//                                                  [|"lrs"; "wds"; "rescale_grad"; "clip_gradient"; "num_weights"|]
//                                                  [|lrs.ToString(); wds.ToString(); rescaleGrad.ToString(); clipGradient.ToString(); numWeights.ToString()|]
//         outputs

//     /// <summary>Momentum update function for multi-precision Stochastic Gradient Descent (SGD) optimizer.
//     /// 
//     /// Momentum update has better convergence rates on neural networks. Mathematically it looks
//     /// like below:
//     /// 
//     /// .. math::
//     /// 
//     ///   v_1 = \alpha * \nabla J(W_0)\\
//     ///   v_t = \gamma v_{t-1} - \alpha * \nabla J(W_{t-1})\\
//     ///   W_t = W_{t-1} + v_t
//     /// 
//     /// It updates the weights using::
//     /// 
//     ///   v = momentum * v - learning_rate * gradient
//     ///   weight += v
//     /// 
//     /// Where the parameter ``momentum`` is the decay rate of momentum estimates at each epoch.
//     /// 
//     /// 
//     /// 
//     /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\optimizer_op.cc:L470</summary>
//     /// <param name="data">Weights</param>
//     /// <param name="lrs">Learning rates.</param>
//     /// <param name="wds">Weight decay augments the objective function with a regularization term that penalizes large weights. The penalty scales with the square of the magnitude of each weight.</param>
//     /// <param name="momentum">The decay rate of momentum estimates at each epoch.</param>
//     /// <param name="rescaleGrad">Rescale gradient to grad = rescale_grad*grad.</param>
//     /// <param name="clipGradient">Clip gradient to the range of [-clip_gradient, clip_gradient] If clip_gradient &lt;= 0, gradient clipping is turned off. grad = max(min(grad, clip_gradient), -clip_gradient).</param>
//     /// <param name="numWeights">Number of updated weights.</param>
//     static member MultiMpSgdMomUpdate([<ParamArray>] data : NDArray[], 
//                                       lrs : , 
//                                       wds : , 
//                                       [<Optional; DefaultParameterValue(0.0)>] momentum : float, 
//                                       [<Optional; DefaultParameterValue(1.0)>] rescaleGrad : float, 
//                                       [<Optional; DefaultParameterValue(-1.0)>] clipGradient : float, 
//                                       [<Optional; DefaultParameterValue(1)>] numWeights : int) =
//         let creator = AtomicSymbolCreator.FromName "multi_mp_sgd_mom_update"
//         let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
//                                                  Array.empty
//                                                  [|"lrs"; "wds"; "momentum"; "rescale_grad"; "clip_gradient"; "num_weights"|]
//                                                  [|lrs.ToString(); wds.ToString(); momentum.ToString(); rescaleGrad.ToString(); clipGradient.ToString(); numWeights.ToString()|]
//         outputs

//     /// <summary>Generate region proposals via RPN</summary>
//     /// <param name="clsProb">Score of how likely proposal is object.</param>
//     /// <param name="bboxPred">BBox Predicted deltas from anchors for proposals</param>
//     /// <param name="imInfo">Image size and scale.</param>
//     /// <param name="rpnPreNmsTopN">Number of top scoring boxes to keep before applying NMS to RPN proposals</param>
//     /// <param name="rpnPostNmsTopN">Number of top scoring boxes to keep after applying NMS to RPN proposals</param>
//     /// <param name="threshold">NMS value, below which to suppress.</param>
//     /// <param name="rpnMinSize">Minimum height or width in proposal</param>
//     /// <param name="scales">Used to generate anchor windows by enumerating scales</param>
//     /// <param name="ratios">Used to generate anchor windows by enumerating ratios</param>
//     /// <param name="featureStride">The size of the receptive field each unit in the convolution layer of the rpn,for example the product of all stride&#39;s prior to this layer.</param>
//     /// <param name="outputScore">Add score to outputs</param>
//     /// <param name="iouLoss">Usage of IoU Loss</param>
//     static member ContribMultiProposal(clsProb : NDArray, 
//                                        bboxPred : NDArray, 
//                                        imInfo : NDArray, 
//                                        [<Optional; DefaultParameterValue(6000)>] rpnPreNmsTopN : int, 
//                                        [<Optional; DefaultParameterValue(300)>] rpnPostNmsTopN : int, 
//                                        [<Optional; DefaultParameterValue(0.699999988)>] threshold : float, 
//                                        [<Optional; DefaultParameterValue(16)>] rpnMinSize : int, 
//                                        scales : , 
//                                        ratios : , 
//                                        [<Optional; DefaultParameterValue(16)>] featureStride : int, 
//                                        [<Optional; DefaultParameterValue(false)>] outputScore : bool, 
//                                        [<Optional; DefaultParameterValue(false)>] iouLoss : bool) =
//         let creator = AtomicSymbolCreator.FromName "_contrib_MultiProposal"
//         let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
//                                                  [|clsProb.NDArrayHandle; bboxPred.NDArrayHandle; imInfo.NDArrayHandle|]
//                                                  [|"rpn_pre_nms_top_n"; "rpn_post_nms_top_n"; "threshold"; "rpn_min_size"; "scales"; "ratios"; "feature_stride"; "output_score"; "iou_loss"|]
//                                                  [|rpnPreNmsTopN.ToString(); rpnPostNmsTopN.ToString(); threshold.ToString(); rpnMinSize.ToString(); scales.ToString(); ratios.ToString(); featureStride.ToString(); outputScore.ToString(); iouLoss.ToString()|]
//         outputs

//     /// <summary>Convert multibox detection predictions.</summary>
//     /// <param name="clsProb">Class probabilities.</param>
//     /// <param name="locPred">Location regression predictions.</param>
//     /// <param name="anchor">Multibox prior anchor boxes</param>
//     /// <param name="clip">Clip out-of-boundary boxes.</param>
//     /// <param name="threshold">Threshold to be a positive prediction.</param>
//     /// <param name="backgroundId">Background id.</param>
//     /// <param name="nmsThreshold">Non-maximum suppression threshold.</param>
//     /// <param name="forceSuppress">Suppress all detections regardless of class_id.</param>
//     /// <param name="variances">Variances to be decoded from box regression output.</param>
//     /// <param name="nmsTopk">Keep maximum top k detections before nms, -1 for no limit.</param>
//     static member ContribMultiBoxDetection(clsProb : NDArray, 
//                                            locPred : NDArray, 
//                                            anchor : NDArray, 
//                                            [<Optional; DefaultParameterValue(true)>] clip : bool, 
//                                            [<Optional; DefaultParameterValue(0.00999999978)>] threshold : float, 
//                                            [<Optional; DefaultParameterValue(0)>] backgroundId : int, 
//                                            [<Optional; DefaultParameterValue(0.5)>] nmsThreshold : float, 
//                                            [<Optional; DefaultParameterValue(false)>] forceSuppress : bool, 
//                                            variances : , 
//                                            [<Optional; DefaultParameterValue(-1)>] nmsTopk : int) =
//         let creator = AtomicSymbolCreator.FromName "_contrib_MultiBoxDetection"
//         let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
//                                                  [|clsProb.NDArrayHandle; locPred.NDArrayHandle; anchor.NDArrayHandle|]
//                                                  [|"clip"; "threshold"; "background_id"; "nms_threshold"; "force_suppress"; "variances"; "nms_topk"|]
//                                                  [|clip.ToString(); threshold.ToString(); backgroundId.ToString(); nmsThreshold.ToString(); forceSuppress.ToString(); variances.ToString(); nmsTopk.ToString()|]
//         outputs

//     /// <summary>Generate prior(anchor) boxes from data, sizes and ratios.</summary>
//     /// <param name="data">Input data.</param>
//     /// <param name="sizes">List of sizes of generated MultiBoxPriores.</param>
//     /// <param name="ratios">List of aspect ratios of generated MultiBoxPriores.</param>
//     /// <param name="clip">Whether to clip out-of-boundary boxes.</param>
//     /// <param name="steps">Priorbox step across y and x, -1 for auto calculation.</param>
//     /// <param name="offsets">Priorbox center offsets, y and x respectively</param>
//     static member ContribMultiBoxPrior(data : NDArray, 
//                                        sizes : , 
//                                        ratios : , 
//                                        [<Optional; DefaultParameterValue(false)>] clip : bool, 
//                                        steps : , 
//                                        offsets : ) =
//         let creator = AtomicSymbolCreator.FromName "_contrib_MultiBoxPrior"
//         let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
//                                                  [|data.NDArrayHandle|]
//                                                  [|"sizes"; "ratios"; "clip"; "steps"; "offsets"|]
//                                                  [|sizes.ToString(); ratios.ToString(); clip.ToString(); steps.ToString(); offsets.ToString()|]
//         outputs

//     /// <summary>Compute Multibox training targets</summary>
//     /// <param name="anchor">Generated anchor boxes.</param>
//     /// <param name="label">Object detection labels.</param>
//     /// <param name="clsPred">Class predictions.</param>
//     /// <param name="overlapThreshold">Anchor-GT overlap threshold to be regarded as a positive match.</param>
//     /// <param name="ignoreLabel">Label for ignored anchors.</param>
//     /// <param name="negativeMiningRatio">Max negative to positive samples ratio, use -1 to disable mining</param>
//     /// <param name="negativeMiningThresh">Threshold used for negative mining.</param>
//     /// <param name="minimumNegativeSamples">Minimum number of negative samples.</param>
//     /// <param name="variances">Variances to be encoded in box regression target.</param>
//     static member ContribMultiBoxTarget(anchor : NDArray, 
//                                         label : NDArray, 
//                                         clsPred : NDArray, 
//                                         [<Optional; DefaultParameterValue(0.5)>] overlapThreshold : float, 
//                                         [<Optional; DefaultParameterValue(-1.0)>] ignoreLabel : float, 
//                                         [<Optional; DefaultParameterValue(-1.0)>] negativeMiningRatio : float, 
//                                         [<Optional; DefaultParameterValue(0.5)>] negativeMiningThresh : float, 
//                                         [<Optional; DefaultParameterValue(0)>] minimumNegativeSamples : int, 
//                                         variances : ) =
//         let creator = AtomicSymbolCreator.FromName "_contrib_MultiBoxTarget"
//         let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
//                                                  [|anchor.NDArrayHandle; label.NDArrayHandle; clsPred.NDArrayHandle|]
//                                                  [|"overlap_threshold"; "ignore_label"; "negative_mining_ratio"; "negative_mining_thresh"; "minimum_negative_samples"; "variances"|]
//                                                  [|overlapThreshold.ToString(); ignoreLabel.ToString(); negativeMiningRatio.ToString(); negativeMiningThresh.ToString(); minimumNegativeSamples.ToString(); variances.ToString()|]
//         outputs

//     /// <summary>Generate region proposals via RPN</summary>
//     /// <param name="clsProb">Score of how likely proposal is object.</param>
//     /// <param name="bboxPred">BBox Predicted deltas from anchors for proposals</param>
//     /// <param name="imInfo">Image size and scale.</param>
//     /// <param name="rpnPreNmsTopN">Number of top scoring boxes to keep before applying NMS to RPN proposals</param>
//     /// <param name="rpnPostNmsTopN">Number of top scoring boxes to keep after applying NMS to RPN proposals</param>
//     /// <param name="threshold">NMS value, below which to suppress.</param>
//     /// <param name="rpnMinSize">Minimum height or width in proposal</param>
//     /// <param name="scales">Used to generate anchor windows by enumerating scales</param>
//     /// <param name="ratios">Used to generate anchor windows by enumerating ratios</param>
//     /// <param name="featureStride">The size of the receptive field each unit in the convolution layer of the rpn,for example the product of all stride&#39;s prior to this layer.</param>
//     /// <param name="outputScore">Add score to outputs</param>
//     /// <param name="iouLoss">Usage of IoU Loss</param>
//     static member ContribProposal(clsProb : NDArray, 
//                                   bboxPred : NDArray, 
//                                   imInfo : NDArray, 
//                                   [<Optional; DefaultParameterValue(6000)>] rpnPreNmsTopN : int, 
//                                   [<Optional; DefaultParameterValue(300)>] rpnPostNmsTopN : int, 
//                                   [<Optional; DefaultParameterValue(0.699999988)>] threshold : float, 
//                                   [<Optional; DefaultParameterValue(16)>] rpnMinSize : int, 
//                                   scales : , 
//                                   ratios : , 
//                                   [<Optional; DefaultParameterValue(16)>] featureStride : int, 
//                                   [<Optional; DefaultParameterValue(false)>] outputScore : bool, 
//                                   [<Optional; DefaultParameterValue(false)>] iouLoss : bool) =
//         let creator = AtomicSymbolCreator.FromName "_contrib_Proposal"
//         let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
//                                                  [|clsProb.NDArrayHandle; bboxPred.NDArrayHandle; imInfo.NDArrayHandle|]
//                                                  [|"rpn_pre_nms_top_n"; "rpn_post_nms_top_n"; "threshold"; "rpn_min_size"; "scales"; "ratios"; "feature_stride"; "output_score"; "iou_loss"|]
//                                                  [|rpnPreNmsTopN.ToString(); rpnPostNmsTopN.ToString(); threshold.ToString(); rpnMinSize.ToString(); scales.ToString(); ratios.ToString(); featureStride.ToString(); outputScore.ToString(); iouLoss.ToString()|]
//         outputs

//     /// <summary>Stub for implementing an operator implemented in native frontend language.</summary>
//     /// <param name="data">Input data for the custom operator.</param>
//     /// <param name="info"></param>
//     /// <param name="needTopGrad">Whether this layer needs out grad for backward. Should be false for loss layers.</param>
//     static member Native([<ParamArray>] data : NDArray[], info : ptr, [<Optional; DefaultParameterValue(true)>] needTopGrad : bool) =
//         let creator = AtomicSymbolCreator.FromName "_Native"
//         let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
//                                                  Array.empty
//                                                  [|"info"; "need_top_grad"|]
//                                                  [|info.ToString(); needTopGrad.ToString()|]
//         outputs

//     /// <summary>Stub for implementing an operator implemented in native frontend language with ndarray.</summary>
//     /// <param name="data">Input data for the custom operator.</param>
//     /// <param name="info"></param>
//     static member NDArray([<ParamArray>] data : NDArray[], info : ptr) =
//         let creator = AtomicSymbolCreator.FromName "_NDArray"
//         let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
//                                                  Array.empty
//                                                  [|"info"|]
//                                                  [|info.ToString()|]
//         outputs

// type PoolType = 
//     | Avg
//     | Max
//     | Sum
//     override x.ToString() =
//         match x with
//             | Avg -> "avg"
//             | Max -> "max"
//             | Sum -> "sum"

// type PoolingConvention = 
//     | Full
//     | Valid
//     override x.ToString() =
//         match x with
//             | Full -> "full"
//             | Valid -> "valid"

//     /// <summary>This operator is DEPRECATED.
//     /// Perform pooling on the input.
//     /// 
//     /// The shapes for 2-D pooling is
//     /// 
//     /// - **data**: *(batch_size, channel, height, width)*
//     /// - **out**: *(batch_size, num_filter, out_height, out_width)*, with::
//     /// 
//     ///     out_height = f(height, kernel[0], pad[0], stride[0])
//     ///     out_width = f(width, kernel[1], pad[1], stride[1])
//     /// 
//     /// The definition of *f* depends on ``pooling_convention``, which has two options:
//     /// 
//     /// - **valid** (default)::
//     /// 
//     ///     f(x, k, p, s) = floor((x+2*p-k)/s)+1
//     /// 
//     /// - **full**, which is compatible with Caffe::
//     /// 
//     ///     f(x, k, p, s) = ceil((x+2*p-k)/s)+1
//     /// 
//     /// But ``global_pool`` is set to be true, then do a global pooling, namely reset
//     /// ``kernel=(height, width)``.
//     /// 
//     /// Three pooling options are supported by ``pool_type``:
//     /// 
//     /// - **avg**: average pooling
//     /// - **max**: max pooling
//     /// - **sum**: sum pooling
//     /// 
//     /// 1-D pooling is special case of 2-D pooling with *weight=1* and
//     /// *kernel[1]=1*.
//     /// 
//     /// For 3-D pooling, an additional *depth* dimension is added before
//     /// *height*. Namely the input data will have shape *(batch_size, channel, depth,
//     /// height, width)*.
//     /// 
//     /// 
//     /// 
//     /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\operator\pooling_v1.cc:L104</summary>
//     /// <param name="data">Input data to the pooling operator.</param>
//     /// <param name="kernel">pooling kernel size: (y, x) or (d, y, x)</param>
//     /// <param name="poolType">Pooling type to be applied.</param>
//     /// <param name="globalPool">Ignore kernel size, do global pooling based on current input feature map. </param>
//     /// <param name="poolingConvention">Pooling convention to be applied.</param>
//     /// <param name="stride">stride: for pooling (y, x) or (d, y, x)</param>
//     /// <param name="pad">pad for pooling: (y, x) or (d, y, x)</param>
//     static member PoolingV1(data : NDArray, 
//                             [<Optional>] kernel : int seq, 
//                             [<Optional>] poolType : PoolType, 
//                             [<Optional; DefaultParameterValue(false)>] globalPool : bool, 
//                             [<Optional>] poolingConvention : PoolingConvention, 
//                             [<Optional>] stride : int seq, 
//                             [<Optional>] pad : int seq) =
//         let creator = AtomicSymbolCreator.FromName "Pooling_v1"
//         let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle
//                                                  [|data.NDArrayHandle|]
//                                                  [|"kernel"; "pool_type"; "global_pool"; "pooling_convention"; "stride"; "pad"|]
//                                                  [|(if isNull (kernel :> obj) then "[]" else kernel.ToString()); (if isNull (poolType :> obj) then "max" else poolType.ToString()); globalPool.ToString(); (if isNull (poolingConvention :> obj) then "valid" else poolingConvention.ToString()); (if isNull (stride :> obj) then "[]" else stride.ToString()); (if isNull (pad :> obj) then "[]" else pad.ToString())|]
//         outputs
