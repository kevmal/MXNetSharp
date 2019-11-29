namespace MXNetSharp
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<Extension>]
type ContextExtensions private () = 
(*
    /// <param name="shape">The shape of the output</param>
    /// <param name="dtype">Target data type.</param>
    [<Extension>]
    static member NpiZeros(ctx : Context, [<Optional>] ?shape : int seq, [<Optional>] ?dtype : DataType) =
        NpiZeros(ctx, ?shape = shape, ?dtype = dtype)
    /// <param name="shape">The shape of the output</param>
    /// <param name="dtype">Target data type.</param>
    [<Extension>]
    static member NpiOnes(ctx : Context, [<Optional>] ?shape : int seq, [<Optional>] ?dtype : DataType) =
        NpiOnes(ctx, ?shape = shape, ?dtype = dtype)
    /// <param name="shape">The shape of the output</param>
    /// <param name="dtype">Target data type.</param>
    [<Extension>]
    static member NpiIdentity(ctx : Context, [<Optional>] ?shape : int seq, [<Optional>] ?dtype : DataType) =
        NpiIdentity(ctx, ?shape = shape, ?dtype = dtype)
    /// <param name="start">Start of interval. The interval includes this value. The default start value is 0.</param>
    /// <param name="stop">End of interval. The interval does not include this value, except in some cases where step is not an integer and floating point round-off affects the length of out.</param>
    /// <param name="step">Spacing between values.</param>
    /// <param name="repeat">The repeating time of all elements. E.g repeat=3, the element a will be repeated three times --&gt; a, a, a.</param>
    /// <param name="inferRange">When set to True, infer the stop position from the start, step, repeat, and output tensor size.</param>
    /// <param name="dtype">Target data type.</param>
    [<Extension>]
    static member NpiArange(ctx : Context, start : double, [<Optional>] ?stop : float, [<Optional>] ?step : double, [<Optional>] ?repeat : int, [<Optional>] ?inferRange : bool, [<Optional>] ?dtype : DataType) =
        NpiArange(ctx, start, ?stop = stop, ?step = step, ?repeat = repeat, ?inferRange = inferRange, ?dtype = dtype)
    /// <param name="dimensions">The shape of the grid.</param>
    /// <param name="dtype">Target data type.</param>
    [<Extension>]
    static member NpiIndices(ctx : Context, dimensions : int seq, [<Optional>] ?dtype : DataType) =
        NpiIndices(ctx, dimensions, ?dtype = dtype)
    /// <param name="M">Number of points in the output window. If zero or less, an empty array is returned.</param>
    /// <param name="dtype">Data-type of the returned array.</param>
    [<Extension>]
    static member NpiHanning(ctx : Context, M : int, [<Optional>] ?dtype : DataType) =
        NpiHanning(ctx, M, ?dtype = dtype)
    /// <param name="M">Number of points in the output window. If zero or less, an empty array is returned.</param>
    /// <param name="dtype">Data-type of the returned array.</param>
    [<Extension>]
    static member NpiHamming(ctx : Context, M : int, [<Optional>] ?dtype : DataType) =
        NpiHamming(ctx, M, ?dtype = dtype)
    /// <param name="M">Number of points in the output window. If zero or less, an empty array is returned.</param>
    /// <param name="dtype">Data-type of the returned array.</param>
    [<Extension>]
    static member NpiBlackman(ctx : Context, M : int, [<Optional>] ?dtype : DataType) =
        NpiBlackman(ctx, M, ?dtype = dtype)
    /// <param name="input1">Source input</param>
    /// <param name="input2">Source input</param>
    /// <param name="a"></param>
    /// <param name="size"></param>
    /// <param name="replace"></param>
    /// <param name="weighted"></param>
    [<Extension>]
    static member NpiChoice(ctx : Context, input1 : Symbol, input2 : Symbol, a : int64, size : int seq, [<Optional>] ?replace : bool, [<Optional>] ?weighted : bool) =
        NpiChoice(ctx, input1, input2, a, size, ?replace = replace, ?weighted = weighted)
    /// <param name="input1">Source input</param>
    /// <param name="input2">Source input</param>
    /// <param name="loc"></param>
    /// <param name="scale"></param>
    /// <param name="size">Output shape. If the given shape is, e.g., (m, n, k), then m * n * k samples are drawn. Default is None, in which case a single value is returned.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    [<Extension>]
    static member NpiNormal(ctx : Context, input1 : Symbol, input2 : Symbol, [<Optional>] ?loc : float, [<Optional>] ?scale : float, [<Optional>] ?size : int seq, [<Optional>] ?dtype : FloatDType) =
        NpiNormal(ctx, input1, input2, ?loc = loc, ?scale = scale, ?size = size, ?dtype = dtype)
    /// <param name="input1">Source input</param>
    /// <param name="input2">Source input</param>
    /// <param name="low"></param>
    /// <param name="high"></param>
    /// <param name="size">Output shape. If the given shape is, e.g., (m, n, k), then m * n * k samples are drawn. Default is None, in which case a single value is returned.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    [<Extension>]
    static member NpiUniform(ctx : Context, input1 : Symbol, input2 : Symbol, [<Optional>] ?low : float, [<Optional>] ?high : float, [<Optional>] ?size : int seq, [<Optional>] ?dtype : FloatDType) =
        NpiUniform(ctx, input1, input2, ?low = low, ?high = high, ?size = size, ?dtype = dtype)
        *)
    /// <summary>Draw random samples from a uniform distribution.
    /// 
    /// .. note:: The existing alias ``uniform`` is deprecated.
    /// 
    /// Samples are uniformly distributed over the half-open interval *[low, high)*
    /// (includes *low*, but excludes *high*).
    /// 
    /// Example::
    /// 
    ///    uniform(low=0, high=1, shape=(2,2)) = [[ 0.60276335,  0.85794562],
    ///                                           [ 0.54488319,  0.84725171]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet\mxnet\src\operator\random\sample_op.cc:L97</summary>
    /// <param name="low">Lower bound of the distribution.</param>
    /// <param name="high">Upper bound of the distribution.</param>
    /// <param name="shape">Shape of the output.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    [<Extension>]
    static member RandomUniform(ctx : Context, [<Optional>] low : float, [<Optional>] high : float, [<Optional>] shape : int seq, [<Optional>] dtype : FloatDType) =
        MX.RandomUniformNDArray(ctx, low = low, high = high, shape = shape, dtype = dtype)
    /// <summary>Draw random samples from a normal (Gaussian) distribution.
    /// 
    /// .. note:: The existing alias ``normal`` is deprecated.
    /// 
    /// Samples are distributed according to a normal distribution parametrized by *loc* (mean) and *scale*
    /// (standard deviation).
    /// 
    /// Example::
    /// 
    ///    normal(loc=0, scale=1, shape=(2,2)) = [[ 1.89171135, -1.16881478],
    ///                                           [-1.23474145,  1.55807114]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet\mxnet\src\operator\random\sample_op.cc:L115</summary>
    /// <param name="loc">Mean of the distribution.</param>
    /// <param name="scale">Standard deviation of the distribution.</param>
    /// <param name="shape">Shape of the output.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    [<Extension>]
    static member RandomNormal(ctx : Context, [<Optional>] loc : float, [<Optional>] scale : float, [<Optional>] shape : int seq, [<Optional>] dtype : FloatDType) =
        MX.RandomNormalNDArray(ctx, loc = loc, scale = scale, shape = shape, dtype = dtype)
    /// <summary>Draw random samples from a gamma distribution.
    /// 
    /// Samples are distributed according to a gamma distribution parametrized by *alpha* (shape) and *beta* (scale).
    /// 
    /// Example::
    /// 
    ///    gamma(alpha=9, beta=0.5, shape=(2,2)) = [[ 7.10486984,  3.37695289],
    ///                                             [ 3.91697288,  3.65933681]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet\mxnet\src\operator\random\sample_op.cc:L127</summary>
    /// <param name="alpha">Alpha parameter (shape) of the gamma distribution.</param>
    /// <param name="beta">Beta parameter (scale) of the gamma distribution.</param>
    /// <param name="shape">Shape of the output.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    [<Extension>]
    static member RandomGamma(ctx : Context, [<Optional>] alpha : float, [<Optional>] beta : float, [<Optional>] shape : int seq, [<Optional>] dtype : FloatDType) =
        MX.RandomGammaNDArray(ctx, alpha = alpha, beta = beta, shape = shape, dtype = dtype)
    /// <summary>Draw random samples from an exponential distribution.
    /// 
    /// Samples are distributed according to an exponential distribution parametrized by *lambda* (rate).
    /// 
    /// Example::
    /// 
    ///    exponential(lam=4, shape=(2,2)) = [[ 0.0097189 ,  0.08999364],
    ///                                       [ 0.04146638,  0.31715935]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet\mxnet\src\operator\random\sample_op.cc:L139</summary>
    /// <param name="lam">Lambda parameter (rate) of the exponential distribution.</param>
    /// <param name="shape">Shape of the output.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    [<Extension>]
    static member RandomExponential(ctx : Context, [<Optional>] lam : float, [<Optional>] shape : int seq, [<Optional>] dtype : FloatDType) =
        MX.RandomExponentialNDArray(ctx, lam = lam, shape = shape, dtype = dtype)
    /// <summary>Draw random samples from a Poisson distribution.
    /// 
    /// Samples are distributed according to a Poisson distribution parametrized by *lambda* (rate).
    /// Samples will always be returned as a floating point data type.
    /// 
    /// Example::
    /// 
    ///    poisson(lam=4, shape=(2,2)) = [[ 5.,  2.],
    ///                                   [ 4.,  6.]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet\mxnet\src\operator\random\sample_op.cc:L152</summary>
    /// <param name="lam">Lambda parameter (rate) of the Poisson distribution.</param>
    /// <param name="shape">Shape of the output.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    [<Extension>]
    static member RandomPoisson(ctx : Context, [<Optional>] lam : float, [<Optional>] shape : int seq, [<Optional>] dtype : FloatDType) =
        MX.RandomPoissonNDArray(ctx, lam = lam, shape = shape, dtype = dtype)
    /// <summary>Draw random samples from a negative binomial distribution.
    /// 
    /// Samples are distributed according to a negative binomial distribution parametrized by
    /// *k* (limit of unsuccessful experiments) and *p* (failure probability in each experiment).
    /// Samples will always be returned as a floating point data type.
    /// 
    /// Example::
    /// 
    ///    negative_binomial(k=3, p=0.4, shape=(2,2)) = [[ 4.,  7.],
    ///                                                  [ 2.,  5.]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet\mxnet\src\operator\random\sample_op.cc:L166</summary>
    /// <param name="k">Limit of unsuccessful experiments.</param>
    /// <param name="p">Failure probability in each experiment.</param>
    /// <param name="shape">Shape of the output.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    [<Extension>]
    static member RandomNegativeBinomial(ctx : Context, [<Optional>] k : int, [<Optional>] p : float, [<Optional>] shape : int seq, [<Optional>] dtype : FloatDType) =
        MX.RandomNegativeBinomialNDArray(ctx, k = k, p = p, shape = shape, dtype = dtype)

    /// <summary>Draw random samples from a generalized negative binomial distribution.
    /// 
    /// Samples are distributed according to a generalized negative binomial distribution parametrized by
    /// *mu* (mean) and *alpha* (dispersion). *alpha* is defined as *1/k* where *k* is the failure limit of the
    /// number of unsuccessful experiments (generalized to real numbers).
    /// Samples will always be returned as a floating point data type.
    /// 
    /// Example::
    /// 
    ///    generalized_negative_binomial(mu=2.0, alpha=0.3, shape=(2,2)) = [[ 2.,  1.],
    ///                                                                     [ 6.,  4.]]
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet\mxnet\src\operator\random\sample_op.cc:L181</summary>
    /// <param name="mu">Mean of the negative binomial distribution.</param>
    /// <param name="alpha">Alpha (dispersion) parameter of the negative binomial distribution.</param>
    /// <param name="shape">Shape of the output.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to float32 if not defined (dtype=None).</param>
    [<Extension>]
    static member RandomGeneralizedNegativeBinomial(ctx : Context, [<Optional>] mu : float, [<Optional>] alpha : float, [<Optional>] shape : int seq, [<Optional>] dtype : FloatDType) =
        MX.RandomGeneralizedNegativeBinomialNDArray(ctx, mu = mu, alpha = alpha, shape = shape, dtype = dtype)

    /// <summary>Draw random samples from a discrete uniform distribution.
    /// 
    /// Samples are uniformly distributed over the half-open interval *[low, high)*
    /// (includes *low*, but excludes *high*).
    /// 
    /// Example::
    /// 
    ///    randint(low=0, high=5, shape=(2,2)) = [[ 0,  2],
    ///                                           [ 3,  1]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet\mxnet\src\operator\random\sample_op.cc:L196</summary>
    /// <param name="low">Lower bound of the distribution.</param>
    /// <param name="high">Upper bound of the distribution.</param>
    /// <param name="shape">Shape of the output.</param>
    /// <param name="dtype">DType of the output in case this can&#39;t be inferred. Defaults to int32 if not defined (dtype=None).</param>
    [<Extension>]
    static member RandomRandint(ctx : Context, low : int64, high : int64, [<Optional>] shape : int seq, [<Optional>] dtype : RandomRandintDtype) =
        MX.RandomRandintNDArray(low, high, ctx, shape = shape, dtype = dtype)
     
    /// <summary>fill target with zeros without default dtype</summary>
    /// <param name="shape">The shape of the output</param>
    /// <param name="dtype">Target data type.</param>
    [<Extension>]
    static member ZerosWithoutDtype(ctx : Context, [<Optional>] shape : int seq, [<Optional>] dtype : int) =
        MX.ZerosWithoutDtypeNDArray(ctx, shape = shape, dtype = dtype)
    
    /// <summary>fill target with zeros</summary>
    /// <param name="shape">The shape of the output</param>
    /// <param name="dtype">Target data type.</param>
    [<Extension>]
    static member Zeros(ctx : Context, [<Optional>] shape : int seq, [<Optional>] dtype : DataType) =
        MX.ZerosNDArray(ctx, shape = shape, dtype = dtype)
    
    /// <summary>Return a 2-D array with ones on the diagonal and zeros elsewhere.</summary>
    /// <param name="N">Number of rows in the output.</param>
    /// <param name="M">Number of columns in the output. If 0, defaults to N</param>
    /// <param name="k">Index of the diagonal. 0 (the default) refers to the main diagonal.A positive value refers to an upper diagonal.A negative value to a lower diagonal.</param>
    /// <param name="dtype">Target data type.</param>
    [<Extension>]
    static member Eye(ctx : Context, N : int64, [<Optional>] M : int64, [<Optional>] k : int64, [<Optional>] dtype : DataType) =
        MX.EyeNDArray(N, ctx, M = M, k = k, dtype = dtype)
    
    /// <summary>fill target with ones</summary>
    /// <param name="shape">The shape of the output</param>
    /// <param name="dtype">Target data type.</param>
    [<Extension>]
    static member Ones(ctx : Context, [<Optional>] shape : int seq, [<Optional>] dtype : DataType) =
        MX.OnesNDArray(ctx, shape = shape, dtype = dtype)
    
    /// <summary>fill target with a scalar value</summary>
    /// <param name="value">Value with which to fill newly created tensor</param>
    /// <param name="shape">The shape of the output</param>
    /// <param name="dtype">Target data type.</param>
    [<Extension>]
    static member Full(ctx : Context, value : double, [<Optional>] shape : int seq, [<Optional>] dtype : DataType) =
        MX.FullNDArray(ctx, value, shape = shape, dtype = dtype)
    
    /// <summary>Return evenly spaced values within a given interval. Similar to Numpy</summary>
    /// <param name="start">Start of interval. The interval includes this value. The default start value is 0.</param>
    /// <param name="stop">End of interval. The interval does not include this value, except in some cases where step is not an integer and floating point round-off affects the length of out.</param>
    /// <param name="step">Spacing between values.</param>
    /// <param name="repeat">The repeating time of all elements. E.g repeat=3, the element a will be repeated three times --&gt; a, a, a.</param>
    /// <param name="inferRange">When set to True, infer the stop position from the start, step, repeat, and output tensor size.</param>
    /// <param name="dtype">Target data type.</param>
    [<Extension>]
    static member Arange(ctx : Context, start : double, [<Optional>] stop : float, [<Optional>] step : double, [<Optional>] repeat : int, [<Optional>] inferRange : bool, [<Optional>] dtype : DataType) =
        MX.ArangeNDArray(start, ctx, stop = stop, step = step, repeat = repeat, inferRange = inferRange, dtype = dtype)
    
    /// <summary>Return an array with evenly spaced values. If axis is not given, the output will 
    /// have the same shape as the input array. Otherwise, the output will be a 1-D array with size of 
    /// the specified axis in input shape.
    /// 
    /// Examples::
    /// 
    ///   x = [[0.14883883 0.7772398  0.94865847 0.7225052 ]
    ///        [0.23729339 0.6112595  0.66538996 0.5132841 ]
    ///        [0.30822644 0.9912457  0.15502319 0.7043658 ]]
    ///        &lt;NDArray 3x4 @cpu(0)&gt;
    /// 
    ///   out = mx.nd.contrib.arange_like(x, start=0)
    /// 
    ///     [[ 0.  1.  2.  3.]
    ///      [ 4.  5.  6.  7.]
    ///      [ 8.  9. 10. 11.]]
    ///      &lt;NDArray 3x4 @cpu(0)&gt;
    /// 
    ///   out = mx.nd.contrib.arange_like(x, start=0, axis=-1)
    /// 
    ///     [0. 1. 2. 3.]
    ///     &lt;NDArray 4 @cpu(0)&gt;
    /// </summary>
    /// <param name="data">The input</param>
    /// <param name="start">Start of interval. The interval includes this value. The default start value is 0.</param>
    /// <param name="step">Spacing between values.</param>
    /// <param name="repeat">The repeating time of all elements. E.g repeat=3, the element a will be repeated three times --&gt; a, a, a.</param>
    /// <param name="axis">Arange elements according to the size of a certain axis of input array. The negative numbers are interpreted counting from the backward. If not provided, will arange elements according to the input shape.</param>
    [<Extension>]
    static member ArangeLike(ctx : Context, data : NDArray, [<Optional>] start : double, [<Optional>] step : double, [<Optional>] repeat : int, [<Optional>] axis : int) =
        MX.ContribArangeLike(data, ctx, start = start, step = step, repeat = repeat, axis = axis)
    
    /// <summary>Return evenly spaced numbers over a specified interval. Similar to Numpy</summary>
    /// <param name="start">Start of interval. The interval includes this value. The default start value is 0.</param>
    /// <param name="stop">End of interval. The interval does not include this value, except in some cases where step is not an integer and floating point round-off affects the length of out.</param>
    /// <param name="step">Spacing between values.</param>
    /// <param name="repeat">The repeating time of all elements. E.g repeat=3, the element a will be repeated three times --&gt; a, a, a.</param>
    /// <param name="inferRange">When set to True, infer the stop position from the start, step, repeat, and output tensor size.</param>
    /// <param name="dtype">Target data type.</param>
    [<Extension>]
    static member Linspace(ctx : Context, start : double, [<Optional>] stop : float, [<Optional>] step : double, [<Optional>] repeat : int, [<Optional>] inferRange : bool, [<Optional>] dtype : DataType) =
        MX.LinspaceNDArray(start, ctx, stop = stop, step = step, repeat = repeat, inferRange = inferRange, dtype = dtype)
