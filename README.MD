# MXNet bindings for .NET [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/kevmal/MXNetSharp/master?urlpath=lab)

## Prerequisites 

Bindings are generated for MXNet 1.5.1 and so "libmxnet" version 1.5.1 needs to be on library search path. For GPU support, relevant CUDA libraries also need to be accessible. 

## Examples
- [DQN](Examples/DQN)
- [CGAN](Examples/CGAN.fsx)
- [Variational Autoencoder](Examples/MNIST%20VAE.fsx)
- [Neural style transfer](Examples/Neural%20Style%20Transfer.fsx)

Note: Samples which use UI (CGAN, VAE) need to specifically reference either net46 or netcore assemblies. By default they're loading net46, to use netcore uncomment the ".NET core" section in loadui.fsx in the Examples directory and comment the Net46 section.

## Quick start 

### Basics
```fsharp
open MXNetSharp

// Symbol API
let x = Variable "x" // Symbol
let y = Variable "y" // Symbol

// elementwise multiplication
let z = x * y //x and y will be infered to have the same shape

// broadcast multiplication
let z2 = x .* y // x and y shapes can differ according to the rules of MXNet broadcasting

// scalar multiplication, overloads are for type `double` but will match type of x
let z3 = 4.0*x

// broadcast operators for +, -, /, and such are analogous to above
// comparison operators at the moment are by default prefixed with a `.` and have no broadcast equivalents
let z4 = x .= y // elementwise

// logical operators do have broadcast equivalents
let z5 = x .&& y // elementwise
let z6 = x ..&& y // broadcast

// For operators sqrt, exp, pow and such we need to open MXNetSharp.PrimitiveOperators
open MXNetSharp.PrimitiveOperators
let z7 = exp x


// Create an NDArray from a .NET array

let a = NDArray.CopyFrom([|1.f .. 10.f|], [5;2], GPU 0)

// This is the same as above
let a2 = GPU(0).CopyFrom([|1.f .. 10.f|], [5;2])


// NDArray's do not need the MXNetSharp.PrimitiveOperators namespace
let b = sqrt(a + 20.0)

let v : float32 [] = b.ToArray<_>() //Copy back to CPU in managed array
let v2 = b.ToFloat32Array() //Same as above
let v3 = b.ToDoubleArray() // Float32 -> Double conversion happens implicitly

// val v : float32 [] =
//  [|4.5825758f; 4.69041586f; 4.79583168f; 4.89897966f; 5.0f; 5.09901953f;
//    5.19615221f; 5.29150248f; 5.38516474f; 5.47722578f|]

// NDArray Operators exist in MXNEtSharp.MX

MX.Mean(b).ToFloat32Scalar()

// val it : float32 = 5.04168653f

// Slicing

// following are equivalent
b.[2..4,*].ToFloat32Array()
b.[2..4].ToFloat32Array()
//val it : float32 [] =
// [|5.0f; 5.09901953f; 5.19615221f; 5.29150248f; 5.38516474f; 5.47722578f|]

// Note that the range is startIndex..endIndex (F# style) as oppose to MXnet slcing where slice stops just up to the end
b.[2..2,*].ToFloat32Array()
//val it : float32 [] = [|5.0f; 5.09901953f|]

// With negative slicing then 'end' value behaves the same as MXNet. startIndex .. -dropCount
b.[2..-2,1].ToFloat32Array()
// val it : float32 [] = [|5.29150248f|]

// Steping syntax is more verbose (the following are all equivalent)

b.[SliceRange(0L, 4L, 2L), *].ToFloat32Array()
b.[SliceRange(stop = 4L, step = 2L), *].ToFloat32Array()
b.[SliceRange(start = 0L, step = 2L), *].ToFloat32Array()
b.[SliceRange(step = 2L), *].ToFloat32Array()

// val it : float32 [] =
// [|4.5825758f; 4.69041586f; 5.0f; 5.09901953f; 5.38516474f; 5.47722578f|]

```

### Linear Regression 
```fsharp

open MXNetSharp
open MXNetSharp.SymbolOperators
open MXNetSharp.PrimitiveOperators
open MXNetSharp.Interop

let ctx = CPU 0
let X = ctx.Arange(1.0, 11.0) // values 1, 2, .. 9, 10
let actualY = 2.5*X + 0.7
MXLib.randomSeed 1000
let observedY = actualY + MX.RandomNormalLike(actualY, 0.0, 0.1)
actualY.ToFloat32Array()
// val it : float32 [] =
// [|3.20000005f; 5.69999981f; 8.19999981f; 10.6999998f; 13.1999998f;
//   15.6999998f; 18.2000008f; 20.7000008f; 23.2000008f; 25.7000008f|]
observedY.ToFloat32Array()
//val it : float32 [] =
//  [|2.95296192f; 5.40489101f; 7.96742868f; 10.7032785f; 13.0787563f;
//    15.9071894f; 18.0810871f; 20.7377892f; 23.1656361f; 25.6353264f|]

let input = Input("x", ndarray = X)
let label = Input("y", ndarray = observedY)
let m = Parameter("m",ndarray = ctx.RandomUniform([1], -1.0, 1.0), 
                      grad = ctx.Zeros(shape = [1]), 
                      opReqType = OpReqType.WriteTo)
let b = Parameter("b",ndarray = ctx.RandomUniform([1], -1.0, 1.0), 
                      grad = ctx.Zeros(shape = [1]), 
                      opReqType = OpReqType.WriteTo)
let model = m.*input .+ b

let loss = MakeLoss(Mean(Square(model - label)))
let execOutput = SymbolGroup(loss, MX.BlockGrad(model))
let executor = execOutput.Bind(ctx)
executor.Forward(true)

// Loss with initial parameters:
executor.Outputs.[0].ToFloat32Array()
// val it : float32 [] = [|425.663239f|]

// Model output:
executor.Outputs.[1].ToFloat32Array()
//val it : float32 [] =
// [|-0.605032206f; -1.35715616f; -2.10928011f; -2.86140394f; -3.61352777f;
//   -4.36565208f; -5.11777592f; -5.86989975f; -6.62202358f; -7.37414742f|]

executor.Backward()
m.Grad.Value.ToFloat32Array()
// val it : float32 [] = [|-256.021332f|]
b.Grad.Value.ToFloat32Array()
// val it : float32 [] = [|-36.7060509f|]



// SGD update
let lr = 0.01   //learning rate

m.NDArray.Value.ToFloat32Array()
// val it : float32 [] = [|-0.752123952f|]
MX.SgdUpdate([m.NDArray.Value], m.NDArray.Value, m.Grad.Value, lr)
m.NDArray.Value.ToFloat32Array()
// val it : float32 [] = [|1.80808938f|]

b.NDArray.Value.ToFloat32Array()
// val it : float32 [] = [|0.147091746f|]
MX.SgdUpdate([b.NDArray.Value], b.NDArray.Value, b.Grad.Value, lr)
b.NDArray.Value.ToFloat32Array()
// val it : float32 [] = [|0.514152288f|]

// Run with new parameters and expect a lower loss (< 425.663239)
executor.Forward(false)
executor.Outputs.[0].ToFloat32Array()
// val it : float32 [] = [|19.5483208f|]

// "Train" in loop
for i = 1 to 5 do 
    executor.Forward(true)
    executor.Backward()
    MX.SgdUpdate([m.NDArray.Value], m.NDArray.Value, m.Grad.Value, lr)
    MX.SgdUpdate([b.NDArray.Value], b.NDArray.Value, b.Grad.Value, lr)
    printfn "%3d Loss: %f" i (executor.Outputs.[0].ToDoubleScalar())
//1 Loss: 19.548321
//2 Loss: 0.915147
//3 Loss: 0.060186
//4 Loss: 0.020915
//5 Loss: 0.019071

// Trained parameters
// True values: m = 2.5 and b = 0.7

m.NDArray.Value.ToDoubleScalar()
// val it : double = 2.50611043

b.NDArray.Value.ToDoubleScalar()
// val it : double = 0.611009717

```


## MXNet 

[MXNet](https://mxnet.apache.org) supports has both a symbolic interface (Symbol API) as well as an imperative interface (NDArray API). The NDArray API can be used on it's own and gradients can be calculated using the Autodiff API. The Symbol API alows a computation graph to be defined and optimizations to take place before NDArray's are bound to it's inputs. An Executor is a Symbol bound to NDArrays, data can be copied into and out of these NDArrays and the graph can be executed foward/backward.

### Low level interop

Though ideally not needed, low level access to libmxnet is available in the following namespaces:
- [MXNetSharp.Interop.CApi](MXNetSharp/capi.fs) ([c_api.h](https://github.com/apache/incubator-mxnet/blob/master/include/mxnet/c_api.h))
- [MXNetSharp.Interop.CPredictApi](MXNetSharp/cpredictapi.fs) ([c_predict_api.h](https://github.com/apache/incubator-mxnet/blob/master/include/mxnet/c_predict_api.h))
- [MXNetSharp.Interop.CNNVMApi](MXNetSharp/cpredictapi.fs) ([cnnvmapi.h](https://github.com/apache/incubator-tvm/blob/master/nnvm/include/nnvm/c_api.h))

With F# friendly wrappers in [MXNetSharp.Interop](MXNetSharp/interop.fs) with the following modules
- MXLib
- MXSymbol
- MXNDArray
- MXExecutor
- MXNDList
- NNVM
- NNGraph
- MXDataIter
- MXAutograd
- MXRtc


## Symbol API

Currently each MXNet operation is represented as a type inheriting from `SymbolOperator` and are generated in the `MXNetSharp.SymbolOperators` namespace.


Actual creation of the `Symbol` (at the MXNet level) is delayed until the symbol handle is needed. This is to allow for delayed naming and late input binding.
```fsharp
open MXNetSharp
open MXNetSharp.SymbolOperators

let x = Variable "x"
let y = Variable "y"

let z = 
    x
    .>> FullyConnected(1024)
    .>> FullyConnected(1024)
    .>> FullyConnected(32)
    .>> FullyConnected(1)

let loss = MakeLoss(Sum(Square(x-y))) 

loss.SymbolHandle // MXNet Symbol handle is created at this point 

```

Since each operation retains it's type, F# operators such as `exp` do not work on their own and so `MXNetSharp.PrimitiveOperators` provides operators for use with
the `Symbol` type such as `tanh`, `exp`, `pow`.

```fsharp

open MXNetSharp
open MXNetSharp.SymbolOperators
open MXNetSharp.PrimitiveOperators

let x = Variable "x"

let y = exp(2.0*x + 33.0) // y is of type `Exp`
let y2 = Exp(2.0*x + 33.0) // same as above but explicitly creates the `Exp` symbol type

```

## NDArray API (TODO)
MXNetSharp.NDArray class. Please see samples and quickstart section.

### Autodiff (TODO)

```fsharp
open MXNetSharp
let x = (CPU 0).Arange(1.0,5.0).Reshape(2,2)
x.AttachGradient()
let y,z = 
    Autograd.record
        (fun () ->
            let y = x*2.0
            let z = y*x
            y,z
        )
z.Backward()

x.Grad.Value.ToFloat32Array()
// val it : float32 [] = [|4.0f; 8.0f; 12.0f; 16.0f|]
```

Higher order gradients (currently limited in MXNet):
```fsharp

open MXNetSharp
let x = (CPU 0).CopyFrom([|1.f;2.f;3.f|], [1;1;-1])
x.AttachGradient()
let yGrad = 
    Autograd.record 
        (fun () ->
            let y = sin x
            let yGrad = Autograd.grad false true true Array.empty [y] [x]
            yGrad.[0]
        )
yGrad.Backward()

x.Grad.Value.ToFloat32Array()
// val it : float32 [] = [|-0.841470957f; -0.909297407f; -0.141120002f|]
(-sin x).ToFloat32Array() 
// val it : float32 [] = [|-0.841470957f; -0.909297407f; -0.141120002f|]

```


## Executor (TODO)
MXNetSharp.Executor class. Please see samples.


## Data loaders (TODO)
MXNetSharp.IO.CSVIter and MXNetSharp.IO.MNISTIter. MNISTIter is used in the VAE and CGAN examples.




