# MXNet bindings for .NET [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/kevmal/MXNetSharp/master?urlpath=lab)

## Prerequisites 

Bindings are generated for MXNet 1.5.1 and so "libmxnet" version 1.5.1 needs to be on library search path. For GPU support, relevant CUDA libraries also need to be accessible. 

## Examples
- [QNN](Examples/QNN)
- [CGAN](Examples/CGAN.fsx)
- [Variational Autoencoder](Examples/MNIST%20VAE.fsx)
- [Neural style transfer](Examples/Neural%20Style%20Transfer.fsx)

Note: Samples which use UI (CGAN, VAE) need to specifically reference either net46 or netcore assemblies. By default they're loading net46, to use netcore uncomment the ".NET core" section in loadui.fsx in the Examples directory and comment the Net46 section.

## Quick start 
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

let v : float32 [] = b.ToArray() //Copy back to CPU in managed array
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

Simple examples adapted from various sources:
- [Neural style transfer](Examples/Neural%20Style%20Transfer.fsx)
- [Variational Autoencoder](Examples/MNIST%20VAE.fsx)
- [CGAN](Examples/CGAN.fsx)



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

## NDArray (TODO)
MXNetSharp.NDArray class. Please see samples and quickstart section.

## Executor (TODO)
MXNetSharp.Executor class. Please see samples.

### Autodiff (TODO)
MXNetSharp.Autodiff module.

## Data loaders (TODO)
MXNetSharp.IO.CSVIter and MXNetSharp.IO.MNISTIter. MNISTIter is used in the VAE and CGAN examples.




