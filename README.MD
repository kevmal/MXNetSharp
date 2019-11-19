# MXNet bindings for .NET

## Prerequisites 

Bindings are generated for MXNet 1.5.1 and so "libmxnet" version 1.5.1 needs to be on library search path. For GPU support, relevant CUDA libraries also need to be accessible. 

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
// comparison operators at the moment are by default prefixed with a `.` and `..`  is used for broadcasting
let z4 = x .= y // elementwise
let z5 = x ..= y // broadcast

// For operators sqrt, exp, pow and such we need to open MXNetSharp.PrimitiveOperators
open MXNetSharp.PrimitiveOperators
let z4 = exp x

// NDArray's do not need the MXNetSharp.PrimitiveOperators namespace

let a = new NDArray([|1.f .. 10.f|], shape = [5;2], context = GPU 0)

let b = sqrt(a + 20.0)


let v : float32 [] = b.ToArray() //Copy back to CPU in managed array

// val v : float32 [] =
//  [|4.5825758f; 4.69041586f; 4.79583168f; 4.89897966f; 5.0f; 5.09901953f;
//    5.19615221f; 5.29150248f; 5.38516474f; 5.47722578f|]

```

Simple examples adapted from various sources:
[Neural style transfer](Examples/Neural Style Transfer.fsx)
[Variational Autoencoder](Examples/MNIST VAE.fsx)
[CGAN](Examples/CGAN.fsx)



## Symbol API

Currently each MXNet operation is represented as a type inheriting from `SymbolOperator` in the `MXNetSharp` namespace (these will soon find a new home in their own namespace). 


Actual creation of the `Symbol` (at the MXNet level) are delayed until symbol handles are needed. This is to allow for delayed naming and late input binding.
```
open MXNetSharp

let inline (.>>) (x : Symbol) (y : 'a) : 'a = 
    let _f() = y :> SymbolOperator
    y.ComposedWith(x) |> box |> unbox

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
open MXNetSharp.PrimitiveOperators

let x = Variable "x"

let y = exp(2.0*x + 33.0) // y is of type `Exp`
let y2 = Exp(2.0*x + 33.0) // same as above but explicitly creates the `Exp` symbol type

```
## Executor 
TODO

## NDArray
TODO
### Autodiff
TODO





