

open System.Collections.Generic
open System.Runtime.InteropServices

#load "load.fsx"
open MXNetSharp
open MXNetSharp.SymbolOperators
open MXNetSharp.IO
open System
open System.Net
open System.IO
open System.IO.Compression

open MXNetSharp.PrimitiveOperators

MXNetSharp.Interop.MXLib.getVersion()

let x = Input("x", ndarray = (CPU 0).CopyFrom([|-2.f; 0.f; 0.5f|], [3]))
let k = 3
let dim = 1

let sorted = Topk(x, axis = dim, k = k, retTyp = RetTyp.Value)
let rho = Arange(1.0)
let mean = NpCumsum(sorted) / rho
let meanSq = NpCumsum(Square(sorted)) / rho
let ss = rho*(meanSq - Square(mean))
let delta = (1.0 - ss) / rho
let deltaNz = Relu(delta)
let tau = mean - sqrt deltaNz
let supportSize = Sum(tau .<= sorted, keepdims=true)


forea



    


