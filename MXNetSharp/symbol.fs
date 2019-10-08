namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop


type Symbol() = 
    member x.SymbolHandle : CApi.SymbolHandle = failwith "" 
