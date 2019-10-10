namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop


type Symbol(handle : CApi.SymbolHandle) = 
    member x.SymbolHandle : CApi.SymbolHandle = handle
    static member Variable(name) = MXSymbol.createVariable(name) |> Symbol
        
