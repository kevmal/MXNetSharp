namespace MXNetSharp
open MXNetSharp.Interop
open System.Collections.Generic

type AtomicSymbolCreator internal (handle : MXNetSharp.Interop.CApi.AtomicSymbolCreatorHandle, info : AtomicSymbolInfo) = 
    static let lookup = 
        MXSymbol.listAtomicSymbolCreators ()
        |> Array.map 
            (fun x -> 
                let info = MXSymbol.getAtomicSymbolInfo x
                info.Name, AtomicSymbolCreator(x,info)
            )
        |> dict
    static member FromName name = lookup.[name] //REVIEW: catch and reload list?
    member x.Name = info.Name
    member x.AtomicSymbolCreatorHandle = handle
    member x.Info = info

        




    
    

