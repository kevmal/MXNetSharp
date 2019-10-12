namespace MXNetSharp
open MXNetSharp.Interop
open System.Collections.Generic
open System

exception AtomicSymbolCreatorNotFound of string with
    override x.Message = 
        match x :> Exception with 
        | AtomicSymbolCreatorNotFound(name) -> sprintf "Atomic symbol creator %s not found." name
        | _ -> failwith "unreachable"

type AtomicSymbolCreator internal (handle : MXNetSharp.Interop.CApi.AtomicSymbolCreatorHandle, info : AtomicSymbolInfo) = 
    static let lookup = 
        MXSymbol.listAtomicSymbolCreators ()
        |> Array.map 
            (fun x -> 
                let info = MXSymbol.getAtomicSymbolInfo x
                info.Name, AtomicSymbolCreator(x,info)
            )
        |> dict
    static member FromName name = 
        let scc,v = lookup.TryGetValue(name) //REVIEW: catch and reload list?
        if scc then 
            v
        else 
            raise(AtomicSymbolCreatorNotFound(name))
    member x.Name = info.Name
    member x.AtomicSymbolCreatorHandle = handle
    member x.Info = info

        




    
    

