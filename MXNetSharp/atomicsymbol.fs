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
    let aliases = ResizeArray<string>()
    static let lookup = Dictionary<String, AtomicSymbolCreator>()
        (*let creators = 
            MXSymbol.listAtomicSymbolCreators ()
            |> Array.map 
                (fun x -> 
                    let info = MXSymbol.getAtomicSymbolInfo x
                    x, AtomicSymbolCreator(x,info)
                )
            |> dict
        NNVM.listAllOpNames()
        |> Array.map 
            (fun name -> 
                let h = NNVM.getOpHandle name
                let c = creators.[h]
                c.AddAlias(name)
                name, c
            )
        |> dict
        *)
    member internal x.AddAlias(name) = aliases.Add name
    static member FromName name = 
        let scc,v = lookup.TryGetValue(name) //REVIEW: catch and reload list?
        if scc then 
            v
        else    
            try
                let h = NNVM.getOpHandle name
                let info = MXSymbol.getAtomicSymbolInfo h
                let v = AtomicSymbolCreator(h,info)
                lookup.[name] <- v
                v
            with 
            | _-> raise(AtomicSymbolCreatorNotFound(name)) //TODO: inner ex
    member x.Name = info.Name
    member x.AtomicSymbolCreatorHandle = handle
    member x.Info = info

        




    
    

