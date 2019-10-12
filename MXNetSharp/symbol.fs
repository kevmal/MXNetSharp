namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop


type Symbol(creator : AtomicSymbolCreator option, parameters, inputs) = 
    let mutable name = None
    let mutable initialized = false
    let str (v : obj) = 
        match v with 
        | :? bool as x -> if x then "1" else "0"
        | :? string as x -> sprintf "%s" x
        | x -> string x
    let parametersStr = parameters |> Array.map (fun (k,v) -> k, str v)
    let handle = 
        lazy
            initialized <- true
            match creator with 
            | None -> 
                match name with 
                | Some n -> MXSymbol.createVariable(n)
                | None -> 0n
            | Some creator -> 
                let symbol = parametersStr |> Array.unzip ||> MXSymbol.createAtomicSymbol creator.AtomicSymbolCreatorHandle
                let name = defaultArg name null
                inputs 
                |> Array.choose (fun (k,v : Symbol) -> let h = v.SymbolHandle in if h > 0n then Some(k,h) else None)
                |> Array.unzip 
                ||> MXSymbol.compose symbol name
                symbol
    new(creator, parameterKeys, parameterValues, inputKeys, inputValues) =  
        Symbol(creator, Array.zip parameterKeys parameterValues, Array.zip inputKeys inputValues)
    member x.Name 
        with get() = match name with Some n -> n | _ -> ""
        and set v = 
            if initialized then 
                failwith "Cannot set name. Symbol has already been created." //TODO: make exception
            name <- Some v 
    member x.WithName(name) = x.Name <- name; x
    member x.SymbolHandle : CApi.SymbolHandle = handle.Value
    static member Variable(name) = Symbol(None, Array.empty, Array.empty, Name = name)
    static member Empty = Symbol(None, Array.empty, Array.empty)
    
