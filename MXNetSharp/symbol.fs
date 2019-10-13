namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop

type SafeSymbolHandle(owner) = 
    inherit SafeHandle(0n, true)
    new() = new SafeSymbolHandle(true)
    new(ptr,owner) as this = new SafeSymbolHandle(owner) then this.SetHandle(ptr)
    override x.IsInvalid = x.handle <= 0n
    override x.ReleaseHandle() = CApi.MXNDArrayFree x.handle = 0
    member internal x.UnsafeHandle = 
        if not x.IsClosed then
            x.handle
        else
            ObjectDisposedException("SafeSymbolHandle", "Symbol handle has been closed") |> raise

type Symbol(creator : AtomicSymbolCreator option, parameters, inputs) = 
    let mutable disposed = false
    let mutable name = None
    let mutable initialized = false
    //TODO: pull this out as it's used all over
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
            | None -> name |> Option.map (fun n -> new SafeSymbolHandle(MXSymbol.createVariable n,true))
            | Some creator -> 
                let symbol = parametersStr |> Array.unzip ||> MXSymbol.createAtomicSymbol creator.AtomicSymbolCreatorHandle
                let name = defaultArg name null
                inputs 
                |> Array.choose (fun (k,v:Symbol) -> v.SymbolHandle |> Option.map (fun h -> k,h.UnsafeHandle))
                |> Array.unzip 
                ||> MXSymbol.compose symbol name
                Some(new SafeSymbolHandle(symbol, true))
    new(creator, parameterKeys, parameterValues, inputKeys, inputValues) =  
        new Symbol(creator, Array.zip parameterKeys parameterValues, Array.zip inputKeys inputValues)
    member x.Name 
        with get() = match name with Some n -> n | _ -> ""
        and set v = 
            if initialized then 
                failwith "Cannot set name. Symbol has already been created." //TODO: make exception
            name <- Some v 
    member x.WithName(name) = x.Name <- name; x
    member x.SymbolHandle : SafeSymbolHandle option = handle.Value
    static member Variable(name) = new Symbol(None, Array.empty, Array.empty, Name = name)
    static member Empty = new Symbol(None, Array.empty, Array.empty)
    member x.IsEmpty = name.IsNone && creator.IsNone
    member x.Dispose(disposing) = 
        if not disposed then 
            if disposing then 
                x.SymbolHandle |> Option.iter (fun x -> x.Dispose())
        disposed <- true
    member x.Dispose() = 
        x.Dispose(true)
        GC.SuppressFinalize(x)
    interface IDisposable with  
        member x.Dispose() = x.Dispose()
