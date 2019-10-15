namespace rec MXNetSharp
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

type BaseSymbol() = class end


type EmptySymbol private () = 
    inherit BaseSymbol()
    static let instance = EmptySymbol()
    static member Instance = instance

[<AbstractClass>]
type Symbol() =
    inherit BaseSymbol()
    let mutable disposed = false
    //TODO: pull this out as it's used all over
    let str (v : obj) = 
        match v with 
        | :? bool as x -> if x then "1" else "0"
        | :? string as x -> sprintf "%s" x
        | x -> string x
    member val internal InternalName : string option = None with get,set
    member val internal InternalHandle : SafeSymbolHandle option = None with get,set
    member x.IsInitialized = x.InternalHandle.IsSome
    member x.Name 
        with get() = match x.InternalName with Some n -> n | _ -> ""
        and set v = 
            if x.IsInitialized then
                failwith "Cannot set name. Symbol has already been created." //TODO: make exception
            x.InternalName <- Some v 
    member x.WithName(name) = x.Name <- name; x
    member x.SymbolHandle : SafeSymbolHandle = 
        match x.InternalHandle with 
        | Some h -> h
        | None -> 
            x.Initialize()
            match x.InternalHandle with
            | Some h -> h
            | None -> failwithf "Failed to initialize Symbol %s" (defaultArg x.InternalName "") //TODO: make exception
    member x.UnsafeHandle = x.SymbolHandle.UnsafeHandle //REVIEW: mark as internal?
    member x.Outputs = 
        let make handle =
            let s = 
                {new Symbol() with 
                    override x.Initialize() = ()
                }
            s.InternalHandle <- Some(new SafeSymbolHandle(handle, true))
            s
        let n = MXSymbol.getNumOutputs x.UnsafeHandle |> int
        Array.init n 
            (fun i ->
                MXSymbol.getOutput x.UnsafeHandle i |> make
            )
    abstract member Initialize : unit -> unit
    static member Empty = EmptySymbol.Instance
    member x.Dispose(disposing) = 
        if not disposed then 
            if disposing then 
                match x.InternalHandle with 
                | Some h -> h.Dispose()
                | None -> ()
        disposed <- true
    member x.Dispose() = 
        x.Dispose(true)
        GC.SuppressFinalize(x)
    member x.ArgumentNames = MXSymbol.listArguments x.UnsafeHandle
    interface IDisposable with  
        member x.Dispose() = x.Dispose()

type Variable() =
    inherit Symbol()
    new (name : string) as this = 
        new Variable() then 
            this.InternalName <- Some name
    override x.Initialize() =   
        match x.InternalHandle with 
        | Some _ -> ()
        | None -> 
            match x.InternalName with 
            | Some n -> 
                x.InternalHandle <- Some(new SafeSymbolHandle(MXSymbol.createVariable n,true))
            | None -> failwith "Variable needs a name" //TODO: make exception or auto naming?


type SymbolFromOperator(creator : AtomicSymbolCreator, parameters, inputs) = 
    inherit Symbol()
    //TODO: pull this out as it's used all over
    let str (v : obj) = 
        match v with 
        | :? bool as x -> if x then "1" else "0"
        | :? string as x -> sprintf "%s" x
        | x -> string x
    let parametersStr = parameters |> Array.map (fun (k,v) -> k, str v)
    new(creator,pnames,ps,inames,ins) = new SymbolFromOperator(creator, Array.zip pnames ps, Array.zip inames ins)
    override x.Initialize() =   
        match x.InternalHandle with 
        | Some _ -> ()
        | None -> 
            let symbol = parametersStr |> Array.unzip ||> MXSymbol.createAtomicSymbol creator.AtomicSymbolCreatorHandle
            let name = defaultArg x.InternalName null
            inputs 
            |> Array.choose 
                (fun (k,v:BaseSymbol) -> 
                    if Object.ReferenceEquals(v, Symbol.Empty) then 
                        None
                    else 
                        Some(k, (v :?> Symbol).UnsafeHandle) )
            |> Array.unzip 
            ||> MXSymbol.compose symbol name
            x.InternalHandle <- Some(new SafeSymbolHandle(symbol, true))
    


type SymbolGroup<'a>(group : 'a, symbols : Symbol []) = 
    inherit Symbol()
    member x.Group = group
    member x.Symbols = symbols
    override x.Initialize() =   
        match x.InternalHandle with 
        | Some _ -> ()
        | None -> 
            let symbol = symbols |> Array.map (fun x -> x.UnsafeHandle) |> MXSymbol.createGroup 
            x.InternalHandle <- Some(new SafeSymbolHandle(symbol, true))
