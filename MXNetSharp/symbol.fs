namespace rec MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop

[<AbstractClass>]
type Symbol() =
    let mutable disposed = false
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
                let h = MXSymbol.getOutput x.UnsafeHandle i
                new SymbolOutput(x,new SafeSymbolHandle(h, true))
            )
    abstract member Initialize : unit -> unit
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

type SymbolOutput internal (parent) = 
    inherit Symbol()
    new(parent, handle) as this = 
        new SymbolOutput(parent) then 
            this.InternalHandle <- Some handle
    override x.Initialize() = ()
    

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

type ImplicitVariable() = 
    inherit Variable() 
      
type SymbolInitilizationException(symbol : Symbol, inner : Exception) =
    inherit Exception(sprintf "Init failed on symbol %O" symbol)
      
//TODO: We should add valiation to the specific symbol types
type SymbolOperator(creator : AtomicSymbolCreator, operatorArguments : Arguments<Symbol>) = 
    inherit Symbol()
    //let parametersStr = parameters |> Array.map (fun (k,v) -> k, Util.valueString v)
    new(name, args) = new SymbolOperator(AtomicSymbolCreator.FromName name, args)
    //new(creator,pnames,ps,inames,ins) = new SymbolOperator(creator, Array.zip pnames ps, Array.zip inames ins)
    override x.Initialize() =   
        match x.InternalHandle with 
        | Some _ -> ()
        | None ->
            try
                //TODO: We should maybe check the varArg count parameter is not specified. Generally this should never happen
                let inputKeys = ResizeArray()
                let inputValues = ResizeArray()
                let pKeys = ResizeArray()
                let pValues = ResizeArray()
                let name = defaultArg x.InternalName null
                for a in creator.Info.Arguments do  
                    let scc,v = operatorArguments.Args.TryGetValue a.Name
                    if scc then 
                        match v with 
                        | Input i -> 
                            inputKeys.Add a.Name
                            match i with 
                            | :? ImplicitVariable as v -> v.Name <- sprintf "%s_%s" name a.Name
                            | _ -> ()
                            inputValues.Add i
                        | VarArg (count,i) -> 
                            inputValues.AddRange i
                            pKeys.Add count
                            pValues.Add(i.Length.ValueString())
                        | Parameter (Some o) -> 
                            pKeys.Add a.Name
                            pValues.Add(o.ValueString())
                        | Parameter None -> ()
                    else 
                        match a.TypeInfo with
                        | "NDArray-or-Symbol" //TODO: I dont like this
                        | "Symbol" -> 
                            inputKeys.Add(a.Name)
                            let i = new ImplicitVariable()
                            i.Name <- sprintf "%s_%s" name a.Name
                            inputValues.Add(i)
                        | _ -> ()
                let symbol = 
                    let keys = pKeys.ToArray()
                    let vals = pValues.ToArray()
                    assert (keys.Length = vals.Length)
                    MXSymbol.createAtomicSymbol creator.AtomicSymbolCreatorHandle keys vals
                let ivals = inputValues |> Seq.map (fun i -> i.UnsafeHandle) |> Seq.toArray
                if inputKeys.Count <> inputValues.Count then 
                    MXSymbol.compose symbol name null ivals
                else //REVIEW: we could just never use keys
                    let keys = inputKeys.ToArray()
                    Seq.zip keys inputValues 
                    |> Seq.filter 
                        (fun (name,v) ->
                            match v with 
                            | :? ImplicitVariable -> false
                            | _ -> true
                        )
                    |> Seq.map (fun (name,v) -> name, v.UnsafeHandle)
                    |> Seq.toArray
                    |> Array.unzip
                    ||> MXSymbol.compose symbol name
                x.InternalHandle <- Some(new SafeSymbolHandle(symbol, true))
            with
            | e -> raise(SymbolInitilizationException(x, e))




type SymbolGroup<'a>(group : 'a, symbols : Symbol []) = 
    inherit Symbol()
    member x.Symbol = group
    member x.SymbolArray = symbols |> Array.copy
    override x.Initialize() =   
        match x.InternalHandle with 
        | Some _ -> ()
        | None -> 
            let symbol = symbols |> Array.map (fun x -> x.UnsafeHandle) |> MXSymbol.createGroup 
            x.InternalHandle <- Some(new SafeSymbolHandle(symbol, true))
