namespace MXNetSharp
open System
open MXNetSharp.Interop


type CachedOp private (handle : SafeCachedOpHandle, symbol : Symbol, flags : (string*string) []) = 
    let mutable disposed = false
    static member Create(symbol : Symbol, flags : (string*string) seq) =
        let flags = flags |> Seq.toArray 
        let handle = flags |> Array.unzip ||> MXCachedOp.createEx symbol.UnsafeHandle 
        new CachedOp(new SafeCachedOpHandle(handle, true), symbol, flags)
    static member Create(symbol : Symbol) = 
        let handle = MXCachedOp.create symbol.UnsafeHandle 
        new CachedOp(new SafeCachedOpHandle(handle, true), symbol, Array.empty)
    member x.Handle = handle
    member x.Symbol = symbol
    member x.Invoke([<ParamArray>] args : NDArray[]) = 
        let ins = args |> Array.map (fun x -> x.UnsafeHandle)
        MXCachedOp.invoke handle.UnsafeHandle ins
    member x.RegisterHook(monitorAll, f) = 
        let cb = 
            CApi.CachedOpMonitorCallback
                (fun a b h -> 
                    use arr = new NDArray(h)
                    f a b arr)
        MXCachedOp.registerOpHook handle.UnsafeHandle cb monitorAll
    member x.Dispose(disposing) = 
        if not disposed then 
            if disposing then 
                handle.Dispose()
        disposed <- true
    member x.Dispose() = 
        x.Dispose(true)
        GC.SuppressFinalize(x)
    interface IDisposable with  
        member x.Dispose() = x.Dispose()


[<AutoOpen>]
module CachedOpSymbolExtension =
    type Symbol with 
        member x.CachedOp() = CachedOp.Create(x)


