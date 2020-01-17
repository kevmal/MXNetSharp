namespace MXNetSharp 
open System
open MXNetSharp.Interop

type KVStoreType = 
    | Local
    | Device
    | NCCL 
    | DistSync
    | DistDeviceSync
    | DistAsync
    override x.ToString() = 
        match x with 
        | Local -> "local"
        | Device -> "device"
        | NCCL -> "nccl"
        | DistSync -> "dist_sync"
        | DistDeviceSync -> "dist_device_sync"
        | DistAsync -> "dist_async"
        
type KVStore(handle : SafeKVStoreHandle) = 
    let mutable disposed = false
    member x.KVStoreHandle = handle
    new(kvStoreTypeStr : string) = 
        let h = MXKVStore.create kvStoreTypeStr
        new KVStore(new SafeKVStoreHandle(h, true))
    new(kvStoreType : KVStoreType) = new KVStore(kvStoreType.ToString())
    member x.KVStoreTypeString = MXKVStore.getType handle.UnsafeHandle
    member x.Init(keys, values : NDArray []) = 
        let values = values |> Array.map (fun x -> x.UnsafeHandle)
        MXKVStore.init handle.UnsafeHandle keys values
    member x.Init(keys, values : NDArray []) = 
        let values = values |> Array.map (fun x -> x.UnsafeHandle)
        MXKVStore.initEx handle.UnsafeHandle keys values
    member x.Pull(keys, priority) = MXKVStore.pull handle.UnsafeHandle keys priority |> Array.map (fun h -> new NDArray(h))
    member x.Pull(keys : int []) = x.Pull(keys, 0)
    member x.Pull(keys, priority) = MXKVStore.pullEx handle.UnsafeHandle keys priority |> Array.map (fun h -> new NDArray(h))
    member x.Pull(keys : string []) = x.Pull(keys, 0)
    member x.Push(keys, values : NDArray[], priority) = 
        let values = values |> Array.map (fun x -> x.UnsafeHandle)
        MXKVStore.push handle.UnsafeHandle keys values priority 
    member x.Push(keys : int [], values : NDArray[]) = x.Push(keys, values, 0)
    member x.Push(keys, values : NDArray[], priority) = 
        let values = values |> Array.map (fun x -> x.UnsafeHandle)
        MXKVStore.pushEx handle.UnsafeHandle keys values priority 
    member x.Push(keys : string [], values) = x.Push(keys, values, 0)
    member x.SetGradientCompression(keys, values) = MXKVStore.setGradientCompression handle.UnsafeHandle keys values
    member x.Rank = MXKVStore.
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



    
    

