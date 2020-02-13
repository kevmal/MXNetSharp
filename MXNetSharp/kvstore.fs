namespace MXNetSharp 
open System
open MXNetSharp.Interop
open System.Runtime.Serialization

type KVStoreType = 
    | Local
    | Device
    | NCCL 
    | DistSync
    | DistDeviceSync
    | DistAsync
    | Horovod
    override x.ToString() = 
        match x with 
        | Local -> "local"
        | Device -> "device"
        | NCCL -> "nccl"
        | DistSync -> "dist_sync"
        | DistDeviceSync -> "dist_device_sync"
        | DistAsync -> "dist_async"
        | Horovod -> "horovod"
type KVCommand = 
    | Controller 
    | SetMultiPrecision
    | StopServer
    | SyncMode
    | SetGradientCompression
    | SetProfilerParams
    member x.ToInt() = 
        match x with 
        | Controller -> 0
        | SetMultiPrecision -> 1
        | StopServer -> 2
        | SyncMode -> 3
        | SetGradientCompression -> 4
        | SetProfilerParams -> 5
    static member op_Explicit(st : KVCommand) = st.ToInt()

[<AutoOpen>]
module internal KVHelper = 
    let (|Dist|_|) (x : string) = if x.Contains("dist") then Some() else None
    let (|Device|_|) (x : string) = if x.Contains("device") then Some() else None
    

type IUpdater = 
    abstract member Invoke : index : string * grad : NDArray * weight : NDArray -> unit
    abstract member Invoke : index : int * grad : NDArray * weight : NDArray -> unit
type IOptimizer = 
    abstract member GetUpdater : unit -> IUpdater
    abstract member IsMultiPrecision : bool
    abstract member GetCommandString : unit -> string

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
    member x.Pull(key : int [], out, ?priority, ?ignoreSparse) = 
        let priority = defaultArg priority 0
        let ignoreSparse = defaultArg ignoreSparse false
        let a = out |> Array.map (fun (x : NDArray) -> x.UnsafeHandle)
        MXKVStore.pullWithSparse handle.UnsafeHandle key a priority ignoreSparse
    member x.Pull(key : string [], out, ?priority, ?ignoreSparse) = 
        let priority = defaultArg priority 0
        let ignoreSparse = defaultArg ignoreSparse false
        let a = out |> Array.map (fun (x : NDArray) -> x.UnsafeHandle)
        MXKVStore.pullWithSparseEx handle.UnsafeHandle key a priority ignoreSparse
    /// Performs push and pull a single value or a sequence of values from the store.
    member x.PushPull(key : int [], value : NDArray [], ?out, ?priority) = 
        let priority = defaultArg priority 0
        match out with
        | Some out ->
            let i = value |> Array.map (fun (x : NDArray) -> x.UnsafeHandle)
            let o = out |> Array.map (fun (x : NDArray) -> x.UnsafeHandle)
            MXKVStore.pushPull handle.UnsafeHandle key key i o priority
        | None -> 
            let i = value |> Array.map (fun (x : NDArray) -> x.UnsafeHandle)
            MXKVStore.pushPull handle.UnsafeHandle key key i i priority
    /// Performs push and pull a single value or a sequence of values from the store.
    member x.PushPull(key : string [], value : NDArray [], ?out, ?priority) = 
        let priority = defaultArg priority 0
        match out with
        | Some out ->
            let i = value |> Array.map (fun (x : NDArray) -> x.UnsafeHandle)
            let o = out |> Array.map (fun (x : NDArray) -> x.UnsafeHandle)
            MXKVStore.pushPullEx handle.UnsafeHandle key key i o priority
        | None -> 
            let i = value |> Array.map (fun (x : NDArray) -> x.UnsafeHandle)
            MXKVStore.pushPullEx handle.UnsafeHandle key key i i priority
    member x.RowSparsePull(key, out, rowIds, ?priority) = 
        let priority = defaultArg priority 0
        let out = out |> Array.map (fun (x : NDArray) -> x.UnsafeHandle)
        let rowIds = rowIds |> Array.map (fun (x : NDArray) -> x.UnsafeHandle)
        MXKVStore.pullRowSparse handle.UnsafeHandle key out rowIds priority
    member x.SetGradientCompression(keys, values) = 
        match x.KVStoreTypeString  with 
        | Device | Dist -> 
            MXKVStore.setGradientCompression handle.UnsafeHandle keys values
        | tp -> 
            invalidOp (sprintf "SetGradientCompression is not supported for this type of kvstore: %s" tp)
    member x.IsWorkerNode = MXKVStore.isWorkerNode() <> 0
    /// Registers an optimizer with the kvstore.
    member x.SetOptimizer(optimizer : IOptimizer) = 
        match x.KVStoreTypeString with 
        | Dist when x.IsWorkerNode ->
            let ostr = optimizer.GetCommandString()
            x.SendCommandToServers(KVCommand.Controller, ostr)
            if optimizer.IsMultiPrecision then 
                x.SendCommandToServers(KVCommand.SetMultiPrecision, "")
        | _ -> 
            x.SetUpdater(optimizer.GetUpdater())
            
    /// Broadcast the `value` NDArray at rank 0 to all ranks, and store the result in `out`.
    /// Note that the native KVStore does not support broadcasting the same key more than once.
    member x.Broadcast(key : string, value, out, ?priority) = 
        let priority = defaultArg priority 0
        x.Init([|key|],[|value|])
        x.Pull([|key|],[|out|],priority)
    member x.Push(keys, values : NDArray[], ?priority) = 
        let priority = defaultArg priority 0
        let values = values |> Array.map (fun x -> x.UnsafeHandle)
        MXKVStore.push handle.UnsafeHandle keys values priority 
    member x.Push(keys, values : NDArray[], ?priority) = 
        let priority = defaultArg priority 0
        let values = values |> Array.map (fun x -> x.UnsafeHandle)
        MXKVStore.pushEx handle.UnsafeHandle keys values priority 
    /// The rank of this node, which is in range [0, NumberOfWorkers)
    member x.Rank = MXKVStore.getRank handle.UnsafeHandle
    /// The number of worker nodes.
    member x.NumberOfWorkers = MXKVStore.getGroupSize handle.UnsafeHandle
    /// Saves the optimizer (updater) state to a file. This is often used when checkpointing
    /// the model during training.
    member x.SaveOptimizerStates(fileName : string, dumpOptimizer : bool) = failwith "to impl" //TODO:
    member x.LoadOptimizerStates(fileName : string) = failwith "to impl"
    /// Sets a push updater into the store. 
    /// This function only changes the local store. When running on multiple machines one must
    /// use `SetOptimizer`.
    member x.SetUpdater(updaterInt, updaterStr, arg) = 
        MXKVStore.setUpdaterEx handle.UnsafeHandle updaterInt updaterStr arg
    /// Sets a push updater into the store. 
    /// This function only changes the local store. When running on multiple machines one must
    /// use `SetOptimizer`.
    member x.SetUpdater(updater : IUpdater) = 
        let u1 = CApi.MXKVStoreUpdater(fun i g w _ -> updater.Invoke(i,new NDArray(g),new NDArray(w)))
        let u2 = CApi.MXKVStoreStrUpdater(fun i g w _ -> updater.Invoke(i,new NDArray(g),new NDArray(w)))
        MXKVStore.setUpdaterEx handle.UnsafeHandle u1 u2 0n
    /// Invokes global barrier among all worker nodes.
    /// For example, assume there are `n` machines. We would like machine `0` to first
    /// `init` the values and then have all the workers `pull` the initialized value.
    /// Before pulling, we can place invoke `Barrier()` to guarantee that the
    /// initialization is finished.
    member x.Barrier() = MXKVStore.barrier handle.UnsafeHandle
    /// Sends a command to all server nodes.
    /// Sending command to a server node will cause that server node to invoke
    /// ``KVStoreServer.controller`` to execute the command.
    /// This function returns after the command has been executed on all server
    /// nodes.
    member x.SendCommandToServers(head : int, body : string) = 
        MXKVStore.sendCommmandToServers handle.UnsafeHandle head body
    member x.SendCommandToServers(head:KVCommand, body : string) = x.SendCommandToServers(head.ToInt(), body)
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



    
    

