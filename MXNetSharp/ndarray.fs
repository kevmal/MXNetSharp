namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop



module internal Helper = 
    let inline (<--) x y = x, Util.valueString y
        
open Helper

type NDArray(handle : SafeNDArrayHandle) = 
    let mutable disposed = false
    static let noarg = new NDArray()
    static let invoke opName inputs parameters =
        let creator = AtomicSymbolCreator.FromName opName
        let inputs = inputs |> Array.map (fun (x : NDArray) -> (x.NDArrayHandle : SafeNDArrayHandle).UnsafeHandle)
        let pkeys,pvals = parameters |> Array.unzip
        MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle inputs pkeys pvals
        |> Array.map (fun x -> new NDArray(x))
    static let invoke1 opName inputs parameters = invoke opName inputs parameters |> Array.head
    static let mutInvoke (out : NDArray) opName inputs parameters =
        let creator = AtomicSymbolCreator.FromName opName
        let inputs = inputs |> Array.map (fun (x : NDArray) -> (x.NDArrayHandle : SafeNDArrayHandle).UnsafeHandle)
        let pkeys,pvals = parameters |> Array.unzip
        let outcount = MXNDArray.imperativeInvokeInto creator.AtomicSymbolCreatorHandle inputs [|(out.NDArrayHandle : SafeNDArrayHandle).UnsafeHandle|] pkeys pvals
        assert(outcount = 1)
        out
    internal new(h : CApi.NDArrayHandle) = new NDArray(new SafeNDArrayHandle(h, true))
    new() = 
        let h1 = MXNDArray.createNone()
        new NDArray(h1)
    new(shape : int seq, context : Context, ?dtype, ?delayAlloc) = 
        let dtype = defaultArg dtype Float32
        let delayAlloc = defaultArg delayAlloc true
        let shape = shape |> Seq.toArray
        new NDArray(MXNDArray.createEx shape context.DeviceType context.DeviceId delayAlloc dtype.TypeFlag)
    static member NoArg = noarg
    static member ConvertCopyFrom(data : 'aa[], shape : int seq, ctx : Context, ?dtype : DataType) =
        let shape = 
            let shape = shape |> Seq.toArray
            let mutable idx = -1
            let mutable sz = 1
            for i = 0 to shape.Length - 1 do 
                if shape.[i] = -1 then 
                    if idx >= 0 then 
                        invalidArg "shape" (sprintf "Invalid shape %A. At most one dimension of shape can be -1" shape)
                    idx <- i
                else    
                    sz <- sz * shape.[i]
            if idx >= 0 then 
                let n, r = Math.DivRem(data.Length, sz)
                if r <> 0 then 
                    invalidArg "shape" (sprintf "Invalid shape %A. Could not infer -1 dimension." shape)
                shape.[idx] <- n
                shape
            elif sz <> data.Length then 
                invalidArg "shape" (sprintf "shape %A with length %d does not match array length %d" shape sz data.Length)
            else
                shape
        let dtype = defaultArg dtype DataType.Float32
        let a = new NDArray(shape, ctx, dtype, false)
        a.CopyFrom(data)
        a

        
    member x.NDArrayHandle = handle
    member x.UnsafeHandle = x.NDArrayHandle.UnsafeHandle

    member x.GetAuxType(index : int64) = MXNDArray.getAuxType64 handle.UnsafeHandle index |> DataType.FromTypeFlag
    member x.GetAuxType(index : int) = x.GetAuxType(int64 index)

    /// Get a deep copy of the ith aux data blob
    /// in the form of an NDArray of default storage type.
    /// This function blocks. Do not use it in performance critical code.
    member x.GetAuxCopy(index : int64) = 
        let handle = MXNDArray.getAuxNDArray64 handle.UnsafeHandle index
        new NDArray(new SafeNDArrayHandle(handle, true))
    member x.GetAuxCopy(index : int) = x.GetAuxCopy(int64 index)

    /// Attach a gradient buffer to this NDArray, so that `backward`
    /// can compute gradient with respect to it.
    /// The gradient is initialized to zeros.
    member x.AttachGradient([<Optional>] ?gradReq, [<Optional>] ?storageType) = 
        let gradReq = defaultArg gradReq OpReqType.WriteTo
        let grad = 
            match storageType with 
            | Some (StorageType.Default | StorageType.Undefined) -> 
                match x.DataType with 
                | Some dtype -> 
                    invoke1 "_zeros" Array.empty [|"shape" <-- x.Shape; "ctx" <-- x.Context; "dtype" <-- dtype |]
                | None ->
                    invoke1 "_zeros" Array.empty [|"shape" <-- x.Shape; "ctx" <-- x.Context |]
            | Some stype ->  //TODO: pull this out into general ctor for sparse ndarray
                let types,shapes = 
                    match stype with 
                    | CSR -> [|int TypeFlag.Int64; int TypeFlag.Int64|], [|[|0u|] ; [|0u|]|]
                    | RowSparse -> [|int TypeFlag.Int64|], [|[|0u|]|]
                    | StorageType.Default 
                    | StorageType.Undefined -> failwith "Unreachable. Default/Undefined case matched"
                let dtype : TypeFlag = x.DataTypeFlag
                let ctx : Context = x.Context
                let handle = 
                    MXNDArray.createSparseEx
                        (int stype)
                        (x.Shape |> Array.map uint32)
                        (int ctx.DeviceType)
                        ctx.DeviceId
                        1 //delay alloc REVIEW: 
                        (int dtype)
                        types 
                        shapes
                let a = new NDArray(new SafeNDArrayHandle(handle, true))
                mutInvoke a "_zeros" [|a|] Array.empty
            | None -> invoke1 "zeros_like" [|x|] Array.empty
        MXAutograd.markVariables [|x.UnsafeHandle|] [|uint32 gradReq|] [|grad.UnsafeHandle|]
    member x.Backward(?outGrad, ?retainGraph, ?train, ?createGraph) = 
        let retainGraph = defaultArg retainGraph false
        let train = defaultArg train true
        let createGraph = defaultArg createGraph false
        let ograd = 
            match outGrad with
            | Some (a : NDArray) -> [| a.UnsafeHandle |]
            | None -> [| 0n |]
        let h,st = MXAutograd.backwardEx [|x.UnsafeHandle|] ograd Array.empty retainGraph createGraph train
        ()

    member x.Grad = 
        let handle = MXNDArray.getGrad handle.UnsafeHandle 
        if handle > 0n then 
            Some(new NDArray(new SafeNDArrayHandle(handle, true)))
        else 
            None
    member x.DataType = MXNDArray.getDType handle.UnsafeHandle |> DataType.FromTypeFlag
    member x.DataTypeFlag = MXNDArray.getDType handle.UnsafeHandle
    member x.StorageType = MXNDArray.getStorageType handle.UnsafeHandle |> StorageType.FromInt
    member x.Shape = MXNDArray.getShape handle.UnsafeHandle
    member x.Size = x.Shape |> Array.reduce (*)
    member x.Context = 
        let struct(dt, id) = MXNDArray.getContext handle.UnsafeHandle
        Context.FromDeviceTypeAndId(dt,id)

    member x.CopyTo(destination : NDArray) = mutInvoke destination "_copyto" [|x|] Array.empty |> ignore

    member x.CopyTo(deviceContext : Context) = 
        let destination = new NDArray(x.Shape, deviceContext, delayAlloc = true)
        x.CopyTo(destination)
        destination

    member x.SyncCopyFromCPUUnchecked(data : Array) = MXNDArray.syncCopyFromCPUArray handle.UnsafeHandle data
    member x.SyncCopyFromCPU(data : Array) = 
        if x.DataType = DataType.TryFromNetType(data.GetType().GetElementType()) then 
            MXNDArray.syncCopyFromCPUArray handle.UnsafeHandle data
        else
            raise (InvalidOperationException(sprintf "Source type of %s does not match NDArray type of %O" (data.GetType().FullName) x.DataTypeFlag))

    member x.SyncCopyFromCPUUnchecked(data : 'a []) = MXNDArray.syncCopyFromCPU handle.UnsafeHandle data
    member x.SyncCopyFromCPU(data : 'a []) = 
        if x.DataType = DataType.TryFromNetType<'a>() then 
            MXNDArray.syncCopyFromCPU handle.UnsafeHandle data
        else
            raise (InvalidOperationException(sprintf "Source type of %s does not match NDArray type of %O" typeof<'a>.FullName x.DataTypeFlag))

    member x.CopyFrom(data : NDArray) = data.CopyTo(x)

    member x.CopyFrom(data : Array) = 
        let etype = data.GetType().GetElementType()
        let dtype = DataType.FromNetType(etype)
        match x.DataType with 
        | None -> failwith "NDArray has no data type"
        | Some t when t = dtype -> x.SyncCopyFromCPUUnchecked(data)
        | Some _ -> 
            use nd : NDArray = NDArray.CopyFrom(data, x.Context)
            nd.CopyTo(x) |> ignore
            
    member x.CopyFrom(data : 'aa []) = 
        match x.DataType with 
        | None -> // REVIEW: what to do here
            failwith "NDArray has no data type"
        | Some t -> 
            let a = data
            match t with 
            | Float16 -> 
                use nd : NDArray = NDArray.CopyFrom(data, x.Context)
                nd.CopyTo(x) |> ignore
            | Float32 -> ArrayConverter.Float32(a) |> x.SyncCopyFromCPUUnchecked
            | Float64 -> ArrayConverter.Float64(a) |> x.SyncCopyFromCPUUnchecked
            | Int32 -> ArrayConverter.Int32(a) |> x.SyncCopyFromCPUUnchecked
            | Int64 -> ArrayConverter.Int64(a) |> x.SyncCopyFromCPUUnchecked
            | Int8 -> ArrayConverter.Int8(a) |> x.SyncCopyFromCPUUnchecked
            | UInt8 -> ArrayConverter.UInt8(a) |> x.SyncCopyFromCPUUnchecked
            | Bool -> ArrayConverter.Bool(a) |> x.SyncCopyFromCPUUnchecked

    static member CopyFrom(data : Array, ctx : Context) = 
        let etype = data.GetType().GetElementType()
        let dtype = DataType.FromNetType(etype)
        let shape = 
            Array.init data.Rank 
                (fun d -> 
                    let l = data.GetLowerBound(d)
                    let u = data.GetUpperBound(d)
                    u - l + 1
                )
        let a = new NDArray(shape, ctx, dtype, false)
        a.CopyFrom(data)
        a

    static member CopyFrom(data : NDArray, ctx : Context) = data.CopyTo(ctx)        
    static member CopyFrom(data : NDArray) = NDArray.CopyFrom(data, data.Context)        
    
        
    static member CopyFrom(data : float32[], shape : int seq, ctx : Context) = NDArray.ConvertCopyFrom(data,shape,ctx,DataType.Float32)
    static member CopyFrom(data : float[], shape : int seq, ctx : Context) = NDArray.ConvertCopyFrom(data,shape,ctx,DataType.Float64)
    static member CopyFrom(data : int[], shape : int seq, ctx : Context) = NDArray.ConvertCopyFrom(data,shape,ctx,DataType.Int32)
    static member CopyFrom(data : int64[], shape : int seq, ctx : Context) = NDArray.ConvertCopyFrom(data,shape,ctx,DataType.Int64)
    static member CopyFrom(data : int8[], shape : int seq, ctx : Context) = NDArray.ConvertCopyFrom(data,shape,ctx,DataType.Int8)
    static member CopyFrom(data : uint8[], shape : int seq, ctx : Context) = NDArray.ConvertCopyFrom(data,shape,ctx,DataType.UInt8)
    
    member x.SyncCopyFromCPU(data : int []) = MXNDArray.syncCopyFromCPU handle.UnsafeHandle data

    member x.Set(value : float32) =
        let setValue = AtomicSymbolCreator.FromName "_set_value"
        MXNDArray.imperativeInvokeInto setValue.AtomicSymbolCreatorHandle null [|handle.UnsafeHandle|] [|"src"|] [|value.ToString()|]
        |> ignore
    static member Load(file : string) = 
        let names,handles = MXNDArray.load file
        let arrs = handles |> Array.map (fun h -> new NDArray(h))
        Array.zip names arrs |> dict
    static member WaitAll() = MXNDArray.waitAll()
    
    
    static member (+)(x : NDArray, y : float) = invoke1 "_plus_scalar" [|x|] [|"scalar" <-- y|]
    static member (+)(y : float, x : NDArray) = invoke1 "_plus_scalar" [|x|] [|"scalar" <-- y|]
    static member (+)(x : NDArray, y : NDArray) = invoke1 "elemwise_add" [|x; y|] Array.empty 
    static member (.+)(x : NDArray, y : NDArray) = invoke1 "broadcast_plus" [|x; y|] Array.empty 
    member x.MutPlus(y : float) = mutInvoke x "_plus_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutPlus(y : NDArray) = mutInvoke x "elemwise_add" [|x; y|] Array.empty 
    member x.MutPlusBroadcast(y : NDArray) = mutInvoke x "broadcast_add" [|x; y|] Array.empty 

   
    static member (-)(x : NDArray, y : float) = invoke1 "_minus_scalar" [|x|] [|"scalar" <-- y|]
    static member (-)(y : float, x : NDArray) = invoke1 "_rminus_scalar" [|x|] [|"scalar" <-- y|]
    static member (-)(x : NDArray, y : NDArray) = invoke1 "elemwise_sub" [|x; y|] Array.empty 
    static member (.-)(x : NDArray, y : NDArray) = invoke1 "broadcast_sub" [|x; y|] Array.empty 
    member x.MutSubstract(y : float) = mutInvoke x "_minus_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutSubstract(y : NDArray) = mutInvoke x "elemwise_sub" [|x; y|] Array.empty 
    member x.MutSubstractFrom(y : float) = mutInvoke x "_rminus_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutSubstractFrom(y : NDArray) = mutInvoke x "elemwise_sub" [|y; x|] Array.empty 
    member x.MutSubstractBroadcast(y : NDArray) = mutInvoke x "broadcast_sub" [|x; y|] Array.empty 
    member x.MutSubstractBroadcastFrom(y : NDArray) = mutInvoke x "broadcast_sub" [|y; x|] Array.empty  
 
 
    static member (/)(x : NDArray, y : float) = invoke1 "_div_scalar" [|x|] [|"scalar" <-- y|]
    static member (/)(y : float, x : NDArray) = invoke1 "_rdiv_scalar" [|x|] [|"scalar" <-- y|]
    static member (/)(x : NDArray, y : NDArray) = invoke1 "elemwise_div" [|x; y|] Array.empty 
    static member (./)(x : NDArray, y : NDArray) = invoke1 "broadcast_div" [|x; y|] Array.empty 
    member x.MutDividedBy(y : float) = mutInvoke x "_div_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutDividedBy(y : NDArray) = mutInvoke x "elemwise_div" [|x; y|] Array.empty 
    member x.MutDividedInto(y : float) = mutInvoke x "_rdiv_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutDividedInto(y : NDArray) = mutInvoke x "elemwise_div" [|y; x|] Array.empty 
    member x.MutDividedBroadcastBy(y : NDArray) = mutInvoke x "broadcast_div" [|x; y|] Array.empty 
    member x.MutDividedBroadcastInto(y : NDArray) = mutInvoke x "broadcast_div" [|y; x|] Array.empty 


    static member ( * )(x : NDArray, y : float) = invoke1 "_mul_scalar" [|x|] [|"scalar" <-- y|]
    static member ( * )(y : float, x : NDArray) = invoke1 "_mul_scalar" [|x|] [|"scalar" <-- y|]
    static member ( * )(x : NDArray, y : NDArray) = invoke1 "elemwise_mul" [|x; y|] Array.empty 
    static member ( .* )(x : NDArray, y : NDArray) = invoke1 "broadcast_mul" [|x; y|] Array.empty 
    member x.MutMultiply(y : float) = mutInvoke x "_mul_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutMultiply(y : NDArray) = mutInvoke x "elemwise_mul" [|x; y|] Array.empty 
    member x.MutMultiplyBroadcast(y : NDArray) = mutInvoke x "broadcast_mul" [|x; y|] Array.empty 

       
    static member Pow(x : NDArray, y : float) = invoke1 "_power_scalar" [|x|] [|"scalar" <-- y|]
    static member Pow(y : float, x : NDArray) = invoke1 "_rpower_scalar" [|x|] [|"scalar" <-- y|]
    static member Pow(x : NDArray, y : NDArray) = invoke1 "_power" [|x; y|] Array.empty 
    //static member ApplyPow(x : NDArray, y : float) = NDArray.Pow(x,y)
    static member ApplyPow(x : float, y : NDArray) = NDArray.Pow(x,y)
    //static member ApplyPow(x : NDArray, y : NDArray) = NDArray.Pow(x,y)
    static member ( .** )(x : NDArray, y : NDArray) = invoke1 "broadcast_power" [|x; y|] Array.empty 
    //member x.ApplyPower(y : float) = NDArray.Pow(x,y)
    //member x.ApplyPowerBaseOf(y : float) = NDArray.Pow(y,x)
    member x.MutPower(y : float) = mutInvoke x "_power_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutPower(y : NDArray) = mutInvoke x "_power" [|x; y|] Array.empty 
    member x.MutPowerBaseOf(y : float) = mutInvoke x "_rpower_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutPowerBaseOf(y : NDArray) = mutInvoke x "_power" [|y; x|] Array.empty 
    member x.MutPowerBroadcast(y : NDArray) = mutInvoke x "broadcast_power" [|x; y|] Array.empty 
    member x.MutPowerBaseOfBroadcast(y : NDArray) = mutInvoke x "broadcast_power" [|y; x|] Array.empty 


    member x.Negate() = -1.0*x
    static member (~-)(x : NDArray) = x.Negate()
    member x.MutNegate() = x.MutMultiply(-1.0)

    static member (%)(x : NDArray, y : float) = invoke1 "_mod_scalar" [|x|] [|"scalar" <-- y|]
    static member (%)(y : float, x : NDArray) = invoke1 "_rmod_scalar" [|x|] [|"scalar" <-- y|]
    static member (%)(x : NDArray, y : NDArray) = invoke1 "broadcast_mod" [|x; y|] Array.empty 
    member x.MutMod(y : float) = mutInvoke x "_mod_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutMod(y : NDArray) = mutInvoke x "broadcast_mod" [|x; y|] Array.empty 
    member x.MutModOf(y : float) = mutInvoke x "_rmod_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutModOf(y : NDArray) = mutInvoke x "broadcast_mod" [|y; x|] Array.empty 

    static member (.=)(x : NDArray, y : float) = invoke1 "_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.=)(y : float, x : NDArray) = invoke1 "_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.=)(x : NDArray, y : NDArray) = invoke1 "broadcast_equal" [|x; y|] Array.empty 
    member x.MutEqual(y : float) = mutInvoke x "_equal_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutEqual(y : NDArray) = mutInvoke x "broadcast_equal" [|x; y|] Array.empty 

    static member (.<>)(x : NDArray, y : float) = invoke1 "_not_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.<>)(y : float, x : NDArray) = invoke1 "_not_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.<>)(x : NDArray, y : NDArray) = invoke1 "broadcast_not_equal" [|x; y|] Array.empty 
    member x.MutNotEqual(y : float) = mutInvoke x "_not_equal_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutNotEqual(y : NDArray) = mutInvoke x "broadcast_not_equal" [|x; y|] Array.empty 

    static member (.>)(x : NDArray, y : float) = invoke1 "_greater_scalar" [|x|] [|"scalar" <-- y|]
    static member (.>)(y : float, x : NDArray) = invoke1 "_lesser_scalar" [|x|] [|"scalar" <-- y|]
    static member (.>)(x : NDArray, y : NDArray) = invoke1 "broadcast_greater" [|x; y|] Array.empty 
    member x.MutGreater(y : float) = mutInvoke x "_greater_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutGreater(y : NDArray) = mutInvoke x "broadcast_greater" [|x; y|] Array.empty 

    static member (.<)(x : NDArray, y : float) = invoke1 "_lesser_scalar" [|x|] [|"scalar" <-- y|]
    static member (.<)(y : float, x : NDArray) = invoke1 "_greater_scalar" [|x|] [|"scalar" <-- y|]
    static member (.<)(x : NDArray, y : NDArray) = invoke1 "broadcast_lesser" [|x; y|] Array.empty 
    member x.MutLesser(y : float) = mutInvoke x "_lesser_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutLesser(y : NDArray) = mutInvoke x "broadcast_lesser" [|x; y|] Array.empty 

    static member (.>=)(x : NDArray, y : float) = invoke1 "_greater_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.>=)(y : float, x : NDArray) = invoke1 "_lesser_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.>=)(x : NDArray, y : NDArray) = invoke1 "broadcast_greater_equal" [|x; y|] Array.empty 
    member x.MutGreaterOrEqual(y : float) = mutInvoke x "_greater_equal_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutGreaterOrEqual(y : NDArray) = mutInvoke x "broadcast_greater_equal" [|x; y|] Array.empty 

    static member (.<=)(x : NDArray, y : float) = invoke1 "_lesser_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.<=)(y : float, x : NDArray) = invoke1 "_greater_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.<=)(x : NDArray, y : NDArray) = invoke1 "broadcast_lesser_equal" [|x; y|] Array.empty 
    member x.MutLesserOrEqual(y : float) = mutInvoke x "_lesser_equal_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutLesserOrEqual(y : NDArray) = mutInvoke x "broadcast_lesser_equal" [|x; y|] Array.empty 

 
    static member (.&&)(x : NDArray, y : float) = invoke1 "_logical_and_scalar" [|x|] [|"scalar" <-- y|]
    static member (.&&)(y : float, x : NDArray) = invoke1 "_logical_and_scalar" [|x|] [|"scalar" <-- y|]
    static member (.&&)(x : NDArray, y : NDArray) = invoke1 "_logical_and" [|x; y|] Array.empty 
    static member (..&&)(x : NDArray, y : NDArray) = invoke1 "broadcast_logical_and" [|x; y|] Array.empty 
    member x.MutLogicalAnd(y : float) = mutInvoke x "_logical_and_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutLogicalAnd(y : NDArray) = mutInvoke x "_logical_and" [|x; y|] Array.empty 
    member x.MutLogicalAndBroadcast(y : NDArray) = mutInvoke x "broadcast_logical_and" [|x; y|] Array.empty 
 

    static member (.||)(x : NDArray, y : float) = invoke1 "_logical_or_scalar" [|x|] [|"scalar" <-- y|]
    static member (.||)(y : float, x : NDArray) = invoke1 "_logical_or_scalar" [|x|] [|"scalar" <-- y|]
    static member (.||)(x : NDArray, y : NDArray) = invoke1 "_logical_or" [|x; y|] Array.empty 
    static member (..||)(x : NDArray, y : NDArray) = invoke1 "broadcast_logical_or" [|x; y|] Array.empty 
    member x.MutLogicalOr(y : float) = mutInvoke x "_logical_or_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutLogicalOr(y : NDArray) = mutInvoke x "_logical_or" [|x; y|] Array.empty 
    member x.MutLogicalOrBroadcast(y : NDArray) = mutInvoke x "broadcast_logical_or" [|x; y|] Array.empty 

    static member (.^^)(x : NDArray, y : float) = invoke1 "_logical_xor_scalar" [|x|] [|"scalar" <-- y|]
    static member (.^^)(y : float, x : NDArray) = invoke1 "_logical_xor_scalar" [|x|] [|"scalar" <-- y|]
    static member (.^^)(x : NDArray, y : NDArray) = invoke1 "_logical_xor" [|x; y|] Array.empty 
    static member (..^^)(x : NDArray, y : NDArray) = invoke1 "broadcast_logical_xor" [|x; y|] Array.empty 
    member x.MutLogicalXor(y : float) = mutInvoke x "_logical_xor_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutLogicalXor(y : NDArray) = mutInvoke x "_logical_xor" [|x; y|] Array.empty 
    member x.MutLogicalXorBroadcast(y : NDArray) = mutInvoke x "broadcast_logical_xor" [|x; y|] Array.empty 


    member x.Exp() = invoke1 "exp" [|x|] Array.empty
    member x.MutExp() = mutInvoke x "exp" [|x|] Array.empty 
    static member Exp(x : NDArray) = x.Exp()
    member x.Log() = invoke1 "log" [|x|] Array.empty
    member x.MutLog() = mutInvoke x "log" [|x|] Array.empty
    static member Log(x : NDArray) = x.Log()
    member x.Abs() = invoke1 "abs" [|x|] Array.empty
    member x.MutAbs() = mutInvoke x "abs" [|x|] Array.empty
    static member Abs(x : NDArray) = x.Abs()
    member x.Acos() = invoke1 "arccos" [|x|] Array.empty
    member x.MutAcos() = mutInvoke x "arccos" [|x|] Array.empty
    static member Acos(x : NDArray) = x.Acos()
    member x.Asin() = invoke1 "arcsin" [|x|] Array.empty
    member x.MutAsin() = mutInvoke x "arcsin" [|x|] Array.empty
    static member Asin(x : NDArray) = x.Asin()
    member x.Atan() = invoke1 "arctan" [|x|] Array.empty
    member x.MutAtan() = mutInvoke x "arctan" [|x|] Array.empty
    static member Atan(x : NDArray) = x.Atan()
    //static member Atan2(x : #NDArray, y : #NDArray) = new NpiArctan2(x, y)  // TODO: Atan2 for NDArray
    //static member Atan2(x : #NDArray, y : double) = new NpiArctan2Scalar(x, y) 
    //static member Atan2(y : double, x : #NDArray) = new NpiRarctan2Scalar(x, y) 
    //static member ArcTan2(x : #NDArray, y : #NDArray) = new NpiArctan2(x, y) 
    //static member ArcTan2(x : #NDArray, y : double) = new NpiArctan2Scalar(x, y) 
    //static member AtcTan2(y : double, x : #NDArray) = new NpiRarctan2Scalar(x, y) 
    member x.Ceiling() = invoke1 "ceil" [|x|] Array.empty
    member x.MutCeiling() = mutInvoke x "ceil" [|x|] Array.empty
    static member Ceiling(x : NDArray) = x.Ceiling()
    member x.Floor() = invoke1 "floor" [|x|] Array.empty
    member x.MutFloor() = mutInvoke x "floor" [|x|] Array.empty
    static member Floor(x : NDArray) = x.Floor()
    member x.Truncate() = invoke1 "trunc" [|x|] Array.empty
    member x.MutTruncate() = mutInvoke x "trunc" [|x|] Array.empty
    static member Truncate(x : NDArray) = x.Truncate()
    member x.Round() = invoke1 "round" [|x|] Array.empty
    member x.MutRound() = mutInvoke x "round" [|x|] Array.empty
    static member Round(x : NDArray) = x.Round()
    member x.Log10() = invoke1 "log10" [|x|] Array.empty
    member x.MutLog10() = mutInvoke x "log10" [|x|] Array.empty
    static member Log10(x : NDArray) = x.Log10()
    member x.Sqrt() = invoke1 "sqrt" [|x|] Array.empty
    member x.MutSqrt() = mutInvoke x "sqrt" [|x|] Array.empty
    static member Sqrt(x : NDArray) = x.Sqrt()
    member x.Cos() = invoke1 "cos" [|x|] Array.empty
    member x.MutCos() = mutInvoke x "cos" [|x|] Array.empty
    static member Cos(x : NDArray) = x.Cos()
    member x.Cosh() = invoke1 "cosh" [|x|] Array.empty
    member x.MutCosh() = mutInvoke x "cosh" [|x|] Array.empty
    static member Cosh(x : NDArray) = x.Cosh()
    member x.Sin() = invoke1 "sin" [|x|] Array.empty
    member x.MutSin() = mutInvoke x "sin" [|x|] Array.empty
    static member Sin(x : NDArray) = x.Sin()
    member x.Sinh() = invoke1 "sinh" [|x|] Array.empty
    member x.MutSinh() = mutInvoke x "sinh" [|x|] Array.empty
    static member Sinh(x : NDArray) = x.Sinh()
    member x.Tan() = invoke1 "tan" [|x|] Array.empty
    member x.MutTan() = mutInvoke x "tan" [|x|] Array.empty
    static member Tan(x : NDArray) = x.Tan()
    member x.Tanh() = invoke1 "tanh" [|x|] Array.empty
    member x.MutTanh() = mutInvoke x "tanh" [|x|] Array.empty
    static member Tanh(x : NDArray) = x.Tanh()

    member x.Slice(startIndices, endIndices, stepIndices) = 
        let b = startIndices |> String.concat "," |> sprintf "(%s)"
        let e = endIndices |> String.concat "," |> sprintf "(%s)"
        let s = stepIndices |> String.concat "," |> sprintf "(%s)"
        invoke "slice" [|x|] [|"begin", b; "end", e; "step", s|]|> Array.head
    member x.At(index : int) = new NDArray(MXNDArray.at x.UnsafeHandle index)
    member internal x.SetItemAtIntex(a : int [], v : obj) = 
        let b = Array.zeroCreate (a.Length + 1)
        for i = 0 to a.Length - 1 do b.[i] <- a.[i] :> obj
        b.[b.Length - 1] <- v
        x.SetSlice(a = b)
    member x.Item 
        with get([<ParamArray>] a : int [])  = (x, a) ||> Array.fold (fun x i -> x.At(i))
        and set ([<ParamArray>] a : int []) (v : double) = x.SetItemAtIntex(a,v)
    member x.Item 
        with set ([<ParamArray>] a : int []) (v : decimal) = x.SetItemAtIntex(a,v)
    member x.Item 
        with set ([<ParamArray>] a : int []) (v : float32) = x.SetItemAtIntex(a,v)
    member x.Item 
        with set ([<ParamArray>] a : int []) (v : int) = x.SetItemAtIntex(a,v)
        
    member x.GetSlice([<ParamArray>] a : obj []) =  //TODO: support all indexing types
        let inline str x = match x with ValueSome v -> v.ValueString() | _ -> "None"
        if a.Length = 0 then 
            x 
        else
            let shape = x.Shape
            let ndim = shape.Length
            let b = ResizeArray<int64 voption>()
            let e = ResizeArray<int64 voption>()
            let s = ResizeArray<int64 voption>()
            let sliceAxis = ResizeArray()
            let newAxis = ResizeArray()
            let mutable i = 0
            let mutable sliceAx = 0
            while i < a.Length do 
                match a.[i] with 
                | :? int as idx -> 
                    b.Add(ValueSome(int64 idx))
                    e.Add(ValueSome(if idx >= 0 then int64 idx + 1L else int64 idx))
                    s.Add(ValueNone)
                    if sliceAx < ndim && (not (-shape.[sliceAx] <= idx) || not (shape.[sliceAx] >= idx)) then 
                        failwithf "Index %d is out of bounds for axis %d with size %d" idx sliceAx shape.[sliceAx]
                    sliceAxis.Add sliceAx
                    sliceAx <- sliceAx + 1
                | :? (int option) as o -> 
                    let o2 = a.[i+1] :?> int option |> Option.map (fun x -> if x >= 0 then x + 1 else x)
                    b.Add(match o with | Some o -> ValueSome (int64 o) | _ -> ValueNone)
                    e.Add(match o2 with | Some o -> ValueSome (int64 o) | _ -> ValueNone)
                    s.Add(ValueNone)
                    sliceAxis.Add sliceAx
                    sliceAx <- sliceAx + 1
                    i <- i + 1
                | :? SliceRange as r -> 
                    match r.Start with 
                    | Some v -> b.Add(ValueSome(v))
                    | None -> b.Add(ValueNone)
                    match r.Stop with 
                    | Some v -> e.Add(ValueSome(if v >= 0L then v + 1L else v))
                    | None -> e.Add(ValueNone)
                    match r.Step with 
                    | Some v -> s.Add(ValueSome(v))
                    | None -> s.Add(ValueNone)
                    sliceAxis.Add sliceAx
                    sliceAx <- sliceAx + 1
                | :? NewAxis -> newAxis.Add sliceAx
                | _ -> invalidArg "index" (sprintf "invalid argument to get slice %A" a.[i])
                i <- i + 1
            // check continuous
            // https://github.com/apache/incubator-mxnet/blob/6bff547465c83ed343a5ef8241d22f56738534bd/python/mxnet/ndarray/ndarray.py#L846
            let indices k = 
                let start = match b.[k] with ValueNone -> 0L | ValueSome v -> v
                let stop = match e.[k] with ValueNone -> int64(shape.[sliceAxis.[k]]) | ValueSome v -> v
                let step = match s.[k] with ValueNone -> 1L | ValueSome v -> v
                let starti = if start >= 0L then start else int64 shape.[sliceAxis.[k]] + start
                let stopi = if stop >= 0L then stop else int64 shape.[sliceAxis.[k]] + stop
                starti, stopi, step
            let continuous = 
                let mutable subset = false
                let mutable k = b.Count - 1
                let mutable continuous = true
                while continuous && k >= 0 do
                    let start,stop,step = indices k
                    let num = 
                        if step > 0L then 
                            double (max (stop - start) 0L) / double step |> int
                        else 
                            double (min (stop - start) 0L) / double step |> int
                    if num <> 1 && (subset || step <> 1L) then 
                        continuous <- false
                    elif num <> shape.[k] then
                        subset <- true
                    k <- k - 1
                continuous
            let sliced = 
                if continuous then 
                    let mutable flatBegin = 0L
                    let mutable flatEnd = 0L
                    for k = 0 to shape.Length - 1 do 
                        flatBegin <- flatBegin*int64 shape.[k]
                        flatEnd <- flatEnd*int64 shape.[k]
                        if k < b.Count then 
                            let b,e,s = indices k
                            if s < 0L then 
                                flatBegin <- flatBegin + e - 1L
                                flatEnd <- flatEnd + b
                            else 
                                flatBegin <- flatBegin + b
                                flatEnd <- flatEnd + e - 1L
                        else
                            flatEnd <- flatEnd + int64 shape.[k] - 1L
                    flatEnd <- flatEnd + 1L
                    let flat : NDArray = x.Reshape(-1)
                    let slicedFlat =    
                        if LibFeature.INT64_TENSOR_SIZE then 
                            new NDArray(MXNDArray.slice64 flat.UnsafeHandle flatBegin flatEnd)
                        else
                            new NDArray(MXNDArray.slice flat.UnsafeHandle flatBegin flatEnd)
                    let slicedShape = 
                        let s = Array.zeroCreate shape.Length
                        let mutable k = shape.Length - 1
                        while k >= 0 do
                            let start,stop,step = 
                                if k >= b.Count then 
                                    (0L,int64 shape.[k],1L)
                                else 
                                    indices k
                            let num = 
                                if step > 0L then 
                                    double (max (stop - start) 0L) / double step |> int
                                else 
                                    double (min (stop - start) 0L) / double step |> int
                            s.[k] <- num
                            k <- k - 1
                        s
                    slicedFlat.Reshape(slicedShape)
                else
                    x.Slice(b |> Seq.map str, e |> Seq.map str, s |> Seq.map str)
            if newAxis.Count = 0 then 
                sliced
            else
                let newShape = 
                    let s = ResizeArray()
                    let mutable j = 0
                    for k = 0 to shape.Length - 1 do
                        while j < newAxis.Count && newAxis.[j] = k do 
                            s.Add(1)
                            j <- j + 1
                        let start,stop,step = 
                            if k >= b.Count then 
                                (0L,int64 shape.[k],1L)
                            else 
                                indices k
                        let num = 
                            if step > 0L then 
                                double (max (stop - start) 0L) / double step |> int
                            else 
                                double (min (stop - start) 0L) / double step |> int
                        s.Add num
                    while j < newAxis.Count && newAxis.[j] = shape.Length do 
                        s.Add(1)
                        j <- j + 1
                    s.ToArray()
                sliced.Reshape newShape
    member x.Item 
        with get([<ParamArray>] a : obj []) = x.GetSlice(a = a)
    member x.SetSlice([<ParamArray>] a : obj []) : unit = 
        let inline str x = match x with ValueSome v -> v.ValueString() | _ -> "None"
        if a.Length = 0 then 
            ()
        else
            let shape = x.Shape
            let ndim = shape.Length
            let b = ResizeArray<int64 voption>()
            let e = ResizeArray<int64 voption>()
            let s = ResizeArray<int64 voption>()
            let sliceAxis = ResizeArray()
            let newAxis = ResizeArray()
            let mutable i = 0
            let mutable sliceAx = 0
            while i < a.Length - 1 do 
                match a.[i] with 
                | :? int as idx -> 
                    b.Add(ValueSome(int64 idx))
                    e.Add(ValueSome(if idx >= 0 then int64 idx + 1L else int64 idx))
                    s.Add(ValueNone)
                    if sliceAx < ndim && (not (-shape.[sliceAx] <= idx) || not (shape.[sliceAx] >= idx)) then 
                        failwithf "Index %d is out of bounds for axis %d with size %d" idx sliceAx shape.[sliceAx]
                    sliceAxis.Add sliceAx
                    sliceAx <- sliceAx + 1
                | :? (int option) as o -> 
                    let o2 = a.[i+1] :?> int option |> Option.map (fun x -> if x >= 0 then x + 1 else x)
                    b.Add(match o with | Some o -> ValueSome (int64 o) | _ -> ValueNone)
                    e.Add(match o2 with | Some o -> ValueSome (int64 o) | _ -> ValueNone)
                    s.Add(ValueNone)
                    sliceAxis.Add sliceAx
                    sliceAx <- sliceAx + 1
                    i <- i + 1
                | :? SliceRange as r -> 
                    match r.Start with 
                    | Some v -> b.Add(ValueSome(v))
                    | None -> b.Add(ValueNone)
                    match r.Stop with 
                    | Some v -> e.Add(ValueSome(if v >= 0L then v + 1L else v))
                    | None -> e.Add(ValueNone)
                    match r.Step with 
                    | Some v -> s.Add(ValueSome(v))
                    | None -> s.Add(ValueNone)
                    sliceAxis.Add sliceAx
                    sliceAx <- sliceAx + 1
                | :? NewAxis -> newAxis.Add sliceAx //TODO: handle NewAxis for SetSlice
                | _ -> invalidArg "index" (sprintf "invalid argument to set slice %A" a.[i])
                i <- i + 1
            let indices k = 
                let start = match b.[k] with ValueNone -> 0L | ValueSome v -> v
                let stop = match e.[k] with ValueNone -> int64(shape.[sliceAxis.[k]]) | ValueSome v -> v
                let step = match s.[k] with ValueNone -> 1L | ValueSome v -> v
                let starti = if start >= 0L then start else int64 shape.[sliceAxis.[k]] + start
                let stopi = if stop >= 0L then stop else int64 shape.[sliceAxis.[k]] + stop
                starti, stopi, step
            let indexedShape : int [] = 
                [|
                    for i = 0 to s.Count - 1 do 
                        let b,e,s = indices i
                        assert(s <> 0L)
                        if b = e then 
                            0
                        elif s > 0L then 
                            assert(b < e)
                            (e - b - 1L) / s + 1L |> int
                        else
                            (b - e - 1L) / -s + 1L |> int
                |]
            if indexedShape = x.Shape && (s |> Seq.exists (fun step -> match step with ValueSome step -> step <= 0L | _ -> false) |> not) then 
                //Overwrite entire array
                let length = x.Shape |> Array.reduce (*)
                match a.[a.Length - 1] with 
                | :? NDArray as v -> 
                    if v.NDArrayHandle <> x.NDArrayHandle then 
                        let v2 : NDArray = v.BroadcastTo(x.Shape)
                        v2.CopyTo(x) |> ignore
                | :? double as v -> x.MutFull(v) |> ignore
                | :? decimal as v -> x.MutFull(v) |> ignore
                | :? int as v -> x.MutFull(v) |> ignore
                | :? float32 as v -> x.MutFull(v) |> ignore
                | :? int64 as v -> x.MutFull(v) |> ignore    
                | :? int8 as v -> x.MutFull(v) |> ignore    
                | :? uint8 as v -> x.MutFull(v) |> ignore    
                | :? (double []) as v -> x.CopyFrom(v)
                | :? (decimal []) as v -> x.CopyFrom(v)
                | :? (int []) as v -> x.CopyFrom(v)
                | :? (float32 []) as v -> x.CopyFrom(v)
                | :? (int64 []) as v -> x.CopyFrom(v)
                | :? (uint8 []) as v -> x.CopyFrom(v)
                | :? (int8 []) as v -> x.CopyFrom(v)
                | :? (double seq) as v -> x.CopyFrom(v |> Seq.take length |> Seq.toArray)
                | :? (decimal seq) as v -> x.CopyFrom(v |> Seq.take length |> Seq.toArray)
                | :? (int seq) as v -> x.CopyFrom(v |> Seq.take length |> Seq.toArray)
                | :? (float32 seq) as v -> x.CopyFrom(v |> Seq.take length |> Seq.toArray)
                | :? (int64 seq) as v -> x.CopyFrom(v |> Seq.take length |> Seq.toArray)
                | :? (uint8 seq) as v -> x.CopyFrom(v |> Seq.take length |> Seq.toArray)
                | :? (int8 seq) as v -> x.CopyFrom(v |> Seq.take length |> Seq.toArray)
                | _ -> failwithf "Cannot assign slice from type %s" (a.[a.Length - 1].GetType().FullName)

            else
                let b = b |> Seq.map str |> String.concat "," |> sprintf "(%s)"
                let e = e |> Seq.map str |> String.concat "," |> sprintf "(%s)"
                let s = s |> Seq.map str |> String.concat "," |> sprintf "(%s)"
                let inline scalar (v : ^a) = 
                    mutInvoke x "_slice_assign_scalar" [|x|] [|"begin", b; "end", e; "step", s; "scalar" <-- v|]       
                    |> ignore
                match a.[a.Length - 1] with 
                | :? double as x -> scalar x
                | :? decimal as x -> scalar x
                | :? int as x -> scalar x
                | :? float32 as x -> scalar x
                | :? int64 as x -> scalar x
                | :? NDArray as y ->
                    mutInvoke x "_slice_assign" [|x;y|] [|"begin", b; "end", e; "step", s|] |> ignore
                | q -> failwithf "Unhandled slice assign type %s with value %A" (q.GetType().Name) q
    
    member x.Concat(dim : int, [<ParamArray>] data : NDArray []) = invoke1 "Concat" data [|"num_args" <-- data.Length; "dim" <-- dim|]
    member x.Concat(dim : int, data : NDArray seq) = x.Concat(dim, data = (data |> Seq.toArray))
    member x.SwapAxis(dim1 : int, dim2 : int) = invoke1 "SwapAxis" [|x|] [|"dim1" <-- dim1; "dim2" <-- dim2|]
    member x.MutReshape([<ParamArray>] dims : int []) = mutInvoke x "Reshape" [|x|] [|"shape" <-- dims|]
    member x.MutReshape(dims : int seq) =  mutInvoke x "Reshape" [|x|] [|"shape" <-- dims|]
    member x.Reshape([<ParamArray>] dims : int []) = invoke1 "Reshape" [|x|] [|"shape" <-- dims|]
    member x.Reshape(dims : int seq) = invoke1 "Reshape" [|x|] [|"shape" <-- dims|]
    member x.ReverseReshape([<ParamArray>] dims : int []) = invoke1 "Reshape" [|x|] [|"shape" <-- dims; "reverse" <-- true|]
    member x.ReverseReshape(dims : int seq) = invoke1 "Reshape" [|x|] [|"shape" <-- dims; "reverse" <-- true|]
    member x.Transpose([<ParamArray>] axes : int []) = invoke1 "transpose" [|x|] [|"axes" <-- axes|]
    member x.Transpose(axes : int seq) = invoke1 "transpose" [|x|] [|"axes" <-- axes|]
    member x.BroadcastTo(shape : int seq) = invoke1 "broadcast_to" [|x|] [|"shape" <-- shape|]
    member x.MutFull(value : double) = 
        mutInvoke x "_full" Array.empty [|"shape" <-- x.Shape; "value" <-- value; "dtype" <-- x.DataType.Value; "ctx" <-- x.Context|]
    member x.MutFull(value : float32) = 
        mutInvoke x "_full" Array.empty [|"shape" <-- x.Shape; "value" <-- value; "dtype" <-- x.DataType.Value; "ctx" <-- x.Context|]
    member x.MutFull(value : decimal) = 
        mutInvoke x "_full" Array.empty [|"shape" <-- x.Shape; "value" <-- value; "dtype" <-- x.DataType.Value; "ctx" <-- x.Context|]
    member x.MutFull(value : int32) = 
        mutInvoke x "_full" Array.empty [|"shape" <-- x.Shape; "value" <-- value; "dtype" <-- x.DataType.Value; "ctx" <-- x.Context|]
    member x.MutFull(value : int64) = 
        mutInvoke x "_full" Array.empty [|"shape" <-- x.Shape; "value" <-- value; "dtype" <-- x.DataType.Value; "ctx" <-- x.Context|]
    member x.MutFull(value : int8) = 
        mutInvoke x "_full" Array.empty [|"shape" <-- x.Shape; "value" <-- value; "dtype" <-- x.DataType.Value; "ctx" <-- x.Context|]
    member x.MutFull(value : uint8) = 
        mutInvoke x "_full" Array.empty [|"shape" <-- x.Shape; "value" <-- value; "dtype" <-- x.DataType.Value; "ctx" <-- x.Context|]
    
    member x.ToArray() : Array = 
        let shape = x.Shape
        if shape.Length > 0 then 
            let a = 
                match x.DataType with 
                | None -> Array.CreateInstance(typeof<float32>, lengths = shape)
                | Some Float16 -> Array.CreateInstance(typeof<uint16>, lengths = shape)
                | Some dt -> 
                    match dt.Type with 
                    | Some t -> Array.CreateInstance(t, lengths = shape)
                    | None -> Array.CreateInstance(typeof<float32>, lengths = shape)
            MXNDArray.syncCopyToCPUArray x.UnsafeHandle a
            a
        else
            match x.DataType with 
            | None -> Array.CreateInstance(typeof<float32>,0)
            | Some Float16 -> Array.CreateInstance(typeof<uint16>,0)
            | Some dt -> 
                match dt.Type with 
                | Some t -> Array.CreateInstance(t,0)
                | None -> Array.CreateInstance(typeof<float32>,0)

    member x.ToArray<'a when 'a : unmanaged>() : 'a[] = 
        match DataType.TryFromNetType<'a>(), x.DataType with 
        | Some t, Some t2 -> 
            if t = t2 then 
                let a = Array.zeroCreate x.Size
                MXNDArray.syncCopyToCPU handle.UnsafeHandle a
                a
            else
                use x2 = invoke1 "cast" [|x|] [|"dtype" <-- t|]
                let a = Array.zeroCreate x.Size
                MXNDArray.syncCopyToCPU x2.UnsafeHandle a
                a
        | None, _ -> raise (InvalidCastException(sprintf "Type %s has no corresponding MXNet type" typeof<'a>.FullName))
        | _ -> [||]
            
    member x.ToFloat32Array() : float32[] = x.ToArray<_>()
    member x.ToDoubleArray() : double[] = x.ToArray<_>()
    member x.ToIntArray() : int[] = x.ToArray<_>()
    member x.ToInt64Array() : int64[] = x.ToArray<_>()
    member x.ToInt8Array() : int8[] = x.ToArray<_>()
    member x.ToUInt8Array() : uint8[] = x.ToArray<_>()

    member x.ToScalar() : 'a = 
        let sz = x.Shape |> Array.reduce (*)
        if sz <> 1 then 
            raise (InvalidOperationException(sprintf "NDArray has size %d" sz))
        let a = x.ToArray<_>()
        assert(a.Length = 1)
        a.[0]
    member x.ToFloat32Scalar() : float32 = x.ToScalar()
    member x.ToDoubleScalar() : double = x.ToScalar()
    member x.ToIntScalar() : int = x.ToScalar()
    member x.ToInt64Scalar() : int64 = x.ToScalar()
    member x.ToInt8Scalar() : int8 = x.ToScalar()
    member x.ToUInt8Scalar() : uint8 = x.ToScalar()

    /// Returns a copy of the array after casting to a specified type
    member x.AsType(dtype : DataType, copy : bool) = 
        match x.DataType with 
        | Some d when d = dtype && not copy -> x
        | _ -> 
            let target = new NDArray(x.Shape, x.Context, dtype = dtype)
            x.CopyTo target
            target
        
    /// Returns a copy of the array after casting to a specified type
    member x.AsType(dtype) = x.AsType(dtype, true)

    override x.ToString() = 
        let struct(dt, id) = MXNDArray.getContext handle.UnsafeHandle
        if int dt = 0 then 
            //Empty NDArray
            sprintf "NDArray[EMPTY]"
        else
            sprintf "NDArray[%s] @%O" (x.Shape |> Array.map string |> String.concat ",") (x.Context)
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



