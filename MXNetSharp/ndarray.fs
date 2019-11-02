namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop



module internal Helper = 
    let inline (<--) x y = x, Util.valueString y

open Helper

type NDArray(handle : SafeNDArrayHandle) = 
    let mutable disposed = false
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
        let dtype = defaultArg dtype TypeFlag.Float32
        let delayAlloc = defaultArg delayAlloc true
        let shape = shape |> Seq.toArray
        new NDArray(MXNDArray.createEx shape context.DeviceType context.DeviceId delayAlloc dtype)
    new(data : float[], shape, context : Context) = 
        let dtype = TypeFlag.Float64
        let delayAlloc = false
        let shape = shape |> Seq.toArray
        let handle = MXNDArray.createEx shape context.DeviceType context.DeviceId delayAlloc dtype
        MXNDArray.syncCopyFromCPU handle data
        new NDArray(handle)
    new(data : float seq, shape, context : Context) = new NDArray(data |> Seq.toArray, shape, context)
    new(data : float32[], shape, context : Context) = 
        let dtype = TypeFlag.Float32
        let delayAlloc = false
        let shape = shape |> Seq.toArray
        let handle = MXNDArray.createEx shape context.DeviceType context.DeviceId delayAlloc dtype
        MXNDArray.syncCopyFromCPU handle data
        new NDArray(handle)
    new(data : float32 seq, shape, context) = new NDArray(data |> Seq.toArray, shape, context)
    new(data : float[]) =
        let h1 = MXNDArray.createNone()
        MXNDArray.syncCopyFromCPU h1 data
        new NDArray(h1)
    new(data : byte[], shape, context : Context) = 
        let dtype = TypeFlag.Uint8
        let delayAlloc = false
        let shape = shape |> Seq.toArray
        let handle = MXNDArray.createEx shape context.DeviceType context.DeviceId delayAlloc dtype
        MXNDArray.syncCopyFromCPU handle data
        new NDArray(handle)
        
    member x.NDArrayHandle = handle
    member x.UnsafeHandle = x.NDArrayHandle.UnsafeHandle

    member x.Shape = MXNDArray.getShape handle.UnsafeHandle
    member x.Size = x.Shape |> Array.reduce (*)
    member x.Context = MXNDArray.getContext handle.UnsafeHandle

    member x.CopyTo(destination : NDArray) = 
        MXNDArray.syncCopyFromNDArray destination.NDArrayHandle.UnsafeHandle x.NDArrayHandle.UnsafeHandle -1

    member x.CopyTo(deviceContext : Context) = 
        let destination = new NDArray(x.Shape, deviceContext, delayAlloc = true)
        x.CopyTo(destination)
        destination

    member x.SyncCopyFromCPU(data : float32 []) = MXNDArray.syncCopyFromCPU handle.UnsafeHandle data

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
    member x.MutPlus(y : NDArray) = mutInvoke x "elemwise_add" [|x; y|]
    member x.MutPlusBroadcast(y : NDArray) = mutInvoke x "broadcast_add" [|x; y|] 

   
    static member (-)(x : NDArray, y : float) = invoke1 "_minus_scalar" [|x|] [|"scalar" <-- y|]
    static member (-)(y : float, x : NDArray) = invoke1 "_rminus_scalar" [|x|] [|"scalar" <-- y|]
    static member (-)(x : NDArray, y : NDArray) = invoke1 "elemwise_sub" [|x; y|] Array.empty 
    static member (.-)(x : NDArray, y : NDArray) = invoke1 "broadcast_sub" [|x; y|] Array.empty 
    member x.MutSubstract(y : float) = mutInvoke x "_sub_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutSubstract(y : NDArray) = mutInvoke x "elemwise_sub" [|x; y|]
    member x.MutSubstractFrom(y : float) = mutInvoke x "_rminus_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutSubstractFrom(y : NDArray) = mutInvoke x "elemwise_sub" [|y; x|]
    member x.MutSubstractBroadcast(y : NDArray) = mutInvoke x "broadcast_sub" [|x; y|] 
    member x.MutSubstractBroadcastFrom(y : NDArray) = mutInvoke x "broadcast_sub" [|y; x|] 
 
 
    static member (/)(x : NDArray, y : float) = invoke1 "_div_scalar" [|x|] [|"scalar" <-- y|]
    static member (/)(y : float, x : NDArray) = invoke1 "_rdiv_scalar" [|x|] [|"scalar" <-- y|]
    static member (/)(x : NDArray, y : NDArray) = invoke1 "elemwise_div" [|x; y|] Array.empty 
    static member (./)(x : NDArray, y : NDArray) = invoke1 "broadcast_div" [|x; y|] Array.empty 
    member x.MutDividedBy(y : float) = mutInvoke x "_div_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutDividedBy(y : NDArray) = mutInvoke x "elemwise_div" [|x; y|]
    member x.MutDividedInto(y : float) = mutInvoke x "_rdiv_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutDividedInto(y : NDArray) = mutInvoke x "elemwise_div" [|y; x|]
    member x.MutDividedBroadcastBy(y : NDArray) = mutInvoke x "broadcast_div" [|x; y|] 
    member x.MutDividedBroadcastInto(y : NDArray) = mutInvoke x "broadcast_div" [|y; x|] 


    static member ( * )(x : NDArray, y : float) = invoke1 "_mul_scalar" [|x|] [|"scalar" <-- y|]
    static member ( * )(y : float, x : NDArray) = invoke1 "_mul_scalar" [|x|] [|"scalar" <-- y|]
    static member ( * )(x : NDArray, y : NDArray) = invoke1 "elemwise_mul" [|x; y|] Array.empty 
    static member ( .* )(x : NDArray, y : NDArray) = invoke1 "broadcast_mul" [|x; y|] Array.empty 
    member x.MutMultiply(y : float) = mutInvoke x "_mul_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutMultiply(y : NDArray) = mutInvoke x "elemwise_mul" [|x; y|]
    member x.MutMultiplyBroadcast(y : NDArray) = mutInvoke x "broadcast_mul" [|x; y|] 

       
    static member Pow(x : NDArray, y : float) = invoke1 "_power_scalar" [|x|] [|"scalar" <-- y|]
    static member Pow(y : float, x : NDArray) = invoke1 "_rpower_scalar" [|x|] [|"scalar" <-- y|]
    static member Pow(x : NDArray, y : NDArray) = invoke1 "elemwise_power" [|x; y|] Array.empty 
    static member ( .** )(x : NDArray, y : NDArray) = invoke1 "broadcast_power" [|x; y|] Array.empty 
    member x.Power(y : float) = mutInvoke x "_power_scalar" [|x|] [|"scalar" <-- y|]
    member x.PowerBaseOf(y : float) = mutInvoke x "_rpower_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutPower(y : float) = mutInvoke x "_power_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutPower(y : NDArray) = mutInvoke x "_power" [|x; y|]
    member x.MutPowerBaseOf(y : float) = mutInvoke x "_rpower_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutPowerBaseOf(y : NDArray) = mutInvoke x "_power" [|y; x|]
    member x.MutPowerBroadcast(y : NDArray) = mutInvoke x "broadcast_power" [|x; y|] 
    member x.MutPowerBaseOfBroadcast(y : NDArray) = mutInvoke x "broadcast_power" [|y; x|] 


    member x.Negate() = -1.0*x
    static member (~-)(x : NDArray) = x.Negate()
    member x.MutNegate() = x.MutMultiply(-1.0)

    static member (%)(x : NDArray, y : float) = invoke1 "_mod_scalar" [|x|] [|"scalar" <-- y|]
    static member (%)(y : float, x : NDArray) = invoke1 "_rmod_scalar" [|x|] [|"scalar" <-- y|]
    static member (%)(x : NDArray, y : NDArray) = invoke1 "broadcast_mod" [|x; y|] Array.empty 
    member x.MutMod(y : float) = mutInvoke x "_mod_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutMod(y : NDArray) = mutInvoke x "broadcast_mod" [|x; y|]
    member x.MutModOf(y : float) = mutInvoke x "_rmod_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutModOf(y : NDArray) = mutInvoke x "broadcast_mod" [|y; x|]

    static member (.=)(x : NDArray, y : float) = invoke1 "_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.=)(y : float, x : NDArray) = invoke1 "_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.=)(x : NDArray, y : NDArray) = invoke1 "broadcast_equal" [|x; y|] Array.empty 
    member x.MutEqual(y : float) = mutInvoke x "_mod_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutEqual(y : NDArray) = mutInvoke x "broadcast_mod" [|x; y|]

    static member (.<>)(x : NDArray, y : float) = invoke1 "_not_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.<>)(y : float, x : NDArray) = invoke1 "_not_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.<>)(x : NDArray, y : NDArray) = invoke1 "broadcast_not_equal" [|x; y|] Array.empty 
    member x.MutNotEqual(y : float) = mutInvoke x "_not_equal_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutNotEqual(y : NDArray) = mutInvoke x "broadcast_not_equal" [|x; y|]

    static member (.>)(x : NDArray, y : float) = invoke1 "_greater_scalar" [|x|] [|"scalar" <-- y|]
    static member (.>)(y : float, x : NDArray) = invoke1 "_lesser_scalar" [|x|] [|"scalar" <-- y|]
    static member (.>)(x : NDArray, y : NDArray) = invoke1 "broadcast_greater" [|x; y|] Array.empty 
    member x.MutGreater(y : float) = mutInvoke x "_greater_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutGreater(y : NDArray) = mutInvoke x "broadcast_greater" [|x; y|]

    static member (.<)(x : NDArray, y : float) = invoke1 "_lesser_scalar" [|x|] [|"scalar" <-- y|]
    static member (.<)(y : float, x : NDArray) = invoke1 "_greater_scalar" [|x|] [|"scalar" <-- y|]
    static member (.<)(x : NDArray, y : NDArray) = invoke1 "broadcast_lesser" [|x; y|] Array.empty 
    member x.MutLesser(y : float) = mutInvoke x "_lesser_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutLesser(y : NDArray) = mutInvoke x "broadcast_lesser" [|x; y|]

    static member (.>=)(x : NDArray, y : float) = invoke1 "_greater_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.>=)(y : float, x : NDArray) = invoke1 "_lesser_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.>=)(x : NDArray, y : NDArray) = invoke1 "broadcast_greater_equal" [|x; y|] Array.empty 
    member x.MutGreaterOrEqual(y : float) = mutInvoke x "_greater_equal_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutGreaterOrEqual(y : NDArray) = mutInvoke x "broadcast_greater_equal" [|x; y|]

    static member (.<=)(x : NDArray, y : float) = invoke1 "_lesser_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.<=)(y : float, x : NDArray) = invoke1 "_greater_equal_scalar" [|x|] [|"scalar" <-- y|]
    static member (.<=)(x : NDArray, y : NDArray) = invoke1 "broadcast_lesser_equal" [|x; y|] Array.empty 
    member x.MutLesserOrEqual(y : float) = mutInvoke x "_lesser_equal_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutLesserOrEqual(y : NDArray) = mutInvoke x "broadcast_lesser_equal" [|x; y|]

 
    static member (.&&)(x : NDArray, y : float) = invoke1 "_logical_and_scalar" [|x|] [|"scalar" <-- y|]
    static member (.&&)(y : float, x : NDArray) = invoke1 "_logical_and_scalar" [|x|] [|"scalar" <-- y|]
    static member (.&&)(x : NDArray, y : NDArray) = invoke1 "_logical_and" [|x; y|] Array.empty 
    static member (..&&)(x : NDArray, y : NDArray) = invoke1 "broadcast_logical_and" [|x; y|] Array.empty 
    member x.MutLogicalAnd(y : float) = mutInvoke x "_logical_and_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutLogicalAnd(y : NDArray) = mutInvoke x "_logical_and" [|x; y|]
    member x.MutLogicalAndBroadcast(y : NDArray) = mutInvoke x "broadcast_logical_and" [|x; y|] 


    static member (.||)(x : NDArray, y : float) = invoke1 "_logical_or_scalar" [|x|] [|"scalar" <-- y|]
    static member (.||)(y : float, x : NDArray) = invoke1 "_logical_or_scalar" [|x|] [|"scalar" <-- y|]
    static member (.||)(x : NDArray, y : NDArray) = invoke1 "_logical_or" [|x; y|] Array.empty 
    static member (..||)(x : NDArray, y : NDArray) = invoke1 "broadcast_logical_or" [|x; y|] Array.empty 
    member x.MutLogicalOr(y : float) = mutInvoke x "_logical_or_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutLogicalOr(y : NDArray) = mutInvoke x "_logical_or" [|x; y|]
    member x.MutLogicalOrBroadcast(y : NDArray) = mutInvoke x "broadcast_logical_or" [|x; y|] 

    static member (.^^)(x : NDArray, y : float) = invoke1 "_logical_xor_scalar" [|x|] [|"scalar" <-- y|]
    static member (.^^)(y : float, x : NDArray) = invoke1 "_logical_xor_scalar" [|x|] [|"scalar" <-- y|]
    static member (.^^)(x : NDArray, y : NDArray) = invoke1 "_logical_xor" [|x; y|] Array.empty 
    static member (..^^)(x : NDArray, y : NDArray) = invoke1 "broadcast_logical_xor" [|x; y|] Array.empty 
    member x.MutLogicalXor(y : float) = mutInvoke x "_logical_xor_scalar" [|x|] [|"scalar" <-- y|]
    member x.MutLogicalXor(y : NDArray) = mutInvoke x "_logical_xor" [|x; y|]
    member x.MutLogicalXorBroadcast(y : NDArray) = mutInvoke x "broadcast_logical_xor" [|x; y|] 


    member x.Exp() = invoke "exp" [|x|] Array.empty |> Array.head
    static member Exp(x : NDArray) = x.Exp()
    member x.Log() = invoke "log" [|x|] Array.empty |> Array.head
    static member Log(x : NDArray) = x.Log()
    member x.Abs() = invoke "abs" [|x|] Array.empty |> Array.head
    static member Abs(x : NDArray) = x.Abs()
    member x.Acos() = invoke "arccos" [|x|] Array.empty |> Array.head
    static member Acos(x : NDArray) = x.Acos()
    member x.Asin() = invoke "arcsin" [|x|] Array.empty |> Array.head
    static member Asin(x : NDArray) = x.Asin()
    member x.Atan() = invoke "arctan" [|x|] Array.empty |> Array.head
    static member Atan(x : NDArray) = x.Atan()
    //static member Atan2(x : #NDArray, y : #NDArray) = new NpiArctan2(x, y)  // TODO: Atan2 for NDArray
    //static member Atan2(x : #NDArray, y : double) = new NpiArctan2Scalar(x, y) 
    //static member Atan2(y : double, x : #NDArray) = new NpiRarctan2Scalar(x, y) 
    //static member ArcTan2(x : #NDArray, y : #NDArray) = new NpiArctan2(x, y) 
    //static member ArcTan2(x : #NDArray, y : double) = new NpiArctan2Scalar(x, y) 
    //static member AtcTan2(y : double, x : #NDArray) = new NpiRarctan2Scalar(x, y) 
    member x.Ceiling() = invoke "ceil" [|x|] Array.empty |> Array.head
    static member Ceiling(x : NDArray) = x.Ceiling()
    member x.Floor() = invoke "floor" [|x|] Array.empty |> Array.head
    static member Floor(x : NDArray) = x.Floor()
    member x.Truncate() = invoke "trunc" [|x|] Array.empty |> Array.head
    static member Truncate(x : NDArray) = x.Truncate()
    member x.Round() = invoke "round" [|x|] Array.empty |> Array.head
    static member Round(x : NDArray) = x.Round()
    member x.Log10() = invoke "log10" [|x|] Array.empty |> Array.head
    static member Log10(x : NDArray) = x.Log10()
    member x.Sqrt() = invoke "sqrt" [|x|] Array.empty |> Array.head
    static member Sqrt(x : NDArray) = x.Sqrt()
    member x.Cos() = invoke "cos" [|x|] Array.empty |> Array.head
    static member Cos(x : NDArray) = x.Cos()
    member x.Cosh() = invoke "cosh" [|x|] Array.empty |> Array.head
    static member Cosh(x : NDArray) = x.Cosh()
    member x.Sin() = invoke "sin" [|x|] Array.empty |> Array.head
    static member Sin(x : NDArray) = x.Sin()
    member x.Sinh() = invoke "sinh" [|x|] Array.empty |> Array.head
    static member Sinh(x : NDArray) = x.Sinh()
    member x.Tan() = invoke "tan" [|x|] Array.empty |> Array.head
    static member Tan(x : NDArray) = x.Tan()
    member x.Tanh() = invoke "tanh" [|x|] Array.empty |> Array.head
    static member Tanh(x : NDArray) = x.Tanh()

    member x.Slice(startIndices, endIndices, stepIndices) = 
        let b = startIndices |> String.concat "," |> sprintf "(%s)"
        let e = endIndices |> String.concat "," |> sprintf "(%s)"
        let s = stepIndices |> String.concat "," |> sprintf "(%s)"
        invoke "slice" [|x|] [|"begin", b; "end", e; "step", s|]|> Array.head
    member x.GetSlice([<ParamArray>] a : obj []) = 
        let inline str x = match x with Some v -> v.ValueString() | _ -> "None"
        if a.Length = 0 then 
            x // REVIEW: copy?
        else
            let b = ResizeArray()
            let e = ResizeArray()
            let s = ResizeArray()
            let mutable i = 0
            while i < a.Length do 
                match a.[i] with 
                | :? int as idx -> 
                    b.Add(string idx)
                    e.Add(string (idx + 1))
                    s.Add "None"
                | :? (int option) as o -> 
                    let o2 = a.[i+1] :?> int option |> Option.map (fun x -> x + 1)
                    b.Add(str o)
                    e.Add(str o2)
                    s.Add "None"
                    i <- i + 1
                | _ -> failwithf "invalid argument to get slice %A" a.[i]
                i <- i + 1
            x.Slice(b,e,s)
    member x.SetSlice([<ParamArray>] a : obj []) = 
        let inline str x = match x with Some v -> v.ValueString() | _ -> "None"
        let b = ResizeArray()
        let e = ResizeArray()
        let s = ResizeArray()
        let mutable i = 0
        while i < a.Length - 1 do 
            match a.[i] with 
            | :? int as idx -> 
                b.Add(string idx)
                e.Add(string (idx + 1))
                s.Add "None"
            | :? (int option) as o -> 
                let o2 = a.[i+1] :?> int option |> Option.map (fun x -> x + 1)
                b.Add(str o)
                e.Add(str o2)
                s.Add "None"
                i <- i + 1
            | _ -> failwithf "invalid argument to get slice %A" a.[i]
            i <- i + 1
        let b = b |> String.concat "," |> sprintf "(%s)" //TODO: Just use a string builder from the start?
        let e = e |> String.concat "," |> sprintf "(%s)"
        let s = s |> String.concat "," |> sprintf "(%s)"
        let inline scalar (v : ^a) = 
            printfn "%A" (b,e,s)
            mutInvoke x "_slice_assign_scalar" [|x|] [|"begin", b; "end", e; "step", s; "scalar" <-- v|]       
            |> ignore
            
        match a.[a.Length - 1] with 
        | :? double as x -> scalar x
        | :? decimal as x -> scalar x
        | :? int as x -> scalar x
        | :? float32 as x -> scalar x
        | :? int64 as x -> scalar x
        | :? NDArray as y -> 
            printfn "%A" (b,e,s)
            mutInvoke x "_slice_assign" [|x;y|] [|"begin", b; "end", e; "step", s|] |> ignore
        | q -> failwithf "Unhandled slice assign type %s with value %A" (q.GetType().Name) q

            

(*
    member x.GetSlice(startIndex0 : int option, endIndex0 : int option) = 
        let inline str x = match x with Some v -> v.ValueString() | _ -> "None"
        x.Slice([|str startIndex0|], [|str endIndex0|], Array.empty)
    member x.GetSlice(startIndex0 : int option, endIndex0 : int option,
                      startIndex1 : int option, endIndex1 : int option) = 
        let inline str x = match x with Some v -> v.ValueString() | _ -> "None"
        let s =
            [|
                str startIndex0
                str startIndex1
            |]
        let e = 
            [|
                str endIndex0
                str endIndex1
            |]
        x.Slice(s, e, Array.empty)
*)        

    member x.SwapAxis(dim1 : int, dim2 : int) = invoke1 "SwapAxis" [|x|] [|"dim1" <-- dim1; "dim2" <-- dim2|]
    member x.Reshape([<ParamArray>] dims : int []) = invoke1 "Reshape" [|x|] [|"shape" <-- dims|]
    member x.Reshape(dims : int seq) = invoke1 "Reshape" [|x|] [|"shape" <-- dims|]
    
    member x.ToArray() : 'a [] = 
        let a = Array.zeroCreate x.Size
        MXNDArray.syncCopyToCPU handle.UnsafeHandle a
        a
    override x.ToString() = sprintf "NDArray[%s]" (x.Shape |> Array.map string |> String.concat ",")
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



