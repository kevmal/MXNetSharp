namespace rec MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop
open System.Collections.Generic
 
type IInitializer = 
    abstract member Initialize : Bind -> bool

type BindType = 
    | OutBind
    | AuxBind
    | ArgBind of (OpReqType option)*(NDArray option)
    member x.WithOpReqType(opReqType : OpReqType) = 
        match x with 
        | OutBind -> x
        | AuxBind -> x
        | ArgBind(_,g) -> ArgBind(Some opReqType,g)

[<NoComparison>]
type Bind = 
    {
        Name : string
        NDArray : NDArray option
        Shape : int [] option
        DataType : DataType option
        StorageType : StorageType option
        IsInitialized : bool
        Initializer : IInitializer option
        Context : Context option
        BindType : BindType
        CanCopy : bool
    }
    member x.IsOutBind =    
        match x.BindType with
        | OutBind -> true 
        | _ -> false
    member x.Grad = 
        match x with 
        | {BindType = ArgBind(_,g)} -> g
        | _ -> None
    member x.OpReqType = 
        match x with 
        | {BindType = ArgBind(ort,_)} -> ort
        | _ -> None
    member x.HasNDArray = x.NDArray.IsSome
    static member Out(name, ?shape, ?dataType) = 
        {
            Name = name 
            NDArray = None 
            Shape = shape
            DataType = dataType
            StorageType = None
            Context = None
            IsInitialized = false
            Initializer = None
            BindType = OutBind
            CanCopy = false

        }
    static member Arg(name, ?ndarray : NDArray, ?grad : NDArray, ?opReqType : OpReqType, ?shape : int seq, ?dataType : DataType, ?storageType : StorageType, ?ctx : Context, ?isInitialized, ?init, ?canCopy) = 
        let shape = 
            match shape, ndarray with 
            | None, Some nd -> 
                let shape = nd.Shape
                if shape.Length > 0 then
                    Some(shape |> Seq.ofArray)
                else 
                    None
            | _ -> shape
        {
            Name = name
            NDArray = ndarray
            Shape = shape |> Option.map Seq.toArray
            DataType = dataType 
            StorageType = storageType
            Context = ctx
            IsInitialized = defaultArg isInitialized false
            Initializer = init
            BindType = ArgBind(opReqType, grad)
            CanCopy = defaultArg canCopy false
        }
    static member Aux(name, ?ndarray : NDArray, ?shape : int seq, ?dataType : DataType, ?storageType : StorageType, ?ctx : Context, ?isInitialized, ?init, ?canCopy) =
        let shape = 
            match shape, ndarray with 
            | None, Some nd -> 
                let shape = nd.Shape
                if shape.Length > 0 then
                    Some(shape |> Seq.ofArray)
                else 
                    None
            | _ -> shape    
        {
            Name = name
            NDArray = ndarray
            Shape = shape |> Option.map Seq.toArray
            DataType = dataType 
            StorageType = storageType
            Context = ctx
            IsInitialized = defaultArg isInitialized false
            Initializer = init
            BindType = AuxBind
            CanCopy = defaultArg canCopy false
        }

[<AutoOpen>]
module ParameterPatterns = 
    let (|NameStart|_|) prefix (b : Bind) = if b.Name.StartsWith(prefix) then Some() else None
    let (|NameEnd|_|) prefix (b : Bind) = if b.Name.EndsWith(prefix) then Some() else None
    let internal pat f = if f() then Some() else None
    //let (|IsUpSampling|_|) (b : Bind) = pat(fun () -> b.Name.StartsWith("upsampling"))
    //let (|IsStnLoc|_|) (b : Bind) = pat(fun () -> b.Name.StartsWith("stn_loc"))
    let (|IsBias|_|) (b : Bind) = pat(fun () -> b.Name.EndsWith("bias"))
    let (|IsGamma|_|) (b : Bind) = pat(fun () -> b.Name.EndsWith("gamma"))
    let (|IsBeta|_|) (b : Bind) = pat(fun () -> b.Name.EndsWith("beta"))
    let (|IsWeight|_|) (b : Bind) = pat(fun () -> b.Name.EndsWith("weight"))
    //let (|IsMovingMean|_|) (b : Bind) = pat(fun () -> b.Name.EndsWith("moving_mean"))
    //let (|IsMovingVar|_|) (b : Bind) = pat(fun () -> b.Name.EndsWith("moving_var"))
    //let (|IsMovingInvVar|_|) (b : Bind) = pat(fun () -> b.Name.EndsWith("moving_inv_var"))
    //let (|IsMovingAvg|_|) (b : Bind) = pat(fun () -> b.Name.EndsWith("moving_avg"))
    let (|IsMin|_|) (b : Bind) = pat(fun () -> b.Name.EndsWith("min"))
    let (|IsMax|_|) (b : Bind) = pat(fun () -> b.Name.EndsWith("max"))
    let (|IsWeightQuantize|_|) (b : Bind) = pat(fun () -> b.Name.EndsWith("weight_quantize"))
    let (|IsBiasQuantize|_|) (b : Bind) = pat(fun () -> b.Name.EndsWith("bias_quantize"))
    let (|ArgBinding|_|) (b : Bind) = 
        match b with 
        | {BindType = ArgBind _} -> Some b
        | _ -> None
    let (|AuxBinding|_|) (b : Bind) = 
        match b with 
        | {BindType = AuxBind} -> Some b
        | _ -> None
    let (|FanInFanOut|_|) (b : Bind) =
        match b, b.NDArray with 
        | IsWeight, Some nd -> 
            let shape = nd.Shape
            if shape.Length < 2 then 
                None 
            elif shape.Length = 2 then 
                Some(shape.[1], shape.[0])
            else
                let rf = shape.[2 .. ] |> Array.reduce (*)
                Some(shape.[1]*rf, shape.[0]*rf)
        | _ -> None
    let (|Factor|_|) (factorType : FactorType) (b : Bind) = 
        match b with 
        | FanInFanOut(fanIn, fanOut) -> 
            match factorType with 
            | FactorType.Avg -> Some (double (fanIn + fanOut) / 2.0)
            | FactorType.In -> Some (double fanIn)
            | FactorType.Out -> Some (double fanOut)
        | _ -> None



[<RequireQualifiedAccess>]
type OrthogonalRandType =
    | Uniform 
    | Normal
[<RequireQualifiedAccess>]
type FactorType = 
    | In
    | Out 
    | Avg
[<RequireQualifiedAccess>]
type XavierRandType = 
    | Uniform
    | Gaussian

module Initializer = 
    type Skip() = 
        interface IInitializer with 
            member x.Initialize(b : Bind) = true
    type Mixed([<ParamArray>] inits : IInitializer []) = 
        member x.Initializers = Array.copy inits
        interface IInitializer with 
            member x.Initialize(b : Bind) = 
                let rec loop i =
                    if i >= inits.Length then 
                        false
                    elif inits.[i].Initialize(b) then 
                        true
                    else 
                        loop (i + 1)
                loop 0
    type Default() = 
        interface IInitializer with
            member x.Initialize(b : Bind) =   
                let a = match b.NDArray with Some a -> a | None -> invalidOp "Initializers expect NDArray to be created"
                match b with 
                | IsBias -> 
                    MX.ZerosLike([a],a)
                    true
                | IsGamma -> 
                    MX.OnesLike([a],a)
                    true
                | IsBeta -> 
                    MX.ZerosLike([a],a)
                    true
                | IsMin -> 
                    MX.ZerosLike([a],a)
                    true
                | IsMax -> 
                    MX.OnesLike([a],a)
                    true
                | IsWeightQuantize -> 
                    use r = MX.RandomRandintNDArray(-127L, 127L, a.Context, a.Shape, RandomRandintDtype.Int32)
                    r.CopyTo(a)
                    true
                | IsBiasQuantize -> 
                    MX.ZerosLike([a],a)
                    true
                | _ -> false
            
    type Zero() = 
        interface IInitializer with 
            member x.Initialize(b : Bind) =     
                let a = match b.NDArray with Some a -> a | None -> invalidOp "Initializers expect NDArray to be created"
                MX.ZerosLike([a],a)
                true
    type One() = 
        interface IInitializer with 
            member x.Initialize(b : Bind) =     
                let a = match b.NDArray with Some a -> a | None -> invalidOp "Initializers expect NDArray to be created"
                MX.OnesLike([a],a)
                true
    type Constant(value : double) = 
        interface IInitializer with 
            member x.Initialize(b : Bind) =     
                let a = match b.NDArray with Some a -> a | None -> invalidOp "Initializers expect NDArray to be created"
                a.MutFull(value) |> ignore
                true
    type Uniform(?scale) =
        let scale = abs(defaultArg scale 0.07)
        member x.Scale = scale
        interface IInitializer with 
            member x.Initialize(b : Bind) =     
                let a = match b.NDArray with Some a -> a | None -> invalidOp "Initializers expect NDArray to be created"
                match b with 
                | IsWeight -> 
                    MX.RandomUniformLike([a], a, -scale, scale)
                    true 
                | _ -> false
    type Normal(?sigma) = 
        let sigma = defaultArg sigma 0.01
        member x.Sigma = sigma
        interface IInitializer with 
            member x.Initialize(b : Bind) =     
                let a = match b.NDArray with Some a -> a | None -> invalidOp "Initializers expect NDArray to be created"
                match b with 
                | IsWeight -> 
                    MX.RandomNormalLike([a], a, 0.0, sigma)
                    true 
                | _ -> false

    type Orthogonal(?scale, ?randType) = 
        let scale = defaultArg scale 1.414
        let randType = defaultArg randType OrthogonalRandType.Uniform
        member x.Scale = scale
        interface IInitializer with 
            member x.Initialize(b : Bind) =     
                let a = match b.NDArray with Some a -> a | None -> invalidOp "Initializers expect NDArray to be created"
                if a.Shape.Length < 2 then 
                    false
                else 
                    match b with 
                    | IsWeight -> 
                        let shape = a.Shape
                        let nout = shape.[0]
                        let nin = shape.[1..] |> Array.reduce (*)
                        use tmp = 
                            match randType with 
                            | OrthogonalRandType.Uniform -> 
                                MX.RandomUniformNDArray(a.Context, -1.0, 1.0, [nout; nin])
                            | OrthogonalRandType.Normal -> 
                                MX.RandomNormalNDArray(a.Context, 0.0, 1.0, [nout; nin])
                        let u, _d, v = MX.NpiSvd(tmp)
                        if u.Shape = tmp.Shape then 
                            u.CopyTo(a)
                        else
                            v.CopyTo(a)
                        u.Dispose()
                        _d.Dispose()
                        v.Dispose()
                        true 
                    | _ -> false
        
    type Xavier(?magnitude, ?randType, ?factorType) = 
        let magnitude = defaultArg magnitude 3.0
        let factorType = defaultArg factorType FactorType.Avg
        let randType = defaultArg randType XavierRandType.Uniform
        member x.Magnitude = magnitude
        member x.FactorType = factorType
        member x.RandType = randType
        interface IInitializer with 
            member x.Initialize(b : Bind) =     
                let a = match b.NDArray with Some a -> a | None -> invalidOp "Initializers expect NDArray to be created"
                match b with 
                | Factor factorType factor -> 
                    let scale = sqrt(magnitude / factor)
                    match randType with 
                    | XavierRandType.Uniform -> 
                        MX.RandomUniformLike([a],a,-scale,scale)
                        true
                    | XavierRandType.Gaussian -> 
                        MX.RandomNormalLike([a],a,0.0,scale)
                        true
                | _ -> false

    type MSRAPrelu(slope, factorType) = 
        inherit Xavier(2.0 / (1.0 + slope ** 2.0), XavierRandType.Gaussian, factorType)
        new(slope) = MSRAPrelu(slope,FactorType.Avg)
        new(factorType) = MSRAPrelu(0.25,factorType)
        new() = MSRAPrelu(0.25,FactorType.Avg)
    type Bilinear() = 
        interface IInitializer with 
            member x.Initialize(b : Bind) =     
                let a = match b.NDArray with Some a -> a | None -> invalidOp "Initializers expect NDArray to be created"
                if a.Shape.Length < 2 then 
                    false
                else 
                    match b with 
                    | IsWeight -> 
                        let shape = a.Shape
                        let l = shape |> Array.reduce (*)
                        let f = ceil(float32 shape.[3] / 2.f)
                        let c = (2.f*f - 1.f - f % 2.f) / (2.f * f)
                        let weight = 
                            Array.init l
                                (fun i ->
                                    let x = i % shape.[3] |> float32
                                    let y = (i / shape.[3]) % shape.[2] |> float32
                                    (1.f - abs(x / f - c)) * (1.f - abs(y / f - c))
                                )
                        a.CopyFrom(weight)
                        true 
                    | _ -> false

module Init =
    open Initializer
    let create f = 
        {new IInitializer with
             member __.Initialize(b: Bind): bool = 
                 let a = match b.NDArray with Some a -> a | None -> invalidOp "Initializers expect NDArray to be created"
                 f b a
        }
    let weight f = 
        {new IInitializer with
             member __.Initialize(b: Bind): bool = 
                 let a = match b.NDArray with Some a -> a | None -> invalidOp "Initializers expect NDArray to be created"
                 match b with 
                 | IsWeight -> f a
                 | _ -> false
        }
    let bias f = 
        {new IInitializer with
             member __.Initialize(b: Bind): bool = 
                 let a = match b.NDArray with Some a -> a | None -> invalidOp "Initializers expect NDArray to be created"
                 match b with 
                 | IsBias -> f a
                 | _ -> false
        }
    let combine inits =  
        let inits = inits |> Seq.toArray
        Mixed(inits = inits)
    let skip = Skip()
    let one = One()
    let zero = Zero()
    let defaultInit = Default()



type Parameter(?name, ?shape : int seq, ?opReqType, ?grad, ?ndarray, ?dataType, ?storageType, ?isInitialized : bool, ?init : IInitializer, ?canCopy, ?ctx) = 
    inherit Variable()
    let isInitialized = defaultArg isInitialized false
    let canCopy = defaultArg canCopy false
    let shape = 
        match ndarray, shape with 
        | Some (ndarray : NDArray), None -> ndarray.Shape |> Some
        | Some (ndarray), Some s ->     
            let s = s |> Seq.toArray
            if ndarray.Shape.Length <> s.Length then 
                invalidArg "shape" (sprintf "NDArray shape %A is not compatable with given parameter shape %A" ndarray.Shape s)
            else
                (ndarray.Shape, s)
                ||> Array.iter2
                    (fun s1 s2 ->
                        if s1 = s2 || s2 = 0 || s2 = -1 then 
                            ()
                        else 
                            invalidArg "shape" (sprintf "NDArray shape %A is not compatable with given parameter shape %A" ndarray.Shape s)
                    )
                ndarray.Shape |> Some
        | None, Some s -> s |> Seq.toArray |> Some
        | None, None -> None
    do 
        match name with 
        | Some n -> base.Name <- n
        | None -> ()
    member x.Shape = shape
    member x.Grad = grad
    member x.OpReqType = opReqType
    member x.NDArray = ndarray
    member x.DataType = dataType 
    member x.StorageType = storageType
    member x.Initializer = init
    member x.CanCopy = canCopy
    member x.Binding = 
       {
           Name = x.Name 
           NDArray = x.NDArray 
           Shape = x.Shape
           DataType = x.DataType 
           StorageType = x.StorageType
           Initializer = init
           IsInitialized = isInitialized
           Context = ctx
           BindType = ArgBind(x.OpReqType, x.Grad)
           CanCopy = canCopy
       }

type Input(?name, ?shape, ?ndarray, ?dataType, ?storageType, ?init) = 
    inherit Parameter(?name = name, 
                      ?shape = shape, 
                      opReqType = OpReqType.NullOp, 
                      grad = new NDArray(), 
                      ?ndarray = ndarray, 
                      ?dataType = dataType, 
                      ?storageType = storageType,
                      init = defaultArg init (Initializer.Skip()))

type Constant(ndarray : NDArray, ?name) = 
    inherit Parameter(?name = name, 
                      shape = ndarray.Shape, 
                      opReqType = OpReqType.NullOp, 
                      grad = new NDArray(), 
                      ndarray = ndarray, 
                      ?dataType = ndarray.DataType, 
                      storageType = ndarray.StorageType,
                      isInitialized = true,
                      canCopy = true)

type Bindings(bindings : IDictionary<string, Bind>) = 
    new() = Bindings(Map.empty)
    member x.TryGetValue(name : string, [<Out>] value : Bind byref) = 
        let scc,v = bindings.TryGetValue(name)
        value <- v
        scc

    member x.WithBindings(newBindings : Bind seq) = 
        let d = Dictionary(bindings)
        newBindings |> Seq.iter (fun b -> d.[b.Name] <- b)
        Bindings d
    member x.ImpliedContext(?context : Context) =
        let ctx = 
            x 
            |> Seq.choose (fun x -> match x.Context with | None -> x.NDArray |> Option.map (fun x -> x.Context) | c -> c)
            |> Seq.distinct
            |> Seq.toList
        match ctx with 
        | [c] -> Some c
        | [] -> context
        | _ -> 
            if x |> Seq.exists (fun x -> x.CanCopy) then 
                let bs = 
                    x 
                    |> Seq.filter (fun x -> not x.CanCopy)
                    |> Seq.map (fun x -> x.Name, x)
                    |> dict
                    |> Bindings
                bs.ImpliedContext(?context = context)
            else 
                None
    member x.InferShapes(symbol : Symbol) =    
        let argNames = symbol.ArgumentNames
        let result = 
            argNames
            |> Array.choose 
                (fun name -> 
                    match bindings.TryGetValue(name) with 
                    | true, {Shape = Some s; BindType = ArgBind _} -> Some(name, s)
                    | _ -> None)
            |> MXSymbol.keyShapeToCsrForm uint32 
            |||> MXSymbol.inferShapePartial symbol.UnsafeHandle 
        let auxBindings = 
            (symbol.AuxiliaryStateNames, result.AuxShapes)
            ||> Array.map2 
                (fun name shape -> 
                    let shape = shape |> Array.map int
                    match bindings.TryGetValue(name) with
                    | true, ({Shape = Some s; BindType = AuxBind} as b) -> { b with Shape = Some shape}
                    | _ -> Bind.Aux(name, shape = shape)
                )
        let outBindings = 
            (symbol.OutputNames, result.OutputShapes)
            ||> Array.map2 
                (fun name shape -> 
                    let shape = shape |> Array.map int
                    match bindings.TryGetValue(name) with
                    | true, a -> {a with Shape = Some shape }
                    | _ -> Bind.Out(name, shape = shape)
                )
        let inBindings = 
            (argNames, result.InputShapes)
            ||> Array.map2 
                (fun name shape -> 
                    let shape = shape |> Array.map int
                    match bindings.TryGetValue(name) with
                    | true, ({BindType = ArgBind _} as a) -> {a with Shape = Some shape }
                    | _ -> Bind.Arg(name, shape = shape)
                )
        x.WithBindings(seq {yield! inBindings; yield! outBindings; yield! auxBindings})
    member x.InferTypes(symbol : Symbol) =    
        let argNames = symbol.ArgumentNames
        let result = 
            argNames
            |> Array.choose 
                (fun name -> 
                    match bindings.TryGetValue(name) with 
                    | true, {DataType = Some dt; BindType = ArgBind _} -> Some (name, int dt.TypeFlag)
                    | true, {DataType = None; NDArray = Some nd; BindType = ArgBind _} when nd.DataType.IsSome -> 
                        Some (name, int nd.DataTypeFlag)
                    | _ -> None)
            |> Array.unzip
            ||> MXSymbol.inferTypePartial symbol.UnsafeHandle 
        let auxBindings = 
            (symbol.AuxiliaryStateNames, result.AuxTypes)
            ||> Array.map2 
                (fun name t -> 
                    match bindings.TryGetValue(name) with
                    | true, ({BindType = AuxBind} as a) -> { a with DataType = DataType.FromInt t}
                    | _ ->  Bind.Aux(name, ?dataType = DataType.FromInt t)
                )
        let outBindings = 
            (symbol.OutputNames, result.OutputTypes)
            ||> Array.map2 
                (fun name t -> 
                    match bindings.TryGetValue(name) with
                    | true, a -> {a with DataType = DataType.FromInt t}
                    | _ -> Bind.Out(name, ?dataType = DataType.FromInt t)
                )
        let inBindings = 
            (argNames, result.InputTypes)
            ||> Array.map2 
                (fun name t -> 
                    match bindings.TryGetValue(name) with
                    | true, ({BindType = ArgBind _} as a)  -> {a with DataType = DataType.FromInt t}
                    | _ -> Bind.Arg(name, ?dataType = DataType.FromInt t)
                )
        x.WithBindings(seq {yield! inBindings; yield! outBindings; yield! auxBindings})
    member x.Bindings = bindings
    member x.Item 
        with get(name : string) = 
            let scc, b = x.TryGetValue(name)
            if not scc then 
                raise (KeyNotFoundException(sprintf "No binding for %s" name))
            b
    member x.Item 
        with get(v : Variable) = x.[v.Name]
    member x.NDArray(name : string) = 
        match x.[name].NDArray with 
        | Some x -> x
        | None -> 
            raise (NullReferenceException(sprintf "NDArray not set for binding %s" name))
    member x.NDArray(v : Variable) = 
        match x.[v].NDArray with 
        | Some x -> x
        | None -> 
            raise (NullReferenceException(sprintf "NDArray not set for binding %s" v.Name))
    member x.Grad(name : string) = 
        match x.[name] with 
        | {BindType = ArgBind(_,Some g)} -> g
        | _ -> 
            raise (NullReferenceException(sprintf "Grad not set for binding %s" name))
    member x.Grad(v : Variable) = 
        match x.[v] with 
        | {BindType = ArgBind(_,Some g)} -> g
        | _ -> 
            raise (NullReferenceException(sprintf "Grad not set for binding %s" v.Name))
    interface IEnumerable<Bind> with 
        member x.GetEnumerator() = bindings.Values.GetEnumerator()
        member x.GetEnumerator() = bindings.Values.GetEnumerator() :> System.Collections.IEnumerator

module Bind = 
    let fromVariable (v : Variable) =
        match v with 
        | :? Parameter as p -> p.Binding
        | _ -> Bind.Arg(v.Name)
    let shape (shape) (b : Bind) = {b with Shape = Some shape}
    let setOpReqType opReqType (b : Bind) = 
        match b with 
        | {BindType = ArgBind(_,g)} -> {b with BindType = ArgBind(Some opReqType, g)}
        | _ -> b
    let noGrad (b : Bind) = setOpReqType OpReqType.NullOp b
    let gradWriteTo (b : Bind) = setOpReqType OpReqType.WriteTo b
    let gradWriteInPlace (b : Bind) = setOpReqType OpReqType.WriteInplace b
    let gradAddTo (b : Bind) = setOpReqType OpReqType.AddTo b
    let init (x : Bind) = 
        if x.IsInitialized || x.IsOutBind then 
            x
        else
            match x.Initializer with 
            | Some initializer -> 
                let x = 
                    match x.NDArray, x.Shape, x.Context with 
                    | Some nd, Some shape, _ when nd.Shape.Length = 0 -> 
                        //TODO: validate shape
                        nd.MutReshape(shape) |> ignore
                        x
                    | Some nd, None, _ -> 
                        {x with Shape = Some nd.Shape}
                    | Some nd, Some s, _ -> x
                    | None, Some(shape), Some ctx -> 
                        {x with NDArray = Some(new NDArray(shape, ctx, ?dtype = x.DataType))}
                    | None, None, _ -> 
                        invalidOp (sprintf"Init called on '%s' with no NDArray or shape set" x.Name)
                    | None, _, None -> 
                        invalidOp (sprintf"Init called on '%s' with no context set" x.Name)
                if initializer.Initialize(x) then 
                    {x with IsInitialized = true}
                else 
                    x
            | None -> invalidOp (sprintf "Init called with no initializer set on %s" x.Name)



module Bindings = 
    /// Apply map f : Bind -> Bind on all aux bindings
    let mapAux f (bm : Bindings) = 
        bm
        |> Seq.map 
            (function 
             | AuxBinding a -> f a
             | x -> x
            )
        |> Seq.map (fun (x : Bind) -> x.Name, x)
        |> dict 
        |> Bindings
    /// Apply map f : Bind -> Bind on all arg bindings
    let mapArg f (bm : Bindings) = 
        bm
        |> Seq.map 
            (function 
             | ArgBinding a -> f a
             | x -> x
            )
        |> Seq.map (fun (x : Bind) -> x.Name, x)
        |> dict 
        |> Bindings
    /// Apply map f : Bind -> Bind on all bindings
    let map f (bm : Bindings) = 
        bm
        |> Seq.map f
        |> Seq.map (fun (x : Bind) -> x.Name, x)
        |> dict 
        |> Bindings
    /// Bindings from sequence of Bind
    let ofSeq l = Bindings().WithBindings l
    /// Infer the shape of symbol and it's arguments given bindings
    let inferShapes (s : Symbol) (bm : Bindings) = bm.InferShapes s
    /// Infer the data type of symbol and it's arguments given bindings
    let inferDataTypes (s : Symbol) (bm : Bindings) = bm.InferTypes s
    /// Apply mapping f to all bindings which are arguments to symbol
    let mapSymbolArgs (symbol : Symbol) f (bm : Bindings) = 
        let argNames = symbol.ArgumentNames |> Set.ofSeq
        bm
        |> mapArg
            (fun a ->
                if argNames.Contains a.Name then 
                    f a
                else
                    a
            )
    /// All OpReqType's set to NullOp (no gradient calc)
    let freezeGraph (symbol : Symbol) (bm : Bindings) = 
        bm 
        |> mapSymbolArgs symbol 
            (fun a -> 
                {a with BindType = a.BindType.WithOpReqType(NullOp)} 
            )
    /// Initilize Bindings with given Variables
    let inputs (variables : Variable seq) = 
        variables 
        |> Seq.map Bind.fromVariable
        |> ofSeq
    /// If shape[0] = 0 then set to given batchSize
    let batchSize batchSize (bm : Bindings) = 
        bm
        |> Seq.map 
            (fun x -> 
                match x.Shape with 
                | Some a when a.Length > 0 && a.[0] = 0 -> 
                    {x with Shape = Some [|yield batchSize; yield! a.[1..]|]}
                | _ -> x
            )
        |> ofSeq
    /// Fill missing OpReqType
    let defaultOpReqType opReqType (bm : Bindings) = 
        bm
        |> mapArg 
            (fun a ->
                match a with 
                | {BindType = ArgBind(None,g)} -> {a with BindType = ArgBind(Some opReqType,g)}
                | _ -> a
            )
    /// Fill missing NDArray's with f : Bind -> NDArray
    let fillNDArray f (bm : Bindings) = 
        bm
        |> map 
            (fun a ->
                match a.NDArray with 
                | None -> 
                    let nd : NDArray = f a 
                    match a.Shape with 
                    | None -> {a with NDArray = Some nd; Shape = Some nd.Shape}
                    | Some s when s = nd.Shape -> {a with NDArray = Some nd}
                    | Some s -> 
                        let nds = nd.Shape
                        if s.Length = 0 then 
                            {a with NDArray = Some nd; Shape = Some nds}
                        elif s.Length <> nds.Length then 
                            raise (RankException(sprintf "Given NDArray shape %A does not match binding %s shape %A" nds a.Name s))
                        else
                            for i = 0 to s.Length - 1 do 
                                if s.[i] > 0 && s.[i] <> nds.[i] then 
                                    raise (RankException(sprintf "Given NDArray shape %A does not match binding %s shape %A" nds a.Name s))
                            {a with NDArray = Some nd; Shape = Some nds}
                | _ -> a
            )
    let defaultContext ctx (bm : Bindings) =
        bm 
        |> Bindings.map 
            (fun a -> 
                if a.Context.IsNone then 
                    if a.NDArray.IsSome && a.NDArray.Value.Context <> ctx && a.CanCopy then 
                        {a with Context = Some ctx; NDArray = Some(ctx.CopyFrom a.NDArray.Value)}
                    else
                        {a with Context = Some ctx}
                else a
            )
    let setContext ctx (bm : Bindings) =
        bm 
        |> Bindings.map 
            (fun a -> 
                match a.NDArray with 
                | Some nd -> 
                    if nd.Context = ctx then 
                        {a with Context = Some ctx}
                    elif a.CanCopy then 
                        {a with Context = Some ctx; NDArray = Some(ctx.CopyFrom nd)}
                    else
                        invalidOp (sprintf "Cannot set context of %s to %O. Specified NDArray is on %O and is set to not copy." a.Name ctx nd.Context)
                | None -> 
                        {a with Context = Some ctx}
            )
    let defaultInitializer init (bm : Bindings) = 
        bm 
        |> Bindings.map 
            (fun a -> 
                if a.Initializer.IsNone then 
                    {a with Initializer = Some init}
                else a
            )
    let initWith initializer bm = bm |> defaultInitializer initializer |> init
    let initWithFunc f bm = bm |> defaultInitializer (Init.create f) |> init
    let appendInitializer initializer bm = 
        bm 
        |> Bindings.map 
            (fun b ->
                match b.Initializer with 
                | None -> {b with Initializer = Some initializer}
                | Some (:? Initializer.Mixed as i) -> 
                    {b with Initializer = Some(Initializer.Mixed(inits = [|yield! i.Initializers; initializer|]) :> IInitializer)}
                | Some i -> 
                    {b with Initializer = Some(Initializer.Mixed(i, initializer) :> IInitializer)}
            )
    let prependInitializer initializer bm = 
        bm 
        |> Bindings.map 
            (fun b ->
                match b.Initializer with 
                | None -> {b with Initializer = Some initializer}
                | Some (:? Initializer.Mixed as i) -> 
                    {b with Initializer = Some(Initializer.Mixed(inits = [|initializer; yield! i.Initializers|]) :> IInitializer)}
                | Some i -> 
                    {b with Initializer = Some(Initializer.Mixed(initializer, i) :> IInitializer)}
            )
    /// Default to OpReqType.WriteTo, zero grads and initialize NDArray
    let init (bm : Bindings) = 
        // if bindings contain a single context will assume that context for all bindings
        let bm2 = 
            match bm.ImpliedContext() with 
            | Some c -> defaultContext c bm
            | _ -> bm
        bm2 
        |> defaultOpReqType OpReqType.WriteTo
        |> Bindings.appendInitializer Init.defaultInit
        |> Bindings.map Bind.init
        |> map 
            (fun a ->
                match a with
                | { BindType = ArgBind((Some OpReqType.NullOp) as opReqType, None) } ->
                    {a with BindType = ArgBind(opReqType, Some(new NDArray()))}
                | { BindType = ArgBind(opReqType, None) } ->
                    {a with BindType = ArgBind(opReqType, Some(MX.ZerosLike(a.NDArray.Value)))}
                | _ -> a
            )
    let withBindings (bs : Bind seq) (bm : Bindings) = bm.WithBindings(ofSeq bs)
    let shareParameters (referenceBindings : Bindings) (bindings : Bindings) = 
        bindings 
        |> map 
            (fun a -> 
                let scc,v = referenceBindings.TryGetValue(a.Name)
                if scc && a.Shape = v.Shape then 
                    { a with NDArray = v.NDArray }
                else
                    a
            )
    let copyParameters (referenceBindings : Bindings) (bindings : Bindings) = 
        bindings 
        |> map 
            (fun a -> 
                let scc,v = referenceBindings.TryGetValue(a.Name)
                if scc && a.Shape = v.Shape then 
                    match a.NDArray, v.NDArray, a.Context with 
                    | _, None, _ -> a
                    | Some nd, Some src, _ -> 
                        nd.CopyFrom(src)
                        a
                    | None, Some src, Some ctx -> 
                        {a with NDArray = Some (ctx.CopyFrom(src))}
                    | None, Some src, None -> 
                        invalidOp (sprintf "Could not copy parameter %s. No context set on binding."  a.Name)
                else
                    a
            )

type SafeExecutorHandle(owner) = 
    inherit SafeHandle(0n, true)
    new() = new SafeExecutorHandle(true)
    new(ptr,owner) as this = new SafeExecutorHandle(owner) then this.SetHandle(ptr)
    override x.IsInvalid = x.handle <= 0n
    override x.ReleaseHandle() = CApi.MXExecutorFree x.handle = 0
    member x.UnsafeHandle = 
        if not x.IsClosed then
            x.handle
        else
            ObjectDisposedException("SafeExecutorHandle", "Executor handle has been closed") |> raise



type BindingIncompleteException(bind : Bind option, fieldOrName : string) =
    inherit Exception(
        match bind with 
        | Some bind -> 
            let tp = 
                match bind with 
                | {BindType = AuxBind } -> "Aux"
                | {BindType = ArgBind _} -> "Arg"
                | {BindType = OutBind _} -> "Out"
            sprintf "Bindings incomplete. Expecting %s in  %s binding '%s'" fieldOrName tp bind.Name
        | None -> 
            sprintf "Bindings incomplete. No binding for '%s'." fieldOrName 
        )
 
type Executor(handle : SafeExecutorHandle, symbol, context, contextMap, inArgs, argGrad, gradReqType, auxStates, sharedExecutor, outputs, bindMap) =   
    let mutable disposed = false
    let mutable outputs = outputs
    new(symbol : Symbol, context : Context, contextMap : IDictionary<string,Context>, inArgs, argGrad, gradReqType, auxStates, sharedExecutor : Executor option, bindMap : Bindings option) = 
        let inArgs = inArgs |> Seq.toArray
        let argGrad = argGrad |> Seq.toArray
        let gradReqType = gradReqType |> Seq.toArray
        let auxStates = auxStates |> Seq.toArray
        let inArgsHandles = inArgs |> Array.map (fun (x : NDArray) -> x.NDArrayHandle.UnsafeHandle)
        let argGradHandles = argGrad |> Array.map (fun (x : NDArray) -> x.NDArrayHandle.UnsafeHandle)
        let gradReqTypeHandles = gradReqType |> Array.map (fun (x : OpReqType) -> uint32 x.OpReqTypeInt)
        let auxStatesHandles = auxStates |> Array.map (fun (x : NDArray) -> x.NDArrayHandle.UnsafeHandle)
        let mapKeys,mapDevTypes,mapDevIds = 
            if contextMap.Count = 0 then 
                null,null,null
            else
                contextMap 
                |> Seq.map 
                    (fun kvp ->
                        kvp.Key, int kvp.Value.DeviceType, kvp.Value.DeviceId
                    )
                |> Seq.toArray
                |> Array.unzip3
        let sharedExecutorHandle = 
            match sharedExecutor with 
            | Some x ->
                x.UnsafeHandle
            | None -> 0n
        let h = MXExecutor.bindEX symbol.UnsafeHandle (int context.DeviceType) context.DeviceId mapKeys mapDevTypes mapDevIds inArgsHandles argGradHandles gradReqTypeHandles auxStatesHandles sharedExecutorHandle
        let safeHandle = new SafeExecutorHandle(h, true)
        let outputs = MXExecutor.outputs h |> Array.map (fun h -> new NDArray(new SafeNDArrayHandle(h, true)))
        new Executor(safeHandle,symbol,context,contextMap,inArgs,argGrad,gradReqType,auxStates,sharedExecutor,outputs, bindMap)
    new(symbol : Symbol, context : Context, contextMap : IDictionary<string,Context>, inArgs, argGrad, gradReqType, auxStates, sharedExecutor : Executor option) = 
        new Executor(symbol, context, contextMap, inArgs, argGrad, gradReqType, auxStates, sharedExecutor, None)
    new(symbol : Symbol, context, inArgs, argGrad, gradReqType, auxStates, bindMap) = 
        new Executor(symbol, context, Map.empty, inArgs,argGrad,gradReqType,auxStates,None,bindMap)
    new(symbol : Symbol, context, inArgs, argGrad, gradReqType, auxStates) = 
        new Executor(symbol, context, Map.empty, inArgs,argGrad,gradReqType,auxStates,None,None)
    new(symbol : Symbol, context, bindings : Bindings) = 
        let args = symbol.ArgumentNames
        let inArgs, argGrad, gradReqType = 
            args 
            |> Array.map 
                (fun name ->
                    match bindings.TryGetValue(name) with 
                    | true, (ArgBinding b) -> 
                        let a = match b.NDArray with Some a -> a | None -> raise (BindingIncompleteException(Some(b), "NDArray"))
                        let g = match b.BindType with ArgBind(_, Some grad) -> grad | _ -> raise (BindingIncompleteException(Some(b), "Grad"))
                        let t = match b.BindType with ArgBind(Some opReqType, _) -> opReqType | _ -> raise (BindingIncompleteException(Some(b), "OpReqType"))
                        a,g,t
                    | _ -> raise(BindingIncompleteException(None, name))
                )
            |> Array.unzip3
        let aux = 
            symbol.AuxiliaryStateNames
            |> Array.map 
                (fun name ->
                    match bindings.TryGetValue(name) with 
                    | true, (AuxBinding b) -> 
                        let a = match b.NDArray with Some a -> a | None -> raise (BindingIncompleteException(Some(b), "NDArray"))
                        a
                    | _ -> raise(BindingIncompleteException(None, name))
                )
        new Executor(symbol, context, inArgs, argGrad, gradReqType, aux, Some bindings)
    /// Refresh executor outputs. Returns false if nothing is updated.
    member x.RefreshOutputs() = 
        let mutable updated = false
        let handles = MXExecutor.outputs handle.UnsafeHandle
        if handles.Length = outputs.Length then 
            for i = 0 to outputs.Length - 1 do 
                if outputs.[i].UnsafeHandle <> handles.[i] then 
                    outputs.[i] <- new NDArray(new SafeNDArrayHandle(handles.[i], true))
                    updated <- true
            updated
        else
            outputs <- handles |> Array.map (fun h -> new NDArray(new SafeNDArrayHandle(h, true)))
            true
    member x.Print() = MXExecutor.print handle.UnsafeHandle
    member x.Bindings =  
        match bindMap with 
        | Some bm -> bm
        | None ->
            let args = Array.zip3 inArgs argGrad gradReqType
            seq {
                yield!
                    (symbol.ArgumentNames, args)
                    ||> Seq.map2
                        (fun name (a,g,t) ->
                            { 
                                Name = name
                                Shape = Some a.Shape
                                NDArray = Some a
                                StorageType = Some a.StorageType
                                DataType = a.DataType 
                                IsInitialized = true
                                Initializer = None
                                Context = Some a.Context
                                BindType = ArgBind(Some t, Some g)
                                CanCopy = false
                            }
                        )
                yield!
                    (symbol.AuxiliaryStateNames, auxStates)
                    ||> Seq.map2
                        (fun name a ->
                            { 
                                Name = name
                                Shape = Some a.Shape
                                NDArray = Some a
                                StorageType = Some a.StorageType
                                DataType = a.DataType
                                IsInitialized = true
                                Initializer = None
                                Context = Some a.Context
                                BindType = AuxBind
                                CanCopy = false
                            }
                        )
                yield!
                    (symbol.OutputNames, outputs)
                    ||> Seq.map2
                        (fun name a ->
                                { 
                                    Name = name
                                    Shape = Some a.Shape
                                    NDArray = Some a
                                    StorageType = Some a.StorageType
                                    DataType = a.DataType 
                                    IsInitialized = true
                                    Initializer = None
                                    Context = Some a.Context
                                    BindType = ArgBind(None,None)
                                    CanCopy = false
                                }
                        )
            }
            |> Bindings.ofSeq
    member x.Symbol = symbol
    member internal x.UnsafeHandle = handle.UnsafeHandle
    member x.Forward(isTraining : bool) = 
        let isTrain = if isTraining then 1 else 0
        MXExecutor.forward handle.UnsafeHandle isTrain
        x.RefreshOutputs() |> ignore
    member x.Backward() = 
        MXExecutor.backward handle.UnsafeHandle null
    member x.Backward(grads) = 
        grads
        |> Seq.map (fun (x : NDArray) -> x.NDArrayHandle.UnsafeHandle) 
        |> Seq.toArray
        |> MXExecutor.backward handle.UnsafeHandle
    member x.Outputs = outputs
    member x.Dispose(disposing) = 
        if not disposed then 
            if disposing then 
                handle.Dispose()
        disposed <- true
    member x.Dispose() = 
        x.Dispose(true)
        GC.SuppressFinalize(x)
    member x.ExecutorHandle = handle
    member x.Item 
        with get(v : Variable) = x.Bindings.NDArray(v)
    member x.Item 
        with get(name : string) = x.Bindings.NDArray(name)
    interface IDisposable with  
        member x.Dispose() = x.Dispose()

[<AutoOpen>]
module SymbolExtension =
    type Symbol with 
        member x.Bindings = 
            x 
            |> SymUtil.choose
                (fun symbol -> 
                    match symbol with 
                    | :? Parameter as p -> Some(false,p)
                    | _ -> None
                )
            |> Seq.cast
            |> Bindings.inputs
        member x.Bind(context, batchSize, bindings) = 
            let bindmap = 
                x.Bindings
                |> Bindings.withBindings bindings
                |> Bindings.batchSize batchSize 
                |> Bindings.setContext context
                |> Bindings.inferShapes x
                |> Bindings.inferDataTypes x
                |> Bindings.init
            new Executor(x,context,bindmap)
        member x.Bind(context, bindings) = 
            let bindmap = 
                x.Bindings
                |> Bindings.withBindings bindings
                |> Bindings.setContext context
                |> Bindings.inferShapes x
                |> Bindings.inferDataTypes x
                |> Bindings.init
            new Executor(x,context,bindmap)
        member x.Bind(context, batchSize) = 
            let bindmap = 
                x.Bindings
                |> Bindings.batchSize batchSize 
                |> Bindings.setContext context
                |> Bindings.inferShapes x
                |> Bindings.inferDataTypes x
                |> Bindings.init
            new Executor(x,context,bindmap)
        member x.Bind(context) = 
            let bindmap = 
                x.Bindings
                |> Bindings.setContext context
                |> Bindings.inferShapes x
                |> Bindings.inferDataTypes x
                |> Bindings.init
            new Executor(x,context,bindmap)
        member x.Bind(batchSize, bindings : Bindings) = 
            let bindmap = x.Bindings |> Bindings.withBindings bindings
            match bindmap.ImpliedContext() with 
            | Some context -> x.Bind(context, batchSize, bindings)
            | None -> invalidOp "Could not determine context for Bind"
        member x.Bind(bindings : Bindings) = 
            let bindmap = x.Bindings |> Bindings.withBindings bindings
            match bindmap.ImpliedContext() with 
            | Some context -> x.Bind(context, bindings)
            | None -> invalidOp "Could not determine context for Bind"
        member x.Bind(batchSize : int) = 
            match x.Bindings.ImpliedContext() with 
            | Some context -> x.Bind(context, batchSize)
            | None -> invalidOp "Could not determine context for Bind"
        member x.Bind() = 
            match x.Bindings.ImpliedContext() with 
            | Some context -> x.Bind(context)
            | None -> invalidOp "Could not determine context for Bind"
        member x.Eval(context) = 
            let exe = x.Bind(context)
            exe.Forward(false)
            exe
        member x.Eval(context, bindings : Bindings) = 
            let exe = x.Bind(context,bindings)
            exe.Forward(false)
            exe
        member x.Eval() = 
            let exe = x.Bind()
            exe.Forward(false)
            exe
        member x.Eval(bindings : Bindings) = 
            let exe = x.Bind(bindings)
            exe.Forward(false)
            exe
    
    
