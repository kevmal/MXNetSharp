namespace rec MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop

type SymbolInitilizationException(symbol : Symbol, inner : Exception) =
    inherit Exception(
        match inner  with 
        | null -> sprintf "Init failed on symbol %O" symbol
        | ex -> sprintf "Init failed on symbol %O: %s" symbol ex.Message)


[<AbstractClass>]
type Symbol() =
    //let mutable disposed = false
    member val internal InternalName : string option = None with get,set
    member val internal InternalHandle : SafeSymbolHandle option = None with get,set
    member x.IsInitialized = x.InternalHandle.IsSome
    //member internal x.CreateId() = 
    //    assert(x.IsInitialized)
    //    match x.Id with 
    //    | None ->
    //        let sid = Guid.NewGuid()
    //        MXSymbol.setAttr x.UnsafeHandle "mxnetsharp_symbolid" (string sid)
    //    | _ -> ()
    //member x.Id = 
    //    if x.IsInitialized then 
    //        MXSymbol.getAttr x.UnsafeHandle "mxnetsharp_symbolid"
    //    else    
    //        None
    member x.Name 
        with get() = 
            if x.IsInitialized then 
                match MXSymbol.getName x.UnsafeHandle with 
                | Some name -> name
                | None ->
                    match x.InternalName with Some n -> n | _ -> ""
            else
                match x.InternalName with Some n -> n | _ -> ""
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
            | None -> 
                // We should never really get to this point. Handle should be set or another excepion already thrown.
                raise (SymbolInitilizationException(x, null))
    member x.UnsafeHandle = x.SymbolHandle.UnsafeHandle //REVIEW: mark as internal?
    member x.Outputs = 
        let n = MXSymbol.getNumOutputs x.UnsafeHandle |> int
        Array.init n 
            (fun i ->
                let h = MXSymbol.getOutput x.UnsafeHandle i
                new SymbolOutput(x,new SafeSymbolHandle(h, true))
            )
    member x.ArgumentNames = MXSymbol.listArguments x.UnsafeHandle
    member x.InputSymbols = MXSymbol.getInputSymbols x.UnsafeHandle |> Array.map (fun h -> new SymbolInput(x, new SafeSymbolHandle(h,true)))
    abstract member Initialize : unit -> unit
    
    static member (+)(x : Symbol, y : float) = new PlusScalar(x,y)
    static member (+)(y : float, x : Symbol) = new PlusScalar(x,y)
    static member (+)(x : Symbol, y : Symbol) = new ElemwiseAdd(x,y)
    static member (.+)(x : Symbol, y : Symbol) = new BroadcastAdd(x,y)
    
    static member (-)(x : Symbol, y : float) = new MinusScalar(x,y)
    static member (-)(y : float, x : Symbol) = new RminusScalar(x,y)
    static member (-)(x : Symbol, y : Symbol) = new ElemwiseSub(x,y)
    static member (.-)(x : Symbol, y : Symbol) = new BroadcastSub(x,y)
    
    static member (/)(x : Symbol, y : float) = new DivScalar(x,y)
    static member (/)(y : float, x : Symbol) = new RdivScalar(x,y)
    static member (/)(x : Symbol, y : Symbol) = new ElemwiseDiv(x,y)
    static member (./)(x : Symbol, y : Symbol) = new BroadcastDiv(x,y)
    
    static member ( * )(x : Symbol, y : float) = new MulScalar(x,y)
    static member ( * )(y : float, x : Symbol) = new MulScalar(x,y)
    static member ( * )(x : Symbol, y : Symbol) = new ElemwiseMul(x,y)
    static member ( .* )(x : Symbol, y : Symbol) = new BroadcastMul(x,y)

    member x.Exp() = new Exp(x)
    static member Exp(x : Symbol) = new Exp(x) :> Symbol
    member x.Log() = new Log(x)
    static member Log(x : Symbol) = new Log(x) :> Symbol
    member x.Abs() = new Abs(x)
    static member Abs(x : Symbol) = new Abs(x) :> Symbol
    member x.Acos() = new Arccos(x)
    static member Acos(x : Symbol) = new Arccos(x) :> Symbol
    member x.Asin() = new Arcsin(x)
    static member Asin(x : Symbol) = new Arcsin(x) :> Symbol
    member x.Atan() = new Arctan(x)
    static member Atan(x : Symbol) = new Arctan(x) :> Symbol
    static member Atan2(x : #Symbol, y : #Symbol) = new NpiArctan2(x, y) 
    static member Atan2(x : #Symbol, y : double) = new NpiArctan2Scalar(x, y) 
    static member Atan2(y : double, x : #Symbol) = new NpiRarctan2Scalar(x, y) 
    static member ArcTan2(x : #Symbol, y : #Symbol) = new NpiArctan2(x, y) 
    static member ArcTan2(x : #Symbol, y : double) = new NpiArctan2Scalar(x, y) 
    static member AtcTan2(y : double, x : #Symbol) = new NpiRarctan2Scalar(x, y) 
    member x.Ceiling() = new Ceil(x)
    static member Ceiling(x : Symbol) = new Ceil(x) :> Symbol
    member x.Floor() = new Floor(x)
    static member Floor(x : Symbol) = new Floor(x) :> Symbol
    member x.Truncate() = new Trunc(x)
    static member Truncate(x : Symbol) = new Trunc(x) :> Symbol
    member x.Round() = new Round(x)
    static member Round(x : Symbol) = new Round(x) :> Symbol
    member x.Log10() = new Log10(x)
    static member Log10(x : Symbol) = new Log10(x) :> Symbol
    member x.Sqrt() = new Sqrt(x)
    static member Sqrt(x : Symbol) = new Sqrt(x) :> Symbol
    member x.Cos() = new Cos(x)
    static member Cos(x : Symbol) = new Cos(x) :> Symbol
    member x.Cosh() = new Cosh(x)
    static member Cosh(x : Symbol) = new Cosh(x) :> Symbol
    member x.Sin() = new Sin(x)
    static member Sin(x : Symbol) = new Sin(x) :> Symbol
    member x.Sinh() = new Sinh(x)
    static member Sinh(x : Symbol) = new Sinh(x) :> Symbol
    member x.Tan() = new Tan(x)
    static member Tan(x : Symbol) = new Tan(x) :> Symbol
    member x.Tanh() = new Tanh(x)
    static member Tanh(x : Symbol) = new Tanh(x) :> Symbol

(*
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
    interface IDisposable with  
        member x.Dispose() = x.Dispose()
*)

type SymbolOutput internal (parent : Symbol) = 
    inherit Symbol()
    new(parent, handle) as this = 
        new SymbolOutput(parent) then 
            this.InternalHandle <- Some handle
    member x.Parent = parent
    override x.Initialize() = ()


type SymbolInput internal (parent : Symbol) = 
    inherit Symbol()
    new(parent, handle) as this = 
        new SymbolInput(parent) then 
            this.InternalHandle <- Some handle
    member x.Parent = parent
    override x.Initialize() = ()

type Variable() =
    inherit Symbol()
    new (name : string) as this = 
        new Variable() then 
            this.InternalName <- Some name
    //member internal x.CreateId() = 
    //    assert(x.IsInitialized)
    //    match x.Id with 
    //    | None ->
    //        let sid = Guid.NewGuid()
    //        MXSymbol.setAttr x.UnsafeHandle "mxnetsharp_symbolid" (string sid)
    //    | _ -> ()
    override x.Initialize() =   
        match x.InternalHandle with 
        | Some _ -> ()
        | None -> 
            match x.InternalName with 
            | Some n -> 
                x.InternalHandle <- Some(new SafeSymbolHandle(MXSymbol.createVariable n,true))
                //x.CreateId()
            | None -> failwith "Variable needs a name" //TODO: make exception or auto naming?

type ImplicitVariable() = 
    inherit Variable() 
      
//TODO: fix histogram
//TODO: override tostring
//TODO: ctx = '' for NDArray ops is invalid even though it's often the default
//TODO: doc comments on generated symbols
//TODO: add a "With" method to copy with updates      
//TODO: We should add valiation to the specific symbol types
type SymbolOperator(creator : AtomicSymbolCreator, operatorArguments : Arguments<Symbol>) = 
    inherit Symbol()
    //let parametersStr = parameters |> Array.map (fun (k,v) -> k, Util.valueString v)
    new(name, args) = new SymbolOperator(AtomicSymbolCreator.FromName name, args)
    //new(creator,pnames,ps,inames,ins) = new SymbolOperator(creator, Array.zip pnames ps, Array.zip inames ins)
    member x.OperatorArguments = operatorArguments
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
                //x.CreateId()
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
            //x.CreateId()

// **************************************************************************************************************************************
// ** GENERATED SYMBOL TYPES SECTION
// ** The below Section is generated and should not be edited
// **************************************************************************************************************************************

(* GERNATED SYMBOL TYPES BEGIN *)//
type CachedOp private (operatorArguments) = 
    inherit SymbolOperator("_CachedOp", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new CachedOp(args)
    new([<Optional>] ?data : Symbol seq) =
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("", data)
            ]
        new CachedOp(Arguments<Symbol>(operatorArguments))
    new([<ParamArray>] data : Symbol[]) =
        let operatorArguments = 
            [
                "data", VarArg("", data)
            ]
        new CachedOp(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetVarArg "data"

type BatchNormV1 private (operatorArguments) = 
    inherit SymbolOperator("BatchNorm_v1", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BatchNormV1(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?gamma : Symbol,
        [<Optional>] ?beta : Symbol,
        [<Optional>] ?eps : float,
        [<Optional>] ?momentum : float,
        [<Optional>] ?fixGamma : bool,
        [<Optional>] ?useGlobalStats : bool,
        [<Optional>] ?outputMeanVar : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let gamma = defaultArg gamma (new ImplicitVariable() :> Symbol)
        let beta = defaultArg beta (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "gamma", Input gamma
                "beta", Input beta
                "eps", eps |> Option.map box |> Parameter
                "momentum", momentum |> Option.map box |> Parameter
                "fix_gamma", fixGamma |> Option.map box |> Parameter
                "use_global_stats", useGlobalStats |> Option.map box |> Parameter
                "output_mean_var", outputMeanVar |> Option.map box |> Parameter
            ]
        new BatchNormV1(Arguments<Symbol>(operatorArguments))
    static member EpsDefault : double = 0.00100000005
    static member MomentumDefault : double = 0.899999976
    static member FixGammaDefault : bool = true
    static member UseGlobalStatsDefault : bool = false
    static member OutputMeanVarDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Gamma = operatorArguments.GetInput "gamma"
    member __.Beta = operatorArguments.GetInput "beta"
    member __.Eps = operatorArguments.GetParameter("eps", BatchNormV1.EpsDefault)
    member __.Momentum = operatorArguments.GetParameter("momentum", BatchNormV1.MomentumDefault)
    member __.FixGamma = operatorArguments.GetParameter("fix_gamma", BatchNormV1.FixGammaDefault)
    member __.UseGlobalStats = operatorArguments.GetParameter("use_global_stats", BatchNormV1.UseGlobalStatsDefault)
    member __.OutputMeanVar = operatorArguments.GetParameter("output_mean_var", BatchNormV1.OutputMeanVarDefault)

type MpAdamwUpdate private (operatorArguments) = 
    inherit SymbolOperator("_mp_adamw_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MpAdamwUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        mean : Symbol,
        var : Symbol,
        weight32 : Symbol,
        rescaleGrad : Symbol,
        lr : float,
        eta : float,
        [<Optional>] ?beta1 : float,
        [<Optional>] ?beta2 : float,
        [<Optional>] ?epsilon : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?clipGradient : float) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mean", Input mean
                "var", Input var
                "weight32", Input weight32
                "rescale_grad", Input rescaleGrad
                "lr", Parameter(Some(box lr))
                "eta", Parameter(Some(box eta))
                "beta1", beta1 |> Option.map box |> Parameter
                "beta2", beta2 |> Option.map box |> Parameter
                "epsilon", epsilon |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
            ]
        new MpAdamwUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        eta : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?mean : Symbol,
        [<Optional>] ?var : Symbol,
        [<Optional>] ?weight32 : Symbol,
        [<Optional>] ?rescaleGrad : Symbol,
        [<Optional>] ?beta1 : float,
        [<Optional>] ?beta2 : float,
        [<Optional>] ?epsilon : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?clipGradient : float) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let mean = defaultArg mean (new ImplicitVariable() :> Symbol)
        let var = defaultArg var (new ImplicitVariable() :> Symbol)
        let weight32 = defaultArg weight32 (new ImplicitVariable() :> Symbol)
        let rescaleGrad = defaultArg rescaleGrad (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mean", Input mean
                "var", Input var
                "weight32", Input weight32
                "rescale_grad", Input rescaleGrad
                "lr", Parameter(Some(box lr))
                "eta", Parameter(Some(box eta))
                "beta1", beta1 |> Option.map box |> Parameter
                "beta2", beta2 |> Option.map box |> Parameter
                "epsilon", epsilon |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
            ]
        new MpAdamwUpdate(Arguments<Symbol>(operatorArguments))
    static member Beta1Default : double = 0.899999976
    static member Beta2Default : double = 0.999000013
    static member EpsilonDefault : double = 0.0000000099999999
    static member WdDefault : double = 0.0
    static member ClipGradientDefault : double = -1.0
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.Mean = operatorArguments.GetInput "mean"
    member __.Var = operatorArguments.GetInput "var"
    member __.Weight32 = operatorArguments.GetInput "weight32"
    member __.RescaleGrad = operatorArguments.GetInput "rescale_grad"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.Eta : float = match operatorArguments.GetParameter "eta" with Some(v) -> unbox v | None -> failwithf "Required parameter eta is missing"
    member __.Beta1 = operatorArguments.GetParameter("beta1", MpAdamwUpdate.Beta1Default)
    member __.Beta2 = operatorArguments.GetParameter("beta2", MpAdamwUpdate.Beta2Default)
    member __.Epsilon = operatorArguments.GetParameter("epsilon", MpAdamwUpdate.EpsilonDefault)
    member __.Wd = operatorArguments.GetParameter("wd", MpAdamwUpdate.WdDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", MpAdamwUpdate.ClipGradientDefault)

type AdamwUpdate private (operatorArguments) = 
    inherit SymbolOperator("_adamw_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new AdamwUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        mean : Symbol,
        var : Symbol,
        rescaleGrad : Symbol,
        lr : float,
        eta : float,
        [<Optional>] ?beta1 : float,
        [<Optional>] ?beta2 : float,
        [<Optional>] ?epsilon : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?clipGradient : float) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mean", Input mean
                "var", Input var
                "rescale_grad", Input rescaleGrad
                "lr", Parameter(Some(box lr))
                "eta", Parameter(Some(box eta))
                "beta1", beta1 |> Option.map box |> Parameter
                "beta2", beta2 |> Option.map box |> Parameter
                "epsilon", epsilon |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
            ]
        new AdamwUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        eta : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?mean : Symbol,
        [<Optional>] ?var : Symbol,
        [<Optional>] ?rescaleGrad : Symbol,
        [<Optional>] ?beta1 : float,
        [<Optional>] ?beta2 : float,
        [<Optional>] ?epsilon : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?clipGradient : float) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let mean = defaultArg mean (new ImplicitVariable() :> Symbol)
        let var = defaultArg var (new ImplicitVariable() :> Symbol)
        let rescaleGrad = defaultArg rescaleGrad (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mean", Input mean
                "var", Input var
                "rescale_grad", Input rescaleGrad
                "lr", Parameter(Some(box lr))
                "eta", Parameter(Some(box eta))
                "beta1", beta1 |> Option.map box |> Parameter
                "beta2", beta2 |> Option.map box |> Parameter
                "epsilon", epsilon |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
            ]
        new AdamwUpdate(Arguments<Symbol>(operatorArguments))
    static member Beta1Default : double = 0.899999976
    static member Beta2Default : double = 0.999000013
    static member EpsilonDefault : double = 0.0000000099999999
    static member WdDefault : double = 0.0
    static member ClipGradientDefault : double = -1.0
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.Mean = operatorArguments.GetInput "mean"
    member __.Var = operatorArguments.GetInput "var"
    member __.RescaleGrad = operatorArguments.GetInput "rescale_grad"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.Eta : float = match operatorArguments.GetParameter "eta" with Some(v) -> unbox v | None -> failwithf "Required parameter eta is missing"
    member __.Beta1 = operatorArguments.GetParameter("beta1", AdamwUpdate.Beta1Default)
    member __.Beta2 = operatorArguments.GetParameter("beta2", AdamwUpdate.Beta2Default)
    member __.Epsilon = operatorArguments.GetParameter("epsilon", AdamwUpdate.EpsilonDefault)
    member __.Wd = operatorArguments.GetParameter("wd", AdamwUpdate.WdDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", AdamwUpdate.ClipGradientDefault)

type ContribAdaptiveAvgPooling2D private (operatorArguments) = 
    inherit SymbolOperator("_contrib_AdaptiveAvgPooling2D", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribAdaptiveAvgPooling2D(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?outputSize : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "output_size", outputSize |> Option.map box |> Parameter
            ]
        new ContribAdaptiveAvgPooling2D(Arguments<Symbol>(operatorArguments))
    static member OutputSizeDefault : int [] = [||]
    member __.Data = operatorArguments.GetInput "data"
    member __.OutputSize = operatorArguments.GetParameter("output_size", ContribAdaptiveAvgPooling2D.OutputSizeDefault)

type MultiAllFinite private (operatorArguments) = 
    inherit SymbolOperator("multi_all_finite", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MultiAllFinite(args)
    new([<Optional>] ?data : Symbol seq,
        [<Optional>] ?numArrays : int,
        [<Optional>] ?initOutput : bool) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "num_arrays", numArrays |> Option.map box |> Parameter
                "init_output", initOutput |> Option.map box |> Parameter
            ]
        new MultiAllFinite(Arguments<Symbol>(operatorArguments))
    static member NumArraysDefault : int = 1
    static member InitOutputDefault : bool = true
    member __.Data = operatorArguments.GetVarArg "data"
    member __.NumArrays = operatorArguments.GetParameter("num_arrays", MultiAllFinite.NumArraysDefault)
    member __.InitOutput = operatorArguments.GetParameter("init_output", MultiAllFinite.InitOutputDefault)

type ContribBilinearResize2D private (operatorArguments) = 
    inherit SymbolOperator("_contrib_BilinearResize2D", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribBilinearResize2D(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?like : Symbol,
        [<Optional>] ?height : int,
        [<Optional>] ?width : int,
        [<Optional>] ?scaleHeight : float,
        [<Optional>] ?scaleWidth : float,
        [<Optional>] ?mode : ContribBilinearResize2DMode) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let like = defaultArg like (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "like", Input like
                "height", height |> Option.map box |> Parameter
                "width", width |> Option.map box |> Parameter
                "scale_height", scaleHeight |> Option.map box |> Parameter
                "scale_width", scaleWidth |> Option.map box |> Parameter
                "mode", mode |> Option.map box |> Parameter
            ]
        new ContribBilinearResize2D(Arguments<Symbol>(operatorArguments))
    static member HeightDefault : int = 1
    static member WidthDefault : int = 1
    static member ScaleHeightDefault : double option = None
    static member ScaleWidthDefault : double option = None
    static member ModeDefault : ContribBilinearResize2DMode = ContribBilinearResize2DMode.Size
    member __.Data = operatorArguments.GetInput "data"
    member __.Like = operatorArguments.GetInput "like"
    member __.Height = operatorArguments.GetParameter("height", ContribBilinearResize2D.HeightDefault)
    member __.Width = operatorArguments.GetParameter("width", ContribBilinearResize2D.WidthDefault)
    member __.ScaleHeight = operatorArguments.GetParameter("scale_height", ContribBilinearResize2D.ScaleHeightDefault)
    member __.ScaleWidth = operatorArguments.GetParameter("scale_width", ContribBilinearResize2D.ScaleWidthDefault)
    member __.Mode = operatorArguments.GetParameter("mode", ContribBilinearResize2D.ModeDefault)

type ContribBooleanMask private (operatorArguments) = 
    inherit SymbolOperator("_contrib_boolean_mask", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribBooleanMask(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?index : Symbol,
        [<Optional>] ?axis : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let index = defaultArg index (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "index", Input index
                "axis", axis |> Option.map box |> Parameter
            ]
        new ContribBooleanMask(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int = 0
    member __.Data = operatorArguments.GetInput "data"
    member __.Index = operatorArguments.GetInput "index"
    member __.Axis = operatorArguments.GetParameter("axis", ContribBooleanMask.AxisDefault)

type ContribBoxNms private (operatorArguments) = 
    inherit SymbolOperator("_contrib_box_nms", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribBoxNms(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?overlapThresh : float,
        [<Optional>] ?validThresh : float,
        [<Optional>] ?topk : int,
        [<Optional>] ?coordStart : int,
        [<Optional>] ?scoreIndex : int,
        [<Optional>] ?idIndex : int,
        [<Optional>] ?backgroundId : int,
        [<Optional>] ?forceSuppress : bool,
        [<Optional>] ?inFormat : Format,
        [<Optional>] ?outFormat : Format) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "overlap_thresh", overlapThresh |> Option.map box |> Parameter
                "valid_thresh", validThresh |> Option.map box |> Parameter
                "topk", topk |> Option.map box |> Parameter
                "coord_start", coordStart |> Option.map box |> Parameter
                "score_index", scoreIndex |> Option.map box |> Parameter
                "id_index", idIndex |> Option.map box |> Parameter
                "background_id", backgroundId |> Option.map box |> Parameter
                "force_suppress", forceSuppress |> Option.map box |> Parameter
                "in_format", inFormat |> Option.map box |> Parameter
                "out_format", outFormat |> Option.map box |> Parameter
            ]
        new ContribBoxNms(Arguments<Symbol>(operatorArguments))
    static member OverlapThreshDefault : double = 0.5
    static member ValidThreshDefault : double = 0.0
    static member TopkDefault : int = -1
    static member CoordStartDefault : int = 2
    static member ScoreIndexDefault : int = 1
    static member IdIndexDefault : int = -1
    static member BackgroundIdDefault : int = -1
    static member ForceSuppressDefault : bool = false
    static member InFormatDefault : Format = Format.Corner
    static member OutFormatDefault : Format = Format.Corner
    member __.Data = operatorArguments.GetInput "data"
    member __.OverlapThresh = operatorArguments.GetParameter("overlap_thresh", ContribBoxNms.OverlapThreshDefault)
    member __.ValidThresh = operatorArguments.GetParameter("valid_thresh", ContribBoxNms.ValidThreshDefault)
    member __.Topk = operatorArguments.GetParameter("topk", ContribBoxNms.TopkDefault)
    member __.CoordStart = operatorArguments.GetParameter("coord_start", ContribBoxNms.CoordStartDefault)
    member __.ScoreIndex = operatorArguments.GetParameter("score_index", ContribBoxNms.ScoreIndexDefault)
    member __.IdIndex = operatorArguments.GetParameter("id_index", ContribBoxNms.IdIndexDefault)
    member __.BackgroundId = operatorArguments.GetParameter("background_id", ContribBoxNms.BackgroundIdDefault)
    member __.ForceSuppress = operatorArguments.GetParameter("force_suppress", ContribBoxNms.ForceSuppressDefault)
    member __.InFormat = operatorArguments.GetParameter("in_format", ContribBoxNms.InFormatDefault)
    member __.OutFormat = operatorArguments.GetParameter("out_format", ContribBoxNms.OutFormatDefault)

type ContribBoxIou private (operatorArguments) = 
    inherit SymbolOperator("_contrib_box_iou", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribBoxIou(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol,
        [<Optional>] ?format : Format) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
                "format", format |> Option.map box |> Parameter
            ]
        new ContribBoxIou(Arguments<Symbol>(operatorArguments))
    static member FormatDefault : Format = Format.Corner
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"
    member __.Format = operatorArguments.GetParameter("format", ContribBoxIou.FormatDefault)

type ContribBipartiteMatching private (operatorArguments) = 
    inherit SymbolOperator("_contrib_bipartite_matching", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribBipartiteMatching(args)
    new(data : Symbol,
        threshold : float,
        [<Optional>] ?isAscend : bool,
        [<Optional>] ?topk : int) = 
        let operatorArguments = 
            [
                "data", Input data
                "threshold", Parameter(Some(box threshold))
                "is_ascend", isAscend |> Option.map box |> Parameter
                "topk", topk |> Option.map box |> Parameter
            ]
        new ContribBipartiteMatching(Arguments<Symbol>(operatorArguments))
    new(threshold : float,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?isAscend : bool,
        [<Optional>] ?topk : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "threshold", Parameter(Some(box threshold))
                "is_ascend", isAscend |> Option.map box |> Parameter
                "topk", topk |> Option.map box |> Parameter
            ]
        new ContribBipartiteMatching(Arguments<Symbol>(operatorArguments))
    static member IsAscendDefault : bool = false
    static member TopkDefault : int = -1
    member __.Data = operatorArguments.GetInput "data"
    member __.Threshold : float = match operatorArguments.GetParameter "threshold" with Some(v) -> unbox v | None -> failwithf "Required parameter threshold is missing"
    member __.IsAscend = operatorArguments.GetParameter("is_ascend", ContribBipartiteMatching.IsAscendDefault)
    member __.Topk = operatorArguments.GetParameter("topk", ContribBipartiteMatching.TopkDefault)

type ContribDglCsrNeighborUniformSample private (operatorArguments) = 
    inherit SymbolOperator("_contrib_dgl_csr_neighbor_uniform_sample", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribDglCsrNeighborUniformSample(args)
    new([<Optional>] ?csrMatrix : Symbol,
        [<Optional>] ?seedArrays : Symbol seq,
        [<Optional>] ?numHops : int64,
        [<Optional>] ?numNeighbor : int64,
        [<Optional>] ?maxNumVertices : int64) = 
        let csrMatrix = defaultArg csrMatrix (new ImplicitVariable() :> Symbol)
        let seedArrays = defaultArg (seedArrays |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "csr_matrix", Input csrMatrix
                "seed_arrays", VarArg("num_args", seedArrays)
                "num_hops", numHops |> Option.map box |> Parameter
                "num_neighbor", numNeighbor |> Option.map box |> Parameter
                "max_num_vertices", maxNumVertices |> Option.map box |> Parameter
            ]
        new ContribDglCsrNeighborUniformSample(Arguments<Symbol>(operatorArguments))
    static member NumHopsDefault : int64 = 1L
    static member NumNeighborDefault : int64 = 2L
    static member MaxNumVerticesDefault : int64 = 100L
    member __.CsrMatrix = operatorArguments.GetInput "csr_matrix"
    member __.SeedArrays = operatorArguments.GetVarArg "seed_arrays"
    member __.NumHops = operatorArguments.GetParameter("num_hops", ContribDglCsrNeighborUniformSample.NumHopsDefault)
    member __.NumNeighbor = operatorArguments.GetParameter("num_neighbor", ContribDglCsrNeighborUniformSample.NumNeighborDefault)
    member __.MaxNumVertices = operatorArguments.GetParameter("max_num_vertices", ContribDglCsrNeighborUniformSample.MaxNumVerticesDefault)

type ContribDglCsrNeighborNonUniformSample private (operatorArguments) = 
    inherit SymbolOperator("_contrib_dgl_csr_neighbor_non_uniform_sample", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribDglCsrNeighborNonUniformSample(args)
    new([<Optional>] ?csrMatrix : Symbol,
        [<Optional>] ?probability : Symbol,
        [<Optional>] ?seedArrays : Symbol seq,
        [<Optional>] ?numHops : int64,
        [<Optional>] ?numNeighbor : int64,
        [<Optional>] ?maxNumVertices : int64) = 
        let csrMatrix = defaultArg csrMatrix (new ImplicitVariable() :> Symbol)
        let probability = defaultArg probability (new ImplicitVariable() :> Symbol)
        let seedArrays = defaultArg (seedArrays |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "csr_matrix", Input csrMatrix
                "probability", Input probability
                "seed_arrays", VarArg("num_args", seedArrays)
                "num_hops", numHops |> Option.map box |> Parameter
                "num_neighbor", numNeighbor |> Option.map box |> Parameter
                "max_num_vertices", maxNumVertices |> Option.map box |> Parameter
            ]
        new ContribDglCsrNeighborNonUniformSample(Arguments<Symbol>(operatorArguments))
    static member NumHopsDefault : int64 = 1L
    static member NumNeighborDefault : int64 = 2L
    static member MaxNumVerticesDefault : int64 = 100L
    member __.CsrMatrix = operatorArguments.GetInput "csr_matrix"
    member __.Probability = operatorArguments.GetInput "probability"
    member __.SeedArrays = operatorArguments.GetVarArg "seed_arrays"
    member __.NumHops = operatorArguments.GetParameter("num_hops", ContribDglCsrNeighborNonUniformSample.NumHopsDefault)
    member __.NumNeighbor = operatorArguments.GetParameter("num_neighbor", ContribDglCsrNeighborNonUniformSample.NumNeighborDefault)
    member __.MaxNumVertices = operatorArguments.GetParameter("max_num_vertices", ContribDglCsrNeighborNonUniformSample.MaxNumVerticesDefault)

type ContribDglSubgraph private (operatorArguments) = 
    inherit SymbolOperator("_contrib_dgl_subgraph", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribDglSubgraph(args)
    new(graph : Symbol,
        data : Symbol seq,
        returnMapping : bool) = 
        let operatorArguments = 
            [
                "graph", Input graph
                "data", VarArg("num_args", data |> Seq.toArray)
                "return_mapping", Parameter(Some(box returnMapping))
            ]
        new ContribDglSubgraph(Arguments<Symbol>(operatorArguments))
    new(returnMapping : bool,
        [<Optional>] ?graph : Symbol,
        [<Optional>] ?data : Symbol seq) = 
        let graph = defaultArg graph (new ImplicitVariable() :> Symbol)
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "graph", Input graph
                "data", VarArg("num_args", data)
                "return_mapping", Parameter(Some(box returnMapping))
            ]
        new ContribDglSubgraph(Arguments<Symbol>(operatorArguments))
    member __.Graph = operatorArguments.GetInput "graph"
    member __.Data = operatorArguments.GetVarArg "data"
    member __.ReturnMapping : bool = match operatorArguments.GetParameter "return_mapping" with Some(v) -> unbox v | None -> failwithf "Required parameter return_mapping is missing"

type ContribEdgeId private (operatorArguments) = 
    inherit SymbolOperator("_contrib_edge_id", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribEdgeId(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?u : Symbol,
        [<Optional>] ?v : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let u = defaultArg u (new ImplicitVariable() :> Symbol)
        let v = defaultArg v (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "u", Input u
                "v", Input v
            ]
        new ContribEdgeId(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.U = operatorArguments.GetInput "u"
    member __.V = operatorArguments.GetInput "v"

type ContribDglAdjacency private (operatorArguments) = 
    inherit SymbolOperator("_contrib_dgl_adjacency", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribDglAdjacency(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new ContribDglAdjacency(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type ContribDglGraphCompact private (operatorArguments) = 
    inherit SymbolOperator("_contrib_dgl_graph_compact", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribDglGraphCompact(args)
    new(graphData : Symbol seq,
        returnMapping : bool,
        graphSizes : int64 seq) = 
        let operatorArguments = 
            [
                "graph_data", VarArg("num_args", graphData |> Seq.toArray)
                "return_mapping", Parameter(Some(box returnMapping))
                "graph_sizes", Parameter(Some(box graphSizes))
            ]
        new ContribDglGraphCompact(Arguments<Symbol>(operatorArguments))
    new(returnMapping : bool,
        graphSizes : int64 seq,
        [<Optional>] ?graphData : Symbol seq) = 
        let graphData = defaultArg (graphData |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "graph_data", VarArg("num_args", graphData)
                "return_mapping", Parameter(Some(box returnMapping))
                "graph_sizes", Parameter(Some(box graphSizes))
            ]
        new ContribDglGraphCompact(Arguments<Symbol>(operatorArguments))
    new(returnMapping : bool,
        graphSizes : int64 seq,
        [<ParamArray>] graphData : Symbol[]) = 
        let operatorArguments = 
            [
                "graph_data", VarArg("num_args", graphData)
                "return_mapping", Parameter(Some(box returnMapping))
                "graph_sizes", Parameter(Some(box graphSizes))
            ]
        new ContribDglGraphCompact(Arguments<Symbol>(operatorArguments))
    member __.GraphData = operatorArguments.GetVarArg "graph_data"
    member __.ReturnMapping : bool = match operatorArguments.GetParameter "return_mapping" with Some(v) -> unbox v | None -> failwithf "Required parameter return_mapping is missing"
    member __.GraphSizes : int64 seq = match operatorArguments.GetParameter "graph_sizes" with Some(v) -> unbox v | None -> failwithf "Required parameter graph_sizes is missing"

type ContribGradientmultiplier private (operatorArguments) = 
    inherit SymbolOperator("_contrib_gradientmultiplier", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribGradientmultiplier(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new ContribGradientmultiplier(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new ContribGradientmultiplier(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type ContribBackwardGradientmultiplier private (operatorArguments) = 
    inherit SymbolOperator("_contrib_backward_gradientmultiplier", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribBackwardGradientmultiplier(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new ContribBackwardGradientmultiplier(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new ContribBackwardGradientmultiplier(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type ContribHawkesll private (operatorArguments) = 
    inherit SymbolOperator("_contrib_hawkesll", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribHawkesll(args)
    new([<Optional>] ?lda : Symbol,
        [<Optional>] ?alpha : Symbol,
        [<Optional>] ?beta : Symbol,
        [<Optional>] ?state : Symbol,
        [<Optional>] ?lags : Symbol,
        [<Optional>] ?marks : Symbol,
        [<Optional>] ?validLength : Symbol,
        [<Optional>] ?maxTime : Symbol) = 
        let lda = defaultArg lda (new ImplicitVariable() :> Symbol)
        let alpha = defaultArg alpha (new ImplicitVariable() :> Symbol)
        let beta = defaultArg beta (new ImplicitVariable() :> Symbol)
        let state = defaultArg state (new ImplicitVariable() :> Symbol)
        let lags = defaultArg lags (new ImplicitVariable() :> Symbol)
        let marks = defaultArg marks (new ImplicitVariable() :> Symbol)
        let validLength = defaultArg validLength (new ImplicitVariable() :> Symbol)
        let maxTime = defaultArg maxTime (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lda", Input lda
                "alpha", Input alpha
                "beta", Input beta
                "state", Input state
                "lags", Input lags
                "marks", Input marks
                "valid_length", Input validLength
                "max_time", Input maxTime
            ]
        new ContribHawkesll(Arguments<Symbol>(operatorArguments))
    member __.Lda = operatorArguments.GetInput "lda"
    member __.Alpha = operatorArguments.GetInput "alpha"
    member __.Beta = operatorArguments.GetInput "beta"
    member __.State = operatorArguments.GetInput "state"
    member __.Lags = operatorArguments.GetInput "lags"
    member __.Marks = operatorArguments.GetInput "marks"
    member __.ValidLength = operatorArguments.GetInput "valid_length"
    member __.MaxTime = operatorArguments.GetInput "max_time"

type ContribIndexArray private (operatorArguments) = 
    inherit SymbolOperator("_contrib_index_array", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribIndexArray(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axes : int seq) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axes", axes |> Option.map box |> Parameter
            ]
        new ContribIndexArray(Arguments<Symbol>(operatorArguments))
    static member AxesDefault : int [] option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Axes = operatorArguments.GetParameter("axes", ContribIndexArray.AxesDefault)

type ContribIndexCopy private (operatorArguments) = 
    inherit SymbolOperator("_contrib_index_copy", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribIndexCopy(args)
    new([<Optional>] ?oldTensor : Symbol,
        [<Optional>] ?indexVector : Symbol,
        [<Optional>] ?newTensor : Symbol) = 
        let oldTensor = defaultArg oldTensor (new ImplicitVariable() :> Symbol)
        let indexVector = defaultArg indexVector (new ImplicitVariable() :> Symbol)
        let newTensor = defaultArg newTensor (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "old_tensor", Input oldTensor
                "index_vector", Input indexVector
                "new_tensor", Input newTensor
            ]
        new ContribIndexCopy(Arguments<Symbol>(operatorArguments))
    member __.OldTensor = operatorArguments.GetInput "old_tensor"
    member __.IndexVector = operatorArguments.GetInput "index_vector"
    member __.NewTensor = operatorArguments.GetInput "new_tensor"

type KhatriRao private (operatorArguments) = 
    inherit SymbolOperator("khatri_rao", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new KhatriRao(args)
    new([<Optional>] ?args : Symbol seq) =
        let args = defaultArg (args |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "args", VarArg("num_args", args)
            ]
        new KhatriRao(Arguments<Symbol>(operatorArguments))
    new([<ParamArray>] args : Symbol[]) =
        let operatorArguments = 
            [
                "args", VarArg("num_args", args)
            ]
        new KhatriRao(Arguments<Symbol>(operatorArguments))
    member __.Args = operatorArguments.GetVarArg "args"

type MultiLars private (operatorArguments) = 
    inherit SymbolOperator("multi_lars", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MultiLars(args)
    new(lrs : Symbol,
        weightsSumSq : Symbol,
        gradsSumSq : Symbol,
        wds : Symbol,
        eta : float,
        eps : float,
        [<Optional>] ?rescaleGrad : float) = 
        let operatorArguments = 
            [
                "lrs", Input lrs
                "weights_sum_sq", Input weightsSumSq
                "grads_sum_sq", Input gradsSumSq
                "wds", Input wds
                "eta", Parameter(Some(box eta))
                "eps", Parameter(Some(box eps))
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
            ]
        new MultiLars(Arguments<Symbol>(operatorArguments))
    new(eta : float,
        eps : float,
        [<Optional>] ?lrs : Symbol,
        [<Optional>] ?weightsSumSq : Symbol,
        [<Optional>] ?gradsSumSq : Symbol,
        [<Optional>] ?wds : Symbol,
        [<Optional>] ?rescaleGrad : float) = 
        let lrs = defaultArg lrs (new ImplicitVariable() :> Symbol)
        let weightsSumSq = defaultArg weightsSumSq (new ImplicitVariable() :> Symbol)
        let gradsSumSq = defaultArg gradsSumSq (new ImplicitVariable() :> Symbol)
        let wds = defaultArg wds (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lrs", Input lrs
                "weights_sum_sq", Input weightsSumSq
                "grads_sum_sq", Input gradsSumSq
                "wds", Input wds
                "eta", Parameter(Some(box eta))
                "eps", Parameter(Some(box eps))
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
            ]
        new MultiLars(Arguments<Symbol>(operatorArguments))
    static member RescaleGradDefault : double = 1.0
    member __.Lrs = operatorArguments.GetInput "lrs"
    member __.WeightsSumSq = operatorArguments.GetInput "weights_sum_sq"
    member __.GradsSumSq = operatorArguments.GetInput "grads_sum_sq"
    member __.Wds = operatorArguments.GetInput "wds"
    member __.Eta : float = match operatorArguments.GetParameter "eta" with Some(v) -> unbox v | None -> failwithf "Required parameter eta is missing"
    member __.Eps : float = match operatorArguments.GetParameter "eps" with Some(v) -> unbox v | None -> failwithf "Required parameter eps is missing"
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", MultiLars.RescaleGradDefault)

type MultiSumSq private (operatorArguments) = 
    inherit SymbolOperator("multi_sum_sq", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MultiSumSq(args)
    new(data : Symbol seq,
        numArrays : int) = 
        let operatorArguments = 
            [
                "data", VarArg("", data |> Seq.toArray)
                "num_arrays", Parameter(Some(box numArrays))
            ]
        new MultiSumSq(Arguments<Symbol>(operatorArguments))
    new(numArrays : int,
        [<Optional>] ?data : Symbol seq) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "num_arrays", Parameter(Some(box numArrays))
            ]
        new MultiSumSq(Arguments<Symbol>(operatorArguments))
    new(numArrays : int,
        [<ParamArray>] data : Symbol[]) = 
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "num_arrays", Parameter(Some(box numArrays))
            ]
        new MultiSumSq(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetVarArg "data"
    member __.NumArrays : int = match operatorArguments.GetParameter "num_arrays" with Some(v) -> unbox v | None -> failwithf "Required parameter num_arrays is missing"

type ContribMultiBoxDetection private (operatorArguments) = 
    inherit SymbolOperator("_contrib_MultiBoxDetection", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribMultiBoxDetection(args)
    new([<Optional>] ?clsProb : Symbol,
        [<Optional>] ?locPred : Symbol,
        [<Optional>] ?anchor : Symbol,
        [<Optional>] ?clip : bool,
        [<Optional>] ?threshold : float,
        [<Optional>] ?backgroundId : int,
        [<Optional>] ?nmsThreshold : float,
        [<Optional>] ?forceSuppress : bool,
        [<Optional>] ?variances : double seq,
        [<Optional>] ?nmsTopk : int) = 
        let clsProb = defaultArg clsProb (new ImplicitVariable() :> Symbol)
        let locPred = defaultArg locPred (new ImplicitVariable() :> Symbol)
        let anchor = defaultArg anchor (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "cls_prob", Input clsProb
                "loc_pred", Input locPred
                "anchor", Input anchor
                "clip", clip |> Option.map box |> Parameter
                "threshold", threshold |> Option.map box |> Parameter
                "background_id", backgroundId |> Option.map box |> Parameter
                "nms_threshold", nmsThreshold |> Option.map box |> Parameter
                "force_suppress", forceSuppress |> Option.map box |> Parameter
                "variances", variances |> Option.map box |> Parameter
                "nms_topk", nmsTopk |> Option.map box |> Parameter
            ]
        new ContribMultiBoxDetection(Arguments<Symbol>(operatorArguments))
    static member ClipDefault : bool = true
    static member ThresholdDefault : double = 0.00999999978
    static member BackgroundIdDefault : int = 0
    static member NmsThresholdDefault : double = 0.5
    static member ForceSuppressDefault : bool = false
    static member VariancesDefault : double [] = [|0.1; 0.1; 0.2; 0.2|]
    static member NmsTopkDefault : int = -1
    member __.ClsProb = operatorArguments.GetInput "cls_prob"
    member __.LocPred = operatorArguments.GetInput "loc_pred"
    member __.Anchor = operatorArguments.GetInput "anchor"
    member __.Clip = operatorArguments.GetParameter("clip", ContribMultiBoxDetection.ClipDefault)
    member __.Threshold = operatorArguments.GetParameter("threshold", ContribMultiBoxDetection.ThresholdDefault)
    member __.BackgroundId = operatorArguments.GetParameter("background_id", ContribMultiBoxDetection.BackgroundIdDefault)
    member __.NmsThreshold = operatorArguments.GetParameter("nms_threshold", ContribMultiBoxDetection.NmsThresholdDefault)
    member __.ForceSuppress = operatorArguments.GetParameter("force_suppress", ContribMultiBoxDetection.ForceSuppressDefault)
    member __.Variances = operatorArguments.GetParameter("variances", ContribMultiBoxDetection.VariancesDefault)
    member __.NmsTopk = operatorArguments.GetParameter("nms_topk", ContribMultiBoxDetection.NmsTopkDefault)

type ContribMultiBoxPrior private (operatorArguments) = 
    inherit SymbolOperator("_contrib_MultiBoxPrior", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribMultiBoxPrior(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?sizes : double seq,
        [<Optional>] ?ratios : double seq,
        [<Optional>] ?clip : bool,
        [<Optional>] ?steps : double seq,
        [<Optional>] ?offsets : double seq) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "sizes", sizes |> Option.map box |> Parameter
                "ratios", ratios |> Option.map box |> Parameter
                "clip", clip |> Option.map box |> Parameter
                "steps", steps |> Option.map box |> Parameter
                "offsets", offsets |> Option.map box |> Parameter
            ]
        new ContribMultiBoxPrior(Arguments<Symbol>(operatorArguments))
    static member SizesDefault : double [] = [|1.0|]
    static member RatiosDefault : double [] = [|1.0|]
    static member ClipDefault : bool = false
    static member StepsDefault : double [] = [|-1.0; -1.0|]
    static member OffsetsDefault : double [] = [|0.5; 0.5|]
    member __.Data = operatorArguments.GetInput "data"
    member __.Sizes = operatorArguments.GetParameter("sizes", ContribMultiBoxPrior.SizesDefault)
    member __.Ratios = operatorArguments.GetParameter("ratios", ContribMultiBoxPrior.RatiosDefault)
    member __.Clip = operatorArguments.GetParameter("clip", ContribMultiBoxPrior.ClipDefault)
    member __.Steps = operatorArguments.GetParameter("steps", ContribMultiBoxPrior.StepsDefault)
    member __.Offsets = operatorArguments.GetParameter("offsets", ContribMultiBoxPrior.OffsetsDefault)

type ContribMultiBoxTarget private (operatorArguments) = 
    inherit SymbolOperator("_contrib_MultiBoxTarget", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribMultiBoxTarget(args)
    new([<Optional>] ?anchor : Symbol,
        [<Optional>] ?label : Symbol,
        [<Optional>] ?clsPred : Symbol,
        [<Optional>] ?overlapThreshold : float,
        [<Optional>] ?ignoreLabel : float,
        [<Optional>] ?negativeMiningRatio : float,
        [<Optional>] ?negativeMiningThresh : float,
        [<Optional>] ?minimumNegativeSamples : int,
        [<Optional>] ?variances : double seq) = 
        let anchor = defaultArg anchor (new ImplicitVariable() :> Symbol)
        let label = defaultArg label (new ImplicitVariable() :> Symbol)
        let clsPred = defaultArg clsPred (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "anchor", Input anchor
                "label", Input label
                "cls_pred", Input clsPred
                "overlap_threshold", overlapThreshold |> Option.map box |> Parameter
                "ignore_label", ignoreLabel |> Option.map box |> Parameter
                "negative_mining_ratio", negativeMiningRatio |> Option.map box |> Parameter
                "negative_mining_thresh", negativeMiningThresh |> Option.map box |> Parameter
                "minimum_negative_samples", minimumNegativeSamples |> Option.map box |> Parameter
                "variances", variances |> Option.map box |> Parameter
            ]
        new ContribMultiBoxTarget(Arguments<Symbol>(operatorArguments))
    static member OverlapThresholdDefault : double = 0.5
    static member IgnoreLabelDefault : double = -1.0
    static member NegativeMiningRatioDefault : double = -1.0
    static member NegativeMiningThreshDefault : double = 0.5
    static member MinimumNegativeSamplesDefault : int = 0
    static member VariancesDefault : double [] = [|0.1; 0.1; 0.2; 0.2|]
    member __.Anchor = operatorArguments.GetInput "anchor"
    member __.Label = operatorArguments.GetInput "label"
    member __.ClsPred = operatorArguments.GetInput "cls_pred"
    member __.OverlapThreshold = operatorArguments.GetParameter("overlap_threshold", ContribMultiBoxTarget.OverlapThresholdDefault)
    member __.IgnoreLabel = operatorArguments.GetParameter("ignore_label", ContribMultiBoxTarget.IgnoreLabelDefault)
    member __.NegativeMiningRatio = operatorArguments.GetParameter("negative_mining_ratio", ContribMultiBoxTarget.NegativeMiningRatioDefault)
    member __.NegativeMiningThresh = operatorArguments.GetParameter("negative_mining_thresh", ContribMultiBoxTarget.NegativeMiningThreshDefault)
    member __.MinimumNegativeSamples = operatorArguments.GetParameter("minimum_negative_samples", ContribMultiBoxTarget.MinimumNegativeSamplesDefault)
    member __.Variances = operatorArguments.GetParameter("variances", ContribMultiBoxTarget.VariancesDefault)

type ContribGetnnz private (operatorArguments) = 
    inherit SymbolOperator("_contrib_getnnz", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribGetnnz(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
            ]
        new ContribGetnnz(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", ContribGetnnz.AxisDefault)

type ContribGroupAdagradUpdate private (operatorArguments) = 
    inherit SymbolOperator("_contrib_group_adagrad_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribGroupAdagradUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        history : Symbol,
        lr : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?epsilon : float) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "history", Input history
                "lr", Parameter(Some(box lr))
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "epsilon", epsilon |> Option.map box |> Parameter
            ]
        new ContribGroupAdagradUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?history : Symbol,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?epsilon : float) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let history = defaultArg history (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "history", Input history
                "lr", Parameter(Some(box lr))
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "epsilon", epsilon |> Option.map box |> Parameter
            ]
        new ContribGroupAdagradUpdate(Arguments<Symbol>(operatorArguments))
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member EpsilonDefault : double = 0.00000999999975
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.History = operatorArguments.GetInput "history"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", ContribGroupAdagradUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", ContribGroupAdagradUpdate.ClipGradientDefault)
    member __.Epsilon = operatorArguments.GetParameter("epsilon", ContribGroupAdagradUpdate.EpsilonDefault)

type PreloadedMultiSgdUpdate private (operatorArguments) = 
    inherit SymbolOperator("preloaded_multi_sgd_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new PreloadedMultiSgdUpdate(args)
    new([<Optional>] ?data : Symbol seq,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?numWeights : int) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "num_weights", numWeights |> Option.map box |> Parameter
            ]
        new PreloadedMultiSgdUpdate(Arguments<Symbol>(operatorArguments))
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member NumWeightsDefault : int = 1
    member __.Data = operatorArguments.GetVarArg "data"
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", PreloadedMultiSgdUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", PreloadedMultiSgdUpdate.ClipGradientDefault)
    member __.NumWeights = operatorArguments.GetParameter("num_weights", PreloadedMultiSgdUpdate.NumWeightsDefault)

type PreloadedMultiSgdMomUpdate private (operatorArguments) = 
    inherit SymbolOperator("preloaded_multi_sgd_mom_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new PreloadedMultiSgdMomUpdate(args)
    new([<Optional>] ?data : Symbol seq,
        [<Optional>] ?momentum : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?numWeights : int) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "momentum", momentum |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "num_weights", numWeights |> Option.map box |> Parameter
            ]
        new PreloadedMultiSgdMomUpdate(Arguments<Symbol>(operatorArguments))
    static member MomentumDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member NumWeightsDefault : int = 1
    member __.Data = operatorArguments.GetVarArg "data"
    member __.Momentum = operatorArguments.GetParameter("momentum", PreloadedMultiSgdMomUpdate.MomentumDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", PreloadedMultiSgdMomUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", PreloadedMultiSgdMomUpdate.ClipGradientDefault)
    member __.NumWeights = operatorArguments.GetParameter("num_weights", PreloadedMultiSgdMomUpdate.NumWeightsDefault)

type PreloadedMultiMpSgdUpdate private (operatorArguments) = 
    inherit SymbolOperator("preloaded_multi_mp_sgd_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new PreloadedMultiMpSgdUpdate(args)
    new([<Optional>] ?data : Symbol seq,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?numWeights : int) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "num_weights", numWeights |> Option.map box |> Parameter
            ]
        new PreloadedMultiMpSgdUpdate(Arguments<Symbol>(operatorArguments))
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member NumWeightsDefault : int = 1
    member __.Data = operatorArguments.GetVarArg "data"
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", PreloadedMultiMpSgdUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", PreloadedMultiMpSgdUpdate.ClipGradientDefault)
    member __.NumWeights = operatorArguments.GetParameter("num_weights", PreloadedMultiMpSgdUpdate.NumWeightsDefault)

type PreloadedMultiMpSgdMomUpdate private (operatorArguments) = 
    inherit SymbolOperator("preloaded_multi_mp_sgd_mom_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new PreloadedMultiMpSgdMomUpdate(args)
    new([<Optional>] ?data : Symbol seq,
        [<Optional>] ?momentum : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?numWeights : int) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "momentum", momentum |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "num_weights", numWeights |> Option.map box |> Parameter
            ]
        new PreloadedMultiMpSgdMomUpdate(Arguments<Symbol>(operatorArguments))
    static member MomentumDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member NumWeightsDefault : int = 1
    member __.Data = operatorArguments.GetVarArg "data"
    member __.Momentum = operatorArguments.GetParameter("momentum", PreloadedMultiMpSgdMomUpdate.MomentumDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", PreloadedMultiMpSgdMomUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", PreloadedMultiMpSgdMomUpdate.ClipGradientDefault)
    member __.NumWeights = operatorArguments.GetParameter("num_weights", PreloadedMultiMpSgdMomUpdate.NumWeightsDefault)

type ContribQuadratic private (operatorArguments) = 
    inherit SymbolOperator("_contrib_quadratic", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribQuadratic(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?a : float,
        [<Optional>] ?b : float,
        [<Optional>] ?c : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "a", a |> Option.map box |> Parameter
                "b", b |> Option.map box |> Parameter
                "c", c |> Option.map box |> Parameter
            ]
        new ContribQuadratic(Arguments<Symbol>(operatorArguments))
    static member ADefault : double = 0.0
    static member BDefault : double = 0.0
    static member CDefault : double = 0.0
    member __.Data = operatorArguments.GetInput "data"
    member __.A = operatorArguments.GetParameter("a", ContribQuadratic.ADefault)
    member __.B = operatorArguments.GetParameter("b", ContribQuadratic.BDefault)
    member __.C = operatorArguments.GetParameter("c", ContribQuadratic.CDefault)

type ContribROIAlign private (operatorArguments) = 
    inherit SymbolOperator("_contrib_ROIAlign", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribROIAlign(args)
    new(data : Symbol,
        rois : Symbol,
        pooledSize : int seq,
        spatialScale : float,
        [<Optional>] ?sampleRatio : int,
        [<Optional>] ?positionSensitive : bool) = 
        let operatorArguments = 
            [
                "data", Input data
                "rois", Input rois
                "pooled_size", Parameter(Some(box pooledSize))
                "spatial_scale", Parameter(Some(box spatialScale))
                "sample_ratio", sampleRatio |> Option.map box |> Parameter
                "position_sensitive", positionSensitive |> Option.map box |> Parameter
            ]
        new ContribROIAlign(Arguments<Symbol>(operatorArguments))
    new(pooledSize : int seq,
        spatialScale : float,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?rois : Symbol,
        [<Optional>] ?sampleRatio : int,
        [<Optional>] ?positionSensitive : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let rois = defaultArg rois (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "rois", Input rois
                "pooled_size", Parameter(Some(box pooledSize))
                "spatial_scale", Parameter(Some(box spatialScale))
                "sample_ratio", sampleRatio |> Option.map box |> Parameter
                "position_sensitive", positionSensitive |> Option.map box |> Parameter
            ]
        new ContribROIAlign(Arguments<Symbol>(operatorArguments))
    static member SampleRatioDefault : int = -1
    static member PositionSensitiveDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Rois = operatorArguments.GetInput "rois"
    member __.PooledSize : int seq = match operatorArguments.GetParameter "pooled_size" with Some(v) -> unbox v | None -> failwithf "Required parameter pooled_size is missing"
    member __.SpatialScale : float = match operatorArguments.GetParameter "spatial_scale" with Some(v) -> unbox v | None -> failwithf "Required parameter spatial_scale is missing"
    member __.SampleRatio = operatorArguments.GetParameter("sample_ratio", ContribROIAlign.SampleRatioDefault)
    member __.PositionSensitive = operatorArguments.GetParameter("position_sensitive", ContribROIAlign.PositionSensitiveDefault)

type ContribRROIAlign private (operatorArguments) = 
    inherit SymbolOperator("_contrib_RROIAlign", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribRROIAlign(args)
    new(data : Symbol,
        rois : Symbol,
        pooledSize : int seq,
        spatialScale : float,
        [<Optional>] ?samplingRatio : int) = 
        let operatorArguments = 
            [
                "data", Input data
                "rois", Input rois
                "pooled_size", Parameter(Some(box pooledSize))
                "spatial_scale", Parameter(Some(box spatialScale))
                "sampling_ratio", samplingRatio |> Option.map box |> Parameter
            ]
        new ContribRROIAlign(Arguments<Symbol>(operatorArguments))
    new(pooledSize : int seq,
        spatialScale : float,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?rois : Symbol,
        [<Optional>] ?samplingRatio : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let rois = defaultArg rois (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "rois", Input rois
                "pooled_size", Parameter(Some(box pooledSize))
                "spatial_scale", Parameter(Some(box spatialScale))
                "sampling_ratio", samplingRatio |> Option.map box |> Parameter
            ]
        new ContribRROIAlign(Arguments<Symbol>(operatorArguments))
    static member SamplingRatioDefault : int = -1
    member __.Data = operatorArguments.GetInput "data"
    member __.Rois = operatorArguments.GetInput "rois"
    member __.PooledSize : int seq = match operatorArguments.GetParameter "pooled_size" with Some(v) -> unbox v | None -> failwithf "Required parameter pooled_size is missing"
    member __.SpatialScale : float = match operatorArguments.GetParameter "spatial_scale" with Some(v) -> unbox v | None -> failwithf "Required parameter spatial_scale is missing"
    member __.SamplingRatio = operatorArguments.GetParameter("sampling_ratio", ContribRROIAlign.SamplingRatioDefault)

type ContribSyncBatchNorm private (operatorArguments) = 
    inherit SymbolOperator("_contrib_SyncBatchNorm", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribSyncBatchNorm(args)
    new(data : Symbol,
        gamma : Symbol,
        beta : Symbol,
        movingMean : Symbol,
        movingVar : Symbol,
        key : string,
        [<Optional>] ?eps : float,
        [<Optional>] ?momentum : float,
        [<Optional>] ?fixGamma : bool,
        [<Optional>] ?useGlobalStats : bool,
        [<Optional>] ?outputMeanVar : bool,
        [<Optional>] ?ndev : int) = 
        let operatorArguments = 
            [
                "data", Input data
                "gamma", Input gamma
                "beta", Input beta
                "moving_mean", Input movingMean
                "moving_var", Input movingVar
                "key", Parameter(Some(box key))
                "eps", eps |> Option.map box |> Parameter
                "momentum", momentum |> Option.map box |> Parameter
                "fix_gamma", fixGamma |> Option.map box |> Parameter
                "use_global_stats", useGlobalStats |> Option.map box |> Parameter
                "output_mean_var", outputMeanVar |> Option.map box |> Parameter
                "ndev", ndev |> Option.map box |> Parameter
            ]
        new ContribSyncBatchNorm(Arguments<Symbol>(operatorArguments))
    new(key : string,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?gamma : Symbol,
        [<Optional>] ?beta : Symbol,
        [<Optional>] ?movingMean : Symbol,
        [<Optional>] ?movingVar : Symbol,
        [<Optional>] ?eps : float,
        [<Optional>] ?momentum : float,
        [<Optional>] ?fixGamma : bool,
        [<Optional>] ?useGlobalStats : bool,
        [<Optional>] ?outputMeanVar : bool,
        [<Optional>] ?ndev : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let gamma = defaultArg gamma (new ImplicitVariable() :> Symbol)
        let beta = defaultArg beta (new ImplicitVariable() :> Symbol)
        let movingMean = defaultArg movingMean (new ImplicitVariable() :> Symbol)
        let movingVar = defaultArg movingVar (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "gamma", Input gamma
                "beta", Input beta
                "moving_mean", Input movingMean
                "moving_var", Input movingVar
                "key", Parameter(Some(box key))
                "eps", eps |> Option.map box |> Parameter
                "momentum", momentum |> Option.map box |> Parameter
                "fix_gamma", fixGamma |> Option.map box |> Parameter
                "use_global_stats", useGlobalStats |> Option.map box |> Parameter
                "output_mean_var", outputMeanVar |> Option.map box |> Parameter
                "ndev", ndev |> Option.map box |> Parameter
            ]
        new ContribSyncBatchNorm(Arguments<Symbol>(operatorArguments))
    static member EpsDefault : double = 0.00100000005
    static member MomentumDefault : double = 0.899999976
    static member FixGammaDefault : bool = true
    static member UseGlobalStatsDefault : bool = false
    static member OutputMeanVarDefault : bool = false
    static member NdevDefault : int = 1
    member __.Data = operatorArguments.GetInput "data"
    member __.Gamma = operatorArguments.GetInput "gamma"
    member __.Beta = operatorArguments.GetInput "beta"
    member __.MovingMean = operatorArguments.GetInput "moving_mean"
    member __.MovingVar = operatorArguments.GetInput "moving_var"
    member __.Key : string = match operatorArguments.GetParameter "key" with Some(v) -> unbox v | None -> failwithf "Required parameter key is missing"
    member __.Eps = operatorArguments.GetParameter("eps", ContribSyncBatchNorm.EpsDefault)
    member __.Momentum = operatorArguments.GetParameter("momentum", ContribSyncBatchNorm.MomentumDefault)
    member __.FixGamma = operatorArguments.GetParameter("fix_gamma", ContribSyncBatchNorm.FixGammaDefault)
    member __.UseGlobalStats = operatorArguments.GetParameter("use_global_stats", ContribSyncBatchNorm.UseGlobalStatsDefault)
    member __.OutputMeanVar = operatorArguments.GetParameter("output_mean_var", ContribSyncBatchNorm.OutputMeanVarDefault)
    member __.Ndev = operatorArguments.GetParameter("ndev", ContribSyncBatchNorm.NdevDefault)

type ContribDivSqrtDim private (operatorArguments) = 
    inherit SymbolOperator("_contrib_div_sqrt_dim", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribDivSqrtDim(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new ContribDivSqrtDim(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Foreach private (operatorArguments) = 
    inherit SymbolOperator("_foreach", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Foreach(args)
    new(fn : Symbol,
        data : Symbol seq,
        numOutputs : int,
        numOutData : int,
        inStateLocs : int64 seq,
        inDataLocs : int64 seq,
        remainLocs : int64 seq) = 
        let operatorArguments = 
            [
                "fn", Input fn
                "data", VarArg("num_args", data |> Seq.toArray)
                "num_outputs", Parameter(Some(box numOutputs))
                "num_out_data", Parameter(Some(box numOutData))
                "in_state_locs", Parameter(Some(box inStateLocs))
                "in_data_locs", Parameter(Some(box inDataLocs))
                "remain_locs", Parameter(Some(box remainLocs))
            ]
        new Foreach(Arguments<Symbol>(operatorArguments))
    new(numOutputs : int,
        numOutData : int,
        inStateLocs : int64 seq,
        inDataLocs : int64 seq,
        remainLocs : int64 seq,
        [<Optional>] ?fn : Symbol,
        [<Optional>] ?data : Symbol seq) = 
        let fn = defaultArg fn (new ImplicitVariable() :> Symbol)
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "fn", Input fn
                "data", VarArg("num_args", data)
                "num_outputs", Parameter(Some(box numOutputs))
                "num_out_data", Parameter(Some(box numOutData))
                "in_state_locs", Parameter(Some(box inStateLocs))
                "in_data_locs", Parameter(Some(box inDataLocs))
                "remain_locs", Parameter(Some(box remainLocs))
            ]
        new Foreach(Arguments<Symbol>(operatorArguments))
    member __.Fn = operatorArguments.GetInput "fn"
    member __.Data = operatorArguments.GetVarArg "data"
    member __.NumOutputs : int = match operatorArguments.GetParameter "num_outputs" with Some(v) -> unbox v | None -> failwithf "Required parameter num_outputs is missing"
    member __.NumOutData : int = match operatorArguments.GetParameter "num_out_data" with Some(v) -> unbox v | None -> failwithf "Required parameter num_out_data is missing"
    member __.InStateLocs : int64 seq = match operatorArguments.GetParameter "in_state_locs" with Some(v) -> unbox v | None -> failwithf "Required parameter in_state_locs is missing"
    member __.InDataLocs : int64 seq = match operatorArguments.GetParameter "in_data_locs" with Some(v) -> unbox v | None -> failwithf "Required parameter in_data_locs is missing"
    member __.RemainLocs : int64 seq = match operatorArguments.GetParameter "remain_locs" with Some(v) -> unbox v | None -> failwithf "Required parameter remain_locs is missing"

type WhileLoop private (operatorArguments) = 
    inherit SymbolOperator("_while_loop", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new WhileLoop(args)
    new(cond : Symbol,
        func : Symbol,
        data : Symbol seq,
        numOutputs : int,
        numOutData : int,
        maxIterations : int,
        condInputLocs : int64 seq,
        funcInputLocs : int64 seq,
        funcVarLocs : int64 seq) = 
        let operatorArguments = 
            [
                "cond", Input cond
                "func", Input func
                "data", VarArg("num_args", data |> Seq.toArray)
                "num_outputs", Parameter(Some(box numOutputs))
                "num_out_data", Parameter(Some(box numOutData))
                "max_iterations", Parameter(Some(box maxIterations))
                "cond_input_locs", Parameter(Some(box condInputLocs))
                "func_input_locs", Parameter(Some(box funcInputLocs))
                "func_var_locs", Parameter(Some(box funcVarLocs))
            ]
        new WhileLoop(Arguments<Symbol>(operatorArguments))
    new(numOutputs : int,
        numOutData : int,
        maxIterations : int,
        condInputLocs : int64 seq,
        funcInputLocs : int64 seq,
        funcVarLocs : int64 seq,
        [<Optional>] ?cond : Symbol,
        [<Optional>] ?func : Symbol,
        [<Optional>] ?data : Symbol seq) = 
        let cond = defaultArg cond (new ImplicitVariable() :> Symbol)
        let func = defaultArg func (new ImplicitVariable() :> Symbol)
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "cond", Input cond
                "func", Input func
                "data", VarArg("num_args", data)
                "num_outputs", Parameter(Some(box numOutputs))
                "num_out_data", Parameter(Some(box numOutData))
                "max_iterations", Parameter(Some(box maxIterations))
                "cond_input_locs", Parameter(Some(box condInputLocs))
                "func_input_locs", Parameter(Some(box funcInputLocs))
                "func_var_locs", Parameter(Some(box funcVarLocs))
            ]
        new WhileLoop(Arguments<Symbol>(operatorArguments))
    member __.Cond = operatorArguments.GetInput "cond"
    member __.Func = operatorArguments.GetInput "func"
    member __.Data = operatorArguments.GetVarArg "data"
    member __.NumOutputs : int = match operatorArguments.GetParameter "num_outputs" with Some(v) -> unbox v | None -> failwithf "Required parameter num_outputs is missing"
    member __.NumOutData : int = match operatorArguments.GetParameter "num_out_data" with Some(v) -> unbox v | None -> failwithf "Required parameter num_out_data is missing"
    member __.MaxIterations : int = match operatorArguments.GetParameter "max_iterations" with Some(v) -> unbox v | None -> failwithf "Required parameter max_iterations is missing"
    member __.CondInputLocs : int64 seq = match operatorArguments.GetParameter "cond_input_locs" with Some(v) -> unbox v | None -> failwithf "Required parameter cond_input_locs is missing"
    member __.FuncInputLocs : int64 seq = match operatorArguments.GetParameter "func_input_locs" with Some(v) -> unbox v | None -> failwithf "Required parameter func_input_locs is missing"
    member __.FuncVarLocs : int64 seq = match operatorArguments.GetParameter "func_var_locs" with Some(v) -> unbox v | None -> failwithf "Required parameter func_var_locs is missing"

type Cond private (operatorArguments) = 
    inherit SymbolOperator("_cond", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Cond(args)
    new(cond : Symbol,
        thenBranch : Symbol,
        elseBranch : Symbol,
        data : Symbol seq,
        numOutputs : int,
        condInputLocs : int64 seq,
        thenInputLocs : int64 seq,
        elseInputLocs : int64 seq) = 
        let operatorArguments = 
            [
                "cond", Input cond
                "then_branch", Input thenBranch
                "else_branch", Input elseBranch
                "data", VarArg("num_args", data |> Seq.toArray)
                "num_outputs", Parameter(Some(box numOutputs))
                "cond_input_locs", Parameter(Some(box condInputLocs))
                "then_input_locs", Parameter(Some(box thenInputLocs))
                "else_input_locs", Parameter(Some(box elseInputLocs))
            ]
        new Cond(Arguments<Symbol>(operatorArguments))
    new(numOutputs : int,
        condInputLocs : int64 seq,
        thenInputLocs : int64 seq,
        elseInputLocs : int64 seq,
        [<Optional>] ?cond : Symbol,
        [<Optional>] ?thenBranch : Symbol,
        [<Optional>] ?elseBranch : Symbol,
        [<Optional>] ?data : Symbol seq) = 
        let cond = defaultArg cond (new ImplicitVariable() :> Symbol)
        let thenBranch = defaultArg thenBranch (new ImplicitVariable() :> Symbol)
        let elseBranch = defaultArg elseBranch (new ImplicitVariable() :> Symbol)
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "cond", Input cond
                "then_branch", Input thenBranch
                "else_branch", Input elseBranch
                "data", VarArg("num_args", data)
                "num_outputs", Parameter(Some(box numOutputs))
                "cond_input_locs", Parameter(Some(box condInputLocs))
                "then_input_locs", Parameter(Some(box thenInputLocs))
                "else_input_locs", Parameter(Some(box elseInputLocs))
            ]
        new Cond(Arguments<Symbol>(operatorArguments))
    member __.Cond = operatorArguments.GetInput "cond"
    member __.ThenBranch = operatorArguments.GetInput "then_branch"
    member __.ElseBranch = operatorArguments.GetInput "else_branch"
    member __.Data = operatorArguments.GetVarArg "data"
    member __.NumOutputs : int = match operatorArguments.GetParameter "num_outputs" with Some(v) -> unbox v | None -> failwithf "Required parameter num_outputs is missing"
    member __.CondInputLocs : int64 seq = match operatorArguments.GetParameter "cond_input_locs" with Some(v) -> unbox v | None -> failwithf "Required parameter cond_input_locs is missing"
    member __.ThenInputLocs : int64 seq = match operatorArguments.GetParameter "then_input_locs" with Some(v) -> unbox v | None -> failwithf "Required parameter then_input_locs is missing"
    member __.ElseInputLocs : int64 seq = match operatorArguments.GetParameter "else_input_locs" with Some(v) -> unbox v | None -> failwithf "Required parameter else_input_locs is missing"

type Custom private (operatorArguments) = 
    inherit SymbolOperator("Custom", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Custom(args)
    new(data : Symbol seq,
        opType : string) = 
        let operatorArguments = 
            [
                "data", VarArg("", data |> Seq.toArray)
                "op_type", Parameter(Some(box opType))
            ]
        new Custom(Arguments<Symbol>(operatorArguments))
    new(opType : string,
        [<Optional>] ?data : Symbol seq) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "op_type", Parameter(Some(box opType))
            ]
        new Custom(Arguments<Symbol>(operatorArguments))
    new(opType : string,
        [<ParamArray>] data : Symbol[]) = 
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "op_type", Parameter(Some(box opType))
            ]
        new Custom(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetVarArg "data"
    member __.OpType : string = match operatorArguments.GetParameter "op_type" with Some(v) -> unbox v | None -> failwithf "Required parameter op_type is missing"

type IdentityAttachKLSparseReg private (operatorArguments) = 
    inherit SymbolOperator("IdentityAttachKLSparseReg", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new IdentityAttachKLSparseReg(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?sparsenessTarget : float,
        [<Optional>] ?penalty : float,
        [<Optional>] ?momentum : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "sparseness_target", sparsenessTarget |> Option.map box |> Parameter
                "penalty", penalty |> Option.map box |> Parameter
                "momentum", momentum |> Option.map box |> Parameter
            ]
        new IdentityAttachKLSparseReg(Arguments<Symbol>(operatorArguments))
    static member SparsenessTargetDefault : double = 0.100000001
    static member PenaltyDefault : double = 0.00100000005
    static member MomentumDefault : double = 0.899999976
    member __.Data = operatorArguments.GetInput "data"
    member __.SparsenessTarget = operatorArguments.GetParameter("sparseness_target", IdentityAttachKLSparseReg.SparsenessTargetDefault)
    member __.Penalty = operatorArguments.GetParameter("penalty", IdentityAttachKLSparseReg.PenaltyDefault)
    member __.Momentum = operatorArguments.GetParameter("momentum", IdentityAttachKLSparseReg.MomentumDefault)

type ImageCrop private (operatorArguments) = 
    inherit SymbolOperator("_image_crop", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ImageCrop(args)
    new(data : Symbol,
        x : int,
        y : int,
        width : int,
        height : int) = 
        let operatorArguments = 
            [
                "data", Input data
                "x", Parameter(Some(box x))
                "y", Parameter(Some(box y))
                "width", Parameter(Some(box width))
                "height", Parameter(Some(box height))
            ]
        new ImageCrop(Arguments<Symbol>(operatorArguments))
    new(x : int,
        y : int,
        width : int,
        height : int,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "x", Parameter(Some(box x))
                "y", Parameter(Some(box y))
                "width", Parameter(Some(box width))
                "height", Parameter(Some(box height))
            ]
        new ImageCrop(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.X : int = match operatorArguments.GetParameter "x" with Some(v) -> unbox v | None -> failwithf "Required parameter x is missing"
    member __.Y : int = match operatorArguments.GetParameter "y" with Some(v) -> unbox v | None -> failwithf "Required parameter y is missing"
    member __.Width : int = match operatorArguments.GetParameter "width" with Some(v) -> unbox v | None -> failwithf "Required parameter width is missing"
    member __.Height : int = match operatorArguments.GetParameter "height" with Some(v) -> unbox v | None -> failwithf "Required parameter height is missing"

type ImageToTensor private (operatorArguments) = 
    inherit SymbolOperator("_image_to_tensor", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ImageToTensor(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new ImageToTensor(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type ImageNormalize private (operatorArguments) = 
    inherit SymbolOperator("_image_normalize", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ImageNormalize(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?mean : double seq,
        [<Optional>] ?std : double seq) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "mean", mean |> Option.map box |> Parameter
                "std", std |> Option.map box |> Parameter
            ]
        new ImageNormalize(Arguments<Symbol>(operatorArguments))
    static member MeanDefault : double [] = [|0.0; 0.0; 0.0; 0.0|]
    static member StdDefault : double [] = [|1.0; 1.0; 1.0; 1.0|]
    member __.Data = operatorArguments.GetInput "data"
    member __.Mean = operatorArguments.GetParameter("mean", ImageNormalize.MeanDefault)
    member __.Std = operatorArguments.GetParameter("std", ImageNormalize.StdDefault)

type ImageFlipLeftRight private (operatorArguments) = 
    inherit SymbolOperator("_image_flip_left_right", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ImageFlipLeftRight(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new ImageFlipLeftRight(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type ImageRandomFlipLeftRight private (operatorArguments) = 
    inherit SymbolOperator("_image_random_flip_left_right", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ImageRandomFlipLeftRight(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new ImageRandomFlipLeftRight(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type ImageFlipTopBottom private (operatorArguments) = 
    inherit SymbolOperator("_image_flip_top_bottom", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ImageFlipTopBottom(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new ImageFlipTopBottom(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type ImageRandomFlipTopBottom private (operatorArguments) = 
    inherit SymbolOperator("_image_random_flip_top_bottom", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ImageRandomFlipTopBottom(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new ImageRandomFlipTopBottom(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type ImageRandomBrightness private (operatorArguments) = 
    inherit SymbolOperator("_image_random_brightness", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ImageRandomBrightness(args)
    new(data : Symbol,
        minFactor : float,
        maxFactor : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "min_factor", Parameter(Some(box minFactor))
                "max_factor", Parameter(Some(box maxFactor))
            ]
        new ImageRandomBrightness(Arguments<Symbol>(operatorArguments))
    new(minFactor : float,
        maxFactor : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "min_factor", Parameter(Some(box minFactor))
                "max_factor", Parameter(Some(box maxFactor))
            ]
        new ImageRandomBrightness(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.MinFactor : float = match operatorArguments.GetParameter "min_factor" with Some(v) -> unbox v | None -> failwithf "Required parameter min_factor is missing"
    member __.MaxFactor : float = match operatorArguments.GetParameter "max_factor" with Some(v) -> unbox v | None -> failwithf "Required parameter max_factor is missing"

type ImageRandomContrast private (operatorArguments) = 
    inherit SymbolOperator("_image_random_contrast", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ImageRandomContrast(args)
    new(data : Symbol,
        minFactor : float,
        maxFactor : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "min_factor", Parameter(Some(box minFactor))
                "max_factor", Parameter(Some(box maxFactor))
            ]
        new ImageRandomContrast(Arguments<Symbol>(operatorArguments))
    new(minFactor : float,
        maxFactor : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "min_factor", Parameter(Some(box minFactor))
                "max_factor", Parameter(Some(box maxFactor))
            ]
        new ImageRandomContrast(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.MinFactor : float = match operatorArguments.GetParameter "min_factor" with Some(v) -> unbox v | None -> failwithf "Required parameter min_factor is missing"
    member __.MaxFactor : float = match operatorArguments.GetParameter "max_factor" with Some(v) -> unbox v | None -> failwithf "Required parameter max_factor is missing"

type ImageRandomSaturation private (operatorArguments) = 
    inherit SymbolOperator("_image_random_saturation", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ImageRandomSaturation(args)
    new(data : Symbol,
        minFactor : float,
        maxFactor : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "min_factor", Parameter(Some(box minFactor))
                "max_factor", Parameter(Some(box maxFactor))
            ]
        new ImageRandomSaturation(Arguments<Symbol>(operatorArguments))
    new(minFactor : float,
        maxFactor : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "min_factor", Parameter(Some(box minFactor))
                "max_factor", Parameter(Some(box maxFactor))
            ]
        new ImageRandomSaturation(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.MinFactor : float = match operatorArguments.GetParameter "min_factor" with Some(v) -> unbox v | None -> failwithf "Required parameter min_factor is missing"
    member __.MaxFactor : float = match operatorArguments.GetParameter "max_factor" with Some(v) -> unbox v | None -> failwithf "Required parameter max_factor is missing"

type ImageRandomHue private (operatorArguments) = 
    inherit SymbolOperator("_image_random_hue", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ImageRandomHue(args)
    new(data : Symbol,
        minFactor : float,
        maxFactor : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "min_factor", Parameter(Some(box minFactor))
                "max_factor", Parameter(Some(box maxFactor))
            ]
        new ImageRandomHue(Arguments<Symbol>(operatorArguments))
    new(minFactor : float,
        maxFactor : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "min_factor", Parameter(Some(box minFactor))
                "max_factor", Parameter(Some(box maxFactor))
            ]
        new ImageRandomHue(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.MinFactor : float = match operatorArguments.GetParameter "min_factor" with Some(v) -> unbox v | None -> failwithf "Required parameter min_factor is missing"
    member __.MaxFactor : float = match operatorArguments.GetParameter "max_factor" with Some(v) -> unbox v | None -> failwithf "Required parameter max_factor is missing"

type ImageRandomColorJitter private (operatorArguments) = 
    inherit SymbolOperator("_image_random_color_jitter", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ImageRandomColorJitter(args)
    new(data : Symbol,
        brightness : float,
        contrast : float,
        saturation : float,
        hue : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "brightness", Parameter(Some(box brightness))
                "contrast", Parameter(Some(box contrast))
                "saturation", Parameter(Some(box saturation))
                "hue", Parameter(Some(box hue))
            ]
        new ImageRandomColorJitter(Arguments<Symbol>(operatorArguments))
    new(brightness : float,
        contrast : float,
        saturation : float,
        hue : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "brightness", Parameter(Some(box brightness))
                "contrast", Parameter(Some(box contrast))
                "saturation", Parameter(Some(box saturation))
                "hue", Parameter(Some(box hue))
            ]
        new ImageRandomColorJitter(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Brightness : float = match operatorArguments.GetParameter "brightness" with Some(v) -> unbox v | None -> failwithf "Required parameter brightness is missing"
    member __.Contrast : float = match operatorArguments.GetParameter "contrast" with Some(v) -> unbox v | None -> failwithf "Required parameter contrast is missing"
    member __.Saturation : float = match operatorArguments.GetParameter "saturation" with Some(v) -> unbox v | None -> failwithf "Required parameter saturation is missing"
    member __.Hue : float = match operatorArguments.GetParameter "hue" with Some(v) -> unbox v | None -> failwithf "Required parameter hue is missing"

type ImageAdjustLighting private (operatorArguments) = 
    inherit SymbolOperator("_image_adjust_lighting", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ImageAdjustLighting(args)
    new(data : Symbol,
        alpha : double seq) = 
        let operatorArguments = 
            [
                "data", Input data
                "alpha", Parameter(Some(box alpha))
            ]
        new ImageAdjustLighting(Arguments<Symbol>(operatorArguments))
    new(alpha : double seq,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "alpha", Parameter(Some(box alpha))
            ]
        new ImageAdjustLighting(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Alpha : double seq = match operatorArguments.GetParameter "alpha" with Some(v) -> unbox v | None -> failwithf "Required parameter alpha is missing"

type ImageRandomLighting private (operatorArguments) = 
    inherit SymbolOperator("_image_random_lighting", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ImageRandomLighting(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?alphaStd : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "alpha_std", alphaStd |> Option.map box |> Parameter
            ]
        new ImageRandomLighting(Arguments<Symbol>(operatorArguments))
    static member AlphaStdDefault : double = 0.0500000007
    member __.Data = operatorArguments.GetInput "data"
    member __.AlphaStd = operatorArguments.GetParameter("alpha_std", ImageRandomLighting.AlphaStdDefault)

type ImageResize private (operatorArguments) = 
    inherit SymbolOperator("_image_resize", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ImageResize(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?size : int,
        [<Optional>] ?keepRatio : bool,
        [<Optional>] ?interp : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "size", size |> Option.map box |> Parameter
                "keep_ratio", keepRatio |> Option.map box |> Parameter
                "interp", interp |> Option.map box |> Parameter
            ]
        new ImageResize(Arguments<Symbol>(operatorArguments))
    static member SizeDefault : int [] = [||]
    static member KeepRatioDefault : bool = false
    static member InterpDefault : int = 1
    member __.Data = operatorArguments.GetInput "data"
    member __.Size = operatorArguments.GetParameter("size", ImageResize.SizeDefault)
    member __.KeepRatio = operatorArguments.GetParameter("keep_ratio", ImageResize.KeepRatioDefault)
    member __.Interp = operatorArguments.GetParameter("interp", ImageResize.InterpDefault)

type LeakyReLU private (operatorArguments) = 
    inherit SymbolOperator("LeakyReLU", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LeakyReLU(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?gamma : Symbol,
        [<Optional>] ?actType : LeakyReLUType,
        [<Optional>] ?slope : float,
        [<Optional>] ?lowerBound : float,
        [<Optional>] ?upperBound : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let gamma = defaultArg gamma (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "gamma", Input gamma
                "act_type", actType |> Option.map box |> Parameter
                "slope", slope |> Option.map box |> Parameter
                "lower_bound", lowerBound |> Option.map box |> Parameter
                "upper_bound", upperBound |> Option.map box |> Parameter
            ]
        new LeakyReLU(Arguments<Symbol>(operatorArguments))
    static member ActTypeDefault : LeakyReLUType = LeakyReLUType.Leaky
    static member SlopeDefault : double = 0.25
    static member LowerBoundDefault : double = 0.125
    static member UpperBoundDefault : double = 0.333999991
    member __.Data = operatorArguments.GetInput "data"
    member __.Gamma = operatorArguments.GetInput "gamma"
    member __.ActType = operatorArguments.GetParameter("act_type", LeakyReLU.ActTypeDefault)
    member __.Slope = operatorArguments.GetParameter("slope", LeakyReLU.SlopeDefault)
    member __.LowerBound = operatorArguments.GetParameter("lower_bound", LeakyReLU.LowerBoundDefault)
    member __.UpperBound = operatorArguments.GetParameter("upper_bound", LeakyReLU.UpperBoundDefault)

type SoftmaxCrossEntropy private (operatorArguments) = 
    inherit SymbolOperator("softmax_cross_entropy", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SoftmaxCrossEntropy(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?label : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let label = defaultArg label (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "label", Input label
            ]
        new SoftmaxCrossEntropy(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Label = operatorArguments.GetInput "label"

type Activation private (operatorArguments) = 
    inherit SymbolOperator("Activation", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Activation(args)
    new(data : Symbol,
        actType : ActType) = 
        let operatorArguments = 
            [
                "data", Input data
                "act_type", Parameter(Some(box actType))
            ]
        new Activation(Arguments<Symbol>(operatorArguments))
    new(actType : ActType,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "act_type", Parameter(Some(box actType))
            ]
        new Activation(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.ActType : ActType = match operatorArguments.GetParameter "act_type" with Some(v) -> unbox v | None -> failwithf "Required parameter act_type is missing"

type BatchNorm private (operatorArguments) = 
    inherit SymbolOperator("BatchNorm", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BatchNorm(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?gamma : Symbol,
        [<Optional>] ?beta : Symbol,
        [<Optional>] ?movingMean : Symbol,
        [<Optional>] ?movingVar : Symbol,
        [<Optional>] ?eps : double,
        [<Optional>] ?momentum : float,
        [<Optional>] ?fixGamma : bool,
        [<Optional>] ?useGlobalStats : bool,
        [<Optional>] ?outputMeanVar : bool,
        [<Optional>] ?axis : int,
        [<Optional>] ?cudnnOff : bool,
        [<Optional>] ?minCalibRange : float,
        [<Optional>] ?maxCalibRange : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let gamma = defaultArg gamma (new ImplicitVariable() :> Symbol)
        let beta = defaultArg beta (new ImplicitVariable() :> Symbol)
        let movingMean = defaultArg movingMean (new ImplicitVariable() :> Symbol)
        let movingVar = defaultArg movingVar (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "gamma", Input gamma
                "beta", Input beta
                "moving_mean", Input movingMean
                "moving_var", Input movingVar
                "eps", eps |> Option.map box |> Parameter
                "momentum", momentum |> Option.map box |> Parameter
                "fix_gamma", fixGamma |> Option.map box |> Parameter
                "use_global_stats", useGlobalStats |> Option.map box |> Parameter
                "output_mean_var", outputMeanVar |> Option.map box |> Parameter
                "axis", axis |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
                "min_calib_range", minCalibRange |> Option.map box |> Parameter
                "max_calib_range", maxCalibRange |> Option.map box |> Parameter
            ]
        new BatchNorm(Arguments<Symbol>(operatorArguments))
    static member EpsDefault : double = 0.0010000000474975
    static member MomentumDefault : double = 0.899999976
    static member FixGammaDefault : bool = true
    static member UseGlobalStatsDefault : bool = false
    static member OutputMeanVarDefault : bool = false
    static member AxisDefault : int = 1
    static member CudnnOffDefault : bool = false
    static member MinCalibRangeDefault : double option = None
    static member MaxCalibRangeDefault : double option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Gamma = operatorArguments.GetInput "gamma"
    member __.Beta = operatorArguments.GetInput "beta"
    member __.MovingMean = operatorArguments.GetInput "moving_mean"
    member __.MovingVar = operatorArguments.GetInput "moving_var"
    member __.Eps = operatorArguments.GetParameter("eps", BatchNorm.EpsDefault)
    member __.Momentum = operatorArguments.GetParameter("momentum", BatchNorm.MomentumDefault)
    member __.FixGamma = operatorArguments.GetParameter("fix_gamma", BatchNorm.FixGammaDefault)
    member __.UseGlobalStats = operatorArguments.GetParameter("use_global_stats", BatchNorm.UseGlobalStatsDefault)
    member __.OutputMeanVar = operatorArguments.GetParameter("output_mean_var", BatchNorm.OutputMeanVarDefault)
    member __.Axis = operatorArguments.GetParameter("axis", BatchNorm.AxisDefault)
    member __.CudnnOff = operatorArguments.GetParameter("cudnn_off", BatchNorm.CudnnOffDefault)
    member __.MinCalibRange = operatorArguments.GetParameter("min_calib_range", BatchNorm.MinCalibRangeDefault)
    member __.MaxCalibRange = operatorArguments.GetParameter("max_calib_range", BatchNorm.MaxCalibRangeDefault)

type Concat private (operatorArguments) = 
    inherit SymbolOperator("Concat", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Concat(args)
    new([<Optional>] ?data : Symbol seq,
        [<Optional>] ?dim : int) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("num_args", data)
                "dim", dim |> Option.map box |> Parameter
            ]
        new Concat(Arguments<Symbol>(operatorArguments))
    static member DimDefault : int = 1
    member __.Data = operatorArguments.GetVarArg "data"
    member __.Dim = operatorArguments.GetParameter("dim", Concat.DimDefault)

type RnnParamConcat private (operatorArguments) = 
    inherit SymbolOperator("_rnn_param_concat", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RnnParamConcat(args)
    new([<Optional>] ?data : Symbol seq,
        [<Optional>] ?dim : int) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("num_args", data)
                "dim", dim |> Option.map box |> Parameter
            ]
        new RnnParamConcat(Arguments<Symbol>(operatorArguments))
    static member DimDefault : int = 1
    member __.Data = operatorArguments.GetVarArg "data"
    member __.Dim = operatorArguments.GetParameter("dim", RnnParamConcat.DimDefault)

type Convolution private (operatorArguments) = 
    inherit SymbolOperator("Convolution", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Convolution(args)
    new(data : Symbol,
        weight : Symbol,
        bias : Symbol,
        kernel : int seq,
        numFilter : int,
        [<Optional>] ?stride : int seq,
        [<Optional>] ?dilate : int seq,
        [<Optional>] ?pad : int seq,
        [<Optional>] ?numGroup : int,
        [<Optional>] ?workspace : int64,
        [<Optional>] ?noBias : bool,
        [<Optional>] ?cudnnTune : CudnnTune,
        [<Optional>] ?cudnnOff : bool,
        [<Optional>] ?layout : ConvolutionLayout) = 
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "bias", Input bias
                "kernel", Parameter(Some(box kernel))
                "num_filter", Parameter(Some(box numFilter))
                "stride", stride |> Option.map box |> Parameter
                "dilate", dilate |> Option.map box |> Parameter
                "pad", pad |> Option.map box |> Parameter
                "num_group", numGroup |> Option.map box |> Parameter
                "workspace", workspace |> Option.map box |> Parameter
                "no_bias", noBias |> Option.map box |> Parameter
                "cudnn_tune", cudnnTune |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
                "layout", layout |> Option.map box |> Parameter
            ]
        new Convolution(Arguments<Symbol>(operatorArguments))
    new(kernel : int seq,
        numFilter : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?bias : Symbol,
        [<Optional>] ?stride : int seq,
        [<Optional>] ?dilate : int seq,
        [<Optional>] ?pad : int seq,
        [<Optional>] ?numGroup : int,
        [<Optional>] ?workspace : int64,
        [<Optional>] ?noBias : bool,
        [<Optional>] ?cudnnTune : CudnnTune,
        [<Optional>] ?cudnnOff : bool,
        [<Optional>] ?layout : ConvolutionLayout) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let bias = defaultArg bias (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "bias", Input bias
                "kernel", Parameter(Some(box kernel))
                "num_filter", Parameter(Some(box numFilter))
                "stride", stride |> Option.map box |> Parameter
                "dilate", dilate |> Option.map box |> Parameter
                "pad", pad |> Option.map box |> Parameter
                "num_group", numGroup |> Option.map box |> Parameter
                "workspace", workspace |> Option.map box |> Parameter
                "no_bias", noBias |> Option.map box |> Parameter
                "cudnn_tune", cudnnTune |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
                "layout", layout |> Option.map box |> Parameter
            ]
        new Convolution(Arguments<Symbol>(operatorArguments))
    static member StrideDefault : int [] = [||]
    static member DilateDefault : int [] = [||]
    static member PadDefault : int [] = [||]
    static member NumGroupDefault : int = 1
    static member WorkspaceDefault : int64 = 1024L
    static member NoBiasDefault : bool = false
    static member CudnnTuneDefault : CudnnTune option = None
    static member CudnnOffDefault : bool = false
    static member LayoutDefault : ConvolutionLayout option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Bias = operatorArguments.GetInput "bias"
    member __.Kernel : int seq = match operatorArguments.GetParameter "kernel" with Some(v) -> unbox v | None -> failwithf "Required parameter kernel is missing"
    member __.NumFilter : int = match operatorArguments.GetParameter "num_filter" with Some(v) -> unbox v | None -> failwithf "Required parameter num_filter is missing"
    member __.Stride = operatorArguments.GetParameter("stride", Convolution.StrideDefault)
    member __.Dilate = operatorArguments.GetParameter("dilate", Convolution.DilateDefault)
    member __.Pad = operatorArguments.GetParameter("pad", Convolution.PadDefault)
    member __.NumGroup = operatorArguments.GetParameter("num_group", Convolution.NumGroupDefault)
    member __.Workspace = operatorArguments.GetParameter("workspace", Convolution.WorkspaceDefault)
    member __.NoBias = operatorArguments.GetParameter("no_bias", Convolution.NoBiasDefault)
    member __.CudnnTune = operatorArguments.GetParameter("cudnn_tune", Convolution.CudnnTuneDefault)
    member __.CudnnOff = operatorArguments.GetParameter("cudnn_off", Convolution.CudnnOffDefault)
    member __.Layout = operatorArguments.GetParameter("layout", Convolution.LayoutDefault)

type CTCLoss private (operatorArguments) = 
    inherit SymbolOperator("CTCLoss", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new CTCLoss(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?label : Symbol,
        [<Optional>] ?dataLengths : Symbol,
        [<Optional>] ?labelLengths : Symbol,
        [<Optional>] ?useDataLengths : bool,
        [<Optional>] ?useLabelLengths : bool,
        [<Optional>] ?blankLabel : BlankLabel) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let label = defaultArg label (new ImplicitVariable() :> Symbol)
        let dataLengths = defaultArg dataLengths (new ImplicitVariable() :> Symbol)
        let labelLengths = defaultArg labelLengths (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "label", Input label
                "data_lengths", Input dataLengths
                "label_lengths", Input labelLengths
                "use_data_lengths", useDataLengths |> Option.map box |> Parameter
                "use_label_lengths", useLabelLengths |> Option.map box |> Parameter
                "blank_label", blankLabel |> Option.map box |> Parameter
            ]
        new CTCLoss(Arguments<Symbol>(operatorArguments))
    static member UseDataLengthsDefault : bool = false
    static member UseLabelLengthsDefault : bool = false
    static member BlankLabelDefault : BlankLabel = BlankLabel.First
    member __.Data = operatorArguments.GetInput "data"
    member __.Label = operatorArguments.GetInput "label"
    member __.DataLengths = operatorArguments.GetInput "data_lengths"
    member __.LabelLengths = operatorArguments.GetInput "label_lengths"
    member __.UseDataLengths = operatorArguments.GetParameter("use_data_lengths", CTCLoss.UseDataLengthsDefault)
    member __.UseLabelLengths = operatorArguments.GetParameter("use_label_lengths", CTCLoss.UseLabelLengthsDefault)
    member __.BlankLabel = operatorArguments.GetParameter("blank_label", CTCLoss.BlankLabelDefault)

type CuDNNBatchNorm private (operatorArguments) = 
    inherit SymbolOperator("CuDNNBatchNorm", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new CuDNNBatchNorm(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?gamma : Symbol,
        [<Optional>] ?beta : Symbol,
        [<Optional>] ?movingMean : Symbol,
        [<Optional>] ?movingVar : Symbol,
        [<Optional>] ?eps : double,
        [<Optional>] ?momentum : float,
        [<Optional>] ?fixGamma : bool,
        [<Optional>] ?useGlobalStats : bool,
        [<Optional>] ?outputMeanVar : bool,
        [<Optional>] ?axis : int,
        [<Optional>] ?cudnnOff : bool,
        [<Optional>] ?minCalibRange : float,
        [<Optional>] ?maxCalibRange : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let gamma = defaultArg gamma (new ImplicitVariable() :> Symbol)
        let beta = defaultArg beta (new ImplicitVariable() :> Symbol)
        let movingMean = defaultArg movingMean (new ImplicitVariable() :> Symbol)
        let movingVar = defaultArg movingVar (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "gamma", Input gamma
                "beta", Input beta
                "moving_mean", Input movingMean
                "moving_var", Input movingVar
                "eps", eps |> Option.map box |> Parameter
                "momentum", momentum |> Option.map box |> Parameter
                "fix_gamma", fixGamma |> Option.map box |> Parameter
                "use_global_stats", useGlobalStats |> Option.map box |> Parameter
                "output_mean_var", outputMeanVar |> Option.map box |> Parameter
                "axis", axis |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
                "min_calib_range", minCalibRange |> Option.map box |> Parameter
                "max_calib_range", maxCalibRange |> Option.map box |> Parameter
            ]
        new CuDNNBatchNorm(Arguments<Symbol>(operatorArguments))
    static member EpsDefault : double = 0.0010000000474975
    static member MomentumDefault : double = 0.899999976
    static member FixGammaDefault : bool = true
    static member UseGlobalStatsDefault : bool = false
    static member OutputMeanVarDefault : bool = false
    static member AxisDefault : int = 1
    static member CudnnOffDefault : bool = false
    static member MinCalibRangeDefault : double option = None
    static member MaxCalibRangeDefault : double option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Gamma = operatorArguments.GetInput "gamma"
    member __.Beta = operatorArguments.GetInput "beta"
    member __.MovingMean = operatorArguments.GetInput "moving_mean"
    member __.MovingVar = operatorArguments.GetInput "moving_var"
    member __.Eps = operatorArguments.GetParameter("eps", CuDNNBatchNorm.EpsDefault)
    member __.Momentum = operatorArguments.GetParameter("momentum", CuDNNBatchNorm.MomentumDefault)
    member __.FixGamma = operatorArguments.GetParameter("fix_gamma", CuDNNBatchNorm.FixGammaDefault)
    member __.UseGlobalStats = operatorArguments.GetParameter("use_global_stats", CuDNNBatchNorm.UseGlobalStatsDefault)
    member __.OutputMeanVar = operatorArguments.GetParameter("output_mean_var", CuDNNBatchNorm.OutputMeanVarDefault)
    member __.Axis = operatorArguments.GetParameter("axis", CuDNNBatchNorm.AxisDefault)
    member __.CudnnOff = operatorArguments.GetParameter("cudnn_off", CuDNNBatchNorm.CudnnOffDefault)
    member __.MinCalibRange = operatorArguments.GetParameter("min_calib_range", CuDNNBatchNorm.MinCalibRangeDefault)
    member __.MaxCalibRange = operatorArguments.GetParameter("max_calib_range", CuDNNBatchNorm.MaxCalibRangeDefault)

type Deconvolution private (operatorArguments) = 
    inherit SymbolOperator("Deconvolution", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Deconvolution(args)
    new(data : Symbol,
        weight : Symbol,
        bias : Symbol,
        kernel : int seq,
        numFilter : int,
        [<Optional>] ?stride : int seq,
        [<Optional>] ?dilate : int seq,
        [<Optional>] ?pad : int seq,
        [<Optional>] ?adj : int seq,
        [<Optional>] ?targetShape : int seq,
        [<Optional>] ?numGroup : int,
        [<Optional>] ?workspace : int64,
        [<Optional>] ?noBias : bool,
        [<Optional>] ?cudnnTune : CudnnTune,
        [<Optional>] ?cudnnOff : bool,
        [<Optional>] ?layout : DeconvolutionLayout) = 
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "bias", Input bias
                "kernel", Parameter(Some(box kernel))
                "num_filter", Parameter(Some(box numFilter))
                "stride", stride |> Option.map box |> Parameter
                "dilate", dilate |> Option.map box |> Parameter
                "pad", pad |> Option.map box |> Parameter
                "adj", adj |> Option.map box |> Parameter
                "target_shape", targetShape |> Option.map box |> Parameter
                "num_group", numGroup |> Option.map box |> Parameter
                "workspace", workspace |> Option.map box |> Parameter
                "no_bias", noBias |> Option.map box |> Parameter
                "cudnn_tune", cudnnTune |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
                "layout", layout |> Option.map box |> Parameter
            ]
        new Deconvolution(Arguments<Symbol>(operatorArguments))
    new(kernel : int seq,
        numFilter : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?bias : Symbol,
        [<Optional>] ?stride : int seq,
        [<Optional>] ?dilate : int seq,
        [<Optional>] ?pad : int seq,
        [<Optional>] ?adj : int seq,
        [<Optional>] ?targetShape : int seq,
        [<Optional>] ?numGroup : int,
        [<Optional>] ?workspace : int64,
        [<Optional>] ?noBias : bool,
        [<Optional>] ?cudnnTune : CudnnTune,
        [<Optional>] ?cudnnOff : bool,
        [<Optional>] ?layout : DeconvolutionLayout) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let bias = defaultArg bias (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "bias", Input bias
                "kernel", Parameter(Some(box kernel))
                "num_filter", Parameter(Some(box numFilter))
                "stride", stride |> Option.map box |> Parameter
                "dilate", dilate |> Option.map box |> Parameter
                "pad", pad |> Option.map box |> Parameter
                "adj", adj |> Option.map box |> Parameter
                "target_shape", targetShape |> Option.map box |> Parameter
                "num_group", numGroup |> Option.map box |> Parameter
                "workspace", workspace |> Option.map box |> Parameter
                "no_bias", noBias |> Option.map box |> Parameter
                "cudnn_tune", cudnnTune |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
                "layout", layout |> Option.map box |> Parameter
            ]
        new Deconvolution(Arguments<Symbol>(operatorArguments))
    static member StrideDefault : int [] = [||]
    static member DilateDefault : int [] = [||]
    static member PadDefault : int [] = [||]
    static member AdjDefault : int [] = [||]
    static member TargetShapeDefault : int [] = [||]
    static member NumGroupDefault : int = 1
    static member WorkspaceDefault : int64 = 512L
    static member NoBiasDefault : bool = true
    static member CudnnTuneDefault : CudnnTune option = None
    static member CudnnOffDefault : bool = false
    static member LayoutDefault : DeconvolutionLayout option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Bias = operatorArguments.GetInput "bias"
    member __.Kernel : int seq = match operatorArguments.GetParameter "kernel" with Some(v) -> unbox v | None -> failwithf "Required parameter kernel is missing"
    member __.NumFilter : int = match operatorArguments.GetParameter "num_filter" with Some(v) -> unbox v | None -> failwithf "Required parameter num_filter is missing"
    member __.Stride = operatorArguments.GetParameter("stride", Deconvolution.StrideDefault)
    member __.Dilate = operatorArguments.GetParameter("dilate", Deconvolution.DilateDefault)
    member __.Pad = operatorArguments.GetParameter("pad", Deconvolution.PadDefault)
    member __.Adj = operatorArguments.GetParameter("adj", Deconvolution.AdjDefault)
    member __.TargetShape = operatorArguments.GetParameter("target_shape", Deconvolution.TargetShapeDefault)
    member __.NumGroup = operatorArguments.GetParameter("num_group", Deconvolution.NumGroupDefault)
    member __.Workspace = operatorArguments.GetParameter("workspace", Deconvolution.WorkspaceDefault)
    member __.NoBias = operatorArguments.GetParameter("no_bias", Deconvolution.NoBiasDefault)
    member __.CudnnTune = operatorArguments.GetParameter("cudnn_tune", Deconvolution.CudnnTuneDefault)
    member __.CudnnOff = operatorArguments.GetParameter("cudnn_off", Deconvolution.CudnnOffDefault)
    member __.Layout = operatorArguments.GetParameter("layout", Deconvolution.LayoutDefault)

type Dropout private (operatorArguments) = 
    inherit SymbolOperator("Dropout", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Dropout(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?p : float,
        [<Optional>] ?mode : DropoutMode,
        [<Optional>] ?axes : int seq,
        [<Optional>] ?cudnnOff : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "p", p |> Option.map box |> Parameter
                "mode", mode |> Option.map box |> Parameter
                "axes", axes |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
            ]
        new Dropout(Arguments<Symbol>(operatorArguments))
    static member PDefault : double = 0.5
    static member ModeDefault : DropoutMode = DropoutMode.Training
    static member AxesDefault : int [] = [||]
    static member CudnnOffDefault : bool option = Some(false)
    member __.Data = operatorArguments.GetInput "data"
    member __.P = operatorArguments.GetParameter("p", Dropout.PDefault)
    member __.Mode = operatorArguments.GetParameter("mode", Dropout.ModeDefault)
    member __.Axes = operatorArguments.GetParameter("axes", Dropout.AxesDefault)
    member __.CudnnOff = operatorArguments.GetParameter("cudnn_off", Dropout.CudnnOffDefault)

type FullyConnected private (operatorArguments) = 
    inherit SymbolOperator("FullyConnected", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new FullyConnected(args)
    new(data : Symbol,
        weight : Symbol,
        bias : Symbol,
        numHidden : int,
        [<Optional>] ?noBias : bool,
        [<Optional>] ?flatten : bool) = 
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "bias", Input bias
                "num_hidden", Parameter(Some(box numHidden))
                "no_bias", noBias |> Option.map box |> Parameter
                "flatten", flatten |> Option.map box |> Parameter
            ]
        new FullyConnected(Arguments<Symbol>(operatorArguments))
    new(numHidden : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?bias : Symbol,
        [<Optional>] ?noBias : bool,
        [<Optional>] ?flatten : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let bias = defaultArg bias (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "bias", Input bias
                "num_hidden", Parameter(Some(box numHidden))
                "no_bias", noBias |> Option.map box |> Parameter
                "flatten", flatten |> Option.map box |> Parameter
            ]
        new FullyConnected(Arguments<Symbol>(operatorArguments))
    static member NoBiasDefault : bool = false
    static member FlattenDefault : bool = true
    member __.Data = operatorArguments.GetInput "data"
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Bias = operatorArguments.GetInput "bias"
    member __.NumHidden : int = match operatorArguments.GetParameter "num_hidden" with Some(v) -> unbox v | None -> failwithf "Required parameter num_hidden is missing"
    member __.NoBias = operatorArguments.GetParameter("no_bias", FullyConnected.NoBiasDefault)
    member __.Flatten = operatorArguments.GetParameter("flatten", FullyConnected.FlattenDefault)

type GroupNorm private (operatorArguments) = 
    inherit SymbolOperator("GroupNorm", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new GroupNorm(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?gamma : Symbol,
        [<Optional>] ?beta : Symbol,
        [<Optional>] ?numGroups : int,
        [<Optional>] ?eps : float,
        [<Optional>] ?outputMeanVar : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let gamma = defaultArg gamma (new ImplicitVariable() :> Symbol)
        let beta = defaultArg beta (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "gamma", Input gamma
                "beta", Input beta
                "num_groups", numGroups |> Option.map box |> Parameter
                "eps", eps |> Option.map box |> Parameter
                "output_mean_var", outputMeanVar |> Option.map box |> Parameter
            ]
        new GroupNorm(Arguments<Symbol>(operatorArguments))
    static member NumGroupsDefault : int = 1
    static member EpsDefault : double = 0.00000999999975
    static member OutputMeanVarDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Gamma = operatorArguments.GetInput "gamma"
    member __.Beta = operatorArguments.GetInput "beta"
    member __.NumGroups = operatorArguments.GetParameter("num_groups", GroupNorm.NumGroupsDefault)
    member __.Eps = operatorArguments.GetParameter("eps", GroupNorm.EpsDefault)
    member __.OutputMeanVar = operatorArguments.GetParameter("output_mean_var", GroupNorm.OutputMeanVarDefault)

type LayerNorm private (operatorArguments) = 
    inherit SymbolOperator("LayerNorm", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LayerNorm(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?gamma : Symbol,
        [<Optional>] ?beta : Symbol,
        [<Optional>] ?axis : int,
        [<Optional>] ?eps : float,
        [<Optional>] ?outputMeanVar : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let gamma = defaultArg gamma (new ImplicitVariable() :> Symbol)
        let beta = defaultArg beta (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "gamma", Input gamma
                "beta", Input beta
                "axis", axis |> Option.map box |> Parameter
                "eps", eps |> Option.map box |> Parameter
                "output_mean_var", outputMeanVar |> Option.map box |> Parameter
            ]
        new LayerNorm(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int = -1
    static member EpsDefault : double = 0.00000999999975
    static member OutputMeanVarDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Gamma = operatorArguments.GetInput "gamma"
    member __.Beta = operatorArguments.GetInput "beta"
    member __.Axis = operatorArguments.GetParameter("axis", LayerNorm.AxisDefault)
    member __.Eps = operatorArguments.GetParameter("eps", LayerNorm.EpsDefault)
    member __.OutputMeanVar = operatorArguments.GetParameter("output_mean_var", LayerNorm.OutputMeanVarDefault)

type LogSoftmax private (operatorArguments) = 
    inherit SymbolOperator("log_softmax", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LogSoftmax(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int,
        [<Optional>] ?temperature : float,
        [<Optional>] ?dtype : FloatDType,
        [<Optional>] ?useLength : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "temperature", temperature |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
                "use_length", useLength |> Option.map box |> Parameter
            ]
        new LogSoftmax(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int = -1
    static member TemperatureDefault : double option = None
    static member DtypeDefault : FloatDType option = None
    static member UseLengthDefault : bool option = Some(false)
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", LogSoftmax.AxisDefault)
    member __.Temperature = operatorArguments.GetParameter("temperature", LogSoftmax.TemperatureDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", LogSoftmax.DtypeDefault)
    member __.UseLength = operatorArguments.GetParameter("use_length", LogSoftmax.UseLengthDefault)

type LRN private (operatorArguments) = 
    inherit SymbolOperator("LRN", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LRN(args)
    new(data : Symbol,
        nsize : int,
        [<Optional>] ?alpha : float,
        [<Optional>] ?beta : float,
        [<Optional>] ?knorm : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "nsize", Parameter(Some(box nsize))
                "alpha", alpha |> Option.map box |> Parameter
                "beta", beta |> Option.map box |> Parameter
                "knorm", knorm |> Option.map box |> Parameter
            ]
        new LRN(Arguments<Symbol>(operatorArguments))
    new(nsize : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?alpha : float,
        [<Optional>] ?beta : float,
        [<Optional>] ?knorm : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "nsize", Parameter(Some(box nsize))
                "alpha", alpha |> Option.map box |> Parameter
                "beta", beta |> Option.map box |> Parameter
                "knorm", knorm |> Option.map box |> Parameter
            ]
        new LRN(Arguments<Symbol>(operatorArguments))
    static member AlphaDefault : double = 0.0000999999975
    static member BetaDefault : double = 0.75
    static member KnormDefault : double = 2.0
    member __.Data = operatorArguments.GetInput "data"
    member __.Nsize : int = match operatorArguments.GetParameter "nsize" with Some(v) -> unbox v | None -> failwithf "Required parameter nsize is missing"
    member __.Alpha = operatorArguments.GetParameter("alpha", LRN.AlphaDefault)
    member __.Beta = operatorArguments.GetParameter("beta", LRN.BetaDefault)
    member __.Knorm = operatorArguments.GetParameter("knorm", LRN.KnormDefault)

type Moments private (operatorArguments) = 
    inherit SymbolOperator("moments", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Moments(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axes : int seq,
        [<Optional>] ?keepdims : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axes", axes |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
            ]
        new Moments(Arguments<Symbol>(operatorArguments))
    static member AxesDefault : int [] option = None
    static member KeepdimsDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Axes = operatorArguments.GetParameter("axes", Moments.AxesDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", Moments.KeepdimsDefault)

type Pooling private (operatorArguments) = 
    inherit SymbolOperator("Pooling", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Pooling(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?kernel : int seq,
        [<Optional>] ?poolType : PoolType,
        [<Optional>] ?globalPool : bool,
        [<Optional>] ?cudnnOff : bool,
        [<Optional>] ?poolingConvention : PoolingConvention,
        [<Optional>] ?stride : int seq,
        [<Optional>] ?pad : int seq,
        [<Optional>] ?pValue : int,
        [<Optional>] ?countIncludePad : bool,
        [<Optional>] ?layout : PoolingLayout) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "kernel", kernel |> Option.map box |> Parameter
                "pool_type", poolType |> Option.map box |> Parameter
                "global_pool", globalPool |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
                "pooling_convention", poolingConvention |> Option.map box |> Parameter
                "stride", stride |> Option.map box |> Parameter
                "pad", pad |> Option.map box |> Parameter
                "p_value", pValue |> Option.map box |> Parameter
                "count_include_pad", countIncludePad |> Option.map box |> Parameter
                "layout", layout |> Option.map box |> Parameter
            ]
        new Pooling(Arguments<Symbol>(operatorArguments))
    static member KernelDefault : int [] = [||]
    static member PoolTypeDefault : PoolType = PoolType.Max
    static member GlobalPoolDefault : bool = false
    static member CudnnOffDefault : bool = false
    static member PoolingConventionDefault : PoolingConvention = PoolingConvention.Valid
    static member StrideDefault : int [] = [||]
    static member PadDefault : int [] = [||]
    static member PValueDefault : int option = None
    static member CountIncludePadDefault : bool option = None
    static member LayoutDefault : PoolingLayout option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Kernel = operatorArguments.GetParameter("kernel", Pooling.KernelDefault)
    member __.PoolType = operatorArguments.GetParameter("pool_type", Pooling.PoolTypeDefault)
    member __.GlobalPool = operatorArguments.GetParameter("global_pool", Pooling.GlobalPoolDefault)
    member __.CudnnOff = operatorArguments.GetParameter("cudnn_off", Pooling.CudnnOffDefault)
    member __.PoolingConvention = operatorArguments.GetParameter("pooling_convention", Pooling.PoolingConventionDefault)
    member __.Stride = operatorArguments.GetParameter("stride", Pooling.StrideDefault)
    member __.Pad = operatorArguments.GetParameter("pad", Pooling.PadDefault)
    member __.PValue = operatorArguments.GetParameter("p_value", Pooling.PValueDefault)
    member __.CountIncludePad = operatorArguments.GetParameter("count_include_pad", Pooling.CountIncludePadDefault)
    member __.Layout = operatorArguments.GetParameter("layout", Pooling.LayoutDefault)

type Softmax private (operatorArguments) = 
    inherit SymbolOperator("softmax", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Softmax(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?length : Symbol,
        [<Optional>] ?axis : int,
        [<Optional>] ?temperature : float,
        [<Optional>] ?dtype : FloatDType,
        [<Optional>] ?useLength : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let length = defaultArg length (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "length", Input length
                "axis", axis |> Option.map box |> Parameter
                "temperature", temperature |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
                "use_length", useLength |> Option.map box |> Parameter
            ]
        new Softmax(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int = -1
    static member TemperatureDefault : double option = None
    static member DtypeDefault : FloatDType option = None
    static member UseLengthDefault : bool option = Some(false)
    member __.Data = operatorArguments.GetInput "data"
    member __.Length = operatorArguments.GetInput "length"
    member __.Axis = operatorArguments.GetParameter("axis", Softmax.AxisDefault)
    member __.Temperature = operatorArguments.GetParameter("temperature", Softmax.TemperatureDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", Softmax.DtypeDefault)
    member __.UseLength = operatorArguments.GetParameter("use_length", Softmax.UseLengthDefault)

type SoftmaxActivation private (operatorArguments) = 
    inherit SymbolOperator("SoftmaxActivation", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SoftmaxActivation(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?mode : SoftmaxActivationMode) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "mode", mode |> Option.map box |> Parameter
            ]
        new SoftmaxActivation(Arguments<Symbol>(operatorArguments))
    static member ModeDefault : SoftmaxActivationMode = SoftmaxActivationMode.Instance
    member __.Data = operatorArguments.GetInput "data"
    member __.Mode = operatorArguments.GetParameter("mode", SoftmaxActivation.ModeDefault)

type Softmin private (operatorArguments) = 
    inherit SymbolOperator("softmin", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Softmin(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int,
        [<Optional>] ?temperature : float,
        [<Optional>] ?dtype : FloatDType,
        [<Optional>] ?useLength : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "temperature", temperature |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
                "use_length", useLength |> Option.map box |> Parameter
            ]
        new Softmin(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int = -1
    static member TemperatureDefault : double option = None
    static member DtypeDefault : FloatDType option = None
    static member UseLengthDefault : bool option = Some(false)
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", Softmin.AxisDefault)
    member __.Temperature = operatorArguments.GetParameter("temperature", Softmin.TemperatureDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", Softmin.DtypeDefault)
    member __.UseLength = operatorArguments.GetParameter("use_length", Softmin.UseLengthDefault)

type UpSampling private (operatorArguments) = 
    inherit SymbolOperator("UpSampling", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new UpSampling(args)
    new(data : Symbol seq,
        scale : int,
        sampleType : SampleType,
        [<Optional>] ?numFilter : int,
        [<Optional>] ?multiInputMode : MultiInputMode,
        [<Optional>] ?workspace : int64) = 
        let operatorArguments = 
            [
                "data", VarArg("num_args", data |> Seq.toArray)
                "scale", Parameter(Some(box scale))
                "sample_type", Parameter(Some(box sampleType))
                "num_filter", numFilter |> Option.map box |> Parameter
                "multi_input_mode", multiInputMode |> Option.map box |> Parameter
                "workspace", workspace |> Option.map box |> Parameter
            ]
        new UpSampling(Arguments<Symbol>(operatorArguments))
    new(scale : int,
        sampleType : SampleType,
        [<Optional>] ?data : Symbol seq,
        [<Optional>] ?numFilter : int,
        [<Optional>] ?multiInputMode : MultiInputMode,
        [<Optional>] ?workspace : int64) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("num_args", data)
                "scale", Parameter(Some(box scale))
                "sample_type", Parameter(Some(box sampleType))
                "num_filter", numFilter |> Option.map box |> Parameter
                "multi_input_mode", multiInputMode |> Option.map box |> Parameter
                "workspace", workspace |> Option.map box |> Parameter
            ]
        new UpSampling(Arguments<Symbol>(operatorArguments))
    static member NumFilterDefault : int = 0
    static member MultiInputModeDefault : MultiInputMode = MultiInputMode.Concat
    static member WorkspaceDefault : int64 = 512L
    member __.Data = operatorArguments.GetVarArg "data"
    member __.Scale : int = match operatorArguments.GetParameter "scale" with Some(v) -> unbox v | None -> failwithf "Required parameter scale is missing"
    member __.SampleType : SampleType = match operatorArguments.GetParameter "sample_type" with Some(v) -> unbox v | None -> failwithf "Required parameter sample_type is missing"
    member __.NumFilter = operatorArguments.GetParameter("num_filter", UpSampling.NumFilterDefault)
    member __.MultiInputMode = operatorArguments.GetParameter("multi_input_mode", UpSampling.MultiInputModeDefault)
    member __.Workspace = operatorArguments.GetParameter("workspace", UpSampling.WorkspaceDefault)

type NpLinalgSvd private (operatorArguments) = 
    inherit SymbolOperator("_np__linalg_svd", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpLinalgSvd(args)
    new([<Optional>] ?A : Symbol) =
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
            ]
        new NpLinalgSvd(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "A"

type NpiArgmax private (operatorArguments) = 
    inherit SymbolOperator("_npi_argmax", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiArgmax(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int,
        [<Optional>] ?keepdims : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
            ]
        new NpiArgmax(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int option = None
    static member KeepdimsDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", NpiArgmax.AxisDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", NpiArgmax.KeepdimsDefault)

type NpSum private (operatorArguments) = 
    inherit SymbolOperator("_np_sum", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpSum(args)
    new([<Optional>] ?a : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?dtype : NpSumDtype,
        [<Optional>] ?keepdims : bool,
        [<Optional>] ?initial : float) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "axis", axis |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
                "initial", initial |> Option.map box |> Parameter
            ]
        new NpSum(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    static member DtypeDefault : NpSumDtype option = None
    static member KeepdimsDefault : bool = false
    static member InitialDefault : double option = None
    member __.A = operatorArguments.GetInput "a"
    member __.Axis = operatorArguments.GetParameter("axis", NpSum.AxisDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", NpSum.DtypeDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", NpSum.KeepdimsDefault)
    member __.Initial = operatorArguments.GetParameter("initial", NpSum.InitialDefault)

type NpMax private (operatorArguments) = 
    inherit SymbolOperator("_np_max", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpMax(args)
    new([<Optional>] ?a : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?keepdims : bool,
        [<Optional>] ?initial : float) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "axis", axis |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
                "initial", initial |> Option.map box |> Parameter
            ]
        new NpMax(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    static member KeepdimsDefault : bool = false
    static member InitialDefault : double option = None
    member __.A = operatorArguments.GetInput "a"
    member __.Axis = operatorArguments.GetParameter("axis", NpMax.AxisDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", NpMax.KeepdimsDefault)
    member __.Initial = operatorArguments.GetParameter("initial", NpMax.InitialDefault)

type NpMin private (operatorArguments) = 
    inherit SymbolOperator("_np_min", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpMin(args)
    new([<Optional>] ?a : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?keepdims : bool,
        [<Optional>] ?initial : float) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "axis", axis |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
                "initial", initial |> Option.map box |> Parameter
            ]
        new NpMin(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    static member KeepdimsDefault : bool = false
    static member InitialDefault : double option = None
    member __.A = operatorArguments.GetInput "a"
    member __.Axis = operatorArguments.GetParameter("axis", NpMin.AxisDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", NpMin.KeepdimsDefault)
    member __.Initial = operatorArguments.GetParameter("initial", NpMin.InitialDefault)

type NpProd private (operatorArguments) = 
    inherit SymbolOperator("_np_prod", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpProd(args)
    new([<Optional>] ?a : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?dtype : NpProdDtype,
        [<Optional>] ?keepdims : bool,
        [<Optional>] ?initial : float) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "axis", axis |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
                "initial", initial |> Option.map box |> Parameter
            ]
        new NpProd(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    static member DtypeDefault : NpProdDtype option = None
    static member KeepdimsDefault : bool = false
    static member InitialDefault : double option = None
    member __.A = operatorArguments.GetInput "a"
    member __.Axis = operatorArguments.GetParameter("axis", NpProd.AxisDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", NpProd.DtypeDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", NpProd.KeepdimsDefault)
    member __.Initial = operatorArguments.GetParameter("initial", NpProd.InitialDefault)

type NpiMean private (operatorArguments) = 
    inherit SymbolOperator("_npi_mean", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiMean(args)
    new([<Optional>] ?a : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?dtype : NpiMeanDtype,
        [<Optional>] ?keepdims : bool,
        [<Optional>] ?initial : float) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "axis", axis |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
                "initial", initial |> Option.map box |> Parameter
            ]
        new NpiMean(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    static member DtypeDefault : NpiMeanDtype option = None
    static member KeepdimsDefault : bool = false
    static member InitialDefault : double option = None
    member __.A = operatorArguments.GetInput "a"
    member __.Axis = operatorArguments.GetParameter("axis", NpiMean.AxisDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", NpiMean.DtypeDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", NpiMean.KeepdimsDefault)
    member __.Initial = operatorArguments.GetParameter("initial", NpiMean.InitialDefault)

type NpiStd private (operatorArguments) = 
    inherit SymbolOperator("_npi_std", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiStd(args)
    new([<Optional>] ?a : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?dtype : NpiStdDtype,
        [<Optional>] ?ddof : int,
        [<Optional>] ?keepdims : bool) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "axis", axis |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
                "ddof", ddof |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
            ]
        new NpiStd(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    static member DtypeDefault : NpiStdDtype option = None
    static member DdofDefault : int = 0
    static member KeepdimsDefault : bool = false
    member __.A = operatorArguments.GetInput "a"
    member __.Axis = operatorArguments.GetParameter("axis", NpiStd.AxisDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", NpiStd.DtypeDefault)
    member __.Ddof = operatorArguments.GetParameter("ddof", NpiStd.DdofDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", NpiStd.KeepdimsDefault)

type NpiVar private (operatorArguments) = 
    inherit SymbolOperator("_npi_var", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiVar(args)
    new([<Optional>] ?a : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?dtype : NpiVarDtype,
        [<Optional>] ?ddof : int,
        [<Optional>] ?keepdims : bool) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "axis", axis |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
                "ddof", ddof |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
            ]
        new NpiVar(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    static member DtypeDefault : NpiVarDtype option = None
    static member DdofDefault : int = 0
    static member KeepdimsDefault : bool = false
    member __.A = operatorArguments.GetInput "a"
    member __.Axis = operatorArguments.GetParameter("axis", NpiVar.AxisDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", NpiVar.DtypeDefault)
    member __.Ddof = operatorArguments.GetParameter("ddof", NpiVar.DdofDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", NpiVar.KeepdimsDefault)

type NpBroadcastTo private (operatorArguments) = 
    inherit SymbolOperator("_np_broadcast_to", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpBroadcastTo(args)
    new([<Optional>] ?array : Symbol,
        [<Optional>] ?shape : int seq) = 
        let array = defaultArg array (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "array", Input array
                "shape", shape |> Option.map box |> Parameter
            ]
        new NpBroadcastTo(Arguments<Symbol>(operatorArguments))
    static member ShapeDefault : int [] = [||]
    member __.Array = operatorArguments.GetInput "array"
    member __.Shape = operatorArguments.GetParameter("shape", NpBroadcastTo.ShapeDefault)

type NpCumsum private (operatorArguments) = 
    inherit SymbolOperator("_np_cumsum", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpCumsum(args)
    new([<Optional>] ?a : Symbol,
        [<Optional>] ?axis : int,
        [<Optional>] ?dtype : NpCumsumDtype) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "axis", axis |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
            ]
        new NpCumsum(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int option = None
    static member DtypeDefault : NpCumsumDtype option = None
    member __.A = operatorArguments.GetInput "a"
    member __.Axis = operatorArguments.GetParameter("axis", NpCumsum.AxisDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", NpCumsum.DtypeDefault)

type NpDot private (operatorArguments) = 
    inherit SymbolOperator("_np_dot", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpDot(args)
    new([<Optional>] ?a : Symbol,
        [<Optional>] ?b : Symbol) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let b = defaultArg b (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "b", Input b
            ]
        new NpDot(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "a"
    member __.B = operatorArguments.GetInput "b"

type NpiAdd private (operatorArguments) = 
    inherit SymbolOperator("_npi_add", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiAdd(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new NpiAdd(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type NpiSubtract private (operatorArguments) = 
    inherit SymbolOperator("_npi_subtract", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiSubtract(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new NpiSubtract(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type NpiMultiply private (operatorArguments) = 
    inherit SymbolOperator("_npi_multiply", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiMultiply(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new NpiMultiply(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type NpiMod private (operatorArguments) = 
    inherit SymbolOperator("_npi_mod", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiMod(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new NpiMod(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type NpiPower private (operatorArguments) = 
    inherit SymbolOperator("_npi_power", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiPower(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new NpiPower(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type NpiCopysign private (operatorArguments) = 
    inherit SymbolOperator("_npi_copysign", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiCopysign(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new NpiCopysign(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type NpiLcm private (operatorArguments) = 
    inherit SymbolOperator("_npi_lcm", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiLcm(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new NpiLcm(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type NpiAddScalar private (operatorArguments) = 
    inherit SymbolOperator("_npi_add_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiAddScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiAddScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiAddScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NpiSubtractScalar private (operatorArguments) = 
    inherit SymbolOperator("_npi_subtract_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiSubtractScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiSubtractScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiSubtractScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NpiRsubtractScalar private (operatorArguments) = 
    inherit SymbolOperator("_npi_rsubtract_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiRsubtractScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiRsubtractScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiRsubtractScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NpiMultiplyScalar private (operatorArguments) = 
    inherit SymbolOperator("_npi_multiply_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiMultiplyScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiMultiplyScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiMultiplyScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NpiModScalar private (operatorArguments) = 
    inherit SymbolOperator("_npi_mod_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiModScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiModScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiModScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NpiRmodScalar private (operatorArguments) = 
    inherit SymbolOperator("_npi_rmod_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiRmodScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiRmodScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiRmodScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NpiPowerScalar private (operatorArguments) = 
    inherit SymbolOperator("_npi_power_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiPowerScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiPowerScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiPowerScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NpiRpowerScalar private (operatorArguments) = 
    inherit SymbolOperator("_npi_rpower_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiRpowerScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiRpowerScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiRpowerScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NpiCopysignScalar private (operatorArguments) = 
    inherit SymbolOperator("_npi_copysign_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiCopysignScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiCopysignScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiCopysignScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NpiRcopysignScalar private (operatorArguments) = 
    inherit SymbolOperator("_npi_rcopysign_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiRcopysignScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiRcopysignScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiRcopysignScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NpiArctan2 private (operatorArguments) = 
    inherit SymbolOperator("_npi_arctan2", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiArctan2(args)
    new([<Optional>] ?x1 : Symbol,
        [<Optional>] ?x2 : Symbol) = 
        let x1 = defaultArg x1 (new ImplicitVariable() :> Symbol)
        let x2 = defaultArg x2 (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x1", Input x1
                "x2", Input x2
            ]
        new NpiArctan2(Arguments<Symbol>(operatorArguments))
    member __.X1 = operatorArguments.GetInput "x1"
    member __.X2 = operatorArguments.GetInput "x2"

type NpiArctan2Scalar private (operatorArguments) = 
    inherit SymbolOperator("_npi_arctan2_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiArctan2Scalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiArctan2Scalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiArctan2Scalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NpiRarctan2Scalar private (operatorArguments) = 
    inherit SymbolOperator("_npi_rarctan2_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiRarctan2Scalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiRarctan2Scalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiRarctan2Scalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NpiHypot private (operatorArguments) = 
    inherit SymbolOperator("_npi_hypot", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiHypot(args)
    new([<Optional>] ?x1 : Symbol,
        [<Optional>] ?x2 : Symbol) = 
        let x1 = defaultArg x1 (new ImplicitVariable() :> Symbol)
        let x2 = defaultArg x2 (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x1", Input x1
                "x2", Input x2
            ]
        new NpiHypot(Arguments<Symbol>(operatorArguments))
    member __.X1 = operatorArguments.GetInput "x1"
    member __.X2 = operatorArguments.GetInput "x2"

type NpiLcmScalar private (operatorArguments) = 
    inherit SymbolOperator("_npi_lcm_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiLcmScalar(args)
    new(data : Symbol,
        scalar : int) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiLcmScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : int,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiLcmScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : int = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NpxRelu private (operatorArguments) = 
    inherit SymbolOperator("_npx_relu", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpxRelu(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new NpxRelu(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type NpxSigmoid private (operatorArguments) = 
    inherit SymbolOperator("_npx_sigmoid", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpxSigmoid(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new NpxSigmoid(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type NpCopy private (operatorArguments) = 
    inherit SymbolOperator("_np_copy", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpCopy(args)
    new([<Optional>] ?a : Symbol) =
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
            ]
        new NpCopy(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "a"

type NpiNegative private (operatorArguments) = 
    inherit SymbolOperator("_npi_negative", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiNegative(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiNegative(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiReciprocal private (operatorArguments) = 
    inherit SymbolOperator("_npi_reciprocal", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiReciprocal(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiReciprocal(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiAbsolute private (operatorArguments) = 
    inherit SymbolOperator("_npi_absolute", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiAbsolute(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiAbsolute(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiSign private (operatorArguments) = 
    inherit SymbolOperator("_npi_sign", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiSign(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiSign(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiRint private (operatorArguments) = 
    inherit SymbolOperator("_npi_rint", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiRint(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiRint(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiCeil private (operatorArguments) = 
    inherit SymbolOperator("_npi_ceil", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiCeil(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiCeil(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiFloor private (operatorArguments) = 
    inherit SymbolOperator("_npi_floor", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiFloor(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiFloor(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiTrunc private (operatorArguments) = 
    inherit SymbolOperator("_npi_trunc", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiTrunc(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiTrunc(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiFix private (operatorArguments) = 
    inherit SymbolOperator("_npi_fix", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiFix(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiFix(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiSquare private (operatorArguments) = 
    inherit SymbolOperator("_npi_square", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiSquare(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiSquare(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiSqrt private (operatorArguments) = 
    inherit SymbolOperator("_npi_sqrt", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiSqrt(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiSqrt(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiCbrt private (operatorArguments) = 
    inherit SymbolOperator("_npi_cbrt", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiCbrt(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiCbrt(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiExp private (operatorArguments) = 
    inherit SymbolOperator("_npi_exp", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiExp(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiExp(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiLog private (operatorArguments) = 
    inherit SymbolOperator("_npi_log", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiLog(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiLog(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiLog10 private (operatorArguments) = 
    inherit SymbolOperator("_npi_log10", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiLog10(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiLog10(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiLog2 private (operatorArguments) = 
    inherit SymbolOperator("_npi_log2", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiLog2(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiLog2(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiLog1p private (operatorArguments) = 
    inherit SymbolOperator("_npi_log1p", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiLog1p(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiLog1p(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiExpm1 private (operatorArguments) = 
    inherit SymbolOperator("_npi_expm1", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiExpm1(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiExpm1(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiLogicalNot private (operatorArguments) = 
    inherit SymbolOperator("_npi_logical_not", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiLogicalNot(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiLogicalNot(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiSin private (operatorArguments) = 
    inherit SymbolOperator("_npi_sin", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiSin(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiSin(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiCos private (operatorArguments) = 
    inherit SymbolOperator("_npi_cos", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiCos(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiCos(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiTan private (operatorArguments) = 
    inherit SymbolOperator("_npi_tan", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiTan(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiTan(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiArcsin private (operatorArguments) = 
    inherit SymbolOperator("_npi_arcsin", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiArcsin(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiArcsin(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiArccos private (operatorArguments) = 
    inherit SymbolOperator("_npi_arccos", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiArccos(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiArccos(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiArctan private (operatorArguments) = 
    inherit SymbolOperator("_npi_arctan", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiArctan(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiArctan(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiDegrees private (operatorArguments) = 
    inherit SymbolOperator("_npi_degrees", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiDegrees(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiDegrees(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiRadians private (operatorArguments) = 
    inherit SymbolOperator("_npi_radians", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiRadians(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiRadians(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiSinh private (operatorArguments) = 
    inherit SymbolOperator("_npi_sinh", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiSinh(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiSinh(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiCosh private (operatorArguments) = 
    inherit SymbolOperator("_npi_cosh", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiCosh(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiCosh(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiTanh private (operatorArguments) = 
    inherit SymbolOperator("_npi_tanh", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiTanh(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiTanh(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiArcsinh private (operatorArguments) = 
    inherit SymbolOperator("_npi_arcsinh", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiArcsinh(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiArcsinh(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiArccosh private (operatorArguments) = 
    inherit SymbolOperator("_npi_arccosh", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiArccosh(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiArccosh(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiArctanh private (operatorArguments) = 
    inherit SymbolOperator("_npi_arctanh", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiArctanh(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpiArctanh(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiAround private (operatorArguments) = 
    inherit SymbolOperator("_npi_around", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiAround(args)
    new([<Optional>] ?x : Symbol,
        [<Optional>] ?decimals : int) = 
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
                "decimals", decimals |> Option.map box |> Parameter
            ]
        new NpiAround(Arguments<Symbol>(operatorArguments))
    static member DecimalsDefault : int = 0
    member __.X = operatorArguments.GetInput "x"
    member __.Decimals = operatorArguments.GetParameter("decimals", NpiAround.DecimalsDefault)

type NpZerosLike private (operatorArguments) = 
    inherit SymbolOperator("_np_zeros_like", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpZerosLike(args)
    new([<Optional>] ?a : Symbol) =
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
            ]
        new NpZerosLike(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "a"

type NpOnesLike private (operatorArguments) = 
    inherit SymbolOperator("_np_ones_like", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpOnesLike(args)
    new([<Optional>] ?a : Symbol) =
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
            ]
        new NpOnesLike(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "a"

type NpTranspose private (operatorArguments) = 
    inherit SymbolOperator("_np_transpose", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpTranspose(args)
    new([<Optional>] ?a : Symbol,
        [<Optional>] ?axes : int seq) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "axes", axes |> Option.map box |> Parameter
            ]
        new NpTranspose(Arguments<Symbol>(operatorArguments))
    static member AxesDefault : int [] option = None
    member __.A = operatorArguments.GetInput "a"
    member __.Axes = operatorArguments.GetParameter("axes", NpTranspose.AxesDefault)

type NpReshape private (operatorArguments) = 
    inherit SymbolOperator("_np_reshape", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpReshape(args)
    new(a : Symbol,
        newshape : int seq,
        [<Optional>] ?order : string) = 
        let operatorArguments = 
            [
                "a", Input a
                "newshape", Parameter(Some(box newshape))
                "order", order |> Option.map box |> Parameter
            ]
        new NpReshape(Arguments<Symbol>(operatorArguments))
    new(newshape : int seq,
        [<Optional>] ?a : Symbol,
        [<Optional>] ?order : string) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "newshape", Parameter(Some(box newshape))
                "order", order |> Option.map box |> Parameter
            ]
        new NpReshape(Arguments<Symbol>(operatorArguments))
    static member OrderDefault : string = "C"
    member __.A = operatorArguments.GetInput "a"
    member __.Newshape : int seq = match operatorArguments.GetParameter "newshape" with Some(v) -> unbox v | None -> failwithf "Required parameter newshape is missing"
    member __.Order = operatorArguments.GetParameter("order", NpReshape.OrderDefault)

type NpSqueeze private (operatorArguments) = 
    inherit SymbolOperator("_np_squeeze", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpSqueeze(args)
    new([<Optional>] ?a : Symbol seq,
        [<Optional>] ?axis : int seq) = 
        let a = defaultArg (a |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "a", VarArg("", a)
                "axis", axis |> Option.map box |> Parameter
            ]
        new NpSqueeze(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    member __.A = operatorArguments.GetVarArg "a"
    member __.Axis = operatorArguments.GetParameter("axis", NpSqueeze.AxisDefault)

type NpiConcatenate private (operatorArguments) = 
    inherit SymbolOperator("_npi_concatenate", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiConcatenate(args)
    new([<Optional>] ?data : Symbol seq,
        [<Optional>] ?dim : int) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("num_args", data)
                "dim", dim |> Option.map box |> Parameter
            ]
        new NpiConcatenate(Arguments<Symbol>(operatorArguments))
    static member DimDefault : int = 1
    member __.Data = operatorArguments.GetVarArg "data"
    member __.Dim = operatorArguments.GetParameter("dim", NpiConcatenate.DimDefault)

type NpiStack private (operatorArguments) = 
    inherit SymbolOperator("_npi_stack", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiStack(args)
    new([<Optional>] ?data : Symbol seq,
        [<Optional>] ?axis : int) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("num_args", data)
                "axis", axis |> Option.map box |> Parameter
            ]
        new NpiStack(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int = 0
    member __.Data = operatorArguments.GetVarArg "data"
    member __.Axis = operatorArguments.GetParameter("axis", NpiStack.AxisDefault)

type NpiVstack private (operatorArguments) = 
    inherit SymbolOperator("_npi_vstack", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiVstack(args)
    new([<Optional>] ?data : Symbol seq) =
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("num_args", data)
            ]
        new NpiVstack(Arguments<Symbol>(operatorArguments))
    new([<ParamArray>] data : Symbol[]) =
        let operatorArguments = 
            [
                "data", VarArg("num_args", data)
            ]
        new NpiVstack(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetVarArg "data"

type NpRoll private (operatorArguments) = 
    inherit SymbolOperator("_np_roll", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpRoll(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?shift : int seq,
        [<Optional>] ?axis : int seq) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "shift", shift |> Option.map box |> Parameter
                "axis", axis |> Option.map box |> Parameter
            ]
        new NpRoll(Arguments<Symbol>(operatorArguments))
    static member ShiftDefault : int [] option = None
    static member AxisDefault : int [] option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Shift = operatorArguments.GetParameter("shift", NpRoll.ShiftDefault)
    member __.Axis = operatorArguments.GetParameter("axis", NpRoll.AxisDefault)

type NpiFlip private (operatorArguments) = 
    inherit SymbolOperator("_npi_flip", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiFlip(args)
    new(data : Symbol,
        axis : int seq) = 
        let operatorArguments = 
            [
                "data", Input data
                "axis", Parameter(Some(box axis))
            ]
        new NpiFlip(Arguments<Symbol>(operatorArguments))
    new(axis : int seq,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", Parameter(Some(box axis))
            ]
        new NpiFlip(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis : int seq = match operatorArguments.GetParameter "axis" with Some(v) -> unbox v | None -> failwithf "Required parameter axis is missing"

type NpxNonzero private (operatorArguments) = 
    inherit SymbolOperator("_npx_nonzero", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpxNonzero(args)
    new([<Optional>] ?x : Symbol) =
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "x", Input x
            ]
        new NpxNonzero(Arguments<Symbol>(operatorArguments))
    member __.X = operatorArguments.GetInput "x"

type NpiTensordot private (operatorArguments) = 
    inherit SymbolOperator("_npi_tensordot", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiTensordot(args)
    new(a : Symbol,
        b : Symbol,
        aAxesSummed : int seq,
        bAxesSummed : int seq) = 
        let operatorArguments = 
            [
                "a", Input a
                "b", Input b
                "a_axes_summed", Parameter(Some(box aAxesSummed))
                "b_axes_summed", Parameter(Some(box bAxesSummed))
            ]
        new NpiTensordot(Arguments<Symbol>(operatorArguments))
    new(aAxesSummed : int seq,
        bAxesSummed : int seq,
        [<Optional>] ?a : Symbol,
        [<Optional>] ?b : Symbol) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let b = defaultArg b (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "b", Input b
                "a_axes_summed", Parameter(Some(box aAxesSummed))
                "b_axes_summed", Parameter(Some(box bAxesSummed))
            ]
        new NpiTensordot(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "a"
    member __.B = operatorArguments.GetInput "b"
    member __.AAxesSummed : int seq = match operatorArguments.GetParameter "a_axes_summed" with Some(v) -> unbox v | None -> failwithf "Required parameter a_axes_summed is missing"
    member __.BAxesSummed : int seq = match operatorArguments.GetParameter "b_axes_summed" with Some(v) -> unbox v | None -> failwithf "Required parameter b_axes_summed is missing"

type NpiTensordotIntAxes private (operatorArguments) = 
    inherit SymbolOperator("_npi_tensordot_int_axes", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiTensordotIntAxes(args)
    new(a : Symbol,
        b : Symbol,
        axes : int) = 
        let operatorArguments = 
            [
                "a", Input a
                "b", Input b
                "axes", Parameter(Some(box axes))
            ]
        new NpiTensordotIntAxes(Arguments<Symbol>(operatorArguments))
    new(axes : int,
        [<Optional>] ?a : Symbol,
        [<Optional>] ?b : Symbol) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let b = defaultArg b (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "b", Input b
                "axes", Parameter(Some(box axes))
            ]
        new NpiTensordotIntAxes(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "a"
    member __.B = operatorArguments.GetInput "b"
    member __.Axes : int = match operatorArguments.GetParameter "axes" with Some(v) -> unbox v | None -> failwithf "Required parameter axes is missing"

type NpTrace private (operatorArguments) = 
    inherit SymbolOperator("_np_trace", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpTrace(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?offset : int,
        [<Optional>] ?axis1 : int,
        [<Optional>] ?axis2 : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "offset", offset |> Option.map box |> Parameter
                "axis1", axis1 |> Option.map box |> Parameter
                "axis2", axis2 |> Option.map box |> Parameter
            ]
        new NpTrace(Arguments<Symbol>(operatorArguments))
    static member OffsetDefault : int = 0
    static member Axis1Default : int = 0
    static member Axis2Default : int = 1
    member __.Data = operatorArguments.GetInput "data"
    member __.Offset = operatorArguments.GetParameter("offset", NpTrace.OffsetDefault)
    member __.Axis1 = operatorArguments.GetParameter("axis1", NpTrace.Axis1Default)
    member __.Axis2 = operatorArguments.GetParameter("axis2", NpTrace.Axis2Default)

type NpiTril private (operatorArguments) = 
    inherit SymbolOperator("_npi_tril", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiTril(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?k : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "k", k |> Option.map box |> Parameter
            ]
        new NpiTril(Arguments<Symbol>(operatorArguments))
    static member KDefault : int = 0
    member __.Data = operatorArguments.GetInput "data"
    member __.K = operatorArguments.GetParameter("k", NpiTril.KDefault)

type NpiTrueDivide private (operatorArguments) = 
    inherit SymbolOperator("_npi_true_divide", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiTrueDivide(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new NpiTrueDivide(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type NpiTrueDivideScalar private (operatorArguments) = 
    inherit SymbolOperator("_npi_true_divide_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiTrueDivideScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiTrueDivideScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiTrueDivideScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NpiRtrueDivideScalar private (operatorArguments) = 
    inherit SymbolOperator("_npi_rtrue_divide_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiRtrueDivideScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiRtrueDivideScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NpiRtrueDivideScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NpiUnique private (operatorArguments) = 
    inherit SymbolOperator("_npi_unique", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiUnique(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?returnIndex : bool,
        [<Optional>] ?returnInverse : bool,
        [<Optional>] ?returnCounts : bool,
        [<Optional>] ?axis : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "return_index", returnIndex |> Option.map box |> Parameter
                "return_inverse", returnInverse |> Option.map box |> Parameter
                "return_counts", returnCounts |> Option.map box |> Parameter
                "axis", axis |> Option.map box |> Parameter
            ]
        new NpiUnique(Arguments<Symbol>(operatorArguments))
    static member ReturnIndexDefault : bool = false
    static member ReturnInverseDefault : bool = false
    static member ReturnCountsDefault : bool = false
    static member AxisDefault : int option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.ReturnIndex = operatorArguments.GetParameter("return_index", NpiUnique.ReturnIndexDefault)
    member __.ReturnInverse = operatorArguments.GetParameter("return_inverse", NpiUnique.ReturnInverseDefault)
    member __.ReturnCounts = operatorArguments.GetParameter("return_counts", NpiUnique.ReturnCountsDefault)
    member __.Axis = operatorArguments.GetParameter("axis", NpiUnique.AxisDefault)

type NpiChoice private (operatorArguments) = 
    inherit SymbolOperator("_npi_choice", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiChoice(args)
    new(input1 : Symbol,
        input2 : Symbol,
        a : int64,
        size : int seq,
        [<Optional>] ?replace : bool,
        [<Optional>] ?weighted : bool) = 
        let operatorArguments = 
            [
                "input1", Input input1
                "input2", Input input2
                "a", Parameter(Some(box a))
                "size", Parameter(Some(box size))
                "replace", replace |> Option.map box |> Parameter
                "weighted", weighted |> Option.map box |> Parameter
            ]
        new NpiChoice(Arguments<Symbol>(operatorArguments))
    new(a : int64,
        size : int seq,
        [<Optional>] ?input1 : Symbol,
        [<Optional>] ?input2 : Symbol,
        [<Optional>] ?replace : bool,
        [<Optional>] ?weighted : bool) = 
        let input1 = defaultArg input1 (new ImplicitVariable() :> Symbol)
        let input2 = defaultArg input2 (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "input1", Input input1
                "input2", Input input2
                "a", Parameter(Some(box a))
                "size", Parameter(Some(box size))
                "replace", replace |> Option.map box |> Parameter
                "weighted", weighted |> Option.map box |> Parameter
            ]
        new NpiChoice(Arguments<Symbol>(operatorArguments))
    static member ReplaceDefault : bool = true
    static member WeightedDefault : bool = false
    member __.Input1 = operatorArguments.GetInput "input1"
    member __.Input2 = operatorArguments.GetInput "input2"
    member __.A : int64 = match operatorArguments.GetParameter "a" with Some(v) -> unbox v | None -> failwithf "Required parameter a is missing"
    member __.Size : int seq = match operatorArguments.GetParameter "size" with Some(v) -> unbox v | None -> failwithf "Required parameter size is missing"
    member __.Replace = operatorArguments.GetParameter("replace", NpiChoice.ReplaceDefault)
    member __.Weighted = operatorArguments.GetParameter("weighted", NpiChoice.WeightedDefault)

type NpiMultinomial private (operatorArguments) = 
    inherit SymbolOperator("_npi_multinomial", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiMultinomial(args)
    new(a : Symbol,
        n : int,
        pvals : double seq,
        [<Optional>] ?size : int seq) = 
        let operatorArguments = 
            [
                "a", Input a
                "n", Parameter(Some(box n))
                "pvals", Parameter(Some(box pvals))
                "size", size |> Option.map box |> Parameter
            ]
        new NpiMultinomial(Arguments<Symbol>(operatorArguments))
    new(n : int,
        pvals : double seq,
        [<Optional>] ?a : Symbol,
        [<Optional>] ?size : int seq) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "n", Parameter(Some(box n))
                "pvals", Parameter(Some(box pvals))
                "size", size |> Option.map box |> Parameter
            ]
        new NpiMultinomial(Arguments<Symbol>(operatorArguments))
    static member SizeDefault : int [] option = None
    member __.A = operatorArguments.GetInput "a"
    member __.N : int = match operatorArguments.GetParameter "n" with Some(v) -> unbox v | None -> failwithf "Required parameter n is missing"
    member __.Pvals : double seq = match operatorArguments.GetParameter "pvals" with Some(v) -> unbox v | None -> failwithf "Required parameter pvals is missing"
    member __.Size = operatorArguments.GetParameter("size", NpiMultinomial.SizeDefault)

type NpiNormal private (operatorArguments) = 
    inherit SymbolOperator("_npi_normal", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiNormal(args)
    new([<Optional>] ?input1 : Symbol,
        [<Optional>] ?input2 : Symbol,
        [<Optional>] ?loc : float,
        [<Optional>] ?scale : float,
        [<Optional>] ?size : int seq,
        [<Optional>] ?dtype : FloatDType) = 
        let input1 = defaultArg input1 (new ImplicitVariable() :> Symbol)
        let input2 = defaultArg input2 (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "input1", Input input1
                "input2", Input input2
                "loc", loc |> Option.map box |> Parameter
                "scale", scale |> Option.map box |> Parameter
                "size", size |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
            ]
        new NpiNormal(Arguments<Symbol>(operatorArguments))
    static member LocDefault : double option = None
    static member ScaleDefault : double option = None
    static member SizeDefault : int [] option = None
    static member DtypeDefault : FloatDType = FloatDType.Float32
    member __.Input1 = operatorArguments.GetInput "input1"
    member __.Input2 = operatorArguments.GetInput "input2"
    member __.Loc = operatorArguments.GetParameter("loc", NpiNormal.LocDefault)
    member __.Scale = operatorArguments.GetParameter("scale", NpiNormal.ScaleDefault)
    member __.Size = operatorArguments.GetParameter("size", NpiNormal.SizeDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", NpiNormal.DtypeDefault)

type NpiUniform private (operatorArguments) = 
    inherit SymbolOperator("_npi_uniform", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NpiUniform(args)
    new([<Optional>] ?input1 : Symbol,
        [<Optional>] ?input2 : Symbol,
        [<Optional>] ?low : float,
        [<Optional>] ?high : float,
        [<Optional>] ?size : int seq,
        [<Optional>] ?dtype : FloatDType) = 
        let input1 = defaultArg input1 (new ImplicitVariable() :> Symbol)
        let input2 = defaultArg input2 (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "input1", Input input1
                "input2", Input input2
                "low", low |> Option.map box |> Parameter
                "high", high |> Option.map box |> Parameter
                "size", size |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
            ]
        new NpiUniform(Arguments<Symbol>(operatorArguments))
    static member LowDefault : double option = None
    static member HighDefault : double option = None
    static member SizeDefault : int [] option = None
    static member DtypeDefault : FloatDType = FloatDType.Float32
    member __.Input1 = operatorArguments.GetInput "input1"
    member __.Input2 = operatorArguments.GetInput "input2"
    member __.Low = operatorArguments.GetParameter("low", NpiUniform.LowDefault)
    member __.High = operatorArguments.GetParameter("high", NpiUniform.HighDefault)
    member __.Size = operatorArguments.GetParameter("size", NpiUniform.SizeDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", NpiUniform.DtypeDefault)

type SignsgdUpdate private (operatorArguments) = 
    inherit SymbolOperator("signsgd_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SignsgdUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        lr : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "lr", Parameter(Some(box lr))
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
            ]
        new SignsgdUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "lr", Parameter(Some(box lr))
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
            ]
        new SignsgdUpdate(Arguments<Symbol>(operatorArguments))
    static member WdDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.Wd = operatorArguments.GetParameter("wd", SignsgdUpdate.WdDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", SignsgdUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", SignsgdUpdate.ClipGradientDefault)

type SignumUpdate private (operatorArguments) = 
    inherit SymbolOperator("signum_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SignumUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        mom : Symbol,
        lr : float,
        [<Optional>] ?momentum : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?wdLh : float) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mom", Input mom
                "lr", Parameter(Some(box lr))
                "momentum", momentum |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "wd_lh", wdLh |> Option.map box |> Parameter
            ]
        new SignumUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?mom : Symbol,
        [<Optional>] ?momentum : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?wdLh : float) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let mom = defaultArg mom (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mom", Input mom
                "lr", Parameter(Some(box lr))
                "momentum", momentum |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "wd_lh", wdLh |> Option.map box |> Parameter
            ]
        new SignumUpdate(Arguments<Symbol>(operatorArguments))
    static member MomentumDefault : double = 0.0
    static member WdDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member WdLhDefault : double = 0.0
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.Mom = operatorArguments.GetInput "mom"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.Momentum = operatorArguments.GetParameter("momentum", SignumUpdate.MomentumDefault)
    member __.Wd = operatorArguments.GetParameter("wd", SignumUpdate.WdDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", SignumUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", SignumUpdate.ClipGradientDefault)
    member __.WdLh = operatorArguments.GetParameter("wd_lh", SignumUpdate.WdLhDefault)

type MultiSgdUpdate private (operatorArguments) = 
    inherit SymbolOperator("multi_sgd_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MultiSgdUpdate(args)
    new(data : Symbol seq,
        lrs : double seq,
        wds : double seq,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?numWeights : int) = 
        let operatorArguments = 
            [
                "data", VarArg("", data |> Seq.toArray)
                "lrs", Parameter(Some(box lrs))
                "wds", Parameter(Some(box wds))
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "num_weights", numWeights |> Option.map box |> Parameter
            ]
        new MultiSgdUpdate(Arguments<Symbol>(operatorArguments))
    new(lrs : double seq,
        wds : double seq,
        [<Optional>] ?data : Symbol seq,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?numWeights : int) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "lrs", Parameter(Some(box lrs))
                "wds", Parameter(Some(box wds))
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "num_weights", numWeights |> Option.map box |> Parameter
            ]
        new MultiSgdUpdate(Arguments<Symbol>(operatorArguments))
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member NumWeightsDefault : int = 1
    member __.Data = operatorArguments.GetVarArg "data"
    member __.Lrs : double seq = match operatorArguments.GetParameter "lrs" with Some(v) -> unbox v | None -> failwithf "Required parameter lrs is missing"
    member __.Wds : double seq = match operatorArguments.GetParameter "wds" with Some(v) -> unbox v | None -> failwithf "Required parameter wds is missing"
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", MultiSgdUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", MultiSgdUpdate.ClipGradientDefault)
    member __.NumWeights = operatorArguments.GetParameter("num_weights", MultiSgdUpdate.NumWeightsDefault)

type MultiSgdMomUpdate private (operatorArguments) = 
    inherit SymbolOperator("multi_sgd_mom_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MultiSgdMomUpdate(args)
    new(data : Symbol seq,
        lrs : double seq,
        wds : double seq,
        [<Optional>] ?momentum : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?numWeights : int) = 
        let operatorArguments = 
            [
                "data", VarArg("", data |> Seq.toArray)
                "lrs", Parameter(Some(box lrs))
                "wds", Parameter(Some(box wds))
                "momentum", momentum |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "num_weights", numWeights |> Option.map box |> Parameter
            ]
        new MultiSgdMomUpdate(Arguments<Symbol>(operatorArguments))
    new(lrs : double seq,
        wds : double seq,
        [<Optional>] ?data : Symbol seq,
        [<Optional>] ?momentum : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?numWeights : int) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "lrs", Parameter(Some(box lrs))
                "wds", Parameter(Some(box wds))
                "momentum", momentum |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "num_weights", numWeights |> Option.map box |> Parameter
            ]
        new MultiSgdMomUpdate(Arguments<Symbol>(operatorArguments))
    static member MomentumDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member NumWeightsDefault : int = 1
    member __.Data = operatorArguments.GetVarArg "data"
    member __.Lrs : double seq = match operatorArguments.GetParameter "lrs" with Some(v) -> unbox v | None -> failwithf "Required parameter lrs is missing"
    member __.Wds : double seq = match operatorArguments.GetParameter "wds" with Some(v) -> unbox v | None -> failwithf "Required parameter wds is missing"
    member __.Momentum = operatorArguments.GetParameter("momentum", MultiSgdMomUpdate.MomentumDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", MultiSgdMomUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", MultiSgdMomUpdate.ClipGradientDefault)
    member __.NumWeights = operatorArguments.GetParameter("num_weights", MultiSgdMomUpdate.NumWeightsDefault)

type MultiMpSgdUpdate private (operatorArguments) = 
    inherit SymbolOperator("multi_mp_sgd_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MultiMpSgdUpdate(args)
    new(data : Symbol seq,
        lrs : double seq,
        wds : double seq,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?numWeights : int) = 
        let operatorArguments = 
            [
                "data", VarArg("", data |> Seq.toArray)
                "lrs", Parameter(Some(box lrs))
                "wds", Parameter(Some(box wds))
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "num_weights", numWeights |> Option.map box |> Parameter
            ]
        new MultiMpSgdUpdate(Arguments<Symbol>(operatorArguments))
    new(lrs : double seq,
        wds : double seq,
        [<Optional>] ?data : Symbol seq,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?numWeights : int) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "lrs", Parameter(Some(box lrs))
                "wds", Parameter(Some(box wds))
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "num_weights", numWeights |> Option.map box |> Parameter
            ]
        new MultiMpSgdUpdate(Arguments<Symbol>(operatorArguments))
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member NumWeightsDefault : int = 1
    member __.Data = operatorArguments.GetVarArg "data"
    member __.Lrs : double seq = match operatorArguments.GetParameter "lrs" with Some(v) -> unbox v | None -> failwithf "Required parameter lrs is missing"
    member __.Wds : double seq = match operatorArguments.GetParameter "wds" with Some(v) -> unbox v | None -> failwithf "Required parameter wds is missing"
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", MultiMpSgdUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", MultiMpSgdUpdate.ClipGradientDefault)
    member __.NumWeights = operatorArguments.GetParameter("num_weights", MultiMpSgdUpdate.NumWeightsDefault)

type MultiMpSgdMomUpdate private (operatorArguments) = 
    inherit SymbolOperator("multi_mp_sgd_mom_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MultiMpSgdMomUpdate(args)
    new(data : Symbol seq,
        lrs : double seq,
        wds : double seq,
        [<Optional>] ?momentum : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?numWeights : int) = 
        let operatorArguments = 
            [
                "data", VarArg("", data |> Seq.toArray)
                "lrs", Parameter(Some(box lrs))
                "wds", Parameter(Some(box wds))
                "momentum", momentum |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "num_weights", numWeights |> Option.map box |> Parameter
            ]
        new MultiMpSgdMomUpdate(Arguments<Symbol>(operatorArguments))
    new(lrs : double seq,
        wds : double seq,
        [<Optional>] ?data : Symbol seq,
        [<Optional>] ?momentum : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?numWeights : int) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "lrs", Parameter(Some(box lrs))
                "wds", Parameter(Some(box wds))
                "momentum", momentum |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "num_weights", numWeights |> Option.map box |> Parameter
            ]
        new MultiMpSgdMomUpdate(Arguments<Symbol>(operatorArguments))
    static member MomentumDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member NumWeightsDefault : int = 1
    member __.Data = operatorArguments.GetVarArg "data"
    member __.Lrs : double seq = match operatorArguments.GetParameter "lrs" with Some(v) -> unbox v | None -> failwithf "Required parameter lrs is missing"
    member __.Wds : double seq = match operatorArguments.GetParameter "wds" with Some(v) -> unbox v | None -> failwithf "Required parameter wds is missing"
    member __.Momentum = operatorArguments.GetParameter("momentum", MultiMpSgdMomUpdate.MomentumDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", MultiMpSgdMomUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", MultiMpSgdMomUpdate.ClipGradientDefault)
    member __.NumWeights = operatorArguments.GetParameter("num_weights", MultiMpSgdMomUpdate.NumWeightsDefault)

type SgdUpdate private (operatorArguments) = 
    inherit SymbolOperator("sgd_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SgdUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        lr : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?lazyUpdate : bool) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "lr", Parameter(Some(box lr))
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "lazy_update", lazyUpdate |> Option.map box |> Parameter
            ]
        new SgdUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?lazyUpdate : bool) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "lr", Parameter(Some(box lr))
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "lazy_update", lazyUpdate |> Option.map box |> Parameter
            ]
        new SgdUpdate(Arguments<Symbol>(operatorArguments))
    static member WdDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member LazyUpdateDefault : bool = true
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.Wd = operatorArguments.GetParameter("wd", SgdUpdate.WdDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", SgdUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", SgdUpdate.ClipGradientDefault)
    member __.LazyUpdate = operatorArguments.GetParameter("lazy_update", SgdUpdate.LazyUpdateDefault)

type SgdMomUpdate private (operatorArguments) = 
    inherit SymbolOperator("sgd_mom_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SgdMomUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        mom : Symbol,
        lr : float,
        [<Optional>] ?momentum : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?lazyUpdate : bool) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mom", Input mom
                "lr", Parameter(Some(box lr))
                "momentum", momentum |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "lazy_update", lazyUpdate |> Option.map box |> Parameter
            ]
        new SgdMomUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?mom : Symbol,
        [<Optional>] ?momentum : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?lazyUpdate : bool) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let mom = defaultArg mom (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mom", Input mom
                "lr", Parameter(Some(box lr))
                "momentum", momentum |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "lazy_update", lazyUpdate |> Option.map box |> Parameter
            ]
        new SgdMomUpdate(Arguments<Symbol>(operatorArguments))
    static member MomentumDefault : double = 0.0
    static member WdDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member LazyUpdateDefault : bool = true
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.Mom = operatorArguments.GetInput "mom"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.Momentum = operatorArguments.GetParameter("momentum", SgdMomUpdate.MomentumDefault)
    member __.Wd = operatorArguments.GetParameter("wd", SgdMomUpdate.WdDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", SgdMomUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", SgdMomUpdate.ClipGradientDefault)
    member __.LazyUpdate = operatorArguments.GetParameter("lazy_update", SgdMomUpdate.LazyUpdateDefault)

type MpSgdUpdate private (operatorArguments) = 
    inherit SymbolOperator("mp_sgd_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MpSgdUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        weight32 : Symbol,
        lr : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?lazyUpdate : bool) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "weight32", Input weight32
                "lr", Parameter(Some(box lr))
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "lazy_update", lazyUpdate |> Option.map box |> Parameter
            ]
        new MpSgdUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?weight32 : Symbol,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?lazyUpdate : bool) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let weight32 = defaultArg weight32 (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "weight32", Input weight32
                "lr", Parameter(Some(box lr))
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "lazy_update", lazyUpdate |> Option.map box |> Parameter
            ]
        new MpSgdUpdate(Arguments<Symbol>(operatorArguments))
    static member WdDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member LazyUpdateDefault : bool = true
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.Weight32 = operatorArguments.GetInput "weight32"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.Wd = operatorArguments.GetParameter("wd", MpSgdUpdate.WdDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", MpSgdUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", MpSgdUpdate.ClipGradientDefault)
    member __.LazyUpdate = operatorArguments.GetParameter("lazy_update", MpSgdUpdate.LazyUpdateDefault)

type MpSgdMomUpdate private (operatorArguments) = 
    inherit SymbolOperator("mp_sgd_mom_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MpSgdMomUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        mom : Symbol,
        weight32 : Symbol,
        lr : float,
        [<Optional>] ?momentum : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?lazyUpdate : bool) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mom", Input mom
                "weight32", Input weight32
                "lr", Parameter(Some(box lr))
                "momentum", momentum |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "lazy_update", lazyUpdate |> Option.map box |> Parameter
            ]
        new MpSgdMomUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?mom : Symbol,
        [<Optional>] ?weight32 : Symbol,
        [<Optional>] ?momentum : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?lazyUpdate : bool) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let mom = defaultArg mom (new ImplicitVariable() :> Symbol)
        let weight32 = defaultArg weight32 (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mom", Input mom
                "weight32", Input weight32
                "lr", Parameter(Some(box lr))
                "momentum", momentum |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "lazy_update", lazyUpdate |> Option.map box |> Parameter
            ]
        new MpSgdMomUpdate(Arguments<Symbol>(operatorArguments))
    static member MomentumDefault : double = 0.0
    static member WdDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member LazyUpdateDefault : bool = true
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.Mom = operatorArguments.GetInput "mom"
    member __.Weight32 = operatorArguments.GetInput "weight32"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.Momentum = operatorArguments.GetParameter("momentum", MpSgdMomUpdate.MomentumDefault)
    member __.Wd = operatorArguments.GetParameter("wd", MpSgdMomUpdate.WdDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", MpSgdMomUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", MpSgdMomUpdate.ClipGradientDefault)
    member __.LazyUpdate = operatorArguments.GetParameter("lazy_update", MpSgdMomUpdate.LazyUpdateDefault)

type FtmlUpdate private (operatorArguments) = 
    inherit SymbolOperator("ftml_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new FtmlUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        d : Symbol,
        v : Symbol,
        z : Symbol,
        lr : float,
        t : int,
        [<Optional>] ?beta1 : float,
        [<Optional>] ?beta2 : float,
        [<Optional>] ?epsilon : double,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGrad : float) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "d", Input d
                "v", Input v
                "z", Input z
                "lr", Parameter(Some(box lr))
                "t", Parameter(Some(box t))
                "beta1", beta1 |> Option.map box |> Parameter
                "beta2", beta2 |> Option.map box |> Parameter
                "epsilon", epsilon |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_grad", clipGrad |> Option.map box |> Parameter
            ]
        new FtmlUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        t : int,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?d : Symbol,
        [<Optional>] ?v : Symbol,
        [<Optional>] ?z : Symbol,
        [<Optional>] ?beta1 : float,
        [<Optional>] ?beta2 : float,
        [<Optional>] ?epsilon : double,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGrad : float) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let d = defaultArg d (new ImplicitVariable() :> Symbol)
        let v = defaultArg v (new ImplicitVariable() :> Symbol)
        let z = defaultArg z (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "d", Input d
                "v", Input v
                "z", Input z
                "lr", Parameter(Some(box lr))
                "t", Parameter(Some(box t))
                "beta1", beta1 |> Option.map box |> Parameter
                "beta2", beta2 |> Option.map box |> Parameter
                "epsilon", epsilon |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_grad", clipGrad |> Option.map box |> Parameter
            ]
        new FtmlUpdate(Arguments<Symbol>(operatorArguments))
    static member Beta1Default : double = 0.600000024
    static member Beta2Default : double = 0.999000013
    static member EpsilonDefault : double = 0.0000000099999999
    static member WdDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradDefault : double = -1.0
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.D = operatorArguments.GetInput "d"
    member __.V = operatorArguments.GetInput "v"
    member __.Z = operatorArguments.GetInput "z"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.T : int = match operatorArguments.GetParameter "t" with Some(v) -> unbox v | None -> failwithf "Required parameter t is missing"
    member __.Beta1 = operatorArguments.GetParameter("beta1", FtmlUpdate.Beta1Default)
    member __.Beta2 = operatorArguments.GetParameter("beta2", FtmlUpdate.Beta2Default)
    member __.Epsilon = operatorArguments.GetParameter("epsilon", FtmlUpdate.EpsilonDefault)
    member __.Wd = operatorArguments.GetParameter("wd", FtmlUpdate.WdDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", FtmlUpdate.RescaleGradDefault)
    member __.ClipGrad = operatorArguments.GetParameter("clip_grad", FtmlUpdate.ClipGradDefault)

type AdamUpdate private (operatorArguments) = 
    inherit SymbolOperator("adam_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new AdamUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        mean : Symbol,
        var : Symbol,
        lr : float,
        [<Optional>] ?beta1 : float,
        [<Optional>] ?beta2 : float,
        [<Optional>] ?epsilon : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?lazyUpdate : bool) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mean", Input mean
                "var", Input var
                "lr", Parameter(Some(box lr))
                "beta1", beta1 |> Option.map box |> Parameter
                "beta2", beta2 |> Option.map box |> Parameter
                "epsilon", epsilon |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "lazy_update", lazyUpdate |> Option.map box |> Parameter
            ]
        new AdamUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?mean : Symbol,
        [<Optional>] ?var : Symbol,
        [<Optional>] ?beta1 : float,
        [<Optional>] ?beta2 : float,
        [<Optional>] ?epsilon : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?lazyUpdate : bool) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let mean = defaultArg mean (new ImplicitVariable() :> Symbol)
        let var = defaultArg var (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mean", Input mean
                "var", Input var
                "lr", Parameter(Some(box lr))
                "beta1", beta1 |> Option.map box |> Parameter
                "beta2", beta2 |> Option.map box |> Parameter
                "epsilon", epsilon |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "lazy_update", lazyUpdate |> Option.map box |> Parameter
            ]
        new AdamUpdate(Arguments<Symbol>(operatorArguments))
    static member Beta1Default : double = 0.899999976
    static member Beta2Default : double = 0.999000013
    static member EpsilonDefault : double = 0.0000000099999999
    static member WdDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member LazyUpdateDefault : bool = true
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.Mean = operatorArguments.GetInput "mean"
    member __.Var = operatorArguments.GetInput "var"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.Beta1 = operatorArguments.GetParameter("beta1", AdamUpdate.Beta1Default)
    member __.Beta2 = operatorArguments.GetParameter("beta2", AdamUpdate.Beta2Default)
    member __.Epsilon = operatorArguments.GetParameter("epsilon", AdamUpdate.EpsilonDefault)
    member __.Wd = operatorArguments.GetParameter("wd", AdamUpdate.WdDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", AdamUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", AdamUpdate.ClipGradientDefault)
    member __.LazyUpdate = operatorArguments.GetParameter("lazy_update", AdamUpdate.LazyUpdateDefault)

type NagMomUpdate private (operatorArguments) = 
    inherit SymbolOperator("nag_mom_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NagMomUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        mom : Symbol,
        lr : float,
        [<Optional>] ?momentum : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mom", Input mom
                "lr", Parameter(Some(box lr))
                "momentum", momentum |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
            ]
        new NagMomUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?mom : Symbol,
        [<Optional>] ?momentum : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let mom = defaultArg mom (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mom", Input mom
                "lr", Parameter(Some(box lr))
                "momentum", momentum |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
            ]
        new NagMomUpdate(Arguments<Symbol>(operatorArguments))
    static member MomentumDefault : double = 0.0
    static member WdDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.Mom = operatorArguments.GetInput "mom"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.Momentum = operatorArguments.GetParameter("momentum", NagMomUpdate.MomentumDefault)
    member __.Wd = operatorArguments.GetParameter("wd", NagMomUpdate.WdDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", NagMomUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", NagMomUpdate.ClipGradientDefault)

type MpNagMomUpdate private (operatorArguments) = 
    inherit SymbolOperator("mp_nag_mom_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MpNagMomUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        mom : Symbol,
        weight32 : Symbol,
        lr : float,
        [<Optional>] ?momentum : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mom", Input mom
                "weight32", Input weight32
                "lr", Parameter(Some(box lr))
                "momentum", momentum |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
            ]
        new MpNagMomUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?mom : Symbol,
        [<Optional>] ?weight32 : Symbol,
        [<Optional>] ?momentum : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let mom = defaultArg mom (new ImplicitVariable() :> Symbol)
        let weight32 = defaultArg weight32 (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "mom", Input mom
                "weight32", Input weight32
                "lr", Parameter(Some(box lr))
                "momentum", momentum |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
            ]
        new MpNagMomUpdate(Arguments<Symbol>(operatorArguments))
    static member MomentumDefault : double = 0.0
    static member WdDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.Mom = operatorArguments.GetInput "mom"
    member __.Weight32 = operatorArguments.GetInput "weight32"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.Momentum = operatorArguments.GetParameter("momentum", MpNagMomUpdate.MomentumDefault)
    member __.Wd = operatorArguments.GetParameter("wd", MpNagMomUpdate.WdDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", MpNagMomUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", MpNagMomUpdate.ClipGradientDefault)

type RmspropUpdate private (operatorArguments) = 
    inherit SymbolOperator("rmsprop_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RmspropUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        n : Symbol,
        lr : float,
        [<Optional>] ?gamma1 : float,
        [<Optional>] ?epsilon : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?clipWeights : float) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "n", Input n
                "lr", Parameter(Some(box lr))
                "gamma1", gamma1 |> Option.map box |> Parameter
                "epsilon", epsilon |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "clip_weights", clipWeights |> Option.map box |> Parameter
            ]
        new RmspropUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?n : Symbol,
        [<Optional>] ?gamma1 : float,
        [<Optional>] ?epsilon : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?clipWeights : float) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let n = defaultArg n (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "n", Input n
                "lr", Parameter(Some(box lr))
                "gamma1", gamma1 |> Option.map box |> Parameter
                "epsilon", epsilon |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "clip_weights", clipWeights |> Option.map box |> Parameter
            ]
        new RmspropUpdate(Arguments<Symbol>(operatorArguments))
    static member Gamma1Default : double = 0.949999988
    static member EpsilonDefault : double = 0.0000000099999999
    static member WdDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member ClipWeightsDefault : double = -1.0
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.N = operatorArguments.GetInput "n"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.Gamma1 = operatorArguments.GetParameter("gamma1", RmspropUpdate.Gamma1Default)
    member __.Epsilon = operatorArguments.GetParameter("epsilon", RmspropUpdate.EpsilonDefault)
    member __.Wd = operatorArguments.GetParameter("wd", RmspropUpdate.WdDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", RmspropUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", RmspropUpdate.ClipGradientDefault)
    member __.ClipWeights = operatorArguments.GetParameter("clip_weights", RmspropUpdate.ClipWeightsDefault)

type RmspropalexUpdate private (operatorArguments) = 
    inherit SymbolOperator("rmspropalex_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RmspropalexUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        n : Symbol,
        g : Symbol,
        delta : Symbol,
        lr : float,
        [<Optional>] ?gamma1 : float,
        [<Optional>] ?gamma2 : float,
        [<Optional>] ?epsilon : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?clipWeights : float) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "n", Input n
                "g", Input g
                "delta", Input delta
                "lr", Parameter(Some(box lr))
                "gamma1", gamma1 |> Option.map box |> Parameter
                "gamma2", gamma2 |> Option.map box |> Parameter
                "epsilon", epsilon |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "clip_weights", clipWeights |> Option.map box |> Parameter
            ]
        new RmspropalexUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?n : Symbol,
        [<Optional>] ?g : Symbol,
        [<Optional>] ?delta : Symbol,
        [<Optional>] ?gamma1 : float,
        [<Optional>] ?gamma2 : float,
        [<Optional>] ?epsilon : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float,
        [<Optional>] ?clipWeights : float) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let n = defaultArg n (new ImplicitVariable() :> Symbol)
        let g = defaultArg g (new ImplicitVariable() :> Symbol)
        let delta = defaultArg delta (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "n", Input n
                "g", Input g
                "delta", Input delta
                "lr", Parameter(Some(box lr))
                "gamma1", gamma1 |> Option.map box |> Parameter
                "gamma2", gamma2 |> Option.map box |> Parameter
                "epsilon", epsilon |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
                "clip_weights", clipWeights |> Option.map box |> Parameter
            ]
        new RmspropalexUpdate(Arguments<Symbol>(operatorArguments))
    static member Gamma1Default : double = 0.949999988
    static member Gamma2Default : double = 0.899999976
    static member EpsilonDefault : double = 0.0000000099999999
    static member WdDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    static member ClipWeightsDefault : double = -1.0
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.N = operatorArguments.GetInput "n"
    member __.G = operatorArguments.GetInput "g"
    member __.Delta = operatorArguments.GetInput "delta"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.Gamma1 = operatorArguments.GetParameter("gamma1", RmspropalexUpdate.Gamma1Default)
    member __.Gamma2 = operatorArguments.GetParameter("gamma2", RmspropalexUpdate.Gamma2Default)
    member __.Epsilon = operatorArguments.GetParameter("epsilon", RmspropalexUpdate.EpsilonDefault)
    member __.Wd = operatorArguments.GetParameter("wd", RmspropalexUpdate.WdDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", RmspropalexUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", RmspropalexUpdate.ClipGradientDefault)
    member __.ClipWeights = operatorArguments.GetParameter("clip_weights", RmspropalexUpdate.ClipWeightsDefault)

type FtrlUpdate private (operatorArguments) = 
    inherit SymbolOperator("ftrl_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new FtrlUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        z : Symbol,
        n : Symbol,
        lr : float,
        [<Optional>] ?lamda1 : float,
        [<Optional>] ?beta : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "z", Input z
                "n", Input n
                "lr", Parameter(Some(box lr))
                "lamda1", lamda1 |> Option.map box |> Parameter
                "beta", beta |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
            ]
        new FtrlUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?z : Symbol,
        [<Optional>] ?n : Symbol,
        [<Optional>] ?lamda1 : float,
        [<Optional>] ?beta : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let z = defaultArg z (new ImplicitVariable() :> Symbol)
        let n = defaultArg n (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "z", Input z
                "n", Input n
                "lr", Parameter(Some(box lr))
                "lamda1", lamda1 |> Option.map box |> Parameter
                "beta", beta |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
            ]
        new FtrlUpdate(Arguments<Symbol>(operatorArguments))
    static member Lamda1Default : double = 0.00999999978
    static member BetaDefault : double = 1.0
    static member WdDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.Z = operatorArguments.GetInput "z"
    member __.N = operatorArguments.GetInput "n"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.Lamda1 = operatorArguments.GetParameter("lamda1", FtrlUpdate.Lamda1Default)
    member __.Beta = operatorArguments.GetParameter("beta", FtrlUpdate.BetaDefault)
    member __.Wd = operatorArguments.GetParameter("wd", FtrlUpdate.WdDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", FtrlUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", FtrlUpdate.ClipGradientDefault)

type SparseAdagradUpdate private (operatorArguments) = 
    inherit SymbolOperator("_sparse_adagrad_update", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SparseAdagradUpdate(args)
    new(weight : Symbol,
        grad : Symbol,
        history : Symbol,
        lr : float,
        [<Optional>] ?epsilon : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float) = 
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "history", Input history
                "lr", Parameter(Some(box lr))
                "epsilon", epsilon |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
            ]
        new SparseAdagradUpdate(Arguments<Symbol>(operatorArguments))
    new(lr : float,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?grad : Symbol,
        [<Optional>] ?history : Symbol,
        [<Optional>] ?epsilon : float,
        [<Optional>] ?wd : float,
        [<Optional>] ?rescaleGrad : float,
        [<Optional>] ?clipGradient : float) = 
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let grad = defaultArg grad (new ImplicitVariable() :> Symbol)
        let history = defaultArg history (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "weight", Input weight
                "grad", Input grad
                "history", Input history
                "lr", Parameter(Some(box lr))
                "epsilon", epsilon |> Option.map box |> Parameter
                "wd", wd |> Option.map box |> Parameter
                "rescale_grad", rescaleGrad |> Option.map box |> Parameter
                "clip_gradient", clipGradient |> Option.map box |> Parameter
            ]
        new SparseAdagradUpdate(Arguments<Symbol>(operatorArguments))
    static member EpsilonDefault : double = 0.000000100000001
    static member WdDefault : double = 0.0
    static member RescaleGradDefault : double = 1.0
    static member ClipGradientDefault : double = -1.0
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Grad = operatorArguments.GetInput "grad"
    member __.History = operatorArguments.GetInput "history"
    member __.Lr : float = match operatorArguments.GetParameter "lr" with Some(v) -> unbox v | None -> failwithf "Required parameter lr is missing"
    member __.Epsilon = operatorArguments.GetParameter("epsilon", SparseAdagradUpdate.EpsilonDefault)
    member __.Wd = operatorArguments.GetParameter("wd", SparseAdagradUpdate.WdDefault)
    member __.RescaleGrad = operatorArguments.GetParameter("rescale_grad", SparseAdagradUpdate.RescaleGradDefault)
    member __.ClipGradient = operatorArguments.GetParameter("clip_gradient", SparseAdagradUpdate.ClipGradientDefault)

type Pad private (operatorArguments) = 
    inherit SymbolOperator("Pad", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Pad(args)
    new(data : Symbol,
        mode : PadMode,
        padWidth : int seq,
        [<Optional>] ?constantValue : double) = 
        let operatorArguments = 
            [
                "data", Input data
                "mode", Parameter(Some(box mode))
                "pad_width", Parameter(Some(box padWidth))
                "constant_value", constantValue |> Option.map box |> Parameter
            ]
        new Pad(Arguments<Symbol>(operatorArguments))
    new(mode : PadMode,
        padWidth : int seq,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?constantValue : double) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "mode", Parameter(Some(box mode))
                "pad_width", Parameter(Some(box padWidth))
                "constant_value", constantValue |> Option.map box |> Parameter
            ]
        new Pad(Arguments<Symbol>(operatorArguments))
    static member ConstantValueDefault : double = 0.0
    member __.Data = operatorArguments.GetInput "data"
    member __.Mode : PadMode = match operatorArguments.GetParameter "mode" with Some(v) -> unbox v | None -> failwithf "Required parameter mode is missing"
    member __.PadWidth : int seq = match operatorArguments.GetParameter "pad_width" with Some(v) -> unbox v | None -> failwithf "Required parameter pad_width is missing"
    member __.ConstantValue = operatorArguments.GetParameter("constant_value", Pad.ConstantValueDefault)

type ContribCalibrateEntropy private (operatorArguments) = 
    inherit SymbolOperator("_contrib_calibrate_entropy", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribCalibrateEntropy(args)
    new([<Optional>] ?hist : Symbol,
        [<Optional>] ?histEdges : Symbol,
        [<Optional>] ?numQuantizedBins : int) = 
        let hist = defaultArg hist (new ImplicitVariable() :> Symbol)
        let histEdges = defaultArg histEdges (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "hist", Input hist
                "hist_edges", Input histEdges
                "num_quantized_bins", numQuantizedBins |> Option.map box |> Parameter
            ]
        new ContribCalibrateEntropy(Arguments<Symbol>(operatorArguments))
    static member NumQuantizedBinsDefault : int = 255
    member __.Hist = operatorArguments.GetInput "hist"
    member __.HistEdges = operatorArguments.GetInput "hist_edges"
    member __.NumQuantizedBins = operatorArguments.GetParameter("num_quantized_bins", ContribCalibrateEntropy.NumQuantizedBinsDefault)

type ContribDequantize private (operatorArguments) = 
    inherit SymbolOperator("_contrib_dequantize", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribDequantize(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?minRange : Symbol,
        [<Optional>] ?maxRange : Symbol,
        [<Optional>] ?outType : ContribDequantizeOutType) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let minRange = defaultArg minRange (new ImplicitVariable() :> Symbol)
        let maxRange = defaultArg maxRange (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "min_range", Input minRange
                "max_range", Input maxRange
                "out_type", outType |> Option.map box |> Parameter
            ]
        new ContribDequantize(Arguments<Symbol>(operatorArguments))
    static member OutTypeDefault : ContribDequantizeOutType = ContribDequantizeOutType.Float32
    member __.Data = operatorArguments.GetInput "data"
    member __.MinRange = operatorArguments.GetInput "min_range"
    member __.MaxRange = operatorArguments.GetInput "max_range"
    member __.OutType = operatorArguments.GetParameter("out_type", ContribDequantize.OutTypeDefault)

type ContribQuantize private (operatorArguments) = 
    inherit SymbolOperator("_contrib_quantize", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribQuantize(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?minRange : Symbol,
        [<Optional>] ?maxRange : Symbol,
        [<Optional>] ?outType : ContribQuantizeOutType) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let minRange = defaultArg minRange (new ImplicitVariable() :> Symbol)
        let maxRange = defaultArg maxRange (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "min_range", Input minRange
                "max_range", Input maxRange
                "out_type", outType |> Option.map box |> Parameter
            ]
        new ContribQuantize(Arguments<Symbol>(operatorArguments))
    static member OutTypeDefault : ContribQuantizeOutType = ContribQuantizeOutType.Uint8
    member __.Data = operatorArguments.GetInput "data"
    member __.MinRange = operatorArguments.GetInput "min_range"
    member __.MaxRange = operatorArguments.GetInput "max_range"
    member __.OutType = operatorArguments.GetParameter("out_type", ContribQuantize.OutTypeDefault)

type ContribQuantizeV2 private (operatorArguments) = 
    inherit SymbolOperator("_contrib_quantize_v2", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribQuantizeV2(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?outType : ContribQuantizeV2OutType,
        [<Optional>] ?minCalibRange : float,
        [<Optional>] ?maxCalibRange : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "out_type", outType |> Option.map box |> Parameter
                "min_calib_range", minCalibRange |> Option.map box |> Parameter
                "max_calib_range", maxCalibRange |> Option.map box |> Parameter
            ]
        new ContribQuantizeV2(Arguments<Symbol>(operatorArguments))
    static member OutTypeDefault : ContribQuantizeV2OutType = ContribQuantizeV2OutType.Int8
    static member MinCalibRangeDefault : double option = None
    static member MaxCalibRangeDefault : double option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.OutType = operatorArguments.GetParameter("out_type", ContribQuantizeV2.OutTypeDefault)
    member __.MinCalibRange = operatorArguments.GetParameter("min_calib_range", ContribQuantizeV2.MinCalibRangeDefault)
    member __.MaxCalibRange = operatorArguments.GetParameter("max_calib_range", ContribQuantizeV2.MaxCalibRangeDefault)

type ContribQuantizedAct private (operatorArguments) = 
    inherit SymbolOperator("_contrib_quantized_act", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribQuantizedAct(args)
    new(data : Symbol,
        minData : Symbol,
        maxData : Symbol,
        actType : ActType) = 
        let operatorArguments = 
            [
                "data", Input data
                "min_data", Input minData
                "max_data", Input maxData
                "act_type", Parameter(Some(box actType))
            ]
        new ContribQuantizedAct(Arguments<Symbol>(operatorArguments))
    new(actType : ActType,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?minData : Symbol,
        [<Optional>] ?maxData : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let minData = defaultArg minData (new ImplicitVariable() :> Symbol)
        let maxData = defaultArg maxData (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "min_data", Input minData
                "max_data", Input maxData
                "act_type", Parameter(Some(box actType))
            ]
        new ContribQuantizedAct(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.MinData = operatorArguments.GetInput "min_data"
    member __.MaxData = operatorArguments.GetInput "max_data"
    member __.ActType : ActType = match operatorArguments.GetParameter "act_type" with Some(v) -> unbox v | None -> failwithf "Required parameter act_type is missing"

type ContribQuantizedBatchNorm private (operatorArguments) = 
    inherit SymbolOperator("_contrib_quantized_batch_norm", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribQuantizedBatchNorm(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?gamma : Symbol,
        [<Optional>] ?beta : Symbol,
        [<Optional>] ?movingMean : Symbol,
        [<Optional>] ?movingVar : Symbol,
        [<Optional>] ?minData : Symbol,
        [<Optional>] ?maxData : Symbol,
        [<Optional>] ?eps : double,
        [<Optional>] ?momentum : float,
        [<Optional>] ?fixGamma : bool,
        [<Optional>] ?useGlobalStats : bool,
        [<Optional>] ?outputMeanVar : bool,
        [<Optional>] ?axis : int,
        [<Optional>] ?cudnnOff : bool,
        [<Optional>] ?minCalibRange : float,
        [<Optional>] ?maxCalibRange : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let gamma = defaultArg gamma (new ImplicitVariable() :> Symbol)
        let beta = defaultArg beta (new ImplicitVariable() :> Symbol)
        let movingMean = defaultArg movingMean (new ImplicitVariable() :> Symbol)
        let movingVar = defaultArg movingVar (new ImplicitVariable() :> Symbol)
        let minData = defaultArg minData (new ImplicitVariable() :> Symbol)
        let maxData = defaultArg maxData (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "gamma", Input gamma
                "beta", Input beta
                "moving_mean", Input movingMean
                "moving_var", Input movingVar
                "min_data", Input minData
                "max_data", Input maxData
                "eps", eps |> Option.map box |> Parameter
                "momentum", momentum |> Option.map box |> Parameter
                "fix_gamma", fixGamma |> Option.map box |> Parameter
                "use_global_stats", useGlobalStats |> Option.map box |> Parameter
                "output_mean_var", outputMeanVar |> Option.map box |> Parameter
                "axis", axis |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
                "min_calib_range", minCalibRange |> Option.map box |> Parameter
                "max_calib_range", maxCalibRange |> Option.map box |> Parameter
            ]
        new ContribQuantizedBatchNorm(Arguments<Symbol>(operatorArguments))
    static member EpsDefault : double = 0.0010000000474975
    static member MomentumDefault : double = 0.899999976
    static member FixGammaDefault : bool = true
    static member UseGlobalStatsDefault : bool = false
    static member OutputMeanVarDefault : bool = false
    static member AxisDefault : int = 1
    static member CudnnOffDefault : bool = false
    static member MinCalibRangeDefault : double option = None
    static member MaxCalibRangeDefault : double option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Gamma = operatorArguments.GetInput "gamma"
    member __.Beta = operatorArguments.GetInput "beta"
    member __.MovingMean = operatorArguments.GetInput "moving_mean"
    member __.MovingVar = operatorArguments.GetInput "moving_var"
    member __.MinData = operatorArguments.GetInput "min_data"
    member __.MaxData = operatorArguments.GetInput "max_data"
    member __.Eps = operatorArguments.GetParameter("eps", ContribQuantizedBatchNorm.EpsDefault)
    member __.Momentum = operatorArguments.GetParameter("momentum", ContribQuantizedBatchNorm.MomentumDefault)
    member __.FixGamma = operatorArguments.GetParameter("fix_gamma", ContribQuantizedBatchNorm.FixGammaDefault)
    member __.UseGlobalStats = operatorArguments.GetParameter("use_global_stats", ContribQuantizedBatchNorm.UseGlobalStatsDefault)
    member __.OutputMeanVar = operatorArguments.GetParameter("output_mean_var", ContribQuantizedBatchNorm.OutputMeanVarDefault)
    member __.Axis = operatorArguments.GetParameter("axis", ContribQuantizedBatchNorm.AxisDefault)
    member __.CudnnOff = operatorArguments.GetParameter("cudnn_off", ContribQuantizedBatchNorm.CudnnOffDefault)
    member __.MinCalibRange = operatorArguments.GetParameter("min_calib_range", ContribQuantizedBatchNorm.MinCalibRangeDefault)
    member __.MaxCalibRange = operatorArguments.GetParameter("max_calib_range", ContribQuantizedBatchNorm.MaxCalibRangeDefault)

type ContribQuantizedConcat private (operatorArguments) = 
    inherit SymbolOperator("_contrib_quantized_concat", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribQuantizedConcat(args)
    new([<Optional>] ?data : Symbol seq,
        [<Optional>] ?dim : int) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("num_args", data)
                "dim", dim |> Option.map box |> Parameter
            ]
        new ContribQuantizedConcat(Arguments<Symbol>(operatorArguments))
    static member DimDefault : int = 1
    member __.Data = operatorArguments.GetVarArg "data"
    member __.Dim = operatorArguments.GetParameter("dim", ContribQuantizedConcat.DimDefault)

type ContribQuantizedConv private (operatorArguments) = 
    inherit SymbolOperator("_contrib_quantized_conv", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribQuantizedConv(args)
    new(data : Symbol,
        weight : Symbol,
        bias : Symbol,
        minData : Symbol,
        maxData : Symbol,
        minWeight : Symbol,
        maxWeight : Symbol,
        minBias : Symbol,
        maxBias : Symbol,
        kernel : int seq,
        numFilter : int,
        [<Optional>] ?stride : int seq,
        [<Optional>] ?dilate : int seq,
        [<Optional>] ?pad : int seq,
        [<Optional>] ?numGroup : int,
        [<Optional>] ?workspace : int64,
        [<Optional>] ?noBias : bool,
        [<Optional>] ?cudnnTune : CudnnTune,
        [<Optional>] ?cudnnOff : bool,
        [<Optional>] ?layout : ContribQuantizedConvLayout) = 
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "bias", Input bias
                "min_data", Input minData
                "max_data", Input maxData
                "min_weight", Input minWeight
                "max_weight", Input maxWeight
                "min_bias", Input minBias
                "max_bias", Input maxBias
                "kernel", Parameter(Some(box kernel))
                "num_filter", Parameter(Some(box numFilter))
                "stride", stride |> Option.map box |> Parameter
                "dilate", dilate |> Option.map box |> Parameter
                "pad", pad |> Option.map box |> Parameter
                "num_group", numGroup |> Option.map box |> Parameter
                "workspace", workspace |> Option.map box |> Parameter
                "no_bias", noBias |> Option.map box |> Parameter
                "cudnn_tune", cudnnTune |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
                "layout", layout |> Option.map box |> Parameter
            ]
        new ContribQuantizedConv(Arguments<Symbol>(operatorArguments))
    new(kernel : int seq,
        numFilter : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?bias : Symbol,
        [<Optional>] ?minData : Symbol,
        [<Optional>] ?maxData : Symbol,
        [<Optional>] ?minWeight : Symbol,
        [<Optional>] ?maxWeight : Symbol,
        [<Optional>] ?minBias : Symbol,
        [<Optional>] ?maxBias : Symbol,
        [<Optional>] ?stride : int seq,
        [<Optional>] ?dilate : int seq,
        [<Optional>] ?pad : int seq,
        [<Optional>] ?numGroup : int,
        [<Optional>] ?workspace : int64,
        [<Optional>] ?noBias : bool,
        [<Optional>] ?cudnnTune : CudnnTune,
        [<Optional>] ?cudnnOff : bool,
        [<Optional>] ?layout : ContribQuantizedConvLayout) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let bias = defaultArg bias (new ImplicitVariable() :> Symbol)
        let minData = defaultArg minData (new ImplicitVariable() :> Symbol)
        let maxData = defaultArg maxData (new ImplicitVariable() :> Symbol)
        let minWeight = defaultArg minWeight (new ImplicitVariable() :> Symbol)
        let maxWeight = defaultArg maxWeight (new ImplicitVariable() :> Symbol)
        let minBias = defaultArg minBias (new ImplicitVariable() :> Symbol)
        let maxBias = defaultArg maxBias (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "bias", Input bias
                "min_data", Input minData
                "max_data", Input maxData
                "min_weight", Input minWeight
                "max_weight", Input maxWeight
                "min_bias", Input minBias
                "max_bias", Input maxBias
                "kernel", Parameter(Some(box kernel))
                "num_filter", Parameter(Some(box numFilter))
                "stride", stride |> Option.map box |> Parameter
                "dilate", dilate |> Option.map box |> Parameter
                "pad", pad |> Option.map box |> Parameter
                "num_group", numGroup |> Option.map box |> Parameter
                "workspace", workspace |> Option.map box |> Parameter
                "no_bias", noBias |> Option.map box |> Parameter
                "cudnn_tune", cudnnTune |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
                "layout", layout |> Option.map box |> Parameter
            ]
        new ContribQuantizedConv(Arguments<Symbol>(operatorArguments))
    static member StrideDefault : int [] = [||]
    static member DilateDefault : int [] = [||]
    static member PadDefault : int [] = [||]
    static member NumGroupDefault : int = 1
    static member WorkspaceDefault : int64 = 1024L
    static member NoBiasDefault : bool = false
    static member CudnnTuneDefault : CudnnTune option = None
    static member CudnnOffDefault : bool = false
    static member LayoutDefault : ContribQuantizedConvLayout option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Bias = operatorArguments.GetInput "bias"
    member __.MinData = operatorArguments.GetInput "min_data"
    member __.MaxData = operatorArguments.GetInput "max_data"
    member __.MinWeight = operatorArguments.GetInput "min_weight"
    member __.MaxWeight = operatorArguments.GetInput "max_weight"
    member __.MinBias = operatorArguments.GetInput "min_bias"
    member __.MaxBias = operatorArguments.GetInput "max_bias"
    member __.Kernel : int seq = match operatorArguments.GetParameter "kernel" with Some(v) -> unbox v | None -> failwithf "Required parameter kernel is missing"
    member __.NumFilter : int = match operatorArguments.GetParameter "num_filter" with Some(v) -> unbox v | None -> failwithf "Required parameter num_filter is missing"
    member __.Stride = operatorArguments.GetParameter("stride", ContribQuantizedConv.StrideDefault)
    member __.Dilate = operatorArguments.GetParameter("dilate", ContribQuantizedConv.DilateDefault)
    member __.Pad = operatorArguments.GetParameter("pad", ContribQuantizedConv.PadDefault)
    member __.NumGroup = operatorArguments.GetParameter("num_group", ContribQuantizedConv.NumGroupDefault)
    member __.Workspace = operatorArguments.GetParameter("workspace", ContribQuantizedConv.WorkspaceDefault)
    member __.NoBias = operatorArguments.GetParameter("no_bias", ContribQuantizedConv.NoBiasDefault)
    member __.CudnnTune = operatorArguments.GetParameter("cudnn_tune", ContribQuantizedConv.CudnnTuneDefault)
    member __.CudnnOff = operatorArguments.GetParameter("cudnn_off", ContribQuantizedConv.CudnnOffDefault)
    member __.Layout = operatorArguments.GetParameter("layout", ContribQuantizedConv.LayoutDefault)

type ContribQuantizedElemwiseAdd private (operatorArguments) = 
    inherit SymbolOperator("_contrib_quantized_elemwise_add", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribQuantizedElemwiseAdd(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol,
        [<Optional>] ?lhsMin : Symbol,
        [<Optional>] ?lhsMax : Symbol,
        [<Optional>] ?rhsMin : Symbol,
        [<Optional>] ?rhsMax : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let lhsMin = defaultArg lhsMin (new ImplicitVariable() :> Symbol)
        let lhsMax = defaultArg lhsMax (new ImplicitVariable() :> Symbol)
        let rhsMin = defaultArg rhsMin (new ImplicitVariable() :> Symbol)
        let rhsMax = defaultArg rhsMax (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
                "lhs_min", Input lhsMin
                "lhs_max", Input lhsMax
                "rhs_min", Input rhsMin
                "rhs_max", Input rhsMax
            ]
        new ContribQuantizedElemwiseAdd(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"
    member __.LhsMin = operatorArguments.GetInput "lhs_min"
    member __.LhsMax = operatorArguments.GetInput "lhs_max"
    member __.RhsMin = operatorArguments.GetInput "rhs_min"
    member __.RhsMax = operatorArguments.GetInput "rhs_max"

type ContribQuantizedFlatten private (operatorArguments) = 
    inherit SymbolOperator("_contrib_quantized_flatten", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribQuantizedFlatten(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?minData : Symbol,
        [<Optional>] ?maxData : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let minData = defaultArg minData (new ImplicitVariable() :> Symbol)
        let maxData = defaultArg maxData (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "min_data", Input minData
                "max_data", Input maxData
            ]
        new ContribQuantizedFlatten(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.MinData = operatorArguments.GetInput "min_data"
    member __.MaxData = operatorArguments.GetInput "max_data"

type Flatten private (operatorArguments) = 
    inherit SymbolOperator("Flatten", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Flatten(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Flatten(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type ContribQuantizedFullyConnected private (operatorArguments) = 
    inherit SymbolOperator("_contrib_quantized_fully_connected", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribQuantizedFullyConnected(args)
    new(data : Symbol,
        weight : Symbol,
        bias : Symbol,
        minData : Symbol,
        maxData : Symbol,
        minWeight : Symbol,
        maxWeight : Symbol,
        minBias : Symbol,
        maxBias : Symbol,
        numHidden : int,
        [<Optional>] ?noBias : bool,
        [<Optional>] ?flatten : bool) = 
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "bias", Input bias
                "min_data", Input minData
                "max_data", Input maxData
                "min_weight", Input minWeight
                "max_weight", Input maxWeight
                "min_bias", Input minBias
                "max_bias", Input maxBias
                "num_hidden", Parameter(Some(box numHidden))
                "no_bias", noBias |> Option.map box |> Parameter
                "flatten", flatten |> Option.map box |> Parameter
            ]
        new ContribQuantizedFullyConnected(Arguments<Symbol>(operatorArguments))
    new(numHidden : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?bias : Symbol,
        [<Optional>] ?minData : Symbol,
        [<Optional>] ?maxData : Symbol,
        [<Optional>] ?minWeight : Symbol,
        [<Optional>] ?maxWeight : Symbol,
        [<Optional>] ?minBias : Symbol,
        [<Optional>] ?maxBias : Symbol,
        [<Optional>] ?noBias : bool,
        [<Optional>] ?flatten : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let bias = defaultArg bias (new ImplicitVariable() :> Symbol)
        let minData = defaultArg minData (new ImplicitVariable() :> Symbol)
        let maxData = defaultArg maxData (new ImplicitVariable() :> Symbol)
        let minWeight = defaultArg minWeight (new ImplicitVariable() :> Symbol)
        let maxWeight = defaultArg maxWeight (new ImplicitVariable() :> Symbol)
        let minBias = defaultArg minBias (new ImplicitVariable() :> Symbol)
        let maxBias = defaultArg maxBias (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "bias", Input bias
                "min_data", Input minData
                "max_data", Input maxData
                "min_weight", Input minWeight
                "max_weight", Input maxWeight
                "min_bias", Input minBias
                "max_bias", Input maxBias
                "num_hidden", Parameter(Some(box numHidden))
                "no_bias", noBias |> Option.map box |> Parameter
                "flatten", flatten |> Option.map box |> Parameter
            ]
        new ContribQuantizedFullyConnected(Arguments<Symbol>(operatorArguments))
    static member NoBiasDefault : bool = false
    static member FlattenDefault : bool = true
    member __.Data = operatorArguments.GetInput "data"
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Bias = operatorArguments.GetInput "bias"
    member __.MinData = operatorArguments.GetInput "min_data"
    member __.MaxData = operatorArguments.GetInput "max_data"
    member __.MinWeight = operatorArguments.GetInput "min_weight"
    member __.MaxWeight = operatorArguments.GetInput "max_weight"
    member __.MinBias = operatorArguments.GetInput "min_bias"
    member __.MaxBias = operatorArguments.GetInput "max_bias"
    member __.NumHidden : int = match operatorArguments.GetParameter "num_hidden" with Some(v) -> unbox v | None -> failwithf "Required parameter num_hidden is missing"
    member __.NoBias = operatorArguments.GetParameter("no_bias", ContribQuantizedFullyConnected.NoBiasDefault)
    member __.Flatten = operatorArguments.GetParameter("flatten", ContribQuantizedFullyConnected.FlattenDefault)

type ContribQuantizedPooling private (operatorArguments) = 
    inherit SymbolOperator("_contrib_quantized_pooling", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribQuantizedPooling(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?minData : Symbol,
        [<Optional>] ?maxData : Symbol,
        [<Optional>] ?kernel : int seq,
        [<Optional>] ?poolType : PoolType,
        [<Optional>] ?globalPool : bool,
        [<Optional>] ?cudnnOff : bool,
        [<Optional>] ?poolingConvention : PoolingConvention,
        [<Optional>] ?stride : int seq,
        [<Optional>] ?pad : int seq,
        [<Optional>] ?pValue : int,
        [<Optional>] ?countIncludePad : bool,
        [<Optional>] ?layout : ContribQuantizedPoolingLayout) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let minData = defaultArg minData (new ImplicitVariable() :> Symbol)
        let maxData = defaultArg maxData (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "min_data", Input minData
                "max_data", Input maxData
                "kernel", kernel |> Option.map box |> Parameter
                "pool_type", poolType |> Option.map box |> Parameter
                "global_pool", globalPool |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
                "pooling_convention", poolingConvention |> Option.map box |> Parameter
                "stride", stride |> Option.map box |> Parameter
                "pad", pad |> Option.map box |> Parameter
                "p_value", pValue |> Option.map box |> Parameter
                "count_include_pad", countIncludePad |> Option.map box |> Parameter
                "layout", layout |> Option.map box |> Parameter
            ]
        new ContribQuantizedPooling(Arguments<Symbol>(operatorArguments))
    static member KernelDefault : int [] = [||]
    static member PoolTypeDefault : PoolType = PoolType.Max
    static member GlobalPoolDefault : bool = false
    static member CudnnOffDefault : bool = false
    static member PoolingConventionDefault : PoolingConvention = PoolingConvention.Valid
    static member StrideDefault : int [] = [||]
    static member PadDefault : int [] = [||]
    static member PValueDefault : int option = None
    static member CountIncludePadDefault : bool option = None
    static member LayoutDefault : ContribQuantizedPoolingLayout option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.MinData = operatorArguments.GetInput "min_data"
    member __.MaxData = operatorArguments.GetInput "max_data"
    member __.Kernel = operatorArguments.GetParameter("kernel", ContribQuantizedPooling.KernelDefault)
    member __.PoolType = operatorArguments.GetParameter("pool_type", ContribQuantizedPooling.PoolTypeDefault)
    member __.GlobalPool = operatorArguments.GetParameter("global_pool", ContribQuantizedPooling.GlobalPoolDefault)
    member __.CudnnOff = operatorArguments.GetParameter("cudnn_off", ContribQuantizedPooling.CudnnOffDefault)
    member __.PoolingConvention = operatorArguments.GetParameter("pooling_convention", ContribQuantizedPooling.PoolingConventionDefault)
    member __.Stride = operatorArguments.GetParameter("stride", ContribQuantizedPooling.StrideDefault)
    member __.Pad = operatorArguments.GetParameter("pad", ContribQuantizedPooling.PadDefault)
    member __.PValue = operatorArguments.GetParameter("p_value", ContribQuantizedPooling.PValueDefault)
    member __.CountIncludePad = operatorArguments.GetParameter("count_include_pad", ContribQuantizedPooling.CountIncludePadDefault)
    member __.Layout = operatorArguments.GetParameter("layout", ContribQuantizedPooling.LayoutDefault)

type ContribRequantize private (operatorArguments) = 
    inherit SymbolOperator("_contrib_requantize", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribRequantize(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?minRange : Symbol,
        [<Optional>] ?maxRange : Symbol,
        [<Optional>] ?outType : ContribRequantizeOutType,
        [<Optional>] ?minCalibRange : float,
        [<Optional>] ?maxCalibRange : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let minRange = defaultArg minRange (new ImplicitVariable() :> Symbol)
        let maxRange = defaultArg maxRange (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "min_range", Input minRange
                "max_range", Input maxRange
                "out_type", outType |> Option.map box |> Parameter
                "min_calib_range", minCalibRange |> Option.map box |> Parameter
                "max_calib_range", maxCalibRange |> Option.map box |> Parameter
            ]
        new ContribRequantize(Arguments<Symbol>(operatorArguments))
    static member OutTypeDefault : ContribRequantizeOutType = ContribRequantizeOutType.Int8
    static member MinCalibRangeDefault : double option = None
    static member MaxCalibRangeDefault : double option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.MinRange = operatorArguments.GetInput "min_range"
    member __.MaxRange = operatorArguments.GetInput "max_range"
    member __.OutType = operatorArguments.GetParameter("out_type", ContribRequantize.OutTypeDefault)
    member __.MinCalibRange = operatorArguments.GetParameter("min_calib_range", ContribRequantize.MinCalibRangeDefault)
    member __.MaxCalibRange = operatorArguments.GetParameter("max_calib_range", ContribRequantize.MaxCalibRangeDefault)

type SampleUniform private (operatorArguments) = 
    inherit SymbolOperator("_sample_uniform", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SampleUniform(args)
    new([<Optional>] ?low : Symbol,
        [<Optional>] ?high : Symbol,
        [<Optional>] ?shape : int seq,
        [<Optional>] ?dtype : FloatDType) = 
        let low = defaultArg low (new ImplicitVariable() :> Symbol)
        let high = defaultArg high (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "low", Input low
                "high", Input high
                "shape", shape |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
            ]
        new SampleUniform(Arguments<Symbol>(operatorArguments))
    static member ShapeDefault : int [] = [||]
    static member DtypeDefault : FloatDType option = None
    member __.Low = operatorArguments.GetInput "low"
    member __.High = operatorArguments.GetInput "high"
    member __.Shape = operatorArguments.GetParameter("shape", SampleUniform.ShapeDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", SampleUniform.DtypeDefault)

type SampleNormal private (operatorArguments) = 
    inherit SymbolOperator("_sample_normal", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SampleNormal(args)
    new([<Optional>] ?mu : Symbol,
        [<Optional>] ?sigma : Symbol,
        [<Optional>] ?shape : int seq,
        [<Optional>] ?dtype : FloatDType) = 
        let mu = defaultArg mu (new ImplicitVariable() :> Symbol)
        let sigma = defaultArg sigma (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "mu", Input mu
                "sigma", Input sigma
                "shape", shape |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
            ]
        new SampleNormal(Arguments<Symbol>(operatorArguments))
    static member ShapeDefault : int [] = [||]
    static member DtypeDefault : FloatDType option = None
    member __.Mu = operatorArguments.GetInput "mu"
    member __.Sigma = operatorArguments.GetInput "sigma"
    member __.Shape = operatorArguments.GetParameter("shape", SampleNormal.ShapeDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", SampleNormal.DtypeDefault)

type SampleGamma private (operatorArguments) = 
    inherit SymbolOperator("_sample_gamma", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SampleGamma(args)
    new([<Optional>] ?alpha : Symbol,
        [<Optional>] ?beta : Symbol,
        [<Optional>] ?shape : int seq,
        [<Optional>] ?dtype : FloatDType) = 
        let alpha = defaultArg alpha (new ImplicitVariable() :> Symbol)
        let beta = defaultArg beta (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "alpha", Input alpha
                "beta", Input beta
                "shape", shape |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
            ]
        new SampleGamma(Arguments<Symbol>(operatorArguments))
    static member ShapeDefault : int [] = [||]
    static member DtypeDefault : FloatDType option = None
    member __.Alpha = operatorArguments.GetInput "alpha"
    member __.Beta = operatorArguments.GetInput "beta"
    member __.Shape = operatorArguments.GetParameter("shape", SampleGamma.ShapeDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", SampleGamma.DtypeDefault)

type SampleExponential private (operatorArguments) = 
    inherit SymbolOperator("_sample_exponential", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SampleExponential(args)
    new([<Optional>] ?lam : Symbol,
        [<Optional>] ?shape : int seq,
        [<Optional>] ?dtype : FloatDType) = 
        let lam = defaultArg lam (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lam", Input lam
                "shape", shape |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
            ]
        new SampleExponential(Arguments<Symbol>(operatorArguments))
    static member ShapeDefault : int [] = [||]
    static member DtypeDefault : FloatDType option = None
    member __.Lam = operatorArguments.GetInput "lam"
    member __.Shape = operatorArguments.GetParameter("shape", SampleExponential.ShapeDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", SampleExponential.DtypeDefault)

type SamplePoisson private (operatorArguments) = 
    inherit SymbolOperator("_sample_poisson", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SamplePoisson(args)
    new([<Optional>] ?lam : Symbol,
        [<Optional>] ?shape : int seq,
        [<Optional>] ?dtype : FloatDType) = 
        let lam = defaultArg lam (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lam", Input lam
                "shape", shape |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
            ]
        new SamplePoisson(Arguments<Symbol>(operatorArguments))
    static member ShapeDefault : int [] = [||]
    static member DtypeDefault : FloatDType option = None
    member __.Lam = operatorArguments.GetInput "lam"
    member __.Shape = operatorArguments.GetParameter("shape", SamplePoisson.ShapeDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", SamplePoisson.DtypeDefault)

type SampleNegativeBinomial private (operatorArguments) = 
    inherit SymbolOperator("_sample_negative_binomial", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SampleNegativeBinomial(args)
    new([<Optional>] ?k : Symbol,
        [<Optional>] ?p : Symbol,
        [<Optional>] ?shape : int seq,
        [<Optional>] ?dtype : FloatDType) = 
        let k = defaultArg k (new ImplicitVariable() :> Symbol)
        let p = defaultArg p (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "k", Input k
                "p", Input p
                "shape", shape |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
            ]
        new SampleNegativeBinomial(Arguments<Symbol>(operatorArguments))
    static member ShapeDefault : int [] = [||]
    static member DtypeDefault : FloatDType option = None
    member __.K = operatorArguments.GetInput "k"
    member __.P = operatorArguments.GetInput "p"
    member __.Shape = operatorArguments.GetParameter("shape", SampleNegativeBinomial.ShapeDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", SampleNegativeBinomial.DtypeDefault)

type SampleGeneralizedNegativeBinomial private (operatorArguments) = 
    inherit SymbolOperator("_sample_generalized_negative_binomial", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SampleGeneralizedNegativeBinomial(args)
    new([<Optional>] ?mu : Symbol,
        [<Optional>] ?alpha : Symbol,
        [<Optional>] ?shape : int seq,
        [<Optional>] ?dtype : FloatDType) = 
        let mu = defaultArg mu (new ImplicitVariable() :> Symbol)
        let alpha = defaultArg alpha (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "mu", Input mu
                "alpha", Input alpha
                "shape", shape |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
            ]
        new SampleGeneralizedNegativeBinomial(Arguments<Symbol>(operatorArguments))
    static member ShapeDefault : int [] = [||]
    static member DtypeDefault : FloatDType option = None
    member __.Mu = operatorArguments.GetInput "mu"
    member __.Alpha = operatorArguments.GetInput "alpha"
    member __.Shape = operatorArguments.GetParameter("shape", SampleGeneralizedNegativeBinomial.ShapeDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", SampleGeneralizedNegativeBinomial.DtypeDefault)

type RandomPdfUniform private (operatorArguments) = 
    inherit SymbolOperator("_random_pdf_uniform", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RandomPdfUniform(args)
    new([<Optional>] ?sample : Symbol,
        [<Optional>] ?low : Symbol,
        [<Optional>] ?high : Symbol,
        [<Optional>] ?isLog : bool) = 
        let sample = defaultArg sample (new ImplicitVariable() :> Symbol)
        let low = defaultArg low (new ImplicitVariable() :> Symbol)
        let high = defaultArg high (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "sample", Input sample
                "low", Input low
                "high", Input high
                "is_log", isLog |> Option.map box |> Parameter
            ]
        new RandomPdfUniform(Arguments<Symbol>(operatorArguments))
    static member IsLogDefault : bool = false
    member __.Sample = operatorArguments.GetInput "sample"
    member __.Low = operatorArguments.GetInput "low"
    member __.High = operatorArguments.GetInput "high"
    member __.IsLog = operatorArguments.GetParameter("is_log", RandomPdfUniform.IsLogDefault)

type RandomPdfNormal private (operatorArguments) = 
    inherit SymbolOperator("_random_pdf_normal", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RandomPdfNormal(args)
    new([<Optional>] ?sample : Symbol,
        [<Optional>] ?mu : Symbol,
        [<Optional>] ?sigma : Symbol,
        [<Optional>] ?isLog : bool) = 
        let sample = defaultArg sample (new ImplicitVariable() :> Symbol)
        let mu = defaultArg mu (new ImplicitVariable() :> Symbol)
        let sigma = defaultArg sigma (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "sample", Input sample
                "mu", Input mu
                "sigma", Input sigma
                "is_log", isLog |> Option.map box |> Parameter
            ]
        new RandomPdfNormal(Arguments<Symbol>(operatorArguments))
    static member IsLogDefault : bool = false
    member __.Sample = operatorArguments.GetInput "sample"
    member __.Mu = operatorArguments.GetInput "mu"
    member __.Sigma = operatorArguments.GetInput "sigma"
    member __.IsLog = operatorArguments.GetParameter("is_log", RandomPdfNormal.IsLogDefault)

type RandomPdfGamma private (operatorArguments) = 
    inherit SymbolOperator("_random_pdf_gamma", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RandomPdfGamma(args)
    new([<Optional>] ?sample : Symbol,
        [<Optional>] ?alpha : Symbol,
        [<Optional>] ?beta : Symbol,
        [<Optional>] ?isLog : bool) = 
        let sample = defaultArg sample (new ImplicitVariable() :> Symbol)
        let alpha = defaultArg alpha (new ImplicitVariable() :> Symbol)
        let beta = defaultArg beta (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "sample", Input sample
                "alpha", Input alpha
                "beta", Input beta
                "is_log", isLog |> Option.map box |> Parameter
            ]
        new RandomPdfGamma(Arguments<Symbol>(operatorArguments))
    static member IsLogDefault : bool = false
    member __.Sample = operatorArguments.GetInput "sample"
    member __.Alpha = operatorArguments.GetInput "alpha"
    member __.Beta = operatorArguments.GetInput "beta"
    member __.IsLog = operatorArguments.GetParameter("is_log", RandomPdfGamma.IsLogDefault)

type RandomPdfExponential private (operatorArguments) = 
    inherit SymbolOperator("_random_pdf_exponential", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RandomPdfExponential(args)
    new([<Optional>] ?sample : Symbol,
        [<Optional>] ?lam : Symbol,
        [<Optional>] ?isLog : bool) = 
        let sample = defaultArg sample (new ImplicitVariable() :> Symbol)
        let lam = defaultArg lam (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "sample", Input sample
                "lam", Input lam
                "is_log", isLog |> Option.map box |> Parameter
            ]
        new RandomPdfExponential(Arguments<Symbol>(operatorArguments))
    static member IsLogDefault : bool = false
    member __.Sample = operatorArguments.GetInput "sample"
    member __.Lam = operatorArguments.GetInput "lam"
    member __.IsLog = operatorArguments.GetParameter("is_log", RandomPdfExponential.IsLogDefault)

type RandomPdfPoisson private (operatorArguments) = 
    inherit SymbolOperator("_random_pdf_poisson", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RandomPdfPoisson(args)
    new([<Optional>] ?sample : Symbol,
        [<Optional>] ?lam : Symbol,
        [<Optional>] ?isLog : bool) = 
        let sample = defaultArg sample (new ImplicitVariable() :> Symbol)
        let lam = defaultArg lam (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "sample", Input sample
                "lam", Input lam
                "is_log", isLog |> Option.map box |> Parameter
            ]
        new RandomPdfPoisson(Arguments<Symbol>(operatorArguments))
    static member IsLogDefault : bool = false
    member __.Sample = operatorArguments.GetInput "sample"
    member __.Lam = operatorArguments.GetInput "lam"
    member __.IsLog = operatorArguments.GetParameter("is_log", RandomPdfPoisson.IsLogDefault)

type RandomPdfNegativeBinomial private (operatorArguments) = 
    inherit SymbolOperator("_random_pdf_negative_binomial", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RandomPdfNegativeBinomial(args)
    new([<Optional>] ?sample : Symbol,
        [<Optional>] ?k : Symbol,
        [<Optional>] ?p : Symbol,
        [<Optional>] ?isLog : bool) = 
        let sample = defaultArg sample (new ImplicitVariable() :> Symbol)
        let k = defaultArg k (new ImplicitVariable() :> Symbol)
        let p = defaultArg p (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "sample", Input sample
                "k", Input k
                "p", Input p
                "is_log", isLog |> Option.map box |> Parameter
            ]
        new RandomPdfNegativeBinomial(Arguments<Symbol>(operatorArguments))
    static member IsLogDefault : bool = false
    member __.Sample = operatorArguments.GetInput "sample"
    member __.K = operatorArguments.GetInput "k"
    member __.P = operatorArguments.GetInput "p"
    member __.IsLog = operatorArguments.GetParameter("is_log", RandomPdfNegativeBinomial.IsLogDefault)

type RandomPdfGeneralizedNegativeBinomial private (operatorArguments) = 
    inherit SymbolOperator("_random_pdf_generalized_negative_binomial", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RandomPdfGeneralizedNegativeBinomial(args)
    new([<Optional>] ?sample : Symbol,
        [<Optional>] ?mu : Symbol,
        [<Optional>] ?alpha : Symbol,
        [<Optional>] ?isLog : bool) = 
        let sample = defaultArg sample (new ImplicitVariable() :> Symbol)
        let mu = defaultArg mu (new ImplicitVariable() :> Symbol)
        let alpha = defaultArg alpha (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "sample", Input sample
                "mu", Input mu
                "alpha", Input alpha
                "is_log", isLog |> Option.map box |> Parameter
            ]
        new RandomPdfGeneralizedNegativeBinomial(Arguments<Symbol>(operatorArguments))
    static member IsLogDefault : bool = false
    member __.Sample = operatorArguments.GetInput "sample"
    member __.Mu = operatorArguments.GetInput "mu"
    member __.Alpha = operatorArguments.GetInput "alpha"
    member __.IsLog = operatorArguments.GetParameter("is_log", RandomPdfGeneralizedNegativeBinomial.IsLogDefault)

type RandomPdfDirichlet private (operatorArguments) = 
    inherit SymbolOperator("_random_pdf_dirichlet", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RandomPdfDirichlet(args)
    new([<Optional>] ?sample : Symbol,
        [<Optional>] ?alpha : Symbol,
        [<Optional>] ?isLog : bool) = 
        let sample = defaultArg sample (new ImplicitVariable() :> Symbol)
        let alpha = defaultArg alpha (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "sample", Input sample
                "alpha", Input alpha
                "is_log", isLog |> Option.map box |> Parameter
            ]
        new RandomPdfDirichlet(Arguments<Symbol>(operatorArguments))
    static member IsLogDefault : bool = false
    member __.Sample = operatorArguments.GetInput "sample"
    member __.Alpha = operatorArguments.GetInput "alpha"
    member __.IsLog = operatorArguments.GetParameter("is_log", RandomPdfDirichlet.IsLogDefault)

type SampleMultinomial private (operatorArguments) = 
    inherit SymbolOperator("_sample_multinomial", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SampleMultinomial(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?shape : int seq,
        [<Optional>] ?getProb : bool,
        [<Optional>] ?dtype : SampleMultinomialDtype) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "shape", shape |> Option.map box |> Parameter
                "get_prob", getProb |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
            ]
        new SampleMultinomial(Arguments<Symbol>(operatorArguments))
    static member ShapeDefault : int [] = [||]
    static member GetProbDefault : bool = false
    static member DtypeDefault : SampleMultinomialDtype = SampleMultinomialDtype.Int32
    member __.Data = operatorArguments.GetInput "data"
    member __.Shape = operatorArguments.GetParameter("shape", SampleMultinomial.ShapeDefault)
    member __.GetProb = operatorArguments.GetParameter("get_prob", SampleMultinomial.GetProbDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", SampleMultinomial.DtypeDefault)

type RandomUniformLike private (operatorArguments) = 
    inherit SymbolOperator("_random_uniform_like", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RandomUniformLike(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?low : float,
        [<Optional>] ?high : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "low", low |> Option.map box |> Parameter
                "high", high |> Option.map box |> Parameter
            ]
        new RandomUniformLike(Arguments<Symbol>(operatorArguments))
    static member LowDefault : double = 0.0
    static member HighDefault : double = 1.0
    member __.Data = operatorArguments.GetInput "data"
    member __.Low = operatorArguments.GetParameter("low", RandomUniformLike.LowDefault)
    member __.High = operatorArguments.GetParameter("high", RandomUniformLike.HighDefault)

type RandomNormalLike private (operatorArguments) = 
    inherit SymbolOperator("_random_normal_like", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RandomNormalLike(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?loc : float,
        [<Optional>] ?scale : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "loc", loc |> Option.map box |> Parameter
                "scale", scale |> Option.map box |> Parameter
            ]
        new RandomNormalLike(Arguments<Symbol>(operatorArguments))
    static member LocDefault : double = 0.0
    static member ScaleDefault : double = 1.0
    member __.Data = operatorArguments.GetInput "data"
    member __.Loc = operatorArguments.GetParameter("loc", RandomNormalLike.LocDefault)
    member __.Scale = operatorArguments.GetParameter("scale", RandomNormalLike.ScaleDefault)

type RandomGammaLike private (operatorArguments) = 
    inherit SymbolOperator("_random_gamma_like", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RandomGammaLike(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?alpha : float,
        [<Optional>] ?beta : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "alpha", alpha |> Option.map box |> Parameter
                "beta", beta |> Option.map box |> Parameter
            ]
        new RandomGammaLike(Arguments<Symbol>(operatorArguments))
    static member AlphaDefault : double = 1.0
    static member BetaDefault : double = 1.0
    member __.Data = operatorArguments.GetInput "data"
    member __.Alpha = operatorArguments.GetParameter("alpha", RandomGammaLike.AlphaDefault)
    member __.Beta = operatorArguments.GetParameter("beta", RandomGammaLike.BetaDefault)

type RandomExponentialLike private (operatorArguments) = 
    inherit SymbolOperator("_random_exponential_like", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RandomExponentialLike(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?lam : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "lam", lam |> Option.map box |> Parameter
            ]
        new RandomExponentialLike(Arguments<Symbol>(operatorArguments))
    static member LamDefault : double = 1.0
    member __.Data = operatorArguments.GetInput "data"
    member __.Lam = operatorArguments.GetParameter("lam", RandomExponentialLike.LamDefault)

type RandomPoissonLike private (operatorArguments) = 
    inherit SymbolOperator("_random_poisson_like", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RandomPoissonLike(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?lam : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "lam", lam |> Option.map box |> Parameter
            ]
        new RandomPoissonLike(Arguments<Symbol>(operatorArguments))
    static member LamDefault : double = 1.0
    member __.Data = operatorArguments.GetInput "data"
    member __.Lam = operatorArguments.GetParameter("lam", RandomPoissonLike.LamDefault)

type RandomNegativeBinomialLike private (operatorArguments) = 
    inherit SymbolOperator("_random_negative_binomial_like", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RandomNegativeBinomialLike(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?k : int,
        [<Optional>] ?p : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "k", k |> Option.map box |> Parameter
                "p", p |> Option.map box |> Parameter
            ]
        new RandomNegativeBinomialLike(Arguments<Symbol>(operatorArguments))
    static member KDefault : int = 1
    static member PDefault : double = 1.0
    member __.Data = operatorArguments.GetInput "data"
    member __.K = operatorArguments.GetParameter("k", RandomNegativeBinomialLike.KDefault)
    member __.P = operatorArguments.GetParameter("p", RandomNegativeBinomialLike.PDefault)

type RandomGeneralizedNegativeBinomialLike private (operatorArguments) = 
    inherit SymbolOperator("_random_generalized_negative_binomial_like", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RandomGeneralizedNegativeBinomialLike(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?mu : float,
        [<Optional>] ?alpha : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "mu", mu |> Option.map box |> Parameter
                "alpha", alpha |> Option.map box |> Parameter
            ]
        new RandomGeneralizedNegativeBinomialLike(Arguments<Symbol>(operatorArguments))
    static member MuDefault : double = 1.0
    static member AlphaDefault : double = 1.0
    member __.Data = operatorArguments.GetInput "data"
    member __.Mu = operatorArguments.GetParameter("mu", RandomGeneralizedNegativeBinomialLike.MuDefault)
    member __.Alpha = operatorArguments.GetParameter("alpha", RandomGeneralizedNegativeBinomialLike.AlphaDefault)

type Shuffle private (operatorArguments) = 
    inherit SymbolOperator("_shuffle", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Shuffle(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Shuffle(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type LinearRegressionOutput private (operatorArguments) = 
    inherit SymbolOperator("LinearRegressionOutput", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinearRegressionOutput(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?label : Symbol,
        [<Optional>] ?gradScale : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let label = defaultArg label (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "label", Input label
                "grad_scale", gradScale |> Option.map box |> Parameter
            ]
        new LinearRegressionOutput(Arguments<Symbol>(operatorArguments))
    static member GradScaleDefault : double = 1.0
    member __.Data = operatorArguments.GetInput "data"
    member __.Label = operatorArguments.GetInput "label"
    member __.GradScale = operatorArguments.GetParameter("grad_scale", LinearRegressionOutput.GradScaleDefault)

type MAERegressionOutput private (operatorArguments) = 
    inherit SymbolOperator("MAERegressionOutput", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MAERegressionOutput(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?label : Symbol,
        [<Optional>] ?gradScale : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let label = defaultArg label (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "label", Input label
                "grad_scale", gradScale |> Option.map box |> Parameter
            ]
        new MAERegressionOutput(Arguments<Symbol>(operatorArguments))
    static member GradScaleDefault : double = 1.0
    member __.Data = operatorArguments.GetInput "data"
    member __.Label = operatorArguments.GetInput "label"
    member __.GradScale = operatorArguments.GetParameter("grad_scale", MAERegressionOutput.GradScaleDefault)

type LogisticRegressionOutput private (operatorArguments) = 
    inherit SymbolOperator("LogisticRegressionOutput", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LogisticRegressionOutput(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?label : Symbol,
        [<Optional>] ?gradScale : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let label = defaultArg label (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "label", Input label
                "grad_scale", gradScale |> Option.map box |> Parameter
            ]
        new LogisticRegressionOutput(Arguments<Symbol>(operatorArguments))
    static member GradScaleDefault : double = 1.0
    member __.Data = operatorArguments.GetInput "data"
    member __.Label = operatorArguments.GetInput "label"
    member __.GradScale = operatorArguments.GetParameter("grad_scale", LogisticRegressionOutput.GradScaleDefault)

type RNN private (operatorArguments) = 
    inherit SymbolOperator("RNN", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RNN(args)
    new(data : Symbol,
        parameters : Symbol,
        state : Symbol,
        stateCell : Symbol,
        sequenceLength : Symbol,
        stateSize : int,
        numLayers : int,
        mode : RNNMode,
        [<Optional>] ?bidirectional : bool,
        [<Optional>] ?p : float,
        [<Optional>] ?stateOutputs : bool,
        [<Optional>] ?projectionSize : int,
        [<Optional>] ?lstmStateClipMin : float,
        [<Optional>] ?lstmStateClipMax : float,
        [<Optional>] ?lstmStateClipNan : bool,
        [<Optional>] ?useSequenceLength : bool) = 
        let operatorArguments = 
            [
                "data", Input data
                "parameters", Input parameters
                "state", Input state
                "state_cell", Input stateCell
                "sequence_length", Input sequenceLength
                "state_size", Parameter(Some(box stateSize))
                "num_layers", Parameter(Some(box numLayers))
                "mode", Parameter(Some(box mode))
                "bidirectional", bidirectional |> Option.map box |> Parameter
                "p", p |> Option.map box |> Parameter
                "state_outputs", stateOutputs |> Option.map box |> Parameter
                "projection_size", projectionSize |> Option.map box |> Parameter
                "lstm_state_clip_min", lstmStateClipMin |> Option.map box |> Parameter
                "lstm_state_clip_max", lstmStateClipMax |> Option.map box |> Parameter
                "lstm_state_clip_nan", lstmStateClipNan |> Option.map box |> Parameter
                "use_sequence_length", useSequenceLength |> Option.map box |> Parameter
            ]
        new RNN(Arguments<Symbol>(operatorArguments))
    new(stateSize : int,
        numLayers : int,
        mode : RNNMode,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?parameters : Symbol,
        [<Optional>] ?state : Symbol,
        [<Optional>] ?stateCell : Symbol,
        [<Optional>] ?sequenceLength : Symbol,
        [<Optional>] ?bidirectional : bool,
        [<Optional>] ?p : float,
        [<Optional>] ?stateOutputs : bool,
        [<Optional>] ?projectionSize : int,
        [<Optional>] ?lstmStateClipMin : float,
        [<Optional>] ?lstmStateClipMax : float,
        [<Optional>] ?lstmStateClipNan : bool,
        [<Optional>] ?useSequenceLength : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let parameters = defaultArg parameters (new ImplicitVariable() :> Symbol)
        let state = defaultArg state (new ImplicitVariable() :> Symbol)
        let stateCell = defaultArg stateCell (new ImplicitVariable() :> Symbol)
        let sequenceLength = defaultArg sequenceLength (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "parameters", Input parameters
                "state", Input state
                "state_cell", Input stateCell
                "sequence_length", Input sequenceLength
                "state_size", Parameter(Some(box stateSize))
                "num_layers", Parameter(Some(box numLayers))
                "mode", Parameter(Some(box mode))
                "bidirectional", bidirectional |> Option.map box |> Parameter
                "p", p |> Option.map box |> Parameter
                "state_outputs", stateOutputs |> Option.map box |> Parameter
                "projection_size", projectionSize |> Option.map box |> Parameter
                "lstm_state_clip_min", lstmStateClipMin |> Option.map box |> Parameter
                "lstm_state_clip_max", lstmStateClipMax |> Option.map box |> Parameter
                "lstm_state_clip_nan", lstmStateClipNan |> Option.map box |> Parameter
                "use_sequence_length", useSequenceLength |> Option.map box |> Parameter
            ]
        new RNN(Arguments<Symbol>(operatorArguments))
    static member BidirectionalDefault : bool = false
    static member PDefault : double = 0.0
    static member StateOutputsDefault : bool = false
    static member ProjectionSizeDefault : int option = None
    static member LstmStateClipMinDefault : double option = None
    static member LstmStateClipMaxDefault : double option = None
    static member LstmStateClipNanDefault : bool = false
    static member UseSequenceLengthDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Parameters = operatorArguments.GetInput "parameters"
    member __.State = operatorArguments.GetInput "state"
    member __.StateCell = operatorArguments.GetInput "state_cell"
    member __.SequenceLength = operatorArguments.GetInput "sequence_length"
    member __.StateSize : int = match operatorArguments.GetParameter "state_size" with Some(v) -> unbox v | None -> failwithf "Required parameter state_size is missing"
    member __.NumLayers : int = match operatorArguments.GetParameter "num_layers" with Some(v) -> unbox v | None -> failwithf "Required parameter num_layers is missing"
    member __.Mode : RNNMode = match operatorArguments.GetParameter "mode" with Some(v) -> unbox v | None -> failwithf "Required parameter mode is missing"
    member __.Bidirectional = operatorArguments.GetParameter("bidirectional", RNN.BidirectionalDefault)
    member __.P = operatorArguments.GetParameter("p", RNN.PDefault)
    member __.StateOutputs = operatorArguments.GetParameter("state_outputs", RNN.StateOutputsDefault)
    member __.ProjectionSize = operatorArguments.GetParameter("projection_size", RNN.ProjectionSizeDefault)
    member __.LstmStateClipMin = operatorArguments.GetParameter("lstm_state_clip_min", RNN.LstmStateClipMinDefault)
    member __.LstmStateClipMax = operatorArguments.GetParameter("lstm_state_clip_max", RNN.LstmStateClipMaxDefault)
    member __.LstmStateClipNan = operatorArguments.GetParameter("lstm_state_clip_nan", RNN.LstmStateClipNanDefault)
    member __.UseSequenceLength = operatorArguments.GetParameter("use_sequence_length", RNN.UseSequenceLengthDefault)

type ROIPooling private (operatorArguments) = 
    inherit SymbolOperator("ROIPooling", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ROIPooling(args)
    new(data : Symbol,
        rois : Symbol,
        pooledSize : int seq,
        spatialScale : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "rois", Input rois
                "pooled_size", Parameter(Some(box pooledSize))
                "spatial_scale", Parameter(Some(box spatialScale))
            ]
        new ROIPooling(Arguments<Symbol>(operatorArguments))
    new(pooledSize : int seq,
        spatialScale : float,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?rois : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let rois = defaultArg rois (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "rois", Input rois
                "pooled_size", Parameter(Some(box pooledSize))
                "spatial_scale", Parameter(Some(box spatialScale))
            ]
        new ROIPooling(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Rois = operatorArguments.GetInput "rois"
    member __.PooledSize : int seq = match operatorArguments.GetParameter "pooled_size" with Some(v) -> unbox v | None -> failwithf "Required parameter pooled_size is missing"
    member __.SpatialScale : float = match operatorArguments.GetParameter "spatial_scale" with Some(v) -> unbox v | None -> failwithf "Required parameter spatial_scale is missing"

type SequenceMask private (operatorArguments) = 
    inherit SymbolOperator("SequenceMask", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SequenceMask(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?sequenceLength : Symbol,
        [<Optional>] ?useSequenceLength : bool,
        [<Optional>] ?value : float,
        [<Optional>] ?axis : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let sequenceLength = defaultArg sequenceLength (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "sequence_length", Input sequenceLength
                "use_sequence_length", useSequenceLength |> Option.map box |> Parameter
                "value", value |> Option.map box |> Parameter
                "axis", axis |> Option.map box |> Parameter
            ]
        new SequenceMask(Arguments<Symbol>(operatorArguments))
    static member UseSequenceLengthDefault : bool = false
    static member ValueDefault : double = 0.0
    static member AxisDefault : int = 0
    member __.Data = operatorArguments.GetInput "data"
    member __.SequenceLength = operatorArguments.GetInput "sequence_length"
    member __.UseSequenceLength = operatorArguments.GetParameter("use_sequence_length", SequenceMask.UseSequenceLengthDefault)
    member __.Value = operatorArguments.GetParameter("value", SequenceMask.ValueDefault)
    member __.Axis = operatorArguments.GetParameter("axis", SequenceMask.AxisDefault)

type SliceChannel private (operatorArguments) = 
    inherit SymbolOperator("SliceChannel", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SliceChannel(args)
    new(data : Symbol,
        numOutputs : int,
        [<Optional>] ?axis : int,
        [<Optional>] ?squeezeAxis : bool) = 
        let operatorArguments = 
            [
                "data", Input data
                "num_outputs", Parameter(Some(box numOutputs))
                "axis", axis |> Option.map box |> Parameter
                "squeeze_axis", squeezeAxis |> Option.map box |> Parameter
            ]
        new SliceChannel(Arguments<Symbol>(operatorArguments))
    new(numOutputs : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int,
        [<Optional>] ?squeezeAxis : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "num_outputs", Parameter(Some(box numOutputs))
                "axis", axis |> Option.map box |> Parameter
                "squeeze_axis", squeezeAxis |> Option.map box |> Parameter
            ]
        new SliceChannel(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int = 1
    static member SqueezeAxisDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.NumOutputs : int = match operatorArguments.GetParameter "num_outputs" with Some(v) -> unbox v | None -> failwithf "Required parameter num_outputs is missing"
    member __.Axis = operatorArguments.GetParameter("axis", SliceChannel.AxisDefault)
    member __.SqueezeAxis = operatorArguments.GetParameter("squeeze_axis", SliceChannel.SqueezeAxisDefault)

type SoftmaxOutput private (operatorArguments) = 
    inherit SymbolOperator("SoftmaxOutput", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SoftmaxOutput(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?label : Symbol,
        [<Optional>] ?gradScale : float,
        [<Optional>] ?ignoreLabel : float,
        [<Optional>] ?multiOutput : bool,
        [<Optional>] ?useIgnore : bool,
        [<Optional>] ?preserveShape : bool,
        [<Optional>] ?normalization : Normalization,
        [<Optional>] ?outGrad : bool,
        [<Optional>] ?smoothAlpha : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let label = defaultArg label (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "label", Input label
                "grad_scale", gradScale |> Option.map box |> Parameter
                "ignore_label", ignoreLabel |> Option.map box |> Parameter
                "multi_output", multiOutput |> Option.map box |> Parameter
                "use_ignore", useIgnore |> Option.map box |> Parameter
                "preserve_shape", preserveShape |> Option.map box |> Parameter
                "normalization", normalization |> Option.map box |> Parameter
                "out_grad", outGrad |> Option.map box |> Parameter
                "smooth_alpha", smoothAlpha |> Option.map box |> Parameter
            ]
        new SoftmaxOutput(Arguments<Symbol>(operatorArguments))
    static member GradScaleDefault : double = 1.0
    static member IgnoreLabelDefault : double = -1.0
    static member MultiOutputDefault : bool = false
    static member UseIgnoreDefault : bool = false
    static member PreserveShapeDefault : bool = false
    static member NormalizationDefault : Normalization = Normalization.Null
    static member OutGradDefault : bool = false
    static member SmoothAlphaDefault : double = 0.0
    member __.Data = operatorArguments.GetInput "data"
    member __.Label = operatorArguments.GetInput "label"
    member __.GradScale = operatorArguments.GetParameter("grad_scale", SoftmaxOutput.GradScaleDefault)
    member __.IgnoreLabel = operatorArguments.GetParameter("ignore_label", SoftmaxOutput.IgnoreLabelDefault)
    member __.MultiOutput = operatorArguments.GetParameter("multi_output", SoftmaxOutput.MultiOutputDefault)
    member __.UseIgnore = operatorArguments.GetParameter("use_ignore", SoftmaxOutput.UseIgnoreDefault)
    member __.PreserveShape = operatorArguments.GetParameter("preserve_shape", SoftmaxOutput.PreserveShapeDefault)
    member __.Normalization = operatorArguments.GetParameter("normalization", SoftmaxOutput.NormalizationDefault)
    member __.OutGrad = operatorArguments.GetParameter("out_grad", SoftmaxOutput.OutGradDefault)
    member __.SmoothAlpha = operatorArguments.GetParameter("smooth_alpha", SoftmaxOutput.SmoothAlphaDefault)

type SwapAxis private (operatorArguments) = 
    inherit SymbolOperator("SwapAxis", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SwapAxis(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?dim1 : int,
        [<Optional>] ?dim2 : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "dim1", dim1 |> Option.map box |> Parameter
                "dim2", dim2 |> Option.map box |> Parameter
            ]
        new SwapAxis(Arguments<Symbol>(operatorArguments))
    static member Dim1Default : int = 0
    static member Dim2Default : int = 0
    member __.Data = operatorArguments.GetInput "data"
    member __.Dim1 = operatorArguments.GetParameter("dim1", SwapAxis.Dim1Default)
    member __.Dim2 = operatorArguments.GetParameter("dim2", SwapAxis.Dim2Default)

type AmpCast private (operatorArguments) = 
    inherit SymbolOperator("amp_cast", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new AmpCast(args)
    new(data : Symbol,
        dtype : IntOrFloatDType) = 
        let operatorArguments = 
            [
                "data", Input data
                "dtype", Parameter(Some(box dtype))
            ]
        new AmpCast(Arguments<Symbol>(operatorArguments))
    new(dtype : IntOrFloatDType,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "dtype", Parameter(Some(box dtype))
            ]
        new AmpCast(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Dtype : IntOrFloatDType = match operatorArguments.GetParameter "dtype" with Some(v) -> unbox v | None -> failwithf "Required parameter dtype is missing"

type AmpMulticast private (operatorArguments) = 
    inherit SymbolOperator("amp_multicast", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new AmpMulticast(args)
    new(data : Symbol seq,
        numOutputs : int) = 
        let operatorArguments = 
            [
                "data", VarArg("", data |> Seq.toArray)
                "num_outputs", Parameter(Some(box numOutputs))
            ]
        new AmpMulticast(Arguments<Symbol>(operatorArguments))
    new(numOutputs : int,
        [<Optional>] ?data : Symbol seq) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "num_outputs", Parameter(Some(box numOutputs))
            ]
        new AmpMulticast(Arguments<Symbol>(operatorArguments))
    new(numOutputs : int,
        [<ParamArray>] data : Symbol[]) = 
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "num_outputs", Parameter(Some(box numOutputs))
            ]
        new AmpMulticast(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetVarArg "data"
    member __.NumOutputs : int = match operatorArguments.GetParameter "num_outputs" with Some(v) -> unbox v | None -> failwithf "Required parameter num_outputs is missing"

type Max private (operatorArguments) = 
    inherit SymbolOperator("max", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Max(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?keepdims : bool,
        [<Optional>] ?exclude : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
                "exclude", exclude |> Option.map box |> Parameter
            ]
        new Max(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    static member KeepdimsDefault : bool = false
    static member ExcludeDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", Max.AxisDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", Max.KeepdimsDefault)
    member __.Exclude = operatorArguments.GetParameter("exclude", Max.ExcludeDefault)

type Min private (operatorArguments) = 
    inherit SymbolOperator("min", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Min(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?keepdims : bool,
        [<Optional>] ?exclude : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
                "exclude", exclude |> Option.map box |> Parameter
            ]
        new Min(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    static member KeepdimsDefault : bool = false
    static member ExcludeDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", Min.AxisDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", Min.KeepdimsDefault)
    member __.Exclude = operatorArguments.GetParameter("exclude", Min.ExcludeDefault)

type Norm private (operatorArguments) = 
    inherit SymbolOperator("norm", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Norm(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?ord : int,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?outDtype : OutDtype,
        [<Optional>] ?keepdims : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "ord", ord |> Option.map box |> Parameter
                "axis", axis |> Option.map box |> Parameter
                "out_dtype", outDtype |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
            ]
        new Norm(Arguments<Symbol>(operatorArguments))
    static member OrdDefault : int = 2
    static member AxisDefault : int [] option = None
    static member OutDtypeDefault : OutDtype option = None
    static member KeepdimsDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Ord = operatorArguments.GetParameter("ord", Norm.OrdDefault)
    member __.Axis = operatorArguments.GetParameter("axis", Norm.AxisDefault)
    member __.OutDtype = operatorArguments.GetParameter("out_dtype", Norm.OutDtypeDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", Norm.KeepdimsDefault)

type Argmax private (operatorArguments) = 
    inherit SymbolOperator("argmax", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Argmax(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int,
        [<Optional>] ?keepdims : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
            ]
        new Argmax(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int option = None
    static member KeepdimsDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", Argmax.AxisDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", Argmax.KeepdimsDefault)

type Argmin private (operatorArguments) = 
    inherit SymbolOperator("argmin", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Argmin(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int,
        [<Optional>] ?keepdims : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
            ]
        new Argmin(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int option = None
    static member KeepdimsDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", Argmin.AxisDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", Argmin.KeepdimsDefault)

type ArgmaxChannel private (operatorArguments) = 
    inherit SymbolOperator("argmax_channel", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ArgmaxChannel(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new ArgmaxChannel(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Pick private (operatorArguments) = 
    inherit SymbolOperator("pick", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Pick(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?index : Symbol,
        [<Optional>] ?axis : int,
        [<Optional>] ?keepdims : bool,
        [<Optional>] ?mode : PickMode) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let index = defaultArg index (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "index", Input index
                "axis", axis |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
                "mode", mode |> Option.map box |> Parameter
            ]
        new Pick(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int option = Some(-1)
    static member KeepdimsDefault : bool = false
    static member ModeDefault : PickMode = PickMode.Clip
    member __.Data = operatorArguments.GetInput "data"
    member __.Index = operatorArguments.GetInput "index"
    member __.Axis = operatorArguments.GetParameter("axis", Pick.AxisDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", Pick.KeepdimsDefault)
    member __.Mode = operatorArguments.GetParameter("mode", Pick.ModeDefault)

type BroadcastAxis private (operatorArguments) = 
    inherit SymbolOperator("broadcast_axis", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastAxis(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?size : int seq) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "size", size |> Option.map box |> Parameter
            ]
        new BroadcastAxis(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] = [||]
    static member SizeDefault : int [] = [||]
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", BroadcastAxis.AxisDefault)
    member __.Size = operatorArguments.GetParameter("size", BroadcastAxis.SizeDefault)

type BroadcastTo private (operatorArguments) = 
    inherit SymbolOperator("broadcast_to", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastTo(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?shape : int seq) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "shape", shape |> Option.map box |> Parameter
            ]
        new BroadcastTo(Arguments<Symbol>(operatorArguments))
    static member ShapeDefault : int [] = [||]
    member __.Data = operatorArguments.GetInput "data"
    member __.Shape = operatorArguments.GetParameter("shape", BroadcastTo.ShapeDefault)

type BroadcastLike private (operatorArguments) = 
    inherit SymbolOperator("broadcast_like", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastLike(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol,
        [<Optional>] ?lhsAxes : int seq,
        [<Optional>] ?rhsAxes : int seq) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
                "lhs_axes", lhsAxes |> Option.map box |> Parameter
                "rhs_axes", rhsAxes |> Option.map box |> Parameter
            ]
        new BroadcastLike(Arguments<Symbol>(operatorArguments))
    static member LhsAxesDefault : int [] option = None
    static member RhsAxesDefault : int [] option = None
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"
    member __.LhsAxes = operatorArguments.GetParameter("lhs_axes", BroadcastLike.LhsAxesDefault)
    member __.RhsAxes = operatorArguments.GetParameter("rhs_axes", BroadcastLike.RhsAxesDefault)

type Prod private (operatorArguments) = 
    inherit SymbolOperator("prod", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Prod(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?keepdims : bool,
        [<Optional>] ?exclude : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
                "exclude", exclude |> Option.map box |> Parameter
            ]
        new Prod(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    static member KeepdimsDefault : bool = false
    static member ExcludeDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", Prod.AxisDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", Prod.KeepdimsDefault)
    member __.Exclude = operatorArguments.GetParameter("exclude", Prod.ExcludeDefault)

type Nanprod private (operatorArguments) = 
    inherit SymbolOperator("nanprod", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Nanprod(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?keepdims : bool,
        [<Optional>] ?exclude : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
                "exclude", exclude |> Option.map box |> Parameter
            ]
        new Nanprod(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    static member KeepdimsDefault : bool = false
    static member ExcludeDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", Nanprod.AxisDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", Nanprod.KeepdimsDefault)
    member __.Exclude = operatorArguments.GetParameter("exclude", Nanprod.ExcludeDefault)

type Sum private (operatorArguments) = 
    inherit SymbolOperator("sum", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Sum(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?keepdims : bool,
        [<Optional>] ?exclude : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
                "exclude", exclude |> Option.map box |> Parameter
            ]
        new Sum(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    static member KeepdimsDefault : bool = false
    static member ExcludeDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", Sum.AxisDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", Sum.KeepdimsDefault)
    member __.Exclude = operatorArguments.GetParameter("exclude", Sum.ExcludeDefault)

type Mean private (operatorArguments) = 
    inherit SymbolOperator("mean", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Mean(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?keepdims : bool,
        [<Optional>] ?exclude : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
                "exclude", exclude |> Option.map box |> Parameter
            ]
        new Mean(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    static member KeepdimsDefault : bool = false
    static member ExcludeDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", Mean.AxisDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", Mean.KeepdimsDefault)
    member __.Exclude = operatorArguments.GetParameter("exclude", Mean.ExcludeDefault)

type Nansum private (operatorArguments) = 
    inherit SymbolOperator("nansum", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Nansum(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?keepdims : bool,
        [<Optional>] ?exclude : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
                "exclude", exclude |> Option.map box |> Parameter
            ]
        new Nansum(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    static member KeepdimsDefault : bool = false
    static member ExcludeDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", Nansum.AxisDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", Nansum.KeepdimsDefault)
    member __.Exclude = operatorArguments.GetParameter("exclude", Nansum.ExcludeDefault)

type CastStorage private (operatorArguments) = 
    inherit SymbolOperator("cast_storage", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new CastStorage(args)
    new(data : Symbol,
        stype : Stype) = 
        let operatorArguments = 
            [
                "data", Input data
                "stype", Parameter(Some(box stype))
            ]
        new CastStorage(Arguments<Symbol>(operatorArguments))
    new(stype : Stype,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "stype", Parameter(Some(box stype))
            ]
        new CastStorage(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Stype : Stype = match operatorArguments.GetParameter "stype" with Some(v) -> unbox v | None -> failwithf "Required parameter stype is missing"

type Where private (operatorArguments) = 
    inherit SymbolOperator("where", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Where(args)
    new([<Optional>] ?condition : Symbol,
        [<Optional>] ?x : Symbol,
        [<Optional>] ?y : Symbol) = 
        let condition = defaultArg condition (new ImplicitVariable() :> Symbol)
        let x = defaultArg x (new ImplicitVariable() :> Symbol)
        let y = defaultArg y (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "condition", Input condition
                "x", Input x
                "y", Input y
            ]
        new Where(Arguments<Symbol>(operatorArguments))
    member __.Condition = operatorArguments.GetInput "condition"
    member __.X = operatorArguments.GetInput "x"
    member __.Y = operatorArguments.GetInput "y"

type Diag private (operatorArguments) = 
    inherit SymbolOperator("diag", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Diag(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?k : int,
        [<Optional>] ?axis1 : int,
        [<Optional>] ?axis2 : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "k", k |> Option.map box |> Parameter
                "axis1", axis1 |> Option.map box |> Parameter
                "axis2", axis2 |> Option.map box |> Parameter
            ]
        new Diag(Arguments<Symbol>(operatorArguments))
    static member KDefault : int = 0
    static member Axis1Default : int = 0
    static member Axis2Default : int = 1
    member __.Data = operatorArguments.GetInput "data"
    member __.K = operatorArguments.GetParameter("k", Diag.KDefault)
    member __.Axis1 = operatorArguments.GetParameter("axis1", Diag.Axis1Default)
    member __.Axis2 = operatorArguments.GetParameter("axis2", Diag.Axis2Default)

type Dot private (operatorArguments) = 
    inherit SymbolOperator("dot", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Dot(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol,
        [<Optional>] ?transposeA : bool,
        [<Optional>] ?transposeB : bool,
        [<Optional>] ?forwardStype : ForwardStype) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
                "transpose_a", transposeA |> Option.map box |> Parameter
                "transpose_b", transposeB |> Option.map box |> Parameter
                "forward_stype", forwardStype |> Option.map box |> Parameter
            ]
        new Dot(Arguments<Symbol>(operatorArguments))
    static member TransposeADefault : bool = false
    static member TransposeBDefault : bool = false
    static member ForwardStypeDefault : ForwardStype option = None
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"
    member __.TransposeA = operatorArguments.GetParameter("transpose_a", Dot.TransposeADefault)
    member __.TransposeB = operatorArguments.GetParameter("transpose_b", Dot.TransposeBDefault)
    member __.ForwardStype = operatorArguments.GetParameter("forward_stype", Dot.ForwardStypeDefault)

type BatchDot private (operatorArguments) = 
    inherit SymbolOperator("batch_dot", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BatchDot(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol,
        [<Optional>] ?transposeA : bool,
        [<Optional>] ?transposeB : bool,
        [<Optional>] ?forwardStype : ForwardStype) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
                "transpose_a", transposeA |> Option.map box |> Parameter
                "transpose_b", transposeB |> Option.map box |> Parameter
                "forward_stype", forwardStype |> Option.map box |> Parameter
            ]
        new BatchDot(Arguments<Symbol>(operatorArguments))
    static member TransposeADefault : bool = false
    static member TransposeBDefault : bool = false
    static member ForwardStypeDefault : ForwardStype option = None
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"
    member __.TransposeA = operatorArguments.GetParameter("transpose_a", BatchDot.TransposeADefault)
    member __.TransposeB = operatorArguments.GetParameter("transpose_b", BatchDot.TransposeBDefault)
    member __.ForwardStype = operatorArguments.GetParameter("forward_stype", BatchDot.ForwardStypeDefault)

type BroadcastAdd private (operatorArguments) = 
    inherit SymbolOperator("broadcast_add", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastAdd(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastAdd(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastSub private (operatorArguments) = 
    inherit SymbolOperator("broadcast_sub", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastSub(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastSub(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastMul private (operatorArguments) = 
    inherit SymbolOperator("broadcast_mul", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastMul(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastMul(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastDiv private (operatorArguments) = 
    inherit SymbolOperator("broadcast_div", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastDiv(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastDiv(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastMod private (operatorArguments) = 
    inherit SymbolOperator("broadcast_mod", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastMod(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastMod(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastPower private (operatorArguments) = 
    inherit SymbolOperator("broadcast_power", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastPower(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastPower(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastMaximum private (operatorArguments) = 
    inherit SymbolOperator("broadcast_maximum", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastMaximum(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastMaximum(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastMinimum private (operatorArguments) = 
    inherit SymbolOperator("broadcast_minimum", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastMinimum(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastMinimum(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastHypot private (operatorArguments) = 
    inherit SymbolOperator("broadcast_hypot", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastHypot(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastHypot(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastEqual private (operatorArguments) = 
    inherit SymbolOperator("broadcast_equal", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastEqual(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastEqual(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastNotEqual private (operatorArguments) = 
    inherit SymbolOperator("broadcast_not_equal", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastNotEqual(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastNotEqual(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastGreater private (operatorArguments) = 
    inherit SymbolOperator("broadcast_greater", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastGreater(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastGreater(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastGreaterEqual private (operatorArguments) = 
    inherit SymbolOperator("broadcast_greater_equal", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastGreaterEqual(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastGreaterEqual(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastLesser private (operatorArguments) = 
    inherit SymbolOperator("broadcast_lesser", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastLesser(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastLesser(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastLesserEqual private (operatorArguments) = 
    inherit SymbolOperator("broadcast_lesser_equal", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastLesserEqual(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastLesserEqual(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastLogicalAnd private (operatorArguments) = 
    inherit SymbolOperator("broadcast_logical_and", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastLogicalAnd(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastLogicalAnd(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastLogicalOr private (operatorArguments) = 
    inherit SymbolOperator("broadcast_logical_or", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastLogicalOr(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastLogicalOr(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type BroadcastLogicalXor private (operatorArguments) = 
    inherit SymbolOperator("broadcast_logical_xor", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BroadcastLogicalXor(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new BroadcastLogicalXor(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type ElemwiseAdd private (operatorArguments) = 
    inherit SymbolOperator("elemwise_add", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ElemwiseAdd(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new ElemwiseAdd(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type GradAdd private (operatorArguments) = 
    inherit SymbolOperator("_grad_add", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new GradAdd(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new GradAdd(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type ElemwiseSub private (operatorArguments) = 
    inherit SymbolOperator("elemwise_sub", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ElemwiseSub(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new ElemwiseSub(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type ElemwiseMul private (operatorArguments) = 
    inherit SymbolOperator("elemwise_mul", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ElemwiseMul(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new ElemwiseMul(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type ElemwiseDiv private (operatorArguments) = 
    inherit SymbolOperator("elemwise_div", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ElemwiseDiv(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new ElemwiseDiv(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type Mod private (operatorArguments) = 
    inherit SymbolOperator("_mod", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Mod(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new Mod(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type Power private (operatorArguments) = 
    inherit SymbolOperator("_power", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Power(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new Power(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type Maximum private (operatorArguments) = 
    inherit SymbolOperator("_maximum", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Maximum(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new Maximum(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type Minimum private (operatorArguments) = 
    inherit SymbolOperator("_minimum", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Minimum(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new Minimum(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type Hypot private (operatorArguments) = 
    inherit SymbolOperator("_hypot", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Hypot(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new Hypot(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type Equal private (operatorArguments) = 
    inherit SymbolOperator("_equal", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Equal(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new Equal(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type NotEqual private (operatorArguments) = 
    inherit SymbolOperator("_not_equal", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NotEqual(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new NotEqual(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type Greater private (operatorArguments) = 
    inherit SymbolOperator("_greater", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Greater(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new Greater(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type GreaterEqual private (operatorArguments) = 
    inherit SymbolOperator("_greater_equal", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new GreaterEqual(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new GreaterEqual(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type Lesser private (operatorArguments) = 
    inherit SymbolOperator("_lesser", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Lesser(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new Lesser(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type LesserEqual private (operatorArguments) = 
    inherit SymbolOperator("_lesser_equal", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LesserEqual(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new LesserEqual(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type LogicalAnd private (operatorArguments) = 
    inherit SymbolOperator("_logical_and", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LogicalAnd(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new LogicalAnd(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type LogicalOr private (operatorArguments) = 
    inherit SymbolOperator("_logical_or", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LogicalOr(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new LogicalOr(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type LogicalXor private (operatorArguments) = 
    inherit SymbolOperator("_logical_xor", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LogicalXor(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new LogicalXor(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type PlusScalar private (operatorArguments) = 
    inherit SymbolOperator("_plus_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new PlusScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new PlusScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new PlusScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type MinusScalar private (operatorArguments) = 
    inherit SymbolOperator("_minus_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MinusScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new MinusScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new MinusScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type RminusScalar private (operatorArguments) = 
    inherit SymbolOperator("_rminus_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RminusScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new RminusScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new RminusScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type MulScalar private (operatorArguments) = 
    inherit SymbolOperator("_mul_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MulScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new MulScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new MulScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type DivScalar private (operatorArguments) = 
    inherit SymbolOperator("_div_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new DivScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new DivScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new DivScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type RdivScalar private (operatorArguments) = 
    inherit SymbolOperator("_rdiv_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RdivScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new RdivScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new RdivScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type ModScalar private (operatorArguments) = 
    inherit SymbolOperator("_mod_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ModScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new ModScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new ModScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type RmodScalar private (operatorArguments) = 
    inherit SymbolOperator("_rmod_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RmodScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new RmodScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new RmodScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type MaximumScalar private (operatorArguments) = 
    inherit SymbolOperator("_maximum_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MaximumScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new MaximumScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new MaximumScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type MinimumScalar private (operatorArguments) = 
    inherit SymbolOperator("_minimum_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MinimumScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new MinimumScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new MinimumScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type PowerScalar private (operatorArguments) = 
    inherit SymbolOperator("_power_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new PowerScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new PowerScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new PowerScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type RpowerScalar private (operatorArguments) = 
    inherit SymbolOperator("_rpower_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RpowerScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new RpowerScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new RpowerScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type HypotScalar private (operatorArguments) = 
    inherit SymbolOperator("_hypot_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new HypotScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new HypotScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new HypotScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type SmoothL1 private (operatorArguments) = 
    inherit SymbolOperator("smooth_l1", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SmoothL1(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new SmoothL1(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new SmoothL1(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type EqualScalar private (operatorArguments) = 
    inherit SymbolOperator("_equal_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new EqualScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new EqualScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new EqualScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type NotEqualScalar private (operatorArguments) = 
    inherit SymbolOperator("_not_equal_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new NotEqualScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NotEqualScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new NotEqualScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type GreaterScalar private (operatorArguments) = 
    inherit SymbolOperator("_greater_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new GreaterScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new GreaterScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new GreaterScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type GreaterEqualScalar private (operatorArguments) = 
    inherit SymbolOperator("_greater_equal_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new GreaterEqualScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new GreaterEqualScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new GreaterEqualScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type LesserScalar private (operatorArguments) = 
    inherit SymbolOperator("_lesser_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LesserScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new LesserScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new LesserScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type LesserEqualScalar private (operatorArguments) = 
    inherit SymbolOperator("_lesser_equal_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LesserEqualScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new LesserEqualScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new LesserEqualScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type LogicalAndScalar private (operatorArguments) = 
    inherit SymbolOperator("_logical_and_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LogicalAndScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new LogicalAndScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new LogicalAndScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type LogicalOrScalar private (operatorArguments) = 
    inherit SymbolOperator("_logical_or_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LogicalOrScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new LogicalOrScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new LogicalOrScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type LogicalXorScalar private (operatorArguments) = 
    inherit SymbolOperator("_logical_xor_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LogicalXorScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new LogicalXorScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new LogicalXorScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type ScatterElemwiseDiv private (operatorArguments) = 
    inherit SymbolOperator("_scatter_elemwise_div", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ScatterElemwiseDiv(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new ScatterElemwiseDiv(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type ScatterPlusScalar private (operatorArguments) = 
    inherit SymbolOperator("_scatter_plus_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ScatterPlusScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new ScatterPlusScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new ScatterPlusScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type ScatterMinusScalar private (operatorArguments) = 
    inherit SymbolOperator("_scatter_minus_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ScatterMinusScalar(args)
    new(data : Symbol,
        scalar : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new ScatterMinusScalar(Arguments<Symbol>(operatorArguments))
    new(scalar : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "scalar", Parameter(Some(box scalar))
            ]
        new ScatterMinusScalar(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Scalar : float = match operatorArguments.GetParameter "scalar" with Some(v) -> unbox v | None -> failwithf "Required parameter scalar is missing"

type AddN private (operatorArguments) = 
    inherit SymbolOperator("add_n", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new AddN(args)
    new([<Optional>] ?args : Symbol seq) =
        let args = defaultArg (args |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "args", VarArg("num_args", args)
            ]
        new AddN(Arguments<Symbol>(operatorArguments))
    new([<ParamArray>] args : Symbol[]) =
        let operatorArguments = 
            [
                "args", VarArg("num_args", args)
            ]
        new AddN(Arguments<Symbol>(operatorArguments))
    member __.Args = operatorArguments.GetVarArg "args"

type Relu private (operatorArguments) = 
    inherit SymbolOperator("relu", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Relu(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Relu(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Sigmoid private (operatorArguments) = 
    inherit SymbolOperator("sigmoid", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Sigmoid(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Sigmoid(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type HardSigmoid private (operatorArguments) = 
    inherit SymbolOperator("hard_sigmoid", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new HardSigmoid(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?alpha : float,
        [<Optional>] ?beta : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "alpha", alpha |> Option.map box |> Parameter
                "beta", beta |> Option.map box |> Parameter
            ]
        new HardSigmoid(Arguments<Symbol>(operatorArguments))
    static member AlphaDefault : double = 0.200000003
    static member BetaDefault : double = 0.5
    member __.Data = operatorArguments.GetInput "data"
    member __.Alpha = operatorArguments.GetParameter("alpha", HardSigmoid.AlphaDefault)
    member __.Beta = operatorArguments.GetParameter("beta", HardSigmoid.BetaDefault)

type Softsign private (operatorArguments) = 
    inherit SymbolOperator("softsign", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Softsign(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Softsign(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Copy private (operatorArguments) = 
    inherit SymbolOperator("_copy", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Copy(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Copy(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type BlockGrad private (operatorArguments) = 
    inherit SymbolOperator("BlockGrad", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BlockGrad(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new BlockGrad(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type IdentityWithAttrLikeRhs private (operatorArguments) = 
    inherit SymbolOperator("_identity_with_attr_like_rhs", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new IdentityWithAttrLikeRhs(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
            ]
        new IdentityWithAttrLikeRhs(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"

type ReshapeLike private (operatorArguments) = 
    inherit SymbolOperator("reshape_like", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ReshapeLike(args)
    new([<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol,
        [<Optional>] ?lhsBegin : int,
        [<Optional>] ?lhsEnd : int,
        [<Optional>] ?rhsBegin : int,
        [<Optional>] ?rhsEnd : int) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
                "lhs_begin", lhsBegin |> Option.map box |> Parameter
                "lhs_end", lhsEnd |> Option.map box |> Parameter
                "rhs_begin", rhsBegin |> Option.map box |> Parameter
                "rhs_end", rhsEnd |> Option.map box |> Parameter
            ]
        new ReshapeLike(Arguments<Symbol>(operatorArguments))
    static member LhsBeginDefault : int option = None
    static member LhsEndDefault : int option = None
    static member RhsBeginDefault : int option = None
    static member RhsEndDefault : int option = None
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"
    member __.LhsBegin = operatorArguments.GetParameter("lhs_begin", ReshapeLike.LhsBeginDefault)
    member __.LhsEnd = operatorArguments.GetParameter("lhs_end", ReshapeLike.LhsEndDefault)
    member __.RhsBegin = operatorArguments.GetParameter("rhs_begin", ReshapeLike.RhsBeginDefault)
    member __.RhsEnd = operatorArguments.GetParameter("rhs_end", ReshapeLike.RhsEndDefault)

type ShapeArray private (operatorArguments) = 
    inherit SymbolOperator("shape_array", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ShapeArray(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new ShapeArray(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type SizeArray private (operatorArguments) = 
    inherit SymbolOperator("size_array", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SizeArray(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new SizeArray(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Cast private (operatorArguments) = 
    inherit SymbolOperator("Cast", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Cast(args)
    new(data : Symbol,
        dtype : IntOrFloatDType) = 
        let operatorArguments = 
            [
                "data", Input data
                "dtype", Parameter(Some(box dtype))
            ]
        new Cast(Arguments<Symbol>(operatorArguments))
    new(dtype : IntOrFloatDType,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "dtype", Parameter(Some(box dtype))
            ]
        new Cast(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Dtype : IntOrFloatDType = match operatorArguments.GetParameter "dtype" with Some(v) -> unbox v | None -> failwithf "Required parameter dtype is missing"

type Negative private (operatorArguments) = 
    inherit SymbolOperator("negative", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Negative(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Negative(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Abs private (operatorArguments) = 
    inherit SymbolOperator("abs", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Abs(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Abs(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Sign private (operatorArguments) = 
    inherit SymbolOperator("sign", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Sign(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Sign(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Round private (operatorArguments) = 
    inherit SymbolOperator("round", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Round(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Round(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Rint private (operatorArguments) = 
    inherit SymbolOperator("rint", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Rint(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Rint(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Ceil private (operatorArguments) = 
    inherit SymbolOperator("ceil", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Ceil(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Ceil(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Floor private (operatorArguments) = 
    inherit SymbolOperator("floor", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Floor(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Floor(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Trunc private (operatorArguments) = 
    inherit SymbolOperator("trunc", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Trunc(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Trunc(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Fix private (operatorArguments) = 
    inherit SymbolOperator("fix", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Fix(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Fix(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Erf private (operatorArguments) = 
    inherit SymbolOperator("erf", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Erf(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Erf(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Erfinv private (operatorArguments) = 
    inherit SymbolOperator("erfinv", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Erfinv(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Erfinv(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Gamma private (operatorArguments) = 
    inherit SymbolOperator("gamma", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Gamma(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Gamma(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Gammaln private (operatorArguments) = 
    inherit SymbolOperator("gammaln", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Gammaln(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Gammaln(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type LogicalNot private (operatorArguments) = 
    inherit SymbolOperator("logical_not", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LogicalNot(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new LogicalNot(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Exp private (operatorArguments) = 
    inherit SymbolOperator("exp", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Exp(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Exp(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Log private (operatorArguments) = 
    inherit SymbolOperator("log", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Log(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Log(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Log10 private (operatorArguments) = 
    inherit SymbolOperator("log10", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Log10(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Log10(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Log2 private (operatorArguments) = 
    inherit SymbolOperator("log2", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Log2(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Log2(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Log1p private (operatorArguments) = 
    inherit SymbolOperator("log1p", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Log1p(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Log1p(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Expm1 private (operatorArguments) = 
    inherit SymbolOperator("expm1", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Expm1(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Expm1(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Reciprocal private (operatorArguments) = 
    inherit SymbolOperator("reciprocal", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Reciprocal(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Reciprocal(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Square private (operatorArguments) = 
    inherit SymbolOperator("square", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Square(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Square(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Sqrt private (operatorArguments) = 
    inherit SymbolOperator("sqrt", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Sqrt(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Sqrt(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Rsqrt private (operatorArguments) = 
    inherit SymbolOperator("rsqrt", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Rsqrt(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Rsqrt(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Cbrt private (operatorArguments) = 
    inherit SymbolOperator("cbrt", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Cbrt(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Cbrt(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Rcbrt private (operatorArguments) = 
    inherit SymbolOperator("rcbrt", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Rcbrt(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Rcbrt(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Sin private (operatorArguments) = 
    inherit SymbolOperator("sin", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Sin(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Sin(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Cos private (operatorArguments) = 
    inherit SymbolOperator("cos", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Cos(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Cos(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Tan private (operatorArguments) = 
    inherit SymbolOperator("tan", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Tan(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Tan(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Arcsin private (operatorArguments) = 
    inherit SymbolOperator("arcsin", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Arcsin(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Arcsin(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Arccos private (operatorArguments) = 
    inherit SymbolOperator("arccos", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Arccos(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Arccos(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Arctan private (operatorArguments) = 
    inherit SymbolOperator("arctan", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Arctan(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Arctan(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Degrees private (operatorArguments) = 
    inherit SymbolOperator("degrees", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Degrees(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Degrees(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Radians private (operatorArguments) = 
    inherit SymbolOperator("radians", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Radians(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Radians(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Sinh private (operatorArguments) = 
    inherit SymbolOperator("sinh", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Sinh(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Sinh(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Cosh private (operatorArguments) = 
    inherit SymbolOperator("cosh", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Cosh(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Cosh(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Tanh private (operatorArguments) = 
    inherit SymbolOperator("tanh", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Tanh(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Tanh(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Arcsinh private (operatorArguments) = 
    inherit SymbolOperator("arcsinh", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Arcsinh(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Arcsinh(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Arccosh private (operatorArguments) = 
    inherit SymbolOperator("arccosh", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Arccosh(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Arccosh(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Arctanh private (operatorArguments) = 
    inherit SymbolOperator("arctanh", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Arctanh(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new Arctanh(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type Histogram private (operatorArguments) = 
    inherit SymbolOperator("_histogram", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Histogram(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?bins : Symbol,
        [<Optional>] ?binCnt : int,
        [<Optional>] ?range : struct(float*float)) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let bins = defaultArg bins (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "bins", Input bins
                "bin_cnt", binCnt |> Option.map box |> Parameter
                "range", range |> Option.map box |> Parameter
            ]
        new Histogram(Arguments<Symbol>(operatorArguments))
    static member BinCntDefault : int option = None
    static member RangeDefault : int [] option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Bins = operatorArguments.GetInput "bins"
    member __.BinCnt = operatorArguments.GetParameter("bin_cnt", Histogram.BinCntDefault)
    member __.Range = operatorArguments.GetParameter("range", Histogram.RangeDefault)

type Embedding private (operatorArguments) = 
    inherit SymbolOperator("Embedding", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Embedding(args)
    new(data : Symbol,
        weight : Symbol,
        inputDim : int,
        outputDim : int,
        [<Optional>] ?dtype : IntOrFloatDType,
        [<Optional>] ?sparseGrad : bool) = 
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "input_dim", Parameter(Some(box inputDim))
                "output_dim", Parameter(Some(box outputDim))
                "dtype", dtype |> Option.map box |> Parameter
                "sparse_grad", sparseGrad |> Option.map box |> Parameter
            ]
        new Embedding(Arguments<Symbol>(operatorArguments))
    new(inputDim : int,
        outputDim : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?dtype : IntOrFloatDType,
        [<Optional>] ?sparseGrad : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "input_dim", Parameter(Some(box inputDim))
                "output_dim", Parameter(Some(box outputDim))
                "dtype", dtype |> Option.map box |> Parameter
                "sparse_grad", sparseGrad |> Option.map box |> Parameter
            ]
        new Embedding(Arguments<Symbol>(operatorArguments))
    static member DtypeDefault : IntOrFloatDType = IntOrFloatDType.Float32
    static member SparseGradDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Weight = operatorArguments.GetInput "weight"
    member __.InputDim : int = match operatorArguments.GetParameter "input_dim" with Some(v) -> unbox v | None -> failwithf "Required parameter input_dim is missing"
    member __.OutputDim : int = match operatorArguments.GetParameter "output_dim" with Some(v) -> unbox v | None -> failwithf "Required parameter output_dim is missing"
    member __.Dtype = operatorArguments.GetParameter("dtype", Embedding.DtypeDefault)
    member __.SparseGrad = operatorArguments.GetParameter("sparse_grad", Embedding.SparseGradDefault)

type ContribSparseEmbedding private (operatorArguments) = 
    inherit SymbolOperator("_contrib_SparseEmbedding", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribSparseEmbedding(args)
    new(data : Symbol,
        weight : Symbol,
        inputDim : int,
        outputDim : int,
        [<Optional>] ?dtype : IntOrFloatDType,
        [<Optional>] ?sparseGrad : bool) = 
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "input_dim", Parameter(Some(box inputDim))
                "output_dim", Parameter(Some(box outputDim))
                "dtype", dtype |> Option.map box |> Parameter
                "sparse_grad", sparseGrad |> Option.map box |> Parameter
            ]
        new ContribSparseEmbedding(Arguments<Symbol>(operatorArguments))
    new(inputDim : int,
        outputDim : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?dtype : IntOrFloatDType,
        [<Optional>] ?sparseGrad : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "input_dim", Parameter(Some(box inputDim))
                "output_dim", Parameter(Some(box outputDim))
                "dtype", dtype |> Option.map box |> Parameter
                "sparse_grad", sparseGrad |> Option.map box |> Parameter
            ]
        new ContribSparseEmbedding(Arguments<Symbol>(operatorArguments))
    static member DtypeDefault : IntOrFloatDType = IntOrFloatDType.Float32
    static member SparseGradDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Weight = operatorArguments.GetInput "weight"
    member __.InputDim : int = match operatorArguments.GetParameter "input_dim" with Some(v) -> unbox v | None -> failwithf "Required parameter input_dim is missing"
    member __.OutputDim : int = match operatorArguments.GetParameter "output_dim" with Some(v) -> unbox v | None -> failwithf "Required parameter output_dim is missing"
    member __.Dtype = operatorArguments.GetParameter("dtype", ContribSparseEmbedding.DtypeDefault)
    member __.SparseGrad = operatorArguments.GetParameter("sparse_grad", ContribSparseEmbedding.SparseGradDefault)

type Take private (operatorArguments) = 
    inherit SymbolOperator("take", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Take(args)
    new([<Optional>] ?a : Symbol,
        [<Optional>] ?indices : Symbol,
        [<Optional>] ?axis : int,
        [<Optional>] ?mode : TakeMode) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let indices = defaultArg indices (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "indices", Input indices
                "axis", axis |> Option.map box |> Parameter
                "mode", mode |> Option.map box |> Parameter
            ]
        new Take(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int = 0
    static member ModeDefault : TakeMode = TakeMode.Clip
    member __.A = operatorArguments.GetInput "a"
    member __.Indices = operatorArguments.GetInput "indices"
    member __.Axis = operatorArguments.GetParameter("axis", Take.AxisDefault)
    member __.Mode = operatorArguments.GetParameter("mode", Take.ModeDefault)

type BatchTake private (operatorArguments) = 
    inherit SymbolOperator("batch_take", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BatchTake(args)
    new([<Optional>] ?a : Symbol,
        [<Optional>] ?indices : Symbol) = 
        let a = defaultArg a (new ImplicitVariable() :> Symbol)
        let indices = defaultArg indices (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "a", Input a
                "indices", Input indices
            ]
        new BatchTake(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "a"
    member __.Indices = operatorArguments.GetInput "indices"

type OneHot private (operatorArguments) = 
    inherit SymbolOperator("one_hot", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new OneHot(args)
    new(indices : Symbol,
        depth : int,
        [<Optional>] ?onValue : double,
        [<Optional>] ?offValue : double,
        [<Optional>] ?dtype : IntOrFloatDType) = 
        let operatorArguments = 
            [
                "indices", Input indices
                "depth", Parameter(Some(box depth))
                "on_value", onValue |> Option.map box |> Parameter
                "off_value", offValue |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
            ]
        new OneHot(Arguments<Symbol>(operatorArguments))
    new(depth : int,
        [<Optional>] ?indices : Symbol,
        [<Optional>] ?onValue : double,
        [<Optional>] ?offValue : double,
        [<Optional>] ?dtype : IntOrFloatDType) = 
        let indices = defaultArg indices (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "indices", Input indices
                "depth", Parameter(Some(box depth))
                "on_value", onValue |> Option.map box |> Parameter
                "off_value", offValue |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
            ]
        new OneHot(Arguments<Symbol>(operatorArguments))
    static member OnValueDefault : double = 1.0
    static member OffValueDefault : double = 0.0
    static member DtypeDefault : IntOrFloatDType = IntOrFloatDType.Float32
    member __.Indices = operatorArguments.GetInput "indices"
    member __.Depth : int = match operatorArguments.GetParameter "depth" with Some(v) -> unbox v | None -> failwithf "Required parameter depth is missing"
    member __.OnValue = operatorArguments.GetParameter("on_value", OneHot.OnValueDefault)
    member __.OffValue = operatorArguments.GetParameter("off_value", OneHot.OffValueDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", OneHot.DtypeDefault)

type GatherNd private (operatorArguments) = 
    inherit SymbolOperator("gather_nd", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new GatherNd(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?indices : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let indices = defaultArg indices (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "indices", Input indices
            ]
        new GatherNd(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Indices = operatorArguments.GetInput "indices"

type ScatterNd private (operatorArguments) = 
    inherit SymbolOperator("scatter_nd", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ScatterNd(args)
    new(data : Symbol,
        indices : Symbol,
        shape : int seq) = 
        let operatorArguments = 
            [
                "data", Input data
                "indices", Input indices
                "shape", Parameter(Some(box shape))
            ]
        new ScatterNd(Arguments<Symbol>(operatorArguments))
    new(shape : int seq,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?indices : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let indices = defaultArg indices (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "indices", Input indices
                "shape", Parameter(Some(box shape))
            ]
        new ScatterNd(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Indices = operatorArguments.GetInput "indices"
    member __.Shape : int seq = match operatorArguments.GetParameter "shape" with Some(v) -> unbox v | None -> failwithf "Required parameter shape is missing"

type ScatterSetNd private (operatorArguments) = 
    inherit SymbolOperator("_scatter_set_nd", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ScatterSetNd(args)
    new(lhs : Symbol,
        rhs : Symbol,
        indices : Symbol,
        shape : int seq) = 
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
                "indices", Input indices
                "shape", Parameter(Some(box shape))
            ]
        new ScatterSetNd(Arguments<Symbol>(operatorArguments))
    new(shape : int seq,
        [<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol,
        [<Optional>] ?indices : Symbol) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let indices = defaultArg indices (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
                "indices", Input indices
                "shape", Parameter(Some(box shape))
            ]
        new ScatterSetNd(Arguments<Symbol>(operatorArguments))
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"
    member __.Indices = operatorArguments.GetInput "indices"
    member __.Shape : int seq = match operatorArguments.GetParameter "shape" with Some(v) -> unbox v | None -> failwithf "Required parameter shape is missing"

type ContribArangeLike private (operatorArguments) = 
    inherit SymbolOperator("_contrib_arange_like", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribArangeLike(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?start : double,
        [<Optional>] ?step : double,
        [<Optional>] ?repeat : int,
        [<Optional>] ?axis : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "start", start |> Option.map box |> Parameter
                "step", step |> Option.map box |> Parameter
                "repeat", repeat |> Option.map box |> Parameter
                "axis", axis |> Option.map box |> Parameter
            ]
        new ContribArangeLike(Arguments<Symbol>(operatorArguments))
    static member StartDefault : double = 0.0
    static member StepDefault : double = 1.0
    static member RepeatDefault : int = 1
    static member AxisDefault : int option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Start = operatorArguments.GetParameter("start", ContribArangeLike.StartDefault)
    member __.Step = operatorArguments.GetParameter("step", ContribArangeLike.StepDefault)
    member __.Repeat = operatorArguments.GetParameter("repeat", ContribArangeLike.RepeatDefault)
    member __.Axis = operatorArguments.GetParameter("axis", ContribArangeLike.AxisDefault)

type ZerosLike private (operatorArguments) = 
    inherit SymbolOperator("zeros_like", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ZerosLike(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new ZerosLike(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type OnesLike private (operatorArguments) = 
    inherit SymbolOperator("ones_like", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new OnesLike(args)
    new([<Optional>] ?data : Symbol) =
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
            ]
        new OnesLike(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"

type LinalgGemm private (operatorArguments) = 
    inherit SymbolOperator("_linalg_gemm", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgGemm(args)
    new([<Optional>] ?A : Symbol,
        [<Optional>] ?B : Symbol,
        [<Optional>] ?C : Symbol,
        [<Optional>] ?transposeA : bool,
        [<Optional>] ?transposeB : bool,
        [<Optional>] ?alpha : double,
        [<Optional>] ?beta : double,
        [<Optional>] ?axis : int) = 
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let B = defaultArg B (new ImplicitVariable() :> Symbol)
        let C = defaultArg C (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
                "B", Input B
                "C", Input C
                "transpose_a", transposeA |> Option.map box |> Parameter
                "transpose_b", transposeB |> Option.map box |> Parameter
                "alpha", alpha |> Option.map box |> Parameter
                "beta", beta |> Option.map box |> Parameter
                "axis", axis |> Option.map box |> Parameter
            ]
        new LinalgGemm(Arguments<Symbol>(operatorArguments))
    static member TransposeADefault : bool = false
    static member TransposeBDefault : bool = false
    static member AlphaDefault : double = 1.0
    static member BetaDefault : double = 1.0
    static member AxisDefault : int = -2
    member __.A = operatorArguments.GetInput "A"
    member __.B = operatorArguments.GetInput "B"
    member __.C = operatorArguments.GetInput "C"
    member __.TransposeA = operatorArguments.GetParameter("transpose_a", LinalgGemm.TransposeADefault)
    member __.TransposeB = operatorArguments.GetParameter("transpose_b", LinalgGemm.TransposeBDefault)
    member __.Alpha = operatorArguments.GetParameter("alpha", LinalgGemm.AlphaDefault)
    member __.Beta = operatorArguments.GetParameter("beta", LinalgGemm.BetaDefault)
    member __.Axis = operatorArguments.GetParameter("axis", LinalgGemm.AxisDefault)

type LinalgGemm2 private (operatorArguments) = 
    inherit SymbolOperator("_linalg_gemm2", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgGemm2(args)
    new([<Optional>] ?A : Symbol,
        [<Optional>] ?B : Symbol,
        [<Optional>] ?transposeA : bool,
        [<Optional>] ?transposeB : bool,
        [<Optional>] ?alpha : double,
        [<Optional>] ?axis : int) = 
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let B = defaultArg B (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
                "B", Input B
                "transpose_a", transposeA |> Option.map box |> Parameter
                "transpose_b", transposeB |> Option.map box |> Parameter
                "alpha", alpha |> Option.map box |> Parameter
                "axis", axis |> Option.map box |> Parameter
            ]
        new LinalgGemm2(Arguments<Symbol>(operatorArguments))
    static member TransposeADefault : bool = false
    static member TransposeBDefault : bool = false
    static member AlphaDefault : double = 1.0
    static member AxisDefault : int = -2
    member __.A = operatorArguments.GetInput "A"
    member __.B = operatorArguments.GetInput "B"
    member __.TransposeA = operatorArguments.GetParameter("transpose_a", LinalgGemm2.TransposeADefault)
    member __.TransposeB = operatorArguments.GetParameter("transpose_b", LinalgGemm2.TransposeBDefault)
    member __.Alpha = operatorArguments.GetParameter("alpha", LinalgGemm2.AlphaDefault)
    member __.Axis = operatorArguments.GetParameter("axis", LinalgGemm2.AxisDefault)

type LinalgPotrf private (operatorArguments) = 
    inherit SymbolOperator("_linalg_potrf", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgPotrf(args)
    new([<Optional>] ?A : Symbol) =
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
            ]
        new LinalgPotrf(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "A"

type LinalgPotri private (operatorArguments) = 
    inherit SymbolOperator("_linalg_potri", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgPotri(args)
    new([<Optional>] ?A : Symbol) =
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
            ]
        new LinalgPotri(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "A"

type LinalgTrmm private (operatorArguments) = 
    inherit SymbolOperator("_linalg_trmm", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgTrmm(args)
    new([<Optional>] ?A : Symbol,
        [<Optional>] ?B : Symbol,
        [<Optional>] ?transpose : bool,
        [<Optional>] ?rightside : bool,
        [<Optional>] ?lower : bool,
        [<Optional>] ?alpha : double) = 
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let B = defaultArg B (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
                "B", Input B
                "transpose", transpose |> Option.map box |> Parameter
                "rightside", rightside |> Option.map box |> Parameter
                "lower", lower |> Option.map box |> Parameter
                "alpha", alpha |> Option.map box |> Parameter
            ]
        new LinalgTrmm(Arguments<Symbol>(operatorArguments))
    static member TransposeDefault : bool = false
    static member RightsideDefault : bool = false
    static member LowerDefault : bool = true
    static member AlphaDefault : double = 1.0
    member __.A = operatorArguments.GetInput "A"
    member __.B = operatorArguments.GetInput "B"
    member __.Transpose = operatorArguments.GetParameter("transpose", LinalgTrmm.TransposeDefault)
    member __.Rightside = operatorArguments.GetParameter("rightside", LinalgTrmm.RightsideDefault)
    member __.Lower = operatorArguments.GetParameter("lower", LinalgTrmm.LowerDefault)
    member __.Alpha = operatorArguments.GetParameter("alpha", LinalgTrmm.AlphaDefault)

type LinalgTrsm private (operatorArguments) = 
    inherit SymbolOperator("_linalg_trsm", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgTrsm(args)
    new([<Optional>] ?A : Symbol,
        [<Optional>] ?B : Symbol,
        [<Optional>] ?transpose : bool,
        [<Optional>] ?rightside : bool,
        [<Optional>] ?lower : bool,
        [<Optional>] ?alpha : double) = 
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let B = defaultArg B (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
                "B", Input B
                "transpose", transpose |> Option.map box |> Parameter
                "rightside", rightside |> Option.map box |> Parameter
                "lower", lower |> Option.map box |> Parameter
                "alpha", alpha |> Option.map box |> Parameter
            ]
        new LinalgTrsm(Arguments<Symbol>(operatorArguments))
    static member TransposeDefault : bool = false
    static member RightsideDefault : bool = false
    static member LowerDefault : bool = true
    static member AlphaDefault : double = 1.0
    member __.A = operatorArguments.GetInput "A"
    member __.B = operatorArguments.GetInput "B"
    member __.Transpose = operatorArguments.GetParameter("transpose", LinalgTrsm.TransposeDefault)
    member __.Rightside = operatorArguments.GetParameter("rightside", LinalgTrsm.RightsideDefault)
    member __.Lower = operatorArguments.GetParameter("lower", LinalgTrsm.LowerDefault)
    member __.Alpha = operatorArguments.GetParameter("alpha", LinalgTrsm.AlphaDefault)

type LinalgSumlogdiag private (operatorArguments) = 
    inherit SymbolOperator("_linalg_sumlogdiag", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgSumlogdiag(args)
    new([<Optional>] ?A : Symbol) =
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
            ]
        new LinalgSumlogdiag(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "A"

type LinalgExtractdiag private (operatorArguments) = 
    inherit SymbolOperator("_linalg_extractdiag", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgExtractdiag(args)
    new([<Optional>] ?A : Symbol,
        [<Optional>] ?offset : int) = 
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
                "offset", offset |> Option.map box |> Parameter
            ]
        new LinalgExtractdiag(Arguments<Symbol>(operatorArguments))
    static member OffsetDefault : int = 0
    member __.A = operatorArguments.GetInput "A"
    member __.Offset = operatorArguments.GetParameter("offset", LinalgExtractdiag.OffsetDefault)

type LinalgMakediag private (operatorArguments) = 
    inherit SymbolOperator("_linalg_makediag", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgMakediag(args)
    new([<Optional>] ?A : Symbol,
        [<Optional>] ?offset : int) = 
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
                "offset", offset |> Option.map box |> Parameter
            ]
        new LinalgMakediag(Arguments<Symbol>(operatorArguments))
    static member OffsetDefault : int = 0
    member __.A = operatorArguments.GetInput "A"
    member __.Offset = operatorArguments.GetParameter("offset", LinalgMakediag.OffsetDefault)

type LinalgExtracttrian private (operatorArguments) = 
    inherit SymbolOperator("_linalg_extracttrian", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgExtracttrian(args)
    new([<Optional>] ?A : Symbol,
        [<Optional>] ?offset : int,
        [<Optional>] ?lower : bool) = 
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
                "offset", offset |> Option.map box |> Parameter
                "lower", lower |> Option.map box |> Parameter
            ]
        new LinalgExtracttrian(Arguments<Symbol>(operatorArguments))
    static member OffsetDefault : int = 0
    static member LowerDefault : bool = true
    member __.A = operatorArguments.GetInput "A"
    member __.Offset = operatorArguments.GetParameter("offset", LinalgExtracttrian.OffsetDefault)
    member __.Lower = operatorArguments.GetParameter("lower", LinalgExtracttrian.LowerDefault)

type LinalgMaketrian private (operatorArguments) = 
    inherit SymbolOperator("_linalg_maketrian", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgMaketrian(args)
    new([<Optional>] ?A : Symbol,
        [<Optional>] ?offset : int,
        [<Optional>] ?lower : bool) = 
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
                "offset", offset |> Option.map box |> Parameter
                "lower", lower |> Option.map box |> Parameter
            ]
        new LinalgMaketrian(Arguments<Symbol>(operatorArguments))
    static member OffsetDefault : int = 0
    static member LowerDefault : bool = true
    member __.A = operatorArguments.GetInput "A"
    member __.Offset = operatorArguments.GetParameter("offset", LinalgMaketrian.OffsetDefault)
    member __.Lower = operatorArguments.GetParameter("lower", LinalgMaketrian.LowerDefault)

type LinalgSyrk private (operatorArguments) = 
    inherit SymbolOperator("_linalg_syrk", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgSyrk(args)
    new([<Optional>] ?A : Symbol,
        [<Optional>] ?transpose : bool,
        [<Optional>] ?alpha : double) = 
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
                "transpose", transpose |> Option.map box |> Parameter
                "alpha", alpha |> Option.map box |> Parameter
            ]
        new LinalgSyrk(Arguments<Symbol>(operatorArguments))
    static member TransposeDefault : bool = false
    static member AlphaDefault : double = 1.0
    member __.A = operatorArguments.GetInput "A"
    member __.Transpose = operatorArguments.GetParameter("transpose", LinalgSyrk.TransposeDefault)
    member __.Alpha = operatorArguments.GetParameter("alpha", LinalgSyrk.AlphaDefault)

type LinalgGelqf private (operatorArguments) = 
    inherit SymbolOperator("_linalg_gelqf", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgGelqf(args)
    new([<Optional>] ?A : Symbol) =
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
            ]
        new LinalgGelqf(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "A"

type LinalgSyevd private (operatorArguments) = 
    inherit SymbolOperator("_linalg_syevd", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgSyevd(args)
    new([<Optional>] ?A : Symbol) =
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
            ]
        new LinalgSyevd(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "A"

type LinalgInverse private (operatorArguments) = 
    inherit SymbolOperator("_linalg_inverse", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgInverse(args)
    new([<Optional>] ?A : Symbol) =
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
            ]
        new LinalgInverse(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "A"

type LinalgDet private (operatorArguments) = 
    inherit SymbolOperator("_linalg_det", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgDet(args)
    new([<Optional>] ?A : Symbol) =
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
            ]
        new LinalgDet(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "A"

type LinalgSlogdet private (operatorArguments) = 
    inherit SymbolOperator("_linalg_slogdet", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new LinalgSlogdet(args)
    new([<Optional>] ?A : Symbol) =
        let A = defaultArg A (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "A", Input A
            ]
        new LinalgSlogdet(Arguments<Symbol>(operatorArguments))
    member __.A = operatorArguments.GetInput "A"

type Reshape private (operatorArguments) = 
    inherit SymbolOperator("Reshape", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Reshape(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?shape : int seq,
        [<Optional>] ?reverse : bool,
        [<Optional>] ?targetShape : int seq,
        [<Optional>] ?keepHighest : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "shape", shape |> Option.map box |> Parameter
                "reverse", reverse |> Option.map box |> Parameter
                "target_shape", targetShape |> Option.map box |> Parameter
                "keep_highest", keepHighest |> Option.map box |> Parameter
            ]
        new Reshape(Arguments<Symbol>(operatorArguments))
    static member ShapeDefault : int [] = [||]
    static member ReverseDefault : bool = false
    static member TargetShapeDefault : int [] = [||]
    static member KeepHighestDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Shape = operatorArguments.GetParameter("shape", Reshape.ShapeDefault)
    member __.Reverse = operatorArguments.GetParameter("reverse", Reshape.ReverseDefault)
    member __.TargetShape = operatorArguments.GetParameter("target_shape", Reshape.TargetShapeDefault)
    member __.KeepHighest = operatorArguments.GetParameter("keep_highest", Reshape.KeepHighestDefault)

type Transpose private (operatorArguments) = 
    inherit SymbolOperator("transpose", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Transpose(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axes : int seq) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axes", axes |> Option.map box |> Parameter
            ]
        new Transpose(Arguments<Symbol>(operatorArguments))
    static member AxesDefault : int [] = [||]
    member __.Data = operatorArguments.GetInput "data"
    member __.Axes = operatorArguments.GetParameter("axes", Transpose.AxesDefault)

type ExpandDims private (operatorArguments) = 
    inherit SymbolOperator("expand_dims", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ExpandDims(args)
    new(data : Symbol,
        axis : int) = 
        let operatorArguments = 
            [
                "data", Input data
                "axis", Parameter(Some(box axis))
            ]
        new ExpandDims(Arguments<Symbol>(operatorArguments))
    new(axis : int,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", Parameter(Some(box axis))
            ]
        new ExpandDims(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis : int = match operatorArguments.GetParameter "axis" with Some(v) -> unbox v | None -> failwithf "Required parameter axis is missing"

type Slice private (operatorArguments) = 
    inherit SymbolOperator("slice", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Slice(args)
    new(data : Symbol,
        sliceBegin : int seq,
        sliceEnd : int seq,
        [<Optional>] ?step : int seq) = 
        let operatorArguments = 
            [
                "data", Input data
                "begin", Parameter(Some(box sliceBegin))
                "end", Parameter(Some(box sliceEnd))
                "step", step |> Option.map box |> Parameter
            ]
        new Slice(Arguments<Symbol>(operatorArguments))
    new(sliceBegin : int seq,
        sliceEnd : int seq,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?step : int seq) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "begin", Parameter(Some(box sliceBegin))
                "end", Parameter(Some(box sliceEnd))
                "step", step |> Option.map box |> Parameter
            ]
        new Slice(Arguments<Symbol>(operatorArguments))
    static member StepDefault : int [] = [||]
    member __.Data = operatorArguments.GetInput "data"
    member __.SliceBegin : int seq = match operatorArguments.GetParameter "begin" with Some(v) -> unbox v | None -> failwithf "Required parameter begin is missing"
    member __.SliceEnd : int seq = match operatorArguments.GetParameter "end" with Some(v) -> unbox v | None -> failwithf "Required parameter end is missing"
    member __.Step = operatorArguments.GetParameter("step", Slice.StepDefault)

type SliceAssign private (operatorArguments) = 
    inherit SymbolOperator("_slice_assign", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SliceAssign(args)
    new(lhs : Symbol,
        rhs : Symbol,
        sliceBegin : int seq,
        sliceEnd : int seq,
        [<Optional>] ?step : int seq) = 
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
                "begin", Parameter(Some(box sliceBegin))
                "end", Parameter(Some(box sliceEnd))
                "step", step |> Option.map box |> Parameter
            ]
        new SliceAssign(Arguments<Symbol>(operatorArguments))
    new(sliceBegin : int seq,
        sliceEnd : int seq,
        [<Optional>] ?lhs : Symbol,
        [<Optional>] ?rhs : Symbol,
        [<Optional>] ?step : int seq) = 
        let lhs = defaultArg lhs (new ImplicitVariable() :> Symbol)
        let rhs = defaultArg rhs (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "lhs", Input lhs
                "rhs", Input rhs
                "begin", Parameter(Some(box sliceBegin))
                "end", Parameter(Some(box sliceEnd))
                "step", step |> Option.map box |> Parameter
            ]
        new SliceAssign(Arguments<Symbol>(operatorArguments))
    static member StepDefault : int [] = [||]
    member __.Lhs = operatorArguments.GetInput "lhs"
    member __.Rhs = operatorArguments.GetInput "rhs"
    member __.SliceBegin : int seq = match operatorArguments.GetParameter "begin" with Some(v) -> unbox v | None -> failwithf "Required parameter begin is missing"
    member __.SliceEnd : int seq = match operatorArguments.GetParameter "end" with Some(v) -> unbox v | None -> failwithf "Required parameter end is missing"
    member __.Step = operatorArguments.GetParameter("step", SliceAssign.StepDefault)

type SliceAssignScalar private (operatorArguments) = 
    inherit SymbolOperator("_slice_assign_scalar", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SliceAssignScalar(args)
    new(data : Symbol,
        sliceBegin : int seq,
        sliceEnd : int seq,
        [<Optional>] ?scalar : double,
        [<Optional>] ?step : int seq) = 
        let operatorArguments = 
            [
                "data", Input data
                "begin", Parameter(Some(box sliceBegin))
                "end", Parameter(Some(box sliceEnd))
                "scalar", scalar |> Option.map box |> Parameter
                "step", step |> Option.map box |> Parameter
            ]
        new SliceAssignScalar(Arguments<Symbol>(operatorArguments))
    new(sliceBegin : int seq,
        sliceEnd : int seq,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?scalar : double,
        [<Optional>] ?step : int seq) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "begin", Parameter(Some(box sliceBegin))
                "end", Parameter(Some(box sliceEnd))
                "scalar", scalar |> Option.map box |> Parameter
                "step", step |> Option.map box |> Parameter
            ]
        new SliceAssignScalar(Arguments<Symbol>(operatorArguments))
    static member ScalarDefault : double = 0.0
    static member StepDefault : int [] = [||]
    member __.Data = operatorArguments.GetInput "data"
    member __.SliceBegin : int seq = match operatorArguments.GetParameter "begin" with Some(v) -> unbox v | None -> failwithf "Required parameter begin is missing"
    member __.SliceEnd : int seq = match operatorArguments.GetParameter "end" with Some(v) -> unbox v | None -> failwithf "Required parameter end is missing"
    member __.Scalar = operatorArguments.GetParameter("scalar", SliceAssignScalar.ScalarDefault)
    member __.Step = operatorArguments.GetParameter("step", SliceAssignScalar.StepDefault)

type SliceAxis private (operatorArguments) = 
    inherit SymbolOperator("slice_axis", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SliceAxis(args)
    new(data : Symbol,
        axis : int,
        sliceBegin : int,
        [<Optional>] ?sliceEnd : int) = 
        let operatorArguments = 
            [
                "data", Input data
                "axis", Parameter(Some(box axis))
                "begin", Parameter(Some(box sliceBegin))
                "end", sliceEnd |> Option.map box |> Parameter
            ]
        new SliceAxis(Arguments<Symbol>(operatorArguments))
    new(axis : int,
        sliceBegin : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?sliceEnd : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", Parameter(Some(box axis))
                "begin", Parameter(Some(box sliceBegin))
                "end", sliceEnd |> Option.map box |> Parameter
            ]
        new SliceAxis(Arguments<Symbol>(operatorArguments))
    static member SliceEndDefault : int option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis : int = match operatorArguments.GetParameter "axis" with Some(v) -> unbox v | None -> failwithf "Required parameter axis is missing"
    member __.SliceBegin : int = match operatorArguments.GetParameter "begin" with Some(v) -> unbox v | None -> failwithf "Required parameter begin is missing"
    member __.SliceEnd = operatorArguments.GetParameter("end", SliceAxis.SliceEndDefault)

type SliceLike private (operatorArguments) = 
    inherit SymbolOperator("slice_like", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SliceLike(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?shapeLike : Symbol,
        [<Optional>] ?axes : int seq) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let shapeLike = defaultArg shapeLike (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "shape_like", Input shapeLike
                "axes", axes |> Option.map box |> Parameter
            ]
        new SliceLike(Arguments<Symbol>(operatorArguments))
    static member AxesDefault : int [] = [||]
    member __.Data = operatorArguments.GetInput "data"
    member __.ShapeLike = operatorArguments.GetInput "shape_like"
    member __.Axes = operatorArguments.GetParameter("axes", SliceLike.AxesDefault)

type Clip private (operatorArguments) = 
    inherit SymbolOperator("clip", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Clip(args)
    new(data : Symbol,
        aMin : float,
        aMax : float) = 
        let operatorArguments = 
            [
                "data", Input data
                "a_min", Parameter(Some(box aMin))
                "a_max", Parameter(Some(box aMax))
            ]
        new Clip(Arguments<Symbol>(operatorArguments))
    new(aMin : float,
        aMax : float,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "a_min", Parameter(Some(box aMin))
                "a_max", Parameter(Some(box aMax))
            ]
        new Clip(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.AMin : float = match operatorArguments.GetParameter "a_min" with Some(v) -> unbox v | None -> failwithf "Required parameter a_min is missing"
    member __.AMax : float = match operatorArguments.GetParameter "a_max" with Some(v) -> unbox v | None -> failwithf "Required parameter a_max is missing"

type Repeat private (operatorArguments) = 
    inherit SymbolOperator("repeat", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Repeat(args)
    new(data : Symbol,
        repeats : int,
        [<Optional>] ?axis : int) = 
        let operatorArguments = 
            [
                "data", Input data
                "repeats", Parameter(Some(box repeats))
                "axis", axis |> Option.map box |> Parameter
            ]
        new Repeat(Arguments<Symbol>(operatorArguments))
    new(repeats : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "repeats", Parameter(Some(box repeats))
                "axis", axis |> Option.map box |> Parameter
            ]
        new Repeat(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Repeats : int = match operatorArguments.GetParameter "repeats" with Some(v) -> unbox v | None -> failwithf "Required parameter repeats is missing"
    member __.Axis = operatorArguments.GetParameter("axis", Repeat.AxisDefault)

type Tile private (operatorArguments) = 
    inherit SymbolOperator("tile", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Tile(args)
    new(data : Symbol,
        reps : int seq) = 
        let operatorArguments = 
            [
                "data", Input data
                "reps", Parameter(Some(box reps))
            ]
        new Tile(Arguments<Symbol>(operatorArguments))
    new(reps : int seq,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "reps", Parameter(Some(box reps))
            ]
        new Tile(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Reps : int seq = match operatorArguments.GetParameter "reps" with Some(v) -> unbox v | None -> failwithf "Required parameter reps is missing"

type Reverse private (operatorArguments) = 
    inherit SymbolOperator("reverse", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Reverse(args)
    new(data : Symbol,
        axis : int seq) = 
        let operatorArguments = 
            [
                "data", Input data
                "axis", Parameter(Some(box axis))
            ]
        new Reverse(Arguments<Symbol>(operatorArguments))
    new(axis : int seq,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", Parameter(Some(box axis))
            ]
        new Reverse(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis : int seq = match operatorArguments.GetParameter "axis" with Some(v) -> unbox v | None -> failwithf "Required parameter axis is missing"

type Stack private (operatorArguments) = 
    inherit SymbolOperator("stack", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Stack(args)
    new([<Optional>] ?data : Symbol seq,
        [<Optional>] ?axis : int) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("num_args", data)
                "axis", axis |> Option.map box |> Parameter
            ]
        new Stack(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int = 0
    member __.Data = operatorArguments.GetVarArg "data"
    member __.Axis = operatorArguments.GetParameter("axis", Stack.AxisDefault)

type Squeeze private (operatorArguments) = 
    inherit SymbolOperator("squeeze", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Squeeze(args)
    new([<Optional>] ?data : Symbol seq,
        [<Optional>] ?axis : int seq) = 
        let data = defaultArg (data |> Option.map Seq.toArray) Array.empty
        let operatorArguments = 
            [
                "data", VarArg("", data)
                "axis", axis |> Option.map box |> Parameter
            ]
        new Squeeze(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    member __.Data = operatorArguments.GetVarArg "data"
    member __.Axis = operatorArguments.GetParameter("axis", Squeeze.AxisDefault)

type DepthToSpace private (operatorArguments) = 
    inherit SymbolOperator("depth_to_space", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new DepthToSpace(args)
    new(data : Symbol,
        blockSize : int) = 
        let operatorArguments = 
            [
                "data", Input data
                "block_size", Parameter(Some(box blockSize))
            ]
        new DepthToSpace(Arguments<Symbol>(operatorArguments))
    new(blockSize : int,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "block_size", Parameter(Some(box blockSize))
            ]
        new DepthToSpace(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.BlockSize : int = match operatorArguments.GetParameter "block_size" with Some(v) -> unbox v | None -> failwithf "Required parameter block_size is missing"

type SpaceToDepth private (operatorArguments) = 
    inherit SymbolOperator("space_to_depth", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SpaceToDepth(args)
    new(data : Symbol,
        blockSize : int) = 
        let operatorArguments = 
            [
                "data", Input data
                "block_size", Parameter(Some(box blockSize))
            ]
        new SpaceToDepth(Arguments<Symbol>(operatorArguments))
    new(blockSize : int,
        [<Optional>] ?data : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "block_size", Parameter(Some(box blockSize))
            ]
        new SpaceToDepth(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.BlockSize : int = match operatorArguments.GetParameter "block_size" with Some(v) -> unbox v | None -> failwithf "Required parameter block_size is missing"

type SplitV2 private (operatorArguments) = 
    inherit SymbolOperator("_split_v2", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SplitV2(args)
    new(data : Symbol,
        indices : int seq,
        [<Optional>] ?axis : int,
        [<Optional>] ?squeezeAxis : bool,
        [<Optional>] ?sections : int) = 
        let operatorArguments = 
            [
                "data", Input data
                "indices", Parameter(Some(box indices))
                "axis", axis |> Option.map box |> Parameter
                "squeeze_axis", squeezeAxis |> Option.map box |> Parameter
                "sections", sections |> Option.map box |> Parameter
            ]
        new SplitV2(Arguments<Symbol>(operatorArguments))
    new(indices : int seq,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int,
        [<Optional>] ?squeezeAxis : bool,
        [<Optional>] ?sections : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "indices", Parameter(Some(box indices))
                "axis", axis |> Option.map box |> Parameter
                "squeeze_axis", squeezeAxis |> Option.map box |> Parameter
                "sections", sections |> Option.map box |> Parameter
            ]
        new SplitV2(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int = 1
    static member SqueezeAxisDefault : bool = false
    static member SectionsDefault : int = 0
    member __.Data = operatorArguments.GetInput "data"
    member __.Indices : int seq = match operatorArguments.GetParameter "indices" with Some(v) -> unbox v | None -> failwithf "Required parameter indices is missing"
    member __.Axis = operatorArguments.GetParameter("axis", SplitV2.AxisDefault)
    member __.SqueezeAxis = operatorArguments.GetParameter("squeeze_axis", SplitV2.SqueezeAxisDefault)
    member __.Sections = operatorArguments.GetParameter("sections", SplitV2.SectionsDefault)

type Topk private (operatorArguments) = 
    inherit SymbolOperator("topk", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Topk(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int,
        [<Optional>] ?k : int,
        [<Optional>] ?retTyp : RetTyp,
        [<Optional>] ?isAscend : bool,
        [<Optional>] ?dtype : TopkDtype) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "k", k |> Option.map box |> Parameter
                "ret_typ", retTyp |> Option.map box |> Parameter
                "is_ascend", isAscend |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
            ]
        new Topk(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int option = Some(-1)
    static member KDefault : int = 1
    static member RetTypDefault : RetTyp = RetTyp.Indices
    static member IsAscendDefault : bool = false
    static member DtypeDefault : TopkDtype = TopkDtype.Float32
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", Topk.AxisDefault)
    member __.K = operatorArguments.GetParameter("k", Topk.KDefault)
    member __.RetTyp = operatorArguments.GetParameter("ret_typ", Topk.RetTypDefault)
    member __.IsAscend = operatorArguments.GetParameter("is_ascend", Topk.IsAscendDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", Topk.DtypeDefault)

type Sort private (operatorArguments) = 
    inherit SymbolOperator("sort", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Sort(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int,
        [<Optional>] ?isAscend : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "is_ascend", isAscend |> Option.map box |> Parameter
            ]
        new Sort(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int option = Some(-1)
    static member IsAscendDefault : bool = true
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", Sort.AxisDefault)
    member __.IsAscend = operatorArguments.GetParameter("is_ascend", Sort.IsAscendDefault)

type Argsort private (operatorArguments) = 
    inherit SymbolOperator("argsort", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Argsort(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int,
        [<Optional>] ?isAscend : bool,
        [<Optional>] ?dtype : ArgsortDtype) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "is_ascend", isAscend |> Option.map box |> Parameter
                "dtype", dtype |> Option.map box |> Parameter
            ]
        new Argsort(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int option = Some(-1)
    static member IsAscendDefault : bool = true
    static member DtypeDefault : ArgsortDtype = ArgsortDtype.Float32
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", Argsort.AxisDefault)
    member __.IsAscend = operatorArguments.GetParameter("is_ascend", Argsort.IsAscendDefault)
    member __.Dtype = operatorArguments.GetParameter("dtype", Argsort.DtypeDefault)

type RavelMultiIndex private (operatorArguments) = 
    inherit SymbolOperator("_ravel_multi_index", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new RavelMultiIndex(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?shape : int seq) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "shape", shape |> Option.map box |> Parameter
            ]
        new RavelMultiIndex(Arguments<Symbol>(operatorArguments))
    static member ShapeDefault : int [] option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Shape = operatorArguments.GetParameter("shape", RavelMultiIndex.ShapeDefault)

type UnravelIndex private (operatorArguments) = 
    inherit SymbolOperator("_unravel_index", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new UnravelIndex(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?shape : int seq) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "shape", shape |> Option.map box |> Parameter
            ]
        new UnravelIndex(Arguments<Symbol>(operatorArguments))
    static member ShapeDefault : int [] option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Shape = operatorArguments.GetParameter("shape", UnravelIndex.ShapeDefault)

type SparseRetain private (operatorArguments) = 
    inherit SymbolOperator("_sparse_retain", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SparseRetain(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?indices : Symbol) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let indices = defaultArg indices (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "indices", Input indices
            ]
        new SparseRetain(Arguments<Symbol>(operatorArguments))
    member __.Data = operatorArguments.GetInput "data"
    member __.Indices = operatorArguments.GetInput "indices"

type SquareSum private (operatorArguments) = 
    inherit SymbolOperator("_square_sum", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SquareSum(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?axis : int seq,
        [<Optional>] ?keepdims : bool,
        [<Optional>] ?exclude : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "axis", axis |> Option.map box |> Parameter
                "keepdims", keepdims |> Option.map box |> Parameter
                "exclude", exclude |> Option.map box |> Parameter
            ]
        new SquareSum(Arguments<Symbol>(operatorArguments))
    static member AxisDefault : int [] option = None
    static member KeepdimsDefault : bool = false
    static member ExcludeDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Axis = operatorArguments.GetParameter("axis", SquareSum.AxisDefault)
    member __.Keepdims = operatorArguments.GetParameter("keepdims", SquareSum.KeepdimsDefault)
    member __.Exclude = operatorArguments.GetParameter("exclude", SquareSum.ExcludeDefault)

type BilinearSampler private (operatorArguments) = 
    inherit SymbolOperator("BilinearSampler", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new BilinearSampler(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?grid : Symbol,
        [<Optional>] ?cudnnOff : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let grid = defaultArg grid (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "grid", Input grid
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
            ]
        new BilinearSampler(Arguments<Symbol>(operatorArguments))
    static member CudnnOffDefault : bool option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Grid = operatorArguments.GetInput "grid"
    member __.CudnnOff = operatorArguments.GetParameter("cudnn_off", BilinearSampler.CudnnOffDefault)

type ContribCountSketch private (operatorArguments) = 
    inherit SymbolOperator("_contrib_count_sketch", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribCountSketch(args)
    new(data : Symbol,
        h : Symbol,
        s : Symbol,
        outDim : int,
        [<Optional>] ?processingBatchSize : int) = 
        let operatorArguments = 
            [
                "data", Input data
                "h", Input h
                "s", Input s
                "out_dim", Parameter(Some(box outDim))
                "processing_batch_size", processingBatchSize |> Option.map box |> Parameter
            ]
        new ContribCountSketch(Arguments<Symbol>(operatorArguments))
    new(outDim : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?h : Symbol,
        [<Optional>] ?s : Symbol,
        [<Optional>] ?processingBatchSize : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let h = defaultArg h (new ImplicitVariable() :> Symbol)
        let s = defaultArg s (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "h", Input h
                "s", Input s
                "out_dim", Parameter(Some(box outDim))
                "processing_batch_size", processingBatchSize |> Option.map box |> Parameter
            ]
        new ContribCountSketch(Arguments<Symbol>(operatorArguments))
    static member ProcessingBatchSizeDefault : int = 32
    member __.Data = operatorArguments.GetInput "data"
    member __.H = operatorArguments.GetInput "h"
    member __.S = operatorArguments.GetInput "s"
    member __.OutDim : int = match operatorArguments.GetParameter "out_dim" with Some(v) -> unbox v | None -> failwithf "Required parameter out_dim is missing"
    member __.ProcessingBatchSize = operatorArguments.GetParameter("processing_batch_size", ContribCountSketch.ProcessingBatchSizeDefault)

type ContribDeformableConvolution private (operatorArguments) = 
    inherit SymbolOperator("_contrib_DeformableConvolution", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribDeformableConvolution(args)
    new(data : Symbol,
        offset : Symbol,
        weight : Symbol,
        bias : Symbol,
        kernel : int seq,
        numFilter : int,
        [<Optional>] ?stride : int seq,
        [<Optional>] ?dilate : int seq,
        [<Optional>] ?pad : int seq,
        [<Optional>] ?numGroup : int,
        [<Optional>] ?numDeformableGroup : int,
        [<Optional>] ?workspace : int64,
        [<Optional>] ?noBias : bool,
        [<Optional>] ?layout : ContribDeformableConvolutionLayout) = 
        let operatorArguments = 
            [
                "data", Input data
                "offset", Input offset
                "weight", Input weight
                "bias", Input bias
                "kernel", Parameter(Some(box kernel))
                "num_filter", Parameter(Some(box numFilter))
                "stride", stride |> Option.map box |> Parameter
                "dilate", dilate |> Option.map box |> Parameter
                "pad", pad |> Option.map box |> Parameter
                "num_group", numGroup |> Option.map box |> Parameter
                "num_deformable_group", numDeformableGroup |> Option.map box |> Parameter
                "workspace", workspace |> Option.map box |> Parameter
                "no_bias", noBias |> Option.map box |> Parameter
                "layout", layout |> Option.map box |> Parameter
            ]
        new ContribDeformableConvolution(Arguments<Symbol>(operatorArguments))
    new(kernel : int seq,
        numFilter : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?offset : Symbol,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?bias : Symbol,
        [<Optional>] ?stride : int seq,
        [<Optional>] ?dilate : int seq,
        [<Optional>] ?pad : int seq,
        [<Optional>] ?numGroup : int,
        [<Optional>] ?numDeformableGroup : int,
        [<Optional>] ?workspace : int64,
        [<Optional>] ?noBias : bool,
        [<Optional>] ?layout : ContribDeformableConvolutionLayout) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let offset = defaultArg offset (new ImplicitVariable() :> Symbol)
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let bias = defaultArg bias (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "offset", Input offset
                "weight", Input weight
                "bias", Input bias
                "kernel", Parameter(Some(box kernel))
                "num_filter", Parameter(Some(box numFilter))
                "stride", stride |> Option.map box |> Parameter
                "dilate", dilate |> Option.map box |> Parameter
                "pad", pad |> Option.map box |> Parameter
                "num_group", numGroup |> Option.map box |> Parameter
                "num_deformable_group", numDeformableGroup |> Option.map box |> Parameter
                "workspace", workspace |> Option.map box |> Parameter
                "no_bias", noBias |> Option.map box |> Parameter
                "layout", layout |> Option.map box |> Parameter
            ]
        new ContribDeformableConvolution(Arguments<Symbol>(operatorArguments))
    static member StrideDefault : int [] = [||]
    static member DilateDefault : int [] = [||]
    static member PadDefault : int [] = [||]
    static member NumGroupDefault : int = 1
    static member NumDeformableGroupDefault : int = 1
    static member WorkspaceDefault : int64 = 1024L
    static member NoBiasDefault : bool = false
    static member LayoutDefault : ContribDeformableConvolutionLayout option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Offset = operatorArguments.GetInput "offset"
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Bias = operatorArguments.GetInput "bias"
    member __.Kernel : int seq = match operatorArguments.GetParameter "kernel" with Some(v) -> unbox v | None -> failwithf "Required parameter kernel is missing"
    member __.NumFilter : int = match operatorArguments.GetParameter "num_filter" with Some(v) -> unbox v | None -> failwithf "Required parameter num_filter is missing"
    member __.Stride = operatorArguments.GetParameter("stride", ContribDeformableConvolution.StrideDefault)
    member __.Dilate = operatorArguments.GetParameter("dilate", ContribDeformableConvolution.DilateDefault)
    member __.Pad = operatorArguments.GetParameter("pad", ContribDeformableConvolution.PadDefault)
    member __.NumGroup = operatorArguments.GetParameter("num_group", ContribDeformableConvolution.NumGroupDefault)
    member __.NumDeformableGroup = operatorArguments.GetParameter("num_deformable_group", ContribDeformableConvolution.NumDeformableGroupDefault)
    member __.Workspace = operatorArguments.GetParameter("workspace", ContribDeformableConvolution.WorkspaceDefault)
    member __.NoBias = operatorArguments.GetParameter("no_bias", ContribDeformableConvolution.NoBiasDefault)
    member __.Layout = operatorArguments.GetParameter("layout", ContribDeformableConvolution.LayoutDefault)

type ContribDeformablePSROIPooling private (operatorArguments) = 
    inherit SymbolOperator("_contrib_DeformablePSROIPooling", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribDeformablePSROIPooling(args)
    new(data : Symbol,
        rois : Symbol,
        trans : Symbol,
        spatialScale : float,
        outputDim : int,
        groupSize : int,
        pooledSize : int,
        [<Optional>] ?partSize : int,
        [<Optional>] ?samplePerPart : int,
        [<Optional>] ?transStd : float,
        [<Optional>] ?noTrans : bool) = 
        let operatorArguments = 
            [
                "data", Input data
                "rois", Input rois
                "trans", Input trans
                "spatial_scale", Parameter(Some(box spatialScale))
                "output_dim", Parameter(Some(box outputDim))
                "group_size", Parameter(Some(box groupSize))
                "pooled_size", Parameter(Some(box pooledSize))
                "part_size", partSize |> Option.map box |> Parameter
                "sample_per_part", samplePerPart |> Option.map box |> Parameter
                "trans_std", transStd |> Option.map box |> Parameter
                "no_trans", noTrans |> Option.map box |> Parameter
            ]
        new ContribDeformablePSROIPooling(Arguments<Symbol>(operatorArguments))
    new(spatialScale : float,
        outputDim : int,
        groupSize : int,
        pooledSize : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?rois : Symbol,
        [<Optional>] ?trans : Symbol,
        [<Optional>] ?partSize : int,
        [<Optional>] ?samplePerPart : int,
        [<Optional>] ?transStd : float,
        [<Optional>] ?noTrans : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let rois = defaultArg rois (new ImplicitVariable() :> Symbol)
        let trans = defaultArg trans (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "rois", Input rois
                "trans", Input trans
                "spatial_scale", Parameter(Some(box spatialScale))
                "output_dim", Parameter(Some(box outputDim))
                "group_size", Parameter(Some(box groupSize))
                "pooled_size", Parameter(Some(box pooledSize))
                "part_size", partSize |> Option.map box |> Parameter
                "sample_per_part", samplePerPart |> Option.map box |> Parameter
                "trans_std", transStd |> Option.map box |> Parameter
                "no_trans", noTrans |> Option.map box |> Parameter
            ]
        new ContribDeformablePSROIPooling(Arguments<Symbol>(operatorArguments))
    static member PartSizeDefault : int = 0
    static member SamplePerPartDefault : int = 1
    static member TransStdDefault : double = 0.0
    static member NoTransDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Rois = operatorArguments.GetInput "rois"
    member __.Trans = operatorArguments.GetInput "trans"
    member __.SpatialScale : float = match operatorArguments.GetParameter "spatial_scale" with Some(v) -> unbox v | None -> failwithf "Required parameter spatial_scale is missing"
    member __.OutputDim : int = match operatorArguments.GetParameter "output_dim" with Some(v) -> unbox v | None -> failwithf "Required parameter output_dim is missing"
    member __.GroupSize : int = match operatorArguments.GetParameter "group_size" with Some(v) -> unbox v | None -> failwithf "Required parameter group_size is missing"
    member __.PooledSize : int = match operatorArguments.GetParameter "pooled_size" with Some(v) -> unbox v | None -> failwithf "Required parameter pooled_size is missing"
    member __.PartSize = operatorArguments.GetParameter("part_size", ContribDeformablePSROIPooling.PartSizeDefault)
    member __.SamplePerPart = operatorArguments.GetParameter("sample_per_part", ContribDeformablePSROIPooling.SamplePerPartDefault)
    member __.TransStd = operatorArguments.GetParameter("trans_std", ContribDeformablePSROIPooling.TransStdDefault)
    member __.NoTrans = operatorArguments.GetParameter("no_trans", ContribDeformablePSROIPooling.NoTransDefault)

type ContribFft private (operatorArguments) = 
    inherit SymbolOperator("_contrib_fft", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribFft(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?computeSize : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "compute_size", computeSize |> Option.map box |> Parameter
            ]
        new ContribFft(Arguments<Symbol>(operatorArguments))
    static member ComputeSizeDefault : int = 128
    member __.Data = operatorArguments.GetInput "data"
    member __.ComputeSize = operatorArguments.GetParameter("compute_size", ContribFft.ComputeSizeDefault)

type ContribIfft private (operatorArguments) = 
    inherit SymbolOperator("_contrib_ifft", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribIfft(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?computeSize : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "compute_size", computeSize |> Option.map box |> Parameter
            ]
        new ContribIfft(Arguments<Symbol>(operatorArguments))
    static member ComputeSizeDefault : int = 128
    member __.Data = operatorArguments.GetInput "data"
    member __.ComputeSize = operatorArguments.GetParameter("compute_size", ContribIfft.ComputeSizeDefault)

type ContribMultiProposal private (operatorArguments) = 
    inherit SymbolOperator("_contrib_MultiProposal", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribMultiProposal(args)
    new([<Optional>] ?clsProb : Symbol,
        [<Optional>] ?bboxPred : Symbol,
        [<Optional>] ?imInfo : Symbol,
        [<Optional>] ?rpnPreNmsTopN : int,
        [<Optional>] ?rpnPostNmsTopN : int,
        [<Optional>] ?threshold : float,
        [<Optional>] ?rpnMinSize : int,
        [<Optional>] ?scales : double seq,
        [<Optional>] ?ratios : double seq,
        [<Optional>] ?featureStride : int,
        [<Optional>] ?outputScore : bool,
        [<Optional>] ?iouLoss : bool) = 
        let clsProb = defaultArg clsProb (new ImplicitVariable() :> Symbol)
        let bboxPred = defaultArg bboxPred (new ImplicitVariable() :> Symbol)
        let imInfo = defaultArg imInfo (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "cls_prob", Input clsProb
                "bbox_pred", Input bboxPred
                "im_info", Input imInfo
                "rpn_pre_nms_top_n", rpnPreNmsTopN |> Option.map box |> Parameter
                "rpn_post_nms_top_n", rpnPostNmsTopN |> Option.map box |> Parameter
                "threshold", threshold |> Option.map box |> Parameter
                "rpn_min_size", rpnMinSize |> Option.map box |> Parameter
                "scales", scales |> Option.map box |> Parameter
                "ratios", ratios |> Option.map box |> Parameter
                "feature_stride", featureStride |> Option.map box |> Parameter
                "output_score", outputScore |> Option.map box |> Parameter
                "iou_loss", iouLoss |> Option.map box |> Parameter
            ]
        new ContribMultiProposal(Arguments<Symbol>(operatorArguments))
    static member RpnPreNmsTopNDefault : int = 6000
    static member RpnPostNmsTopNDefault : int = 300
    static member ThresholdDefault : double = 0.699999988
    static member RpnMinSizeDefault : int = 16
    static member ScalesDefault : double [] = [|4.0; 8.0; 16.0; 32.0|]
    static member RatiosDefault : double [] = [|0.5; 1.0; 2.0|]
    static member FeatureStrideDefault : int = 16
    static member OutputScoreDefault : bool = false
    static member IouLossDefault : bool = false
    member __.ClsProb = operatorArguments.GetInput "cls_prob"
    member __.BboxPred = operatorArguments.GetInput "bbox_pred"
    member __.ImInfo = operatorArguments.GetInput "im_info"
    member __.RpnPreNmsTopN = operatorArguments.GetParameter("rpn_pre_nms_top_n", ContribMultiProposal.RpnPreNmsTopNDefault)
    member __.RpnPostNmsTopN = operatorArguments.GetParameter("rpn_post_nms_top_n", ContribMultiProposal.RpnPostNmsTopNDefault)
    member __.Threshold = operatorArguments.GetParameter("threshold", ContribMultiProposal.ThresholdDefault)
    member __.RpnMinSize = operatorArguments.GetParameter("rpn_min_size", ContribMultiProposal.RpnMinSizeDefault)
    member __.Scales = operatorArguments.GetParameter("scales", ContribMultiProposal.ScalesDefault)
    member __.Ratios = operatorArguments.GetParameter("ratios", ContribMultiProposal.RatiosDefault)
    member __.FeatureStride = operatorArguments.GetParameter("feature_stride", ContribMultiProposal.FeatureStrideDefault)
    member __.OutputScore = operatorArguments.GetParameter("output_score", ContribMultiProposal.OutputScoreDefault)
    member __.IouLoss = operatorArguments.GetParameter("iou_loss", ContribMultiProposal.IouLossDefault)

type ContribProposal private (operatorArguments) = 
    inherit SymbolOperator("_contrib_Proposal", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribProposal(args)
    new([<Optional>] ?clsProb : Symbol,
        [<Optional>] ?bboxPred : Symbol,
        [<Optional>] ?imInfo : Symbol,
        [<Optional>] ?rpnPreNmsTopN : int,
        [<Optional>] ?rpnPostNmsTopN : int,
        [<Optional>] ?threshold : float,
        [<Optional>] ?rpnMinSize : int,
        [<Optional>] ?scales : double seq,
        [<Optional>] ?ratios : double seq,
        [<Optional>] ?featureStride : int,
        [<Optional>] ?outputScore : bool,
        [<Optional>] ?iouLoss : bool) = 
        let clsProb = defaultArg clsProb (new ImplicitVariable() :> Symbol)
        let bboxPred = defaultArg bboxPred (new ImplicitVariable() :> Symbol)
        let imInfo = defaultArg imInfo (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "cls_prob", Input clsProb
                "bbox_pred", Input bboxPred
                "im_info", Input imInfo
                "rpn_pre_nms_top_n", rpnPreNmsTopN |> Option.map box |> Parameter
                "rpn_post_nms_top_n", rpnPostNmsTopN |> Option.map box |> Parameter
                "threshold", threshold |> Option.map box |> Parameter
                "rpn_min_size", rpnMinSize |> Option.map box |> Parameter
                "scales", scales |> Option.map box |> Parameter
                "ratios", ratios |> Option.map box |> Parameter
                "feature_stride", featureStride |> Option.map box |> Parameter
                "output_score", outputScore |> Option.map box |> Parameter
                "iou_loss", iouLoss |> Option.map box |> Parameter
            ]
        new ContribProposal(Arguments<Symbol>(operatorArguments))
    static member RpnPreNmsTopNDefault : int = 6000
    static member RpnPostNmsTopNDefault : int = 300
    static member ThresholdDefault : double = 0.699999988
    static member RpnMinSizeDefault : int = 16
    static member ScalesDefault : double [] = [|4.0; 8.0; 16.0; 32.0|]
    static member RatiosDefault : double [] = [|0.5; 1.0; 2.0|]
    static member FeatureStrideDefault : int = 16
    static member OutputScoreDefault : bool = false
    static member IouLossDefault : bool = false
    member __.ClsProb = operatorArguments.GetInput "cls_prob"
    member __.BboxPred = operatorArguments.GetInput "bbox_pred"
    member __.ImInfo = operatorArguments.GetInput "im_info"
    member __.RpnPreNmsTopN = operatorArguments.GetParameter("rpn_pre_nms_top_n", ContribProposal.RpnPreNmsTopNDefault)
    member __.RpnPostNmsTopN = operatorArguments.GetParameter("rpn_post_nms_top_n", ContribProposal.RpnPostNmsTopNDefault)
    member __.Threshold = operatorArguments.GetParameter("threshold", ContribProposal.ThresholdDefault)
    member __.RpnMinSize = operatorArguments.GetParameter("rpn_min_size", ContribProposal.RpnMinSizeDefault)
    member __.Scales = operatorArguments.GetParameter("scales", ContribProposal.ScalesDefault)
    member __.Ratios = operatorArguments.GetParameter("ratios", ContribProposal.RatiosDefault)
    member __.FeatureStride = operatorArguments.GetParameter("feature_stride", ContribProposal.FeatureStrideDefault)
    member __.OutputScore = operatorArguments.GetParameter("output_score", ContribProposal.OutputScoreDefault)
    member __.IouLoss = operatorArguments.GetParameter("iou_loss", ContribProposal.IouLossDefault)

type ContribPSROIPooling private (operatorArguments) = 
    inherit SymbolOperator("_contrib_PSROIPooling", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ContribPSROIPooling(args)
    new(data : Symbol,
        rois : Symbol,
        spatialScale : float,
        outputDim : int,
        pooledSize : int,
        [<Optional>] ?groupSize : int) = 
        let operatorArguments = 
            [
                "data", Input data
                "rois", Input rois
                "spatial_scale", Parameter(Some(box spatialScale))
                "output_dim", Parameter(Some(box outputDim))
                "pooled_size", Parameter(Some(box pooledSize))
                "group_size", groupSize |> Option.map box |> Parameter
            ]
        new ContribPSROIPooling(Arguments<Symbol>(operatorArguments))
    new(spatialScale : float,
        outputDim : int,
        pooledSize : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?rois : Symbol,
        [<Optional>] ?groupSize : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let rois = defaultArg rois (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "rois", Input rois
                "spatial_scale", Parameter(Some(box spatialScale))
                "output_dim", Parameter(Some(box outputDim))
                "pooled_size", Parameter(Some(box pooledSize))
                "group_size", groupSize |> Option.map box |> Parameter
            ]
        new ContribPSROIPooling(Arguments<Symbol>(operatorArguments))
    static member GroupSizeDefault : int = 0
    member __.Data = operatorArguments.GetInput "data"
    member __.Rois = operatorArguments.GetInput "rois"
    member __.SpatialScale : float = match operatorArguments.GetParameter "spatial_scale" with Some(v) -> unbox v | None -> failwithf "Required parameter spatial_scale is missing"
    member __.OutputDim : int = match operatorArguments.GetParameter "output_dim" with Some(v) -> unbox v | None -> failwithf "Required parameter output_dim is missing"
    member __.PooledSize : int = match operatorArguments.GetParameter "pooled_size" with Some(v) -> unbox v | None -> failwithf "Required parameter pooled_size is missing"
    member __.GroupSize = operatorArguments.GetParameter("group_size", ContribPSROIPooling.GroupSizeDefault)

type ConvolutionV1 private (operatorArguments) = 
    inherit SymbolOperator("Convolution_v1", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new ConvolutionV1(args)
    new(data : Symbol,
        weight : Symbol,
        bias : Symbol,
        kernel : int seq,
        numFilter : int,
        [<Optional>] ?stride : int seq,
        [<Optional>] ?dilate : int seq,
        [<Optional>] ?pad : int seq,
        [<Optional>] ?numGroup : int,
        [<Optional>] ?workspace : int64,
        [<Optional>] ?noBias : bool,
        [<Optional>] ?cudnnTune : CudnnTune,
        [<Optional>] ?cudnnOff : bool,
        [<Optional>] ?layout : ConvolutionV1Layout) = 
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "bias", Input bias
                "kernel", Parameter(Some(box kernel))
                "num_filter", Parameter(Some(box numFilter))
                "stride", stride |> Option.map box |> Parameter
                "dilate", dilate |> Option.map box |> Parameter
                "pad", pad |> Option.map box |> Parameter
                "num_group", numGroup |> Option.map box |> Parameter
                "workspace", workspace |> Option.map box |> Parameter
                "no_bias", noBias |> Option.map box |> Parameter
                "cudnn_tune", cudnnTune |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
                "layout", layout |> Option.map box |> Parameter
            ]
        new ConvolutionV1(Arguments<Symbol>(operatorArguments))
    new(kernel : int seq,
        numFilter : int,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?weight : Symbol,
        [<Optional>] ?bias : Symbol,
        [<Optional>] ?stride : int seq,
        [<Optional>] ?dilate : int seq,
        [<Optional>] ?pad : int seq,
        [<Optional>] ?numGroup : int,
        [<Optional>] ?workspace : int64,
        [<Optional>] ?noBias : bool,
        [<Optional>] ?cudnnTune : CudnnTune,
        [<Optional>] ?cudnnOff : bool,
        [<Optional>] ?layout : ConvolutionV1Layout) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let weight = defaultArg weight (new ImplicitVariable() :> Symbol)
        let bias = defaultArg bias (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "weight", Input weight
                "bias", Input bias
                "kernel", Parameter(Some(box kernel))
                "num_filter", Parameter(Some(box numFilter))
                "stride", stride |> Option.map box |> Parameter
                "dilate", dilate |> Option.map box |> Parameter
                "pad", pad |> Option.map box |> Parameter
                "num_group", numGroup |> Option.map box |> Parameter
                "workspace", workspace |> Option.map box |> Parameter
                "no_bias", noBias |> Option.map box |> Parameter
                "cudnn_tune", cudnnTune |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
                "layout", layout |> Option.map box |> Parameter
            ]
        new ConvolutionV1(Arguments<Symbol>(operatorArguments))
    static member StrideDefault : int [] = [||]
    static member DilateDefault : int [] = [||]
    static member PadDefault : int [] = [||]
    static member NumGroupDefault : int = 1
    static member WorkspaceDefault : int64 = 1024L
    static member NoBiasDefault : bool = false
    static member CudnnTuneDefault : CudnnTune option = None
    static member CudnnOffDefault : bool = false
    static member LayoutDefault : ConvolutionV1Layout option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Weight = operatorArguments.GetInput "weight"
    member __.Bias = operatorArguments.GetInput "bias"
    member __.Kernel : int seq = match operatorArguments.GetParameter "kernel" with Some(v) -> unbox v | None -> failwithf "Required parameter kernel is missing"
    member __.NumFilter : int = match operatorArguments.GetParameter "num_filter" with Some(v) -> unbox v | None -> failwithf "Required parameter num_filter is missing"
    member __.Stride = operatorArguments.GetParameter("stride", ConvolutionV1.StrideDefault)
    member __.Dilate = operatorArguments.GetParameter("dilate", ConvolutionV1.DilateDefault)
    member __.Pad = operatorArguments.GetParameter("pad", ConvolutionV1.PadDefault)
    member __.NumGroup = operatorArguments.GetParameter("num_group", ConvolutionV1.NumGroupDefault)
    member __.Workspace = operatorArguments.GetParameter("workspace", ConvolutionV1.WorkspaceDefault)
    member __.NoBias = operatorArguments.GetParameter("no_bias", ConvolutionV1.NoBiasDefault)
    member __.CudnnTune = operatorArguments.GetParameter("cudnn_tune", ConvolutionV1.CudnnTuneDefault)
    member __.CudnnOff = operatorArguments.GetParameter("cudnn_off", ConvolutionV1.CudnnOffDefault)
    member __.Layout = operatorArguments.GetParameter("layout", ConvolutionV1.LayoutDefault)

type Correlation private (operatorArguments) = 
    inherit SymbolOperator("Correlation", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Correlation(args)
    new([<Optional>] ?data1 : Symbol,
        [<Optional>] ?data2 : Symbol,
        [<Optional>] ?kernelSize : int,
        [<Optional>] ?maxDisplacement : int,
        [<Optional>] ?stride1 : int,
        [<Optional>] ?stride2 : int,
        [<Optional>] ?padSize : int,
        [<Optional>] ?isMultiply : bool) = 
        let data1 = defaultArg data1 (new ImplicitVariable() :> Symbol)
        let data2 = defaultArg data2 (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data1", Input data1
                "data2", Input data2
                "kernel_size", kernelSize |> Option.map box |> Parameter
                "max_displacement", maxDisplacement |> Option.map box |> Parameter
                "stride1", stride1 |> Option.map box |> Parameter
                "stride2", stride2 |> Option.map box |> Parameter
                "pad_size", padSize |> Option.map box |> Parameter
                "is_multiply", isMultiply |> Option.map box |> Parameter
            ]
        new Correlation(Arguments<Symbol>(operatorArguments))
    static member KernelSizeDefault : int = 1
    static member MaxDisplacementDefault : int = 1
    static member Stride1Default : int = 1
    static member Stride2Default : int = 1
    static member PadSizeDefault : int = 0
    static member IsMultiplyDefault : bool = true
    member __.Data1 = operatorArguments.GetInput "data1"
    member __.Data2 = operatorArguments.GetInput "data2"
    member __.KernelSize = operatorArguments.GetParameter("kernel_size", Correlation.KernelSizeDefault)
    member __.MaxDisplacement = operatorArguments.GetParameter("max_displacement", Correlation.MaxDisplacementDefault)
    member __.Stride1 = operatorArguments.GetParameter("stride1", Correlation.Stride1Default)
    member __.Stride2 = operatorArguments.GetParameter("stride2", Correlation.Stride2Default)
    member __.PadSize = operatorArguments.GetParameter("pad_size", Correlation.PadSizeDefault)
    member __.IsMultiply = operatorArguments.GetParameter("is_multiply", Correlation.IsMultiplyDefault)

type GridGenerator private (operatorArguments) = 
    inherit SymbolOperator("GridGenerator", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new GridGenerator(args)
    new(data : Symbol,
        transformType : GridGeneratorTransformType,
        [<Optional>] ?targetShape : int seq) = 
        let operatorArguments = 
            [
                "data", Input data
                "transform_type", Parameter(Some(box transformType))
                "target_shape", targetShape |> Option.map box |> Parameter
            ]
        new GridGenerator(Arguments<Symbol>(operatorArguments))
    new(transformType : GridGeneratorTransformType,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?targetShape : int seq) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "transform_type", Parameter(Some(box transformType))
                "target_shape", targetShape |> Option.map box |> Parameter
            ]
        new GridGenerator(Arguments<Symbol>(operatorArguments))
    static member TargetShapeDefault : int [] = [|0; 0|]
    member __.Data = operatorArguments.GetInput "data"
    member __.TransformType : GridGeneratorTransformType = match operatorArguments.GetParameter "transform_type" with Some(v) -> unbox v | None -> failwithf "Required parameter transform_type is missing"
    member __.TargetShape = operatorArguments.GetParameter("target_shape", GridGenerator.TargetShapeDefault)

type InstanceNorm private (operatorArguments) = 
    inherit SymbolOperator("InstanceNorm", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new InstanceNorm(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?gamma : Symbol,
        [<Optional>] ?beta : Symbol,
        [<Optional>] ?eps : float) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let gamma = defaultArg gamma (new ImplicitVariable() :> Symbol)
        let beta = defaultArg beta (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "gamma", Input gamma
                "beta", Input beta
                "eps", eps |> Option.map box |> Parameter
            ]
        new InstanceNorm(Arguments<Symbol>(operatorArguments))
    static member EpsDefault : double = 0.00100000005
    member __.Data = operatorArguments.GetInput "data"
    member __.Gamma = operatorArguments.GetInput "gamma"
    member __.Beta = operatorArguments.GetInput "beta"
    member __.Eps = operatorArguments.GetParameter("eps", InstanceNorm.EpsDefault)

type L2Normalization private (operatorArguments) = 
    inherit SymbolOperator("L2Normalization", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new L2Normalization(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?eps : float,
        [<Optional>] ?mode : L2NormalizationMode) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "eps", eps |> Option.map box |> Parameter
                "mode", mode |> Option.map box |> Parameter
            ]
        new L2Normalization(Arguments<Symbol>(operatorArguments))
    static member EpsDefault : double = 0.0000000001
    static member ModeDefault : L2NormalizationMode = L2NormalizationMode.Instance
    member __.Data = operatorArguments.GetInput "data"
    member __.Eps = operatorArguments.GetParameter("eps", L2Normalization.EpsDefault)
    member __.Mode = operatorArguments.GetParameter("mode", L2Normalization.ModeDefault)

type MakeLoss private (operatorArguments) = 
    inherit SymbolOperator("MakeLoss", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new MakeLoss(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?gradScale : float,
        [<Optional>] ?validThresh : float,
        [<Optional>] ?normalization : Normalization) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "grad_scale", gradScale |> Option.map box |> Parameter
                "valid_thresh", validThresh |> Option.map box |> Parameter
                "normalization", normalization |> Option.map box |> Parameter
            ]
        new MakeLoss(Arguments<Symbol>(operatorArguments))
    static member GradScaleDefault : double = 1.0
    static member ValidThreshDefault : double = 0.0
    static member NormalizationDefault : Normalization = Normalization.Null
    member __.Data = operatorArguments.GetInput "data"
    member __.GradScale = operatorArguments.GetParameter("grad_scale", MakeLoss.GradScaleDefault)
    member __.ValidThresh = operatorArguments.GetParameter("valid_thresh", MakeLoss.ValidThreshDefault)
    member __.Normalization = operatorArguments.GetParameter("normalization", MakeLoss.NormalizationDefault)

type SequenceLast private (operatorArguments) = 
    inherit SymbolOperator("SequenceLast", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SequenceLast(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?sequenceLength : Symbol,
        [<Optional>] ?useSequenceLength : bool,
        [<Optional>] ?axis : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let sequenceLength = defaultArg sequenceLength (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "sequence_length", Input sequenceLength
                "use_sequence_length", useSequenceLength |> Option.map box |> Parameter
                "axis", axis |> Option.map box |> Parameter
            ]
        new SequenceLast(Arguments<Symbol>(operatorArguments))
    static member UseSequenceLengthDefault : bool = false
    static member AxisDefault : int = 0
    member __.Data = operatorArguments.GetInput "data"
    member __.SequenceLength = operatorArguments.GetInput "sequence_length"
    member __.UseSequenceLength = operatorArguments.GetParameter("use_sequence_length", SequenceLast.UseSequenceLengthDefault)
    member __.Axis = operatorArguments.GetParameter("axis", SequenceLast.AxisDefault)

type SequenceReverse private (operatorArguments) = 
    inherit SymbolOperator("SequenceReverse", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SequenceReverse(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?sequenceLength : Symbol,
        [<Optional>] ?useSequenceLength : bool,
        [<Optional>] ?axis : int) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let sequenceLength = defaultArg sequenceLength (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "sequence_length", Input sequenceLength
                "use_sequence_length", useSequenceLength |> Option.map box |> Parameter
                "axis", axis |> Option.map box |> Parameter
            ]
        new SequenceReverse(Arguments<Symbol>(operatorArguments))
    static member UseSequenceLengthDefault : bool = false
    static member AxisDefault : int = 0
    member __.Data = operatorArguments.GetInput "data"
    member __.SequenceLength = operatorArguments.GetInput "sequence_length"
    member __.UseSequenceLength = operatorArguments.GetParameter("use_sequence_length", SequenceReverse.UseSequenceLengthDefault)
    member __.Axis = operatorArguments.GetParameter("axis", SequenceReverse.AxisDefault)

type SpatialTransformer private (operatorArguments) = 
    inherit SymbolOperator("SpatialTransformer", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SpatialTransformer(args)
    new(data : Symbol,
        loc : Symbol,
        transformType : SpatialTransformerTransformType,
        samplerType : SamplerType,
        [<Optional>] ?targetShape : int seq,
        [<Optional>] ?cudnnOff : bool) = 
        let operatorArguments = 
            [
                "data", Input data
                "loc", Input loc
                "transform_type", Parameter(Some(box transformType))
                "sampler_type", Parameter(Some(box samplerType))
                "target_shape", targetShape |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
            ]
        new SpatialTransformer(Arguments<Symbol>(operatorArguments))
    new(transformType : SpatialTransformerTransformType,
        samplerType : SamplerType,
        [<Optional>] ?data : Symbol,
        [<Optional>] ?loc : Symbol,
        [<Optional>] ?targetShape : int seq,
        [<Optional>] ?cudnnOff : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let loc = defaultArg loc (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "loc", Input loc
                "transform_type", Parameter(Some(box transformType))
                "sampler_type", Parameter(Some(box samplerType))
                "target_shape", targetShape |> Option.map box |> Parameter
                "cudnn_off", cudnnOff |> Option.map box |> Parameter
            ]
        new SpatialTransformer(Arguments<Symbol>(operatorArguments))
    static member TargetShapeDefault : int [] = [|0; 0|]
    static member CudnnOffDefault : bool option = None
    member __.Data = operatorArguments.GetInput "data"
    member __.Loc = operatorArguments.GetInput "loc"
    member __.TransformType : SpatialTransformerTransformType = match operatorArguments.GetParameter "transform_type" with Some(v) -> unbox v | None -> failwithf "Required parameter transform_type is missing"
    member __.SamplerType : SamplerType = match operatorArguments.GetParameter "sampler_type" with Some(v) -> unbox v | None -> failwithf "Required parameter sampler_type is missing"
    member __.TargetShape = operatorArguments.GetParameter("target_shape", SpatialTransformer.TargetShapeDefault)
    member __.CudnnOff = operatorArguments.GetParameter("cudnn_off", SpatialTransformer.CudnnOffDefault)

type SVMOutput private (operatorArguments) = 
    inherit SymbolOperator("SVMOutput", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new SVMOutput(args)
    new([<Optional>] ?data : Symbol,
        [<Optional>] ?label : Symbol,
        [<Optional>] ?margin : float,
        [<Optional>] ?regularizationCoefficient : float,
        [<Optional>] ?useLinear : bool) = 
        let data = defaultArg data (new ImplicitVariable() :> Symbol)
        let label = defaultArg label (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "data", Input data
                "label", Input label
                "margin", margin |> Option.map box |> Parameter
                "regularization_coefficient", regularizationCoefficient |> Option.map box |> Parameter
                "use_linear", useLinear |> Option.map box |> Parameter
            ]
        new SVMOutput(Arguments<Symbol>(operatorArguments))
    static member MarginDefault : double = 1.0
    static member RegularizationCoefficientDefault : double = 1.0
    static member UseLinearDefault : bool = false
    member __.Data = operatorArguments.GetInput "data"
    member __.Label = operatorArguments.GetInput "label"
    member __.Margin = operatorArguments.GetParameter("margin", SVMOutput.MarginDefault)
    member __.RegularizationCoefficient = operatorArguments.GetParameter("regularization_coefficient", SVMOutput.RegularizationCoefficientDefault)
    member __.UseLinear = operatorArguments.GetParameter("use_linear", SVMOutput.UseLinearDefault)

type Imdecode private (operatorArguments) = 
    inherit SymbolOperator("_imdecode", operatorArguments)
    static member CreateFromArguments(args : Arguments<Symbol>) = new Imdecode(args)
    new(mean : Symbol,
        index : int,
        x0 : int,
        y0 : int,
        x1 : int,
        y1 : int,
        c : int,
        size : int) = 
        let operatorArguments = 
            [
                "mean", Input mean
                "index", Parameter(Some(box index))
                "x0", Parameter(Some(box x0))
                "y0", Parameter(Some(box y0))
                "x1", Parameter(Some(box x1))
                "y1", Parameter(Some(box y1))
                "c", Parameter(Some(box c))
                "size", Parameter(Some(box size))
            ]
        new Imdecode(Arguments<Symbol>(operatorArguments))
    new(index : int,
        x0 : int,
        y0 : int,
        x1 : int,
        y1 : int,
        c : int,
        size : int,
        [<Optional>] ?mean : Symbol) = 
        let mean = defaultArg mean (new ImplicitVariable() :> Symbol)
        let operatorArguments = 
            [
                "mean", Input mean
                "index", Parameter(Some(box index))
                "x0", Parameter(Some(box x0))
                "y0", Parameter(Some(box y0))
                "x1", Parameter(Some(box x1))
                "y1", Parameter(Some(box y1))
                "c", Parameter(Some(box c))
                "size", Parameter(Some(box size))
            ]
        new Imdecode(Arguments<Symbol>(operatorArguments))
    member __.Mean = operatorArguments.GetInput "mean"
    member __.Index : int = match operatorArguments.GetParameter "index" with Some(v) -> unbox v | None -> failwithf "Required parameter index is missing"
    member __.X0 : int = match operatorArguments.GetParameter "x0" with Some(v) -> unbox v | None -> failwithf "Required parameter x0 is missing"
    member __.Y0 : int = match operatorArguments.GetParameter "y0" with Some(v) -> unbox v | None -> failwithf "Required parameter y0 is missing"
    member __.X1 : int = match operatorArguments.GetParameter "x1" with Some(v) -> unbox v | None -> failwithf "Required parameter x1 is missing"
    member __.Y1 : int = match operatorArguments.GetParameter "y1" with Some(v) -> unbox v | None -> failwithf "Required parameter y1 is missing"
    member __.C : int = match operatorArguments.GetParameter "c" with Some(v) -> unbox v | None -> failwithf "Required parameter c is missing"
    member __.Size : int = match operatorArguments.GetParameter "size" with Some(v) -> unbox v | None -> failwithf "Required parameter size is missing"
(* GERNATED SYMBOL TYPES END *)//

