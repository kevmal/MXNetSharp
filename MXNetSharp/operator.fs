namespace MXNetSharp
open System
open System.Runtime.InteropServices
open System.Collections.Generic

type ISymbol =
    abstract member Handle : SafeSymbolHandle

type INDArray =
    abstract member Handle : SafeNDArrayHandle

type INDArrayOrSymbol<'a when 'a :> SafeHandle> = 
    abstract member Handle : 'a

type VarArg<'a> = 
    {
        Name : string 
        Inputs : 'a []
        NumArgsName : string option
    }   
type OpArg<'a> = 
    | VarArg of string option * 'a[]
    | Parameter of obj option
    | DefaultParameter of obj
    | Input of 'a

module internal Dict = 
    let addReplace (src : IDictionary<'k, 'v>) (dest : IDictionary<'k, 'v>) = 
        let d = Dictionary<_,_>(dest)
        for kvp in src do 
            d.[kvp.Key] <- kvp.Value
        d :> IDictionary<_,_>
    let addIgnore (src : IDictionary<'k, 'v>) (dest : IDictionary<'k, 'v>) = 
        let d = Dictionary<_,_>(dest)
        for kvp in src do 
            if not(d.ContainsKey kvp.Key) then 
                d.[kvp.Key] <- kvp.Value
        d :> IDictionary<_,_>
type Arguments<'a>(args : IDictionary<string, OpArg<'a>>) =
    new(args : (string*OpArg<'a>) seq) = Arguments(dict args)
    member x.Args = args
    member x.AddReplace(args2 : Arguments<'a>) = args2.Args |> Dict.addReplace args |> Arguments
    member x.AddIgnore(args2 : Arguments<'a>) = args2.Args |> Dict.addIgnore args |> Arguments
    member x.GetInput(name) = 
        match args.[name] with 
        | Input a -> a
        | w -> failwithf "Expecting %s to be an input but is a %A" name w
    member x.GetParameter(name) =
        match args.[name] with 
        | Parameter(v) -> v
        | w -> failwithf "Expecting %s to be an parameter but is a %A" name w
    member x.GetParameter(name,def : 'v) : 'v = 
        match args.[name] with 
        | Parameter(Some (:? 'v as v)) -> v
        | Parameter(Some v) -> Convert.ChangeType(v,typeof<'v>) :?> 'v
        | Parameter None -> def
        | w -> failwithf "Expecting %s to be an parameter but is a %A" name w
    member x.GetVarArg(name) =
        match args.[name] with 
        | VarArg(d,v) -> v
        | w -> failwithf "Expecting %s to be a var arg but is a %A" name w

type Operator<'a>(name : string, args : Arguments<'a>) = 
    member x.Name = name 
    member x.Arguments = args
    static member (.>>)(o1 : Operator<'a>, position : int) = 
        fun (o2 : Operator<'a>) ->
            o2
    static member (.>>)(o1 : Operator<'a>, o2 : Operator<'a>) = 
        o2

    
type Bleh<'a> private (args) = 
    inherit Operator<'a>("_contrib_dgl_csr_neighbor_non_uniform_sample", args)
    new(csrMatrix : 'a, 
        probability : 'a,
        seedArrays : 'a seq,
        ?numHops : int,
        ?numNeighbor : int,
        ?maxNumVertices : int) =
        let args = 
            Arguments([
                "csr_matrix", Input csrMatrix
                "probability", Input probability
                "seed_arrays", VarArg(Some "num_args", seedArrays |> Seq.toArray)
                "num_hops", numHops |> Option.map box |> Parameter
                "num_neighbor", numNeighbor |> Option.map box |> Parameter
                "max_num_vertices", maxNumVertices |> Option.map box |> Parameter
            ])
        Bleh<'a>(args)
    static member NumHopsDefault = 1

    member x.CsrMatrix = args.GetInput "csr_matrix"
    member x.Probability = args.GetInput "probability"
    member x.SeedArrays = args.GetVarArg "seed_arrays"
    member x.NumHops : int = args.GetParameter("num_hops", Bleh<'a>.NumHopsDefault)




(*

module OpArg = 
    let addReplace 
type Op<'a>(name, args : OpArg<'a> list) = 
    
    
    

type OperatorDef<'a> = 
    {
        Name : string 
        Inputs : IDictionary<string, 'a>
        Parameters : IDictionary<string, obj>
        VarArg : VarArg<'a> option
    }
module internal Dict = 
    let addReplace (src : IDictionary<'k, 'v>) (dest : IDictionary<'k, 'v>) = 
        let d = Dictionary<_,_>(dest)
        for kvp in src do 
            d.[kvp.Key] <- kvp.Value
        d :> IDictionary<_,_>
    let addIgnore (src : IDictionary<'k, 'v>) (dest : IDictionary<'k, 'v>) = 
        let d = Dictionary<_,_>(dest)
        for kvp in src do 
            if not(d.ContainsKey kvp.Key) then 
                d.[kvp.Key] <- kvp.Value
        d :> IDictionary<_,_>
module OperatorDef = 
    let addReplaceInputs (ins : IDictionary<string, 'a>) (x : OperatorDef<'a>) = 
        {x with 
            Inputs = x.Inputs |> Dict.addReplace ins
        }
    let addIgnoreInputs (ins : IDictionary<string, 'a>) (x : OperatorDef<'a>) = 
        {x with 
            Inputs = x.Inputs |> Dict.addIgnore ins
        }
    let addReplaceParameters (ins : IDictionary<string, 'a>) (x : OperatorDef<'a>) = 
        {x with 
            Inputs = x.Inputs |> Dict.addReplace ins
        }
    let addIgnoreInputs (ins : IDictionary<string, 'a>) (x : OperatorDef<'a>) = 
        {x with 
            Inputs = x.Inputs |> Dict.addIgnore ins
        }

            



type Operator<'a>(name, inputKeys : string [] option, inputValues : 'a [], parameterKeys : string [] option, parameterValues : string [], varArg : 'a [] option) = 
    member x.WithInputs

    

type OperatorDefinition(name) =   
    member x.Name = name
    abstract member Aliases : string seq
    abstract member Arguments : IOperatorArgument []
    abstract member ArgumentCount : int
    abstract member InputCount : int
    abstract member ParameterCount : int

    abstract member InputKeys : string []
    abstract member InputValues : string []
    abstract member ParameterStringValues : string []
    abstract member ParameterKeys : string []

    member x.Invoke()


and IOperatorArgument = 
    abstract member Name : string
    abstract member Key : string
    abstract member IsParameter : bool
    abstract member TypeString : string
    abstract member Description : string
    abstract member DefaultValueString : string option
    abstract member DefaultValueObj : obj option
    abstract member Type : Type

and IOperatorArgument<'a> = 
    inherit IOperatorArgument
    abstract member DefaultValue : 'a option
    

type OperatorArgument<'a> = 
    {
        Name : string
        Key : string
        TypeString : string
        Description : string
        DefaultValueString : string option
        DefaultValue : 'a option
        IsParameter : bool
    }
    interface IOperatorArgument<'a> with 
        member x.Name = x.Name
        member x.Key = x.Key
        member x.TypeString = x.TypeString
        member x.Description = x.Description
        member x.DefaultValueString = x.DefaultValueString
        member x.DefaultValueObj = x.DefaultValue |> Option.map box
        member x.DefaultValue = x.DefaultValue
        member x.Type = typeof<'a>
        member x.IsParameter = x.IsParameter



open Util    

type RandomUniform private () = 
    //inherit Operator("RandomUniform")
    static member val Name = "_random_uniform"
    static let keys = [|"low"; "high"; "shape"; "context"; "dtype"|]
    static let defaultValueStrings = [|"0"; "1"; "None"; ""; "None"|]
    static member Low = 
        {
            Name = "Low"
            Key = "low"
            TypeString = "float, optional, default=0"
            Description = "Lower bound of the distribution."
            DefaultValue = Some 0
        }
    static member GetValueArray(?low,?high,?shape,?context,?dtype) = 
        let values = Array.copy defaultValueStrings
        if low.IsSome then values.[0] <- valueString low
        if high.IsSome then values.[1] <- valueString high
        if shape.IsSome then values.[1] <- valueString shape
        values
*)
