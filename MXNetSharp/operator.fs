namespace MXNetSharp
open System
open System.Collections.Generic

type VarArg<'a> = 
    {
        Name : string 
        Inputs : 'a []
        NumArgsName : string option
    }   

[<NoComparison>]
type OpArg<'a> = 
    | VarArg of string * 'a[]
    | Parameter of obj option
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

type Arguments<'a>(args : IDictionary<string, OpArg<'a>>, ordering : string []) =
    new(args : (string*OpArg<'a>) seq) = Arguments(dict args, args |> Seq.map fst |> Seq.toArray)
    member x.Args = args
    member x.Ordering = ordering
    member x.AddReplace(args2 : Arguments<'a>) = Arguments(args2.Args |> Dict.addReplace args, x.Ordering)
    member x.AddIgnore(args2 : Arguments<'a>) = Arguments(args2.Args |> Dict.addIgnore args, x.Ordering)
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
    interface IEnumerable<string*OpArg<'a>> with
        member x.GetEnumerator() = 
            let d = Dictionary(args)
            (seq {
                yield! ordering |> Seq.map (fun m -> d.Remove(m) |> ignore; m,args.[m])
                yield! d.Keys |> Seq.map (fun k -> k, d.[k])
            }).GetEnumerator()
        member this.GetEnumerator() = (this :> IEnumerable<string*OpArg<'a>>).GetEnumerator() :> Collections.IEnumerator
           
