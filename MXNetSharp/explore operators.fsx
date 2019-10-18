open System
open System.Runtime.InteropServices
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Runtime.CompilerServices

#load "capi.fs"
#load "cpredictapi.fs"
#load "cnnvmapi.fs"
#load "coretypes.fs"
#load "interop.fs"

open MXNetSharp.Interop


exception AtomicSymbolCreatorNotFound of string with
    override x.Message = 
        match x :> Exception with 
        | AtomicSymbolCreatorNotFound(name) -> sprintf "Atomic symbol creator %s not found." name
        | _ -> failwith "unreachable"

type AtomicSymbolCreator internal (handle : MXNetSharp.Interop.CApi.AtomicSymbolCreatorHandle, info : AtomicSymbolInfo) = 
    let aliases = ResizeArray<string>()
    member internal x.AddAlias(name) = aliases.Add name
    member x.Aliases = aliases |> Seq.map id
    member x.Name = info.Name
    member x.AtomicSymbolCreatorHandle = handle
    member x.Info = info

let lookup = 
    let creators = 
        MXSymbol.listAtomicSymbolCreators ()
        |> Array.map 
            (fun x -> 
                let info = MXSymbol.getAtomicSymbolInfo x
                x, AtomicSymbolCreator(x,info)
            )
        |> dict
    NNVM.listAllOpNames()
    |> Array.map 
        (fun name -> 
            let h = NNVM.getOpHandle name
            let c = creators.[h]
            c.AddAlias(name)
            name, c
        )
    |> dict


lookup.Values
|> Seq.iter 
    (fun v ->
        printfn "------------------------------------ %s ------------------------------------"  v.Name
        printfn "Names {%s}" (v.Aliases |> String.concat ", ")
        printfn "%A" v.Info
        printfn "----------------------------------------------------------------------------" 
    )

let allNames = 
    lookup.Values 
    |> Seq.collect (fun x -> x.Aliases)
    |> Seq.filter (fun x -> x.Contains("scalar") || x.Contains("elemwise") || x.Contains("broadcast"))
    |> Seq.groupBy
        (fun name ->
            let name = 
                if name.StartsWith "_backward_" then 
                    name.Substring("_backward_".Length)
                else 
                    name
            let name = 
                if name.StartsWith "_scatter_" then 
                    name.Substring("_scatter_".Length)
                else 
                    name
            let name = 
                if name.StartsWith "_sparse_" then 
                    name.Substring("_sparse_".Length)
                else 
                    name
            if name.EndsWith "scalar" then 
                name.Substring(0, name.Length - 7).Trim '_'
            elif name.StartsWith "broadcast_" then 
                name.Substring("broadcast_".Length)     
            elif name.StartsWith "elemwise_" then 
                name.Substring("elemwise_".Length)     
            else 
                name
        )

allNames 
|> Seq.iter 
    (fun (g, xs) ->
        printfn "%s {%s}" g (xs |> String.concat ", ")
        //printfn "%s" g
    )

[<Extension>]
type Ext() =
    [<Extension>]
    static member inline Replace(l : string list, [<ReflectedDefinitionAttribute(true)>] v : Expr< ^a>) =
        let name,s = 
            match v with 
            | Patterns.WithValue(_, _, Patterns.ValueWithName(o,t,name))
            | Patterns.ValueWithName(o,t,name) -> 
                name, string (o :?> ^a)
            | _ -> failwithf "Replacee %A" v
        let tag = "%" + name + "%"
        l
        |> List.map (fun str -> str.Replace(tag, s))
        
    
let inline broacastTemp op name = 
    [
        sprintf """static member %%op%%(x : NDArray, y : NDArray) = invoke1 "broadcast_%%name%%" [|x; y|] Array.empty"""
    ]
        .Replace(op)
        .Replace(name)
        
let inline scalarTemp op name rname = 
        [
            sprintf """static member %%op%%(x : NDArray, y : float) = invoke1 "_%%name%%_scalar" [|x|] [|"scalar", valueString y|]"""
            sprintf """static member %%op%%(y : float, x : NDArray) = invoke1 "_%%rname%%_scalar" [|x|] [|"scalar", valueString y|]"""
        ]
            .Replace(op)
            .Replace(name)
            .Replace(rname)

let op2 (op : string) (name : string) (names : string seq) = 
    let names = names |> Seq.toArray

    
    

broacastTemp "(+)" "plus"
scalarTemp "(+)" "plus"

[
    "_np_broadcast_to", ignore
    "np_broadcast_to", ignore
    "npi_add", ignore
    "npi_subtract", ignore
    "npi_rsubtract", ignore
    "npi_multiply", ignore
    "npi_mod", ignore
    "npi_rmod", ignore
    "npi_power", ignore
    "npi_rpower", ignore
    "npi_copysign", ignore
    "npi_rcopysign", ignore
    "npi_arctan2", ignore
    "npi_rarctan2", ignore
    "npi_lcm", ignore
    "npi_true_divide", ignore
    "npi_rtrue_divide", ignore
    "_contrib_quantized_elemwise_add", ignore
    "axis", ignore
    "to", ignore
    "_broadcast_backward", ignore
    "like", ignore
    "add", op2 "(+)" "Plus" 
    "sub", op2 "(-)" "Substract" 
    "mul", op2 "( * )" "Multiply"
    "div", op2 "(/)" "Multiply"
    "mod", op2 "(%)" "Multiply"
    "power", op2 "( ** )" "Multiply"
    "maximum", ignore
    "minimum", ignore
    "hypot", ignore
    "equal", ignore
    "not_equal", ignore
    "greater", ignore
    "greater_equal", ignore
    "lesser", ignore
    "lesser_equal", ignore
    "logical_and", ignore
    "logical_or", ignore
    "logical_xor", ignore
    "plus", ignore
    "minus", ignore
    "rminus", ignore
    "rdiv", ignore
    "rmod", ignore
    "rpower", ignore
    "slice_assign", ignore
]
