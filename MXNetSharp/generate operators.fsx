open System.Runtime.InteropServices
open System.Collections.Generic
open System.Diagnostics

#load "capi.fs"
#load "cpredictapi.fs"
#load "cnnvmapi.fs"
#load "interop.fs"

open MXNetSharp.Interop
open System


type Mapper<'a> = 
    | Final of 'a
    | Mapped of 'a
    | NoMap


type Mappings<'a>() = 
    static let ra = ResizeArray<'a -> 'a Mapper>()
    static member Add(f) = ra.Add f
    static member Modify(f : 'a -> 'a) =
        let g x = Mapped (f x)
        ra.Add g
    static member Apply(x : 'a) =   
        let rec loop i v = 
            if i >= ra.Count then 
                v
            else
                match ra.[i] v with 
                | Final v -> v
                | Mapped v -> loop (i + 1) v
                | NoMap -> loop (i + 1) v
        loop 0 x
            
    
    
        

let creatorInfos = MXSymbol.listAtomicSymbolCreators() |> Array.map MXSymbol.getAtomicSymbolInfo

let quote x = sprintf "\"%s\"" x
let ensureQuote (x : string) =  
    if x.StartsWith "\"" && x.EndsWith "\"" then 
        x
    else
        quote x

let capitalize (x : string) = if x.Length > 0 then [| Char.ToUpper(x.[0]); yield! x.[1 ..] |] |> String else ""
let toCaseName (x : string) = x.Trim(''').Split '_' |> Array.map capitalize |> String.concat ""
let toMethodName (x : string) = x.Split '_' |> Array.map capitalize |> String.concat ""
let toParamName (x : string) = 
    let comps = x.Split '_' 
    if comps.Length = 1 then 
        x
    else
        [
            comps.[0]
            yield! comps.[1..] |> Array.map capitalize
        ]
        |> String.concat ""

let fsName x = x

let splitTypeInfo (x : string) = 
    let a = ResizeArray(3)
    let sb = Text.StringBuilder()
    let rec loop i =
        if i >= x.Length then 
            ()
        else
            match x.[i] with
            | ',' -> 
                a.Add(sb.ToString())
                sb.Clear() |> ignore
                loop (i + 1)
            | '{' -> bracket '}' i
            | '[' -> bracket ']' i
            | c -> 
                sb.Append(c) |> ignore
                loop (i + 1)
    and bracket b i =
        if i >= x.Length then 
            ()
        else
            match x.[i] with
            | c when c = b -> loop i
            | c -> 
                sb.Append(c) |> ignore
                bracket b (i + 1)
    loop 0
    if sb.Length > 0 then 
        a.Add(sb.ToString())
    a.ToArray()
    |> Array.map (fun x -> x.Trim())
              
type SymbolOrNDArray = 
    | Neither
    | SymbolOrNDArray
    | ManySymbolOrNDArray
    | NDArray
    | Symbol
    
type ArgOptional = 
    | Required
    | Optional of string


let tpinfo x = 
    match splitTypeInfo x  with 
    | [|tp|] 
    | [|tp; "required"|] -> tp, Required
    | [|tp; "optional"; d|] -> tp, Optional (d.Split([|'='|], 2).[1].Trim())
    | _ -> failwithf "type info str not recognized %A" x
        
type DefaultMode = 
    | UseAttr of string
    | ReplaceNull of string
    | ReplaceOptionWithString of string
    | IgnoreNull

type Returns = 
    | Unknown 
    | Single

type Arg = 
    {
        AtomicSymbolInfo : AtomicSymbolInfo
        ArgumentInfo : ArgumentInfo
        Name : string 
        TypeString : string
        RequiredOrOptional : ArgOptional
    }
type ArgCodeGen = 
    | Normal
    | SkipArg
    | ValueString of string
    | ConstantArg of string

type UnionType = 
    {
        TypeString : string
        Name : string
        Cases : (string*string) []
    }

type ProcessedArg = 
    {
        ProcessedAtomicSymbol : ProcessedAtomicSymbol option
        Arg : Arg
        Name : string
        TypeString : string
        DefaultMode : DefaultMode option
        SymbolOrNDArray : SymbolOrNDArray option
        Doc : string []
        CodeGenerator : ArgCodeGen
        DefinedType : UnionType Option
    }

and ProcessedAtomicSymbol = 
    {
        AtomicSymbolInfo : AtomicSymbolInfo
        Doc : string []
        Name : string 
        Args : ProcessedArg []
        SymbolOrNDArray : SymbolOrNDArray
        Returns : Returns
    }

type CodeBlock = 
    {
        AtomicSymbolInfo : AtomicSymbolInfo
        MemberDefinitions : (ProcessedAtomicSymbol*(string list)) list
        TypeDefinitions : (ProcessedArg*(String list)) list
    }


let xmlEncodeString = System.Web.HttpUtility.HtmlEncode
let splitLines (x : String) = x.Replace("\r\n", "\n").Split('\n') 
let docComment (x : string) =
    x
    |> splitLines
    |> Array.rev
    |> Array.skipWhile String.IsNullOrWhiteSpace
    |> Array.rev
    |> Array.map (fun x -> @"/// " + x)

let argDoc (a : ProcessedArg) = 
    {a with Doc = sprintf "<param name=\"%s\">%s</param>" a.Name (xmlEncodeString a.Arg.ArgumentInfo.Description) |> docComment}

let mapArgBase (a : Arg) = 
    let tp = 
        match a.TypeString with 
        | "Symbol" -> Some Symbol
        | "NDArray" -> Some NDArray
        | "NDArray-or-Symbol" -> Some SymbolOrNDArray
        | "NDArray-or-Symbol[]" -> Some ManySymbolOrNDArray
        | _ -> None
    {
        ProcessedAtomicSymbol = None
        Arg = a
        Name = a.Name
        TypeString = a.TypeString
        DefaultMode = None
        SymbolOrNDArray = tp
        Doc = Array.empty
        CodeGenerator = Normal
        DefinedType = None
    }
    |> argDoc
    |> Mappings.Apply




let toBoolVal x = 
    try 
        if int x <> 0 then 
            "true"
        else 
            "false"
    with 
    | _ -> failwithf "bad bool val %A" x

Mappings.Modify
    (fun (x : ProcessedArg) ->
        if x.TypeString.StartsWith "{" then 
            let cases = x.TypeString.Trim([|'{'; '}'|]).Split ',' |> Array.map (fun x -> x.Trim())
            if cases |> Array.exists (fun x -> x = "None" || x = "'None'") then 
                let dmode = 
                    match x.DefaultMode with 
                    | None -> Some(DefaultMode.ReplaceNull "null")
                    | dm -> dm
                {x with     
                    DefaultMode = dmode
                    DefinedType = 
                        Some({  
                            TypeString = x.TypeString
                            Name = x.Name |> toCaseName
                            Cases = 
                                cases 
                                |> Array.filter (fun x -> x <> "None" && x <> "'None'") 
                                |> Array.map (fun x -> toCaseName x, x.Trim ''')
                        })}
            else
                {x with 
                    DefinedType = 
                        Some({  
                            TypeString = x.TypeString
                            Name = x.Name |> toCaseName
                            Cases = cases |> Array.map (fun x -> toCaseName x, x.Trim ''')
                        })}
        else 
            x
    )



Mappings.Modify
    (fun (x : ProcessedArg) ->
        {x with 
            TypeString = 
                match x.TypeString with 
                | "boolean" -> "bool"
                | "real_t" -> "double"
                | str -> str}
    )

Mappings.Modify
    (fun (x : ProcessedArg) ->
        let dblString x = 
            let str = string x
            if str.Contains "." then 
                str
            else 
                str + ".0"
        let dmode = 
            match x.Arg.RequiredOrOptional with 
            | Optional str -> 
                match x.Arg.TypeString with 
                | "int" -> str.Trim ''' |> int |> string |> UseAttr |> Some
                | "float" -> str.Trim ''' |> double |> dblString |> UseAttr |> Some
                | "boolean" -> str |> toBoolVal |> UseAttr |> Some
                | "string" -> str.Trim ''' |> quote |> UseAttr |> Some
                | "Shape(tuple)" -> 
                    if str = "None" then 
                        ReplaceNull "null" |> Some
                    else 
                        ReplaceNull (sprintf "\"%s\"" str) |> Some
                | t when t.StartsWith "{" -> 
                    Some(ReplaceNull(str.Replace(''','\"')))
                | _ -> None
            | _ -> None
        { x with DefaultMode = dmode }
    )

Mappings.Modify
    (fun (x : ProcessedAtomicSymbol) ->
        { x with Args = x.Args |> Array.map (fun a -> {a with ProcessedAtomicSymbol = Some x}) }
    )

let mapAtomicSymbolInfoBase (x : AtomicSymbolInfo) = 
    let mname = x.Name |> toMethodName
    let doc = 
        if String.IsNullOrWhiteSpace x.Description then 
            Array.empty
        else
            sprintf "<summary>%s</summary>" (xmlEncodeString x.Description) |> docComment
    let args = 
        [|
            for a in x.Arguments do 
                let arg = 
                    let tp,req = tpinfo a.TypeInfo
                    {
                        AtomicSymbolInfo = x
                        ArgumentInfo = a
                        TypeString = tp
                        RequiredOrOptional = req
                        Name = fsName a.Name
                    }
                mapArgBase arg
        |]
    {
        AtomicSymbolInfo = x
        Doc = doc
        Name = mname
        Args = args
        Returns = Unknown
        SymbolOrNDArray = 
            match args |> Array.choose (fun x -> x.SymbolOrNDArray) |> Array.distinct with 
            | [|Symbol|] -> Symbol
            | [|NDArray|] -> NDArray
            | [|SymbolOrNDArray|] -> SymbolOrNDArray
            | [|SymbolOrNDArray; ManySymbolOrNDArray|]
            | [|ManySymbolOrNDArray; SymbolOrNDArray|]
            | [|ManySymbolOrNDArray|] -> SymbolOrNDArray
            | [||] -> Neither
            | x -> failwithf "Unexpected SymbolOrNDArray %A" x
    } 
    |> Mappings.Apply
    |> List.singleton
    |> Mappings.Apply



let indent n lines = 
    let indent = String.replicate n "    "
    lines |> Seq.map (fun x -> indent + x)

let comment lines = lines |> List.map (fun x -> "// " + x) 

let toCStr (a : ProcessedArg) (str : string) = 
    match a.TypeString with 
    | "int seq" -> sprintf "(%s |> Seq.map string |> String.concat \", \")" str 
    //| "bool" -> sprintf "(if %s then \"1\" else \"0\")" str
    | "string" -> str
    | _ -> sprintf "string %s" str

let toCodeTarget suffix ndarray (x : ProcessedAtomicSymbol) =
    let args = 
        x.Args 
        |> Seq.filter (fun x -> match x.CodeGenerator with ConstantArg _ -> false | _ -> true)
        |> Seq.map 
            (fun x -> 
                let t = 
                    match x.SymbolOrNDArray with 
                    | Some ManySymbolOrNDArray -> 
                        if ndarray then "NDArray[]" else "Symbol[]"
                    | Some _ -> if ndarray then "NDArray" else "Symbol"
                    | _ -> x.TypeString
                match x.DefaultMode with 
                | Some (ReplaceOptionWithString _) -> 
                    sprintf "?%s : %s" x.Name t
                | Some (ReplaceNull _)
                | Some IgnoreNull -> 
                    sprintf "[<Optional>] %s : %s" x.Name t
                | None ->
                    match x.SymbolOrNDArray with 
                    | Some ManySymbolOrNDArray -> 
                        sprintf "[<ParamArray>] %s : %s" x.Name t
                    | _ -> sprintf "%s : %s" x.Name t
                | Some (UseAttr d)-> 
                    sprintf "[<Optional; DefaultParameterValue(%s)>] %s : %s" d x.Name t
                )
        |> Seq.toArray
    let define = 
        let name = 
            if suffix then 
                if ndarray then 
                    x.Name + "NDArray"
                else
                    x.Name + "Symbol"
            else x.Name
        if args.Length < 5 then 
            let argStr = args |> String.concat ", "
            [sprintf "static member %s(%s) =" name argStr]
        else
            let dstr = sprintf "static member %s(" name
            let indent = String.replicate dstr.Length " "
            [
                dstr + args.[0] + ", "
                yield! args.[1 .. args.Length - 2] |> Seq.map (fun x -> indent + x + ", ")
                indent + args.[args.Length - 1] + ") ="
            ]
    let arr x = if Array.isEmpty x then "Array.empty" else sprintf "[|%s|]" (x |> String.concat "; ")
    let inputNamesStr = 
        if ndarray then 
            x.Args 
            |> Array.choose 
                (fun x ->
                    match x.SymbolOrNDArray with 
                    | Some NDArray | Some SymbolOrNDArray -> Some(quote x.Name)
                    | _ -> None
                )
            |> arr
        else 
            x.Args 
            |> Array.choose 
                (fun x ->
                    match x.SymbolOrNDArray with 
                    | Some Symbol | Some SymbolOrNDArray -> Some(quote x.Name)
                    | _ -> None
                )
            |> arr
        
    let inputsStr = 
        let handle x =  
            if ndarray then 
                sprintf "%s.NDArrayHandle.UnsafeHandle" x
            else
                sprintf "%s" x
        let arr (x : _ []) = 
            match x with 
            | [| Choice2Of2 name |] -> sprintf "(%s |> Array.map (fun x -> %s))" name (handle "x")
            | [||] -> "Array.empty"
            | _ -> 
                x
                |> Array.map (function
                    | Choice1Of2 str -> str
                    | Choice2Of2 name -> 
                        sprintf "yield! (%s |> Seq.map (fun x -> %s))" name (handle "x") )
                |> String.concat "; "
                |> sprintf "[|%s|]" 
        if ndarray then 
            x.Args 
            |> Array.choose 
                (fun x ->
                    match x.SymbolOrNDArray with 
                    | Some NDArray | Some SymbolOrNDArray -> Choice1Of2(handle x.Name) |> Some
                    | Some ManySymbolOrNDArray -> Choice2Of2(x.Name) |> Some
                    | _ -> None
                )
            |> arr
        else 
            x.Args 
            |> Array.choose 
                (fun x ->
                    match x.SymbolOrNDArray with 
                    | Some Symbol | Some SymbolOrNDArray -> Choice1Of2(handle x.Name) |> Some
                    | Some ManySymbolOrNDArray -> Choice2Of2(x.Name) |> Some
                    | _ -> None
                )
            |> arr
    let paramNamesStr = 
        x.Args 
        |> Array.filter (fun x -> match x.CodeGenerator with | SkipArg -> false | _ -> true)
        |> Array.choose
            (fun a ->
                match a.SymbolOrNDArray with 
                | None -> Some ("\"" + a.Arg.ArgumentInfo.Name + "\"")
                | _ -> None
            )  
        |> arr
    let paramValuesStr = 
        x.Args 
        |> Array.choose
            (fun a ->
                match a.SymbolOrNDArray with 
                | None -> 
                    let valueStr = 
                        match a.CodeGenerator with 
                        | SkipArg -> None
                        | ValueString str -> Some str
                        | Normal -> Some (toCStr a a.Name)
                        | ConstantArg a -> Some a
                    match a.DefaultMode with 
                    | Some(ReplaceOptionWithString v) -> 
                        valueStr |> Option.map (fun s -> sprintf "(match %s with None -> %s | Some %s -> %s)" a.Name v a.Name s) 
                    | Some(ReplaceNull v) -> 
                        valueStr |> Option.map (fun s -> sprintf "(if isNull (%s :> obj) then %s else %s)" a.Name v s)
                    | _ -> valueStr
                | _ -> None
            ) 
        |> arr
    let invoke = 
        if ndarray then 
            [
                sprintf "let creator = AtomicSymbolCreator.FromName \"%s\"" x.AtomicSymbolInfo.Name
                sprintf "let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle"
                sprintf "                                         %s" inputsStr
                sprintf "                                         %s" paramNamesStr
                sprintf "                                         %s" paramValuesStr
                sprintf "outputs |> Array.map (fun h -> new NDArray(h))"
            ]
        else
            [
            (* OLD
                sprintf "let creator = AtomicSymbolCreator.FromName \"%s\"" x.AtomicSymbolInfo.Name
                sprintf "let symbol = MXSymbol.createAtomicSymbol creator.AtomicSymbolCreatorHandle"
                sprintf "                                         %s" paramNamesStr
                sprintf "                                         %s" paramValuesStr
                sprintf "MXSymbol.compose symbol null %s" inputNamesStr
                //sprintf "                             (%s |> Array.filter (fun x -> x > 0n))" inputsStr
                sprintf "                             %s" inputsStr
                sprintf "Symbol(symbol)"
            *)
                sprintf "let creator = AtomicSymbolCreator.FromName \"%s\"" x.AtomicSymbolInfo.Name
                sprintf "new Symbol(Some creator,"
                sprintf "           %s," paramNamesStr
                sprintf "           %s," paramValuesStr
                sprintf "           %s," inputNamesStr
                sprintf "           %s)" inputsStr
            ]
    let defineInto = 
        let name = x.Name
        if args.Length = 0 then 
            [sprintf "static member %s(outputArray : NDArray seq) =" name]
        elif args.Length < 5 then 
            let argStr = args |> String.concat ", "
            [sprintf "static member %s(outputArray : NDArray seq, %s) =" name argStr]
        else
            let dstr = sprintf "static member %s(" name
            let indent = String.replicate dstr.Length " "
            [
                dstr + "outputArray : NDArray seq" + ", "
                yield! args.[0 .. args.Length - 2] |> Seq.map (fun x -> indent + x + ", ")
                indent + args.[args.Length - 1] + ") ="
            ]
    let invokeInto = 
        [
            sprintf "let creator = AtomicSymbolCreator.FromName \"%s\"" x.AtomicSymbolInfo.Name
            sprintf "let names = %s" paramNamesStr
            sprintf "let vals = %s" paramValuesStr
            sprintf "let names,vals = (names, vals) ||> Array.zip |> Array.choose (fun (n,v) -> if isNull v then None else Some(n,v)) |> Array.unzip"
            sprintf "let outputs = MXNDArray.imperativeInvokeInto creator.AtomicSymbolCreatorHandle"
            sprintf "                                             %s" inputsStr
            sprintf "                                             (outputArray |> Seq.map (fun x -> x.NDArrayHandle.UnsafeHandle) |> Seq.toArray)"
            sprintf "                                             names" //paramNamesStr
            sprintf "                                             vals" //paramValuesStr
            sprintf "()"
        ]
        
    [
        yield! indent 1 x.Doc
        yield! indent 1 (x.Args |> Array.collect (fun x -> x.Doc))
        yield! indent 1 define 
        yield! indent 2 invoke 
        if ndarray then 
            yield! indent 1 x.Doc
            yield! indent 1 ["/// <param name = \"outputArray\">Array of NDArray for outputs</param>"]
            yield! indent 1 (x.Args |> Array.collect (fun x -> x.Doc))
            yield! indent 1 defineInto 
            yield! indent 2 invokeInto 
            
    ]

let toCode (x : ProcessedAtomicSymbol) =
    let ndArray = 
        match x.SymbolOrNDArray with 
        | NDArray | SymbolOrNDArray | ManySymbolOrNDArray -> true 
        | _ -> false
    let symbol = 
        match x.SymbolOrNDArray with 
        | Symbol | SymbolOrNDArray | ManySymbolOrNDArray -> true 
        | _ -> false
    [
        if not ndArray && not symbol then 
            yield! toCodeTarget true true x 
            yield! toCodeTarget true false x 
            ()
        else    
            if ndArray then 
                yield! toCodeTarget false true x
            if symbol then 
                yield! toCodeTarget false false x
    ]

let definedTypeToCode (a : ProcessedArg) =
    match a.DefinedType with 
    | None -> []
    | Some(t) -> 
        let toString = 
            [
                "override x.ToString() ="
                yield! indent 1 
                    [
                        yield "match x with"
                        yield! indent 1
                            [
                                for (name, str) in t.Cases do 
                                    sprintf "| %s -> \"%s\""  name str
                            ]
                    ]
            ]
        [
            yield sprintf "type %s = " t.Name
            yield! t.Cases |> Seq.map fst |> Seq.map (fun x -> "    | " + x)
            yield! indent 1 toString
        ]
        
            
// **************************** remove underscores from param names *******************************

Mappings.Modify(fun (x : ProcessedArg) -> {x with Name = toParamName x.Name} |> argDoc)


// **************************** (non-negative) types *******************************
// REVIEW: Should we check and throw on negatives?

Mappings.Modify(fun (x : ProcessedArg) -> 
    match x.TypeString with 
    | "int (non-negative)" -> {x with TypeString = "int" }   
    | "long (non-negative)"-> {x with TypeString = "int64"}   
    | _ -> x
    )
       
       
// **************************** _histogram *******************************
Mappings.Modify(fun (x : ProcessedAtomicSymbol list) ->
    match x with 
    | [h] when h.AtomicSymbolInfo.Name = "_histogram" -> 
        let replaceArg f = 
            h.Args 
            |> Array.map 
                (fun a ->
                    if a.Arg.ArgumentInfo.Name = "range" then 
                        f a
                    else 
                        a
                )
        let vtupleDef = 
            {h with 
                Args = replaceArg (fun a -> 
                    {a with 
                        TypeString = "struct(float*float)"
                        DefaultMode = Some(ReplaceNull "\"None\"")
                    })
            }
        let tupleDef = 
            {h with 
                Args = replaceArg (fun a -> 
                    {a with 
                        TypeString = "float*float"
                        DefaultMode = Some(ReplaceNull "\"None\"")
                    })
            }
        [
            vtupleDef
            tupleDef
        ]
    | _ -> x
    )




// **************************** SpatialTransformer *******************************

Mappings.Modify(fun (x : ProcessedAtomicSymbol) -> 
    if x.AtomicSymbolInfo.Name = "SpatialTransformer" then 
        x.Args
        |> Seq.iter 
            (fun a ->
                // We'll check if the definition is consistent with this transform
                if (a.Arg.ArgumentInfo.Name = "transform_type" && a.Arg.ArgumentInfo.TypeInfo <> "{'affine'}, required") ||
                   (a.Arg.ArgumentInfo.Name = "sampler_type" && a.Arg.ArgumentInfo.TypeInfo <> "{'bilinear'}, required") then 
                    failwith "SpatialTransformer transform may no longer be valid"
            )
        let args = 
            x.Args 
            |> Array.map 
                (fun a ->
                    match a.Arg.ArgumentInfo.Name with 
                    | "transform_type" -> 
                        {a with 
                            CodeGenerator = ConstantArg "\"affine\""
                        }
                    | "sampler_type" -> 
                        {a with 
                            CodeGenerator = ConstantArg "\"bilinear\""
                        }
                    | _ -> a
                )
        {x with Args = args}
    else
        x
)

// **************************** Sort parameters so required args appear first *******************************

Mappings.Modify(fun (x : ProcessedAtomicSymbol) -> 
    //if x.AtomicSymbolInfo.Name = "RNN" then
        let optional, required = x.Args |> Array.partition (fun x -> x.DefaultMode.IsSome) 
        { x  with 
            Args = Array.append required optional
        }
    //else    
    //    x
    )


// **************************** _image_normalize *******************************
// Blank type for mean and str

Mappings.Modify(fun (x : ProcessedArg) -> 
    if x.Arg.AtomicSymbolInfo.Name = "_image_normalize" then
        match x.Arg.ArgumentInfo.Name with 
        | "mean" | "std" -> {x with TypeString = "double []"}
        | _ -> x
    else    
        x
    )

// **************************** _random_randint *******************************
// Blank type for some integer args

Mappings.Modify(fun (x : ProcessedArg) -> 
    if x.Arg.AtomicSymbolInfo.Name = "_random_randint" then
        match x.Arg.ArgumentInfo.Name with 
        | "low" | "high" -> {x with TypeString = "int"}
        | _ -> x
    else    
        x
    )


// **************************** Slice ops *******************************
// begin end names 

Mappings.Modify(fun (x : ProcessedArg) -> 
    match x.Arg.AtomicSymbolInfo.Name with 
    | "slice"
    | "_slice_assign"
    | "_slice_assign_scalar"
    | "slice_axis" -> 
        match x.Arg.ArgumentInfo.Name with 
        | "begin" -> {x with Name = "sliceBegin"} |> argDoc
        | "end" -> {x with Name = "sliceEnd"} |> argDoc
        | _ -> x
    | _ -> x
    )

// **************************** Eye *******************************
// Blank type for some integer args

Mappings.Modify(fun (x : ProcessedArg) -> 
    if x.Arg.AtomicSymbolInfo.Name = "_eye" then
        match x.Arg.ArgumentInfo.Name with 
        | "N" | "M" | "k" -> {x with TypeString = "int"}
        | _ -> x
    else    
        x
    )



// **************************** _contrib_dgl_csr_neighbor_uniform_sample *******************************
// **************************** _contrib_dgl_csr_neighbor_non_uniform_sample *******************************
// Blank type for some integer args

Mappings.Modify(fun (x : ProcessedArg) -> 
    if x.Arg.AtomicSymbolInfo.Name = "_contrib_dgl_csr_neighbor_uniform_sample" ||
       x.Arg.AtomicSymbolInfo.Name = "_contrib_dgl_csr_neighbor_non_uniform_sample" then
        match x.Arg.ArgumentInfo.Name with 
        | "num_hops" 
        | "num_neighbor" 
        | "max_num_vertices" -> 
            {x with TypeString = "int"}
        | _ -> x
    else    
        x
    )


// **************************** _contrib_dgl_graph_compact *******************************
// graph_sizes has no type

Mappings.Modify(fun (x : ProcessedArg) -> 
    if x.Arg.AtomicSymbolInfo.Name = "_contrib_dgl_graph_compact" then
        match x.Arg.ArgumentInfo.Name with 
        | "graph_sizes" -> {x with TypeString = "int"}
        | _ -> x
    else    
        x
    )


// **************************** _contrib_index_array *******************************
// Shape or None `axes` parameter



Mappings.Modify(fun (x : ProcessedArg) -> 
    if x.TypeString = "Shape or None" then 
        {x with 
            TypeString = "int seq"
            DefaultMode = Some(DefaultMode.IgnoreNull)
        }   
    else    
        x
    )


// **************************** _cvcopyMakeBorder *******************************

Mappings.Modify(fun (x : ProcessedArg) -> 
    if x.Arg.AtomicSymbolInfo.Name = "_cvcopyMakeBorder"  then
        match x.Name with 
        | "type" -> {x with Name = "fillingType"} |> argDoc
        | "values" -> {x with TypeString = "string (*REVIEW: What's the type here?*)"} 
        | _ -> x
    else    
        x
    )

// **************************** _contrib_ROIAlign *******************************

// output_size can be either a size (int) or heigh(int)/width(int)
Mappings.Modify(fun (x : ProcessedAtomicSymbol list) ->
    match x with 
    | [h] when h.AtomicSymbolInfo.Name = "_contrib_ROIAlign" -> 
        let replaceArg f = 
            h.Args 
            |> Array.map 
                (fun a ->
                    if a.Arg.ArgumentInfo.Name = "pooled_size" then 
                        f a
                    else 
                        a
                )
        let sizeDef = 
            {h with 
                Args = replaceArg (fun a -> 
                    {a with 
                        TypeString = "int seq"
                    })
            }
        let heighWidthDef = 
            let args = 
                replaceArg (fun a -> 
                    {a with 
                        TypeString = "int"
                        Name = "height"
                        Doc = [|"""/// <param name="height">ROI Align output roi feature map height</param>"""|]
                        CodeGenerator = ValueString("(height, width).ToString()")
                    }
                )
            let args = 
                [|
                    for a in args do 
                        yield a
                        if a.Name = "height" then 
                            yield
                                {a with 
                                    TypeString = "int"
                                    Name = "width"
                                    Doc = [|"""/// <param name="width">ROI Align output roi feature map width</param>"""|]
                                    CodeGenerator = SkipArg
                                }
                |]
            {h with 
                Args = args}
        [
            sizeDef
            heighWidthDef
        ]
    | _ -> x
    )



// **************************** _image_resize *******************************


// output_size can be either a size (int) or heigh(int)/width(int)
Mappings.Modify(fun (x : ProcessedAtomicSymbol list) ->
    match x with 
    | [h] when h.AtomicSymbolInfo.Name = "_image_resize" -> 
        let replaceArg f = 
            h.Args 
            |> Array.map 
                (fun a ->
                    if a.Arg.ArgumentInfo.Name = "size" then 
                        f a
                    else 
                        a
                )
        let sizeDef = 
            {h with 
                Args = replaceArg (fun a -> 
                    {a with 
                        TypeString = "int"
                        Name = "size"
                        Doc = [|"""/// <param name="outputSize">Size of new image</param>"""|]
                    })
            }
        let heighWidthDef = 
            let args = 
                replaceArg (fun a -> 
                    {a with 
                        TypeString = "int"
                        Name = "height"
                        Doc = [|"""/// <param name="height">Height of new image</param>"""|]
                        CodeGenerator = ValueString("(height, width).ToString()")
                    }
                )
            let args = 
                [|
                    for a in args do 
                        yield a
                        if a.Name = "height" then 
                            yield
                                {a with 
                                    TypeString = "int"
                                    Name = "width"
                                    Doc = [|"""/// <param name="width">ROI Align output roi feature map width</param>"""|]
                                    CodeGenerator = SkipArg
                                }
                |]
            {h with 
                Args = args}
        [
            sizeDef
            heighWidthDef
        ]
    | _ -> x
    )

// **************************** _contrib_AdaptiveAvgPooling2D *******************************


// output_size can be either a size (int) or heigh(int)/width(int)
Mappings.Modify(fun (x : ProcessedAtomicSymbol list) ->
    match x with 
    | [h] when h.AtomicSymbolInfo.Name = "_contrib_AdaptiveAvgPooling2D" -> 
        let replaceArg f = 
            h.Args 
            |> Array.map 
                (fun a ->
                    if a.Arg.ArgumentInfo.Name = "output_size" then 
                        f a
                    else 
                        a
                )
        let sizeDef = 
            {h with 
                Args = replaceArg (fun a -> 
                    {a with 
                        TypeString = "int"
                        Name = "outputSize"
                        Doc = [|"""/// <param name="outputSize">output size</param>"""|]
                    })
            }
        let heighWidthDef = 
            let args = 
                replaceArg (fun a -> 
                    {a with 
                        TypeString = "int"
                        Name = "height"
                        Doc = [|"""/// <param name="height">height</param>"""|]
                        CodeGenerator = ValueString("(height, width).ToString()")
                    }
                )
            let args = 
                [|
                    yield! args
                    {args.[args.Length - 1] with 
                        TypeString = "int"
                        Name = "width"
                        Doc = [|"""/// <param name="width">width</param>"""|]
                        CodeGenerator = SkipArg
                    }
                |]
            {h with 
                Args = args}
        [
            sizeDef
            heighWidthDef
        ]
    | _ -> x
    )

// **************************** "or None" handling *******************************


Mappings.Modify(fun (l : ProcessedAtomicSymbol list) ->
    l
    |> List.collect
        (fun x ->
            if x.Args |> Seq.exists (fun a -> a.TypeString.EndsWith "or None") then 
                let nullable = 
                    {x with 
                        Args = 
                            x.Args
                            |> Array.map    
                                (fun a ->
                                    match a.TypeString with 
                                    | "boolean or None" -> 
                                        {a with 
                                            TypeString = "bool Nullable"
                                            DefaultMode = Some IgnoreNull
                                        }
                                    | "int or None" -> 
                                        {a with 
                                            TypeString = "int Nullable"
                                            DefaultMode = Some IgnoreNull
                                        }
                                    | "double or None"
                                    | "float or None" -> 
                                        {a with 
                                            TypeString = "float Nullable"
                                            DefaultMode = Some IgnoreNull
                                        }
                                    | _ -> a
                                )
                        }
                let optional = 
                    {x with 
                        Args = 
                            x.Args
                            |> Array.map    
                                (fun a ->
                                    match a.TypeString with 
                                    | "boolean or None" -> 
                                        {a with 
                                            TypeString = "bool"
                                            DefaultMode = Some(ReplaceOptionWithString "\"None\"")
                                        }
                                    | "int or None" -> 
                                        {a with 
                                            TypeString = "int"
                                            DefaultMode = Some(ReplaceOptionWithString "\"None\"")
                                        }
                                    | "double or None"
                                    | "float or None" -> 
                                        {a with 
                                            TypeString = "float"
                                            DefaultMode = Some(ReplaceOptionWithString "\"None\"")
                                        }
                                    | _ -> a
                                )
                            |> Array.map 
                                (fun a ->
                                    match a.DefaultMode with 
                                    | Some(UseAttr d) 
                                    | Some(ReplaceNull d) ->
                                        {a with DefaultMode = Some(ReplaceOptionWithString (ensureQuote d))}
                                    | _ -> a
                                )
                        }
                [
                    nullable 
                    optional
                ]
            else
                [x]

        )
)

// **************************** Simple Shaple(tuple) handling *******************************

Mappings.Modify(fun (x : ProcessedArg) -> 
    if x.TypeString = "Shape(tuple)" then 
        match x.Arg.AtomicSymbolInfo.Name with 
        //| "Pooling"
        //| "Deconvolution"
        //| "Dropout"
        //| "Convolution" ->
        | _ ->
            {x with 
                TypeString = "int seq"
            }   
        //| _ -> x
    else    
        x
    )
       

let processDefinedType (t : UnionType) (arg : ProcessedArg) =
    let (|Name|_|) name (t : UnionType,_) = if t.Name = name then Some() else None
    let (|Cases|_|) cases (t : UnionType,_) = 
        let cases = cases |> Seq.toArray |> Array.sort
        let tcases = t.Cases |> Seq.map fst |> Seq.toArray
        if tcases = cases then Some() else None
    let newt = 
        match t, arg with 
        | (Name "OutFormat" | Name "InFormat" | Name "Format") & Cases ["Center"; "Corner"] -> 
            {
                Name = "Format"
                Cases = 
                    [|
                        "Center", "center"
                        "Corner", "corner"
                    |]
                TypeString = ""
            } 
        | Cases ["Float16"; "Float32"; "Float64"; "Int32"; "Int64"; "Int8"; "Uint8"] ->  
            {
                Name = "IntOrFloatDType"
                Cases = 
                    [|
                        "Float16", "float16"
                        "Float32", "float32"
                        "Float64", "float64"
                        "Int32", "int32"
                        "Int64", "int64"
                        "Int8", "int8"
                        "UInt8", "uint8"
                    |]
                TypeString = ""
            } 
            
        | Cases ["Float16"; "Float32"; "Float64"] -> 
            {
                Name = "FloatDType"
                Cases = 
                    [|
                        "Float16", "float16"
                        "Float32", "float32"
                        "Float64", "float64"
                    |]
                TypeString = ""
            } 
        | Name "ActType" & (_,a) when a.Arg.AtomicSymbolInfo.Name = "LeakyReLU" -> 
            {t with Name = "LeakyReLUType"}
        | (Name "Mode" | Name "OutType" | Name "Dtype" | Name "Layout" | Name "TransformType") & (_,{ProcessedAtomicSymbol = Some pas}) -> 
            {t with Name = pas.Name + t.Name}
        | _ -> t
    { arg with 
        DefinedType = Some newt
        TypeString = newt.Name
    }
        

Mappings.Modify(fun (x : ProcessedAtomicSymbol) ->
        { x with 
            Args = 
                x.Args 
                |> Array.map 
                    (fun a -> 
                        match a.DefinedType with 
                        | Some t -> processDefinedType t a
                        | None -> a
                    )
        }
    )

let processed = 
    creatorInfos
    |> Array.choose     
        (fun x ->
            try 
                let r = 
                    let y = mapAtomicSymbolInfoBase x
                    Ok y
                Some(x,r)
            with 
            | e -> Some(x, Error(x,e))
        )
    |> Array.map
        (fun result ->
            match result with 
            | x, Ok(y) -> 
                let memberDefs = y |> List.map (fun i -> i, toCode i)
                let typeDefs = 
                    y
                    |> Seq.collect (fun x -> x.Args |> Seq.choose (fun x -> x.DefinedType |> Option.map (fun d -> x, d)))
                    |> Seq.distinctBy snd
                    |> Seq.map 
                        (fun (a,t) ->
                            a, definedTypeToCode a
                        )
                    |> Seq.filter (fun (_,l) -> l |> List.isEmpty |> not)
                    |> Seq.toList
                {
                    AtomicSymbolInfo = x
                    MemberDefinitions = memberDefs
                    TypeDefinitions = typeDefs
                }
                |> Ok
            | _, Error(x,e) -> Error(x,e)
        )


// ******************************** Skip *************************************
let skipped = 
    [
        // Not sure how to handle NDArray-or-Symbol[] yet
        //"_CachedOp"
        //"multi_all_finite"
        //"_contrib_dgl_csr_neighbor_uniform_sample"
        /////////////////////////////////////////////////
        "_arange"
        "_linspace"
        "Pooling_v1" //DEPRECATED
        "Crop" //DEPRECATED
        "_foreach" // Need to figure out types of args
        "_cond" // Need to figure out types of args
        "_while_loop" // Need to figure out types of args
        "_image_adjust_lighting" // Need to figure out types of args
        "multi_sgd_update" // Need to figure out types of args
        "multi_sgd_mom_update" // Need to figure out types of args
        "multi_mp_sgd_update" // Need to figure out types of args
        "multi_mp_sgd_mom_update" // Need to figure out types of args
        "_contrib_MultiProposal" // Need to figure out types of args
        "_contrib_MultiBoxDetection" // Need to figure out types of args
        "_contrib_MultiBoxPrior" // Need to figure out types of args
        "_contrib_Proposal" // Need to figure out types of args
        "_contrib_MultiBoxTarget" // Need to figure out types of args
        "_NDArray" // ptr type
        "_Native" // ptr type
    ] |> Set.ofSeq


let generatedLines = 
    let definedTypes = HashSet()
    let types = ResizeArray()
    let members = ResizeArray()
    let errors = ResizeArray()
    let skip = ResizeArray()
    for b in processed do 
        match b with 
        | Error(x,e) -> 
            let code = 
                [
                    ""
                    "// ********************************************************************************************************"
                    sprintf "// EXCEPTION" 
                    sprintf "// %s" e.Message
                    yield! (sprintf "%A" x) |> splitLines |> Array.map (fun x -> "// " + x)
                    "// ********************************************************************************************************"
                    ""
                ]
            if skipped.Contains x.Name then 
                skip.Add code
            else
                errors.Add 
                    [
                        yield "// ========================================== Not Skipped =========================================="
                        yield! code
                    ]
        | Ok(cb) when skipped.Contains cb.AtomicSymbolInfo.Name->     
            cb.TypeDefinitions
            |> List.map snd
            |> List.map comment
            |> List.iter (skip.Add)
            cb.MemberDefinitions
            |> List.map snd 
            |> List.map comment
            |> List.iter (skip.Add)
        | Ok(cb) ->
            cb.MemberDefinitions
            |> List.map snd 
            |> List.iter (members.Add)
            cb.TypeDefinitions
            |> List.map snd
            |> List.filter definedTypes.Add
            |> List.iter (types.Add)
    let concatWith sep (lines : string list list) =
        if lines.IsEmpty then [] 
        else 
            [
                yield lines.Head
                for a in lines.Tail do
                    yield sep
                    yield a
            ] |> List.concat
    let breakBlocks x = x |> Seq.toList |> concatWith [""]
        
    [
        """namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop
"""
        yield! types |> breakBlocks
        """
type Operators() =  
"""  
        yield! members |> breakBlocks
        yield! skip |> breakBlocks
        yield! errors |> breakBlocks
    ]
            

System.IO.File.WriteAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__,"operators.fs"),  generatedLines)
