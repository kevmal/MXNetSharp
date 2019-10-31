
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Diagnostics

#load "capi.fs"
#load "cpredictapi.fs"
#load "cnnvmapi.fs"
#load "coretypes.fs"
#load "interop.fs"
#load "atomicsymbol.fs"

open MXNetSharp
open MXNetSharp.Interop
open System

let opOutCount = 
    System.IO.File.ReadAllLines(IO.Path.Combine(__SOURCE_DIRECTORY__, "OpOutCount.txt"))
    |> Seq.map (fun x -> let a = x.Split ',' in a.[0], int a.[1])
    |> dict

let lookupOpCount op = 
    let scc, v = opOutCount.TryGetValue op
    if scc then 
        v
    else 
        let c = AtomicSymbolCreator.FromName op
        let scc,v = opOutCount.TryGetValue c.Name
        if scc then 
            v
        else
            -2

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
    
let trimQuotes (str : string) = str.Trim [| '''; '"'|]
//let parseInt (str : string) = str.Trim ''' |> int
//let parseInt64 (str : string) = str.Trim ''' |> int64
//let parseInt64 (str : string) = str.Trim ''' |> int64
let parseBoolStr (str) = 
    match trimQuotes str with 
    | "true"
    | "1" -> true
    | "false"
    | "0" -> false
    | x -> failwithf "unhandled bool str %A" x
let inline parseSeq f (str : string) = 
    match str with 
    | "[]" -> Array.empty
    | _ -> 
        str.Trim([|'['; ']'|]).Split(',')
        |> Seq.map f
        |> Seq.toArray

type UnionType = 
    {
        TypeString : string
        Name : string
        Cases : (string*string) []
    }


let registeredTypes = 
    [
        "_npi_hanning", "M", "int"
        "_npi_hamming", "M", "int"
        "_npi_blackman", "M", "int"
        "_npi_choice", "a", "int64"
        "_npi_choice", "size", "Shape(tuple)"
        "_histogram", "range", "Shape(tuple)" //TODO: not technically correct
    ]
    |> List.map (fun (n,p,t) -> (n,p), t)
    |> dict


type TranslatedType = 
    | Int of int option
    | Int64 of int64 option
    | Double of double option
    | Float32 of double option
    | Bool of bool option
    | String of string option
    | Context of string option
    | StringChoices of UnionType*string option
    | TupleInt of int [] option
    | TupleFloat of double [] option
    | TupleInt64 of int64 [] option
    | Opt of TranslatedType
    member x.TargetTypeString(fsOption, tupleTypeString )= 
        match x with 
        | Int _ -> "int"
        | Int64 _ -> "int64"
        | Double _ -> "double"
        | Float32 _ -> "double"
        | String _ -> "string"
        | Bool _ -> "bool"
        | Context _ -> "Context"
        | StringChoices(t,_) -> t.Name
        | TupleInt _ -> "int " + tupleTypeString
        | TupleFloat _ -> "double " + tupleTypeString
        | TupleInt64 _ -> "int64 " + tupleTypeString
        | Opt t -> (t.TargetTypeString(fsOption, tupleTypeString)) + " option"
    member x.DefaultString = 
        match x with 
        | Int o -> o |> Option.map string
        | Int64 o -> o |> Option.map (fun n -> string n + "L")
        | Double o -> o |> Option.map (fun n -> n.ToString("0.0###############"))
        | Float32 o -> o |> Option.map (fun n -> n.ToString("0.0###############"))
        | String o -> o |> Option.map quote
        | Bool o -> o |> Option.map (fun n -> if n then "true" else "false")
        | Context o -> o |> Option.map string
        | StringChoices(t,o) -> o |> Option.map (toCaseName)
        | TupleInt o -> o |> Option.map (fun n -> n |> Array.map string |> String.concat "; " |> sprintf "[|%s|]")
        | TupleFloat o -> o |> Option.map (fun n -> n |> Array.map (fun i -> i.ToString("0.0###############")) |> String.concat "; " |> sprintf "[|%s|]")
        | TupleInt64 o -> o |> Option.map (fun n -> n |> Array.map (fun i -> string i + "L") |> String.concat "; " |> sprintf "[|%s|]")
        | Opt t -> 
            match t.DefaultString with 
            | None -> Some "None"
            | Some str -> Some (sprintf "Some(%s)" str)
    static member FromArgumentInfo(s : AtomicSymbolInfo, arg : ArgumentInfo) =
        let typeInfo = arg.TypeInfo
        printfn "FromArgumentInfo %A" typeInfo
        let t,ro = tpinfo typeInfo
        let ro = 
            match ro with
            | Required -> None
            | Optional s -> Some s
        let t = 
            if t = "" then 
                let scc,v = registeredTypes.TryGetValue((s.Name, arg.Name))
                if scc then 
                    v
                else 
                    failwithf "Blank type %s %s %A" s.Name arg.Name typeInfo
            else t
        let defaultIsNone str =
            match str with
            | Some "None"
            | Some "'None'" -> true //Some()
            | _ -> false
        let checkNone str = 
            match str with 
            | Some "None"
            | Some "'None'" -> None 
            | x -> x
        match t with 
        | "boolean or None" -> ro |> checkNone |> Option.map parseBoolStr |> Bool |> Opt
        | "Shape or None" -> ro |> checkNone |> Option.map (parseSeq int) |> TupleInt |> Opt
        | "double or None" -> ro |> checkNone |> Option.map (trimQuotes >> double) |> Double |> Opt
        | "int or None" -> ro |> checkNone |> Option.map (trimQuotes >> int) |> Int |> Opt
        | "float or None" -> ro |> checkNone |> Option.map (trimQuotes >> double) |> Float32 |> Opt
        | "int (non-negative)"
        | "int" -> ro |> Option.map (trimQuotes >> int) |> Int
        | "long"
        | "long (non-negative)"
        | "int64" -> ro |> Option.map (trimQuotes >> int64) |> Int64
        | "double" -> ro |> Option.map (trimQuotes >> double) |> Double
        | "float" -> ro |> Option.map (trimQuotes >> double) |> Float32
        | "boolean" -> ro |> Option.map parseBoolStr |> Bool
        | "string" when arg.Name = "ctx" -> 
            match ro with 
            | Some "" -> Opt (Context None)
            | Some str -> Context(Some str)
            | None -> Context None
        | "string" -> ro |> Option.map (trimQuotes) |> String
        | "tuple of <float>" -> ro |> Option.map (parseSeq double) |> TupleFloat
        | "Shape(tuple)" when defaultIsNone ro -> TupleInt None |> Opt
        | "Shape(tuple)"
        | "tuple of <int>" -> ro |> Option.map (parseSeq int) |> TupleInt
        | "tuple of <long>" -> ro |> Option.map (parseSeq int64) |> TupleInt64
        | t when t.StartsWith "{" -> 
            let cases = t.Trim([|'{'; '}'|]).Split ',' |> Array.map (fun x -> x.Trim())
            if cases |> Array.exists (fun x -> x = "None" || x = "'None'") then 
                let ut = 
                    {  
                        TypeString = t
                        Name = arg.Name |> toCaseName
                        Cases = 
                            cases 
                            |> Array.filter (fun x -> x <> "None" && x <> "'None'") 
                            |> Array.map (fun x -> toCaseName x, x.Trim ''')
                    }
                match ro with 
                | None
                | Some "None"
                | Some "'None'" -> StringChoices(ut, None) |> Opt
                | Some str -> StringChoices(ut, Some (trimQuotes str))
            else
                let ut = 
                    {  
                        TypeString = t
                        Name = arg.Name |> toCaseName
                        Cases = cases |> Array.map (fun x -> toCaseName x, x.Trim ''')
                    }
                StringChoices(ut, ro |> Option.map trimQuotes)
        | p -> 
            failwithf "Unhandled type %A" t
           



type DefaultMode = 
    | UseAttr of string
    | ReplaceNull of string
    | ReplaceOptionWithString of string
    | IgnoreNull
    | IgnoreNone

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
        SymbolTypeDefinitions : (ProcessedAtomicSymbol*(String list)) list
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
                | "long" -> "int64"
                | "tuple of <double>"
                | "tuple of <float>" -> "double seq"
                | "tuple of <long>" -> "int64 seq"
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
                | "long"
                | "long (non-negative)" -> str.Trim ''' |> int64 |> string |> sprintf "%sL" |> UseAttr |> Some
                | "int (non-negative)" 
                | "int" -> str.Trim ''' |> int |> string |> UseAttr |> Some
                | "double"
                | "float" -> str.Trim ''' |> double |> dblString |> UseAttr |> Some
                | "boolean" -> str |> toBoolVal |> UseAttr |> Some
                | "string" -> str.Trim ''' |> quote |> UseAttr |> Some
                | "tuple of <float>"
                | "tuple of <long>"
                | "Shape(tuple)" -> 
                    if str = "None" then 
                        ReplaceNull "null" |> Some
                    else 
                        ReplaceNull (sprintf "\"%s\"" str) |> Some
                | t when t.StartsWith "{" -> 
                    Some(ReplaceNull(str.Replace(''','\"')))
                | p -> 
                    printfn "Warning: Unhandled default %A for type %A" str x.Arg.TypeString
                    None
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
            // In the case of _while_loop, _foreach and _cond we may have specific Symbol args
            | [|Symbol;ManySymbolOrNDArray|] 
            | [|ManySymbolOrNDArray; Symbol|] -> SymbolOrNDArray
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
    | "int seq" -> sprintf "(%s |> Seq.map string |> String.concat \", \" |> sprintf \"[%%s]\")" str 
    //| "bool" -> sprintf "(if %s then \"1\" else \"0\")" str
    | "string" -> str
    | _ -> sprintf "string %s" str


let toNDArrayCode suffix (x : ProcessedAtomicSymbol) =
    if x.AtomicSymbolInfo.Name.StartsWith "_backward_" then [] else
    let args = 
        x.Args 
        |> Seq.filter (fun x -> match x.CodeGenerator with ConstantArg _ -> false | _ -> true)
        |> Seq.filter (fun a -> a.Arg.ArgumentInfo.Name <> x.AtomicSymbolInfo.KeyVarNumArgs)
        |> Seq.map 
            (fun x -> 
                let t = 
                    match x.SymbolOrNDArray with 
                    | Some ManySymbolOrNDArray -> "NDArray[]" 
                    | Some _ -> "NDArray"
                    | _ -> x.TypeString
                match x.DefaultMode with 
                | Some (ReplaceOptionWithString _) -> 
                    sprintf "[<Optional>] ?%s : %s" x.Name t
                | Some (ReplaceNull _)
                | Some IgnoreNull -> sprintf "[<Optional>] %s : %s" x.Name t
                | Some IgnoreNone -> sprintf "[<Optional>] ?%s : %s" x.Name t
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
                x.Name + "NDArray"
            else 
                x.Name
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
            x.Args 
            |> Array.choose 
                (fun x ->
                    match x.SymbolOrNDArray with 
                    | Some NDArray | Some SymbolOrNDArray -> Some(quote x.Name)
                    | Some ManySymbolOrNDArray -> Some (sprintf "yield! %s |> Array.mapi (fun i _ -> sprintf \"arg%%d\" i)" x.Name)
                    | _ -> None
                )
            |> arr
    let inputsStr = 
        let handle x = sprintf "%s.NDArrayHandle.UnsafeHandle" x
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
        x.Args 
        |> Array.choose 
            (fun x ->
                match x.SymbolOrNDArray with 
                | Some NDArray | Some SymbolOrNDArray -> Choice1Of2(handle x.Name) |> Some
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
                | None when a.Arg.ArgumentInfo.Name = x.AtomicSymbolInfo.KeyVarNumArgs -> 
                    match x.Args |> Seq.tryFind (fun a -> match a.SymbolOrNDArray with Some(ManySymbolOrNDArray) -> true | _ -> false) with 
                    | Some (arrArg) -> 
                        Some (sprintf "string %s.Length" arrArg.Name)
                    | None -> //failwithf "Key var num arg with no input of type NDArray-or-Symbol[] %A" x 
                        Some (sprintf "%s (*TODO: this should be the length of the vararg*)" a.Name) //TODO: this needs to go
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
                    | Some(IgnoreNone) -> 
                        valueStr |> Option.map (fun s -> sprintf "(match %s with None -> \"None\" | Some %s -> %s)" a.Name a.Name s)  //TODO: we need to just not pass the arg
                    | _ -> valueStr
                | _ -> None
            ) 
        |> arr
    let invoke = 
        match lookupOpCount x.AtomicSymbolInfo.Name with 
        | c when c > 0 -> 
            [
                sprintf "let creator = AtomicSymbolCreator.FromName \"%s\"" x.AtomicSymbolInfo.Name
                sprintf "let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle"
                sprintf "                                         %s" inputsStr
                sprintf "                                         %s" paramNamesStr
                sprintf "                                         %s" paramValuesStr
                [
                    for i = 0 to c - 1 do 
                        yield sprintf "(new NDArray(outputs.[%d]))" i
                ] 
                |> String.concat ", "
            ]
        | c when c < 0 ->
            if c = -2 then printfn "No out count for %s" x.AtomicSymbolInfo.Name 
            [
                sprintf "let creator = AtomicSymbolCreator.FromName \"%s\"" x.AtomicSymbolInfo.Name
                sprintf "let outputs = MXNDArray.imperativeInvoke creator.AtomicSymbolCreatorHandle"
                sprintf "                                         %s" inputsStr
                sprintf "                                         %s" paramNamesStr
                sprintf "                                         %s" paramValuesStr
                sprintf "outputs |> Array.map (fun h -> new NDArray(h))"
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
        yield! indent 1 x.Doc
        yield! indent 1 ["/// <param name = \"outputArray\">Array of NDArray for outputs</param>"]
        yield! indent 1 (x.Args |> Array.collect (fun x -> x.Doc))
        yield! indent 1 defineInto 
        yield! indent 2 invokeInto 
    ]

let toSymbolCode suffix (h : ProcessedAtomicSymbol) =
    if h.AtomicSymbolInfo.Name.StartsWith "_backward_" then [] else
    let trim (x : string) = x.TrimStart('?')
    let make doc hargs (args : _ list) =
        [
            yield! doc
            yield! hargs 
                   |> Array.filter (fun a -> a.Arg.ArgumentInfo.Name <> "ctx" && a.Arg.ArgumentInfo.Name <> h.AtomicSymbolInfo.KeyVarNumArgs ) 
                   |> Array.collect (fun x -> x.Doc)
            if args.Length = 0 then 
                sprintf "static member %s() =" h.Name
            else
                sprintf "static member %s(%s) =" h.Name (args |> String.concat ", ")
            yield! indent 1 
                [
                    let toparam (x : string) =     
                        let p = x.Replace("[<Optional>] ", "").Split(':').[0].Trim()
                        if not(x.Contains "?") then
                            p
                        else
                            sprintf "%s = %s" p (trim p)
                    sprintf "%s(%s)" h.Name (args |> Seq.map toparam |> String.concat ", ")
                ]
        ]
    let meth = 
        let args = 
            [
                for a in h.Args do 
                    let tp = 
                        match a.SymbolOrNDArray with 
                        | Some ManySymbolOrNDArray -> "Symbol seq"
                        | Some SymbolOrNDArray
                        | Some Symbol -> "Symbol"
                        | _ -> a.TypeString 
                    if a.Arg.ArgumentInfo.Name <> "ctx" && a.Arg.ArgumentInfo.Name <> h.AtomicSymbolInfo.KeyVarNumArgs then 
                        if a.DefaultMode.IsSome then 
                            sprintf "[<Optional>] ?%s : %s" a.Name tp
                        else
                            sprintf "%s : %s" a.Name tp
            ]
        make h.Doc h.Args args
    let ins,ps = 
        h.Args 
        |> Array.partition 
            (fun a ->
                match a.SymbolOrNDArray with 
                | Some ManySymbolOrNDArray 
                | Some SymbolOrNDArray
                | Some Symbol -> true
                | _ -> false
            )
    let req,opt = ps |> Array.filter (fun x -> x.Arg.ArgumentInfo.Name <> "ctx" && x.Arg.ArgumentInfo.Name <> h.AtomicSymbolInfo.KeyVarNumArgs) |> Array.partition (fun a -> a.DefaultMode.IsNone )
    let meth2 = 
        let hargs, args = 
            [
                for a in req do 
                    a, sprintf "%s : %s" a.Name a.TypeString 
                for a in ins do 
                    let tp = 
                        match a.SymbolOrNDArray with 
                        | Some ManySymbolOrNDArray -> "Symbol seq"
                        | Some SymbolOrNDArray
                        | Some Symbol -> "Symbol"
                        | _ -> a.TypeString 
                    a,sprintf "[<Optional>] ?%s : %s" a.Name tp
                for a in opt do 
                    a,sprintf "[<Optional>] ?%s : %s" a.Name a.TypeString 
            ] |> List.unzip
        make h.Doc (List.toArray hargs) args
    let meth3 = 
        if opt.Length = 0 && ins.Length = 1 then 
            match ins.[0].SymbolOrNDArray with 
            | Some ManySymbolOrNDArray ->
                let hargs, args = 
                    [
                        for a in req do 
                            a, sprintf "%s : %s" a.Name a.TypeString 
                        ins.[0], sprintf "[<ParamArray>] %s : Symbol[]" ins.[0].Name
                    ] |> List.unzip
                [
                    yield! h.Doc
                    yield! hargs
                           |> Seq.filter (fun a -> a.Arg.ArgumentInfo.Name <> "ctx" && a.Arg.ArgumentInfo.Name <> h.AtomicSymbolInfo.KeyVarNumArgs ) 
                           |> Seq.collect (fun x -> x.Doc)
                    if args.Length = 0 then 
                        sprintf "static member %s() =" h.Name
                    else
                        sprintf "static member %s(%s) =" h.Name (args |> String.concat ", ")
                    yield! indent 1 
                        [
                            let toparam (x : string) =     
                                let p = x.Replace("[<ParamArray>] ","").Replace("[<Optional>] ", "").Split(':').[0].Trim()
                                if not(x.Contains "?") then
                                    p
                                else
                                    sprintf "%s = %s" p (trim p)
                            sprintf "%s(%s)" h.Name (args |> Seq.map toparam |> String.concat ", ")
                        ]
                ]
            | _ -> []
        else    
            []
    [
        if req.Length = 0 && ins.Length = 1 && meth3.Length > 0 then 
            yield! indent 1 meth3
        else   
            if req.Length = 0 || ins.Length = 0 then 
                yield! indent 1 meth2
            else
                yield! indent 1 meth
                yield! indent 1 meth2
            yield! indent 1 meth3
    ]

let toSymbolTypeCode (x : ProcessedAtomicSymbol list) =
    let h = x |> List.head
    let ndArray = 
        match h.SymbolOrNDArray with 
        | NDArray | SymbolOrNDArray | ManySymbolOrNDArray -> true 
        | _ -> false
    let symbol = 
        match h.SymbolOrNDArray with 
        | Symbol | SymbolOrNDArray | ManySymbolOrNDArray -> true 
        | _ -> false
    // REVIEW: we skip _make_loss in favor of MakeLoss. Is there any reason to have both?
    if (not symbol && ndArray) || h.AtomicSymbolInfo.Name.StartsWith "_backward_" || h.AtomicSymbolInfo.Name = "make_loss" then [] else
    let ctor = 
        let args = 
            [
                for a in h.Args do 
                    let tp = 
                        match a.SymbolOrNDArray with 
                        | Some ManySymbolOrNDArray -> "Symbol seq"
                        | Some SymbolOrNDArray
                        | Some Symbol -> "Symbol"
                        | _ -> a.TypeString 
                    if a.Arg.ArgumentInfo.Name <> "ctx" && a.Arg.ArgumentInfo.Name <> h.AtomicSymbolInfo.KeyVarNumArgs then 
                        if a.DefaultMode.IsSome then 
                            sprintf "[<Optional>] ?%s : %s" a.Name tp
                        else
                            sprintf "%s : %s" a.Name tp
            ]
        [
            yield! h.Doc
            yield! h.Args 
                   |> Array.filter (fun a -> a.Arg.ArgumentInfo.Name <> "ctx" && a.Arg.ArgumentInfo.Name <> h.AtomicSymbolInfo.KeyVarNumArgs ) 
                   |> Array.collect (fun x -> x.Doc)
            if args.Length = 0 then 
                "new() ="
            elif args.Length = 1 then 
                sprintf "new(%s) =" args.[0]
            else
                sprintf "new(%s," args.[0]
                let padding = String.replicate "new(".Length " "
                for a in args.[1.. args.Length - 2] do 
                    sprintf "%s%s," padding a
                sprintf "%s%s) = " padding args.[args.Length - 1]
            yield! indent 1 
                [
                    sprintf "let operatorArguments = "
                    sprintf "    ["
                    yield! indent 2 
                        [
                            for a in h.Args do 
                                if a.Arg.ArgumentInfo.Name = h.AtomicSymbolInfo.KeyVarNumArgs || a.Arg.ArgumentInfo.Name = "ctx" then 
                                    ()
                                elif a.DefaultMode.IsSome then 
                                    sprintf "\"%s\", %s |> Option.map box |> Parameter" a.Arg.ArgumentInfo.Name a.Name 
                                elif a.SymbolOrNDArray.IsSome then 
                                    match a.SymbolOrNDArray.Value with 
                                    | ManySymbolOrNDArray ->
                                        sprintf "\"%s\", VarArg(\"%s\", %s |> Seq.toArray)" a.Arg.ArgumentInfo.Name a.ProcessedAtomicSymbol.Value.AtomicSymbolInfo.KeyVarNumArgs a.Name 
                                    | _ -> sprintf "\"%s\", Input %s" a.Arg.ArgumentInfo.Name a.Name 
                                else
                                    sprintf "\"%s\", Parameter(Some(box %s))" a.Arg.ArgumentInfo.Name a.Name 
                                    
                            
                        ]
                    sprintf "    ]"
                    sprintf "new %s(Arguments<Symbol>(operatorArguments))" h.Name
                ]
        ]
    let ins,ps = 
        h.Args 
        |> Array.partition 
            (fun a ->
                match a.SymbolOrNDArray with 
                | Some ManySymbolOrNDArray 
                | Some SymbolOrNDArray
                | Some Symbol -> true
                | _ -> false
            )
    let req,opt = ps |> Array.filter (fun x -> x.Arg.ArgumentInfo.Name <> "ctx" && x.Arg.ArgumentInfo.Name <> h.AtomicSymbolInfo.KeyVarNumArgs) |> Array.partition (fun a -> a.DefaultMode.IsNone )
    let ctor2 = 
        let hargs, args = 
            [
                for a in req do 
                    a, sprintf "%s : %s" a.Name a.TypeString 
                for a in ins do 
                    let tp = 
                        match a.SymbolOrNDArray with 
                        | Some ManySymbolOrNDArray -> "Symbol seq"
                        | Some SymbolOrNDArray
                        | Some Symbol -> "Symbol"
                        | _ -> a.TypeString 
                    a,sprintf "[<Optional>] ?%s : %s" a.Name tp
                for a in opt do 
                    a,sprintf "[<Optional>] ?%s : %s" a.Name a.TypeString 
            ] |> List.unzip
        [
            yield! h.Doc
            yield! hargs
                   |> Seq.filter (fun a -> a.Arg.ArgumentInfo.Name <> "ctx" && a.Arg.ArgumentInfo.Name <> h.AtomicSymbolInfo.KeyVarNumArgs ) 
                   |> Seq.collect (fun x -> x.Doc)
            if args.Length = 0 then 
                "new() ="
            elif args.Length = 1 then 
                sprintf "new(%s) =" args.[0]
            else
                sprintf "new(%s," args.[0]
                let padding = String.replicate "new(".Length " "
                for a in args.[1.. args.Length - 2] do 
                    sprintf "%s%s," padding a
                sprintf "%s%s) = " padding args.[args.Length - 1]
            yield! indent 1 
                [
                    for a in ins do 
                        match a.SymbolOrNDArray with 
                        | Some ManySymbolOrNDArray -> sprintf "let %s = defaultArg (%s |> Option.map Seq.toArray) Array.empty" a.Name a.Name
                        | _ -> sprintf "let %s = defaultArg %s (new ImplicitVariable() :> Symbol)" a.Name a.Name
                    sprintf "let operatorArguments = "
                    sprintf "    ["
                    yield! indent 2 
                        [
                            for a in h.Args do 
                                if a.Arg.ArgumentInfo.Name = h.AtomicSymbolInfo.KeyVarNumArgs || a.Arg.ArgumentInfo.Name = "ctx" then 
                                    ()
                                elif a.DefaultMode.IsSome then 
                                    sprintf "\"%s\", %s |> Option.map box |> Parameter" a.Arg.ArgumentInfo.Name a.Name 
                                elif a.SymbolOrNDArray.IsSome then 
                                    match a.SymbolOrNDArray.Value with 
                                    | ManySymbolOrNDArray ->
                                        sprintf "\"%s\", VarArg(\"%s\", %s)" a.Arg.ArgumentInfo.Name a.ProcessedAtomicSymbol.Value.AtomicSymbolInfo.KeyVarNumArgs a.Name 
                                    | _ -> sprintf "\"%s\", Input %s" a.Arg.ArgumentInfo.Name a.Name 
                                else
                                    sprintf "\"%s\", Parameter(Some(box %s))" a.Arg.ArgumentInfo.Name a.Name 
                                    
                            
                        ]
                    sprintf "    ]"
                    sprintf "new %s(Arguments<Symbol>(operatorArguments))" h.Name
                ]
        ]
    let ctor3 = 
        if opt.Length = 0 && ins.Length = 1 then 
            match ins.[0].SymbolOrNDArray with 
            | Some ManySymbolOrNDArray ->
                let hargs, args = 
                    [
                        for a in req do 
                            a, sprintf "%s : %s" a.Name a.TypeString 
                        ins.[0], sprintf "[<ParamArray>] %s : Symbol[]" ins.[0].Name
                    ] |> List.unzip
                [
                    yield! h.Doc
                    yield! hargs
                           |> Seq.filter (fun a -> a.Arg.ArgumentInfo.Name <> "ctx" && a.Arg.ArgumentInfo.Name <> h.AtomicSymbolInfo.KeyVarNumArgs ) 
                           |> Seq.collect (fun x -> x.Doc)
                    if args.Length = 0 then 
                        "new() ="
                    elif args.Length = 1 then 
                        sprintf "new(%s) =" args.[0]
                    else
                        sprintf "new(%s," args.[0]
                        let padding = String.replicate "new(".Length " "
                        for a in args.[1.. args.Length - 2] do 
                            sprintf "%s%s," padding a
                        sprintf "%s%s) = " padding args.[args.Length - 1]
                    yield! indent 1 
                        [
                            sprintf "let operatorArguments = "
                            sprintf "    ["
                            yield! indent 2 
                                [
                                    for a in h.Args do 
                                        if a.Arg.ArgumentInfo.Name = h.AtomicSymbolInfo.KeyVarNumArgs || a.Arg.ArgumentInfo.Name = "ctx" then 
                                            ()
                                        elif a.DefaultMode.IsSome then 
                                            sprintf "\"%s\", %s |> Option.map box |> Parameter" a.Arg.ArgumentInfo.Name a.Name 
                                        elif a.SymbolOrNDArray.IsSome then 
                                            match a.SymbolOrNDArray.Value with 
                                            | ManySymbolOrNDArray ->
                                                sprintf "\"%s\", VarArg(\"%s\", %s)" a.Arg.ArgumentInfo.Name a.ProcessedAtomicSymbol.Value.AtomicSymbolInfo.KeyVarNumArgs a.Name 
                                            | _ -> sprintf "\"%s\", Input %s" a.Arg.ArgumentInfo.Name a.Name 
                                        else
                                            sprintf "\"%s\", Parameter(Some(box %s))" a.Arg.ArgumentInfo.Name a.Name 
                                            
                                    
                                ]
                            sprintf "    ]"
                            sprintf "new %s(Arguments<Symbol>(operatorArguments))" h.Name
                        ]
                ]
            | _ -> []
        else    
            []
    let withMethod = 
        let args = 
            [
                for a in h.Args do 
                    let tp = 
                        match a.SymbolOrNDArray with 
                        | Some ManySymbolOrNDArray -> "Symbol seq"
                        | Some SymbolOrNDArray
                        | Some Symbol -> "Symbol"
                        | _ -> a.TypeString 
                    if a.Arg.ArgumentInfo.Name <> "ctx" && a.Arg.ArgumentInfo.Name <> h.AtomicSymbolInfo.KeyVarNumArgs then 
                        sprintf "[<Optional>] ?%s : %s" a.Name tp
            ]
        [
            let docs = 
                [
                    sprintf """/// <summary>Copy %s instance with updated inputs/parameters.</summary>""" h.Name
                    yield! h.Args
                           |> Seq.filter (fun a -> a.Arg.ArgumentInfo.Name <> "ctx" && a.Arg.ArgumentInfo.Name <> h.AtomicSymbolInfo.KeyVarNumArgs ) 
                           |> Seq.collect (fun x -> x.Doc)
                ]
            let call =
                [
                    sprintf "let operatorArguments = "
                    sprintf "    ["
                    yield! indent 2 
                        [
                            for a in h.Args do 
                                if a.Arg.ArgumentInfo.Name = h.AtomicSymbolInfo.KeyVarNumArgs || a.Arg.ArgumentInfo.Name = "ctx" then 
                                    ()
                                elif a.SymbolOrNDArray.IsSome then 
                                    match a.SymbolOrNDArray.Value with 
                                    | ManySymbolOrNDArray ->
                                        sprintf "%s |> Option.map (fun x -> \"%s\", VarArg(\"%s\", Seq.toArray x))" a.Name a.Arg.ArgumentInfo.Name a.ProcessedAtomicSymbol.Value.AtomicSymbolInfo.KeyVarNumArgs
                                    | _ -> sprintf "%s |> Option.map (fun x -> \"%s\", Input x)" a.Name a.Arg.ArgumentInfo.Name
                                else
                                    sprintf "%s |> Option.map (fun x -> \"%s\", Parameter(Some (box x)))" a.Name a.Arg.ArgumentInfo.Name
                        ]
                    sprintf "    ] |> List.choose id"
                    sprintf "new %s(this.OperatorArguments.AddReplace(Arguments<Symbol>(operatorArguments)))" h.Name
                ]
            if args.Length = 0 then 
                ()
            elif args.Length = 1 then 
                yield! docs
                sprintf "member this.With(%s) =" args.[0]
                yield! indent 1 call
            else
                yield! docs
                sprintf "member this.With(%s," args.[0]
                let padding = String.replicate "new(".Length " "
                for a in args.[1.. args.Length - 2] do 
                    sprintf "%s%s," padding a
                sprintf "%s%s) = " padding args.[args.Length - 1]
                yield! indent 1 call
        ]
    
    let stripParam (lines : string []) = 
        lines 
        |> Array.map 
            (fun x ->
                let x = x.Replace("</param>", "")
                let i = x.IndexOf "<param name="
                if i >= 0 then 
                    let j = x.IndexOf('>', i)
                    x.Remove(i, j - i + 1)
                else x
            )
    let props = 
        [
            for a in opt do
                let t = TranslatedType.FromArgumentInfo(a.ProcessedAtomicSymbol.Value.AtomicSymbolInfo, a.Arg.ArgumentInfo)
                sprintf "/// Default value for %s" (capitalize a.Name)
                yield! (stripParam a.Doc)
                match t with 
                | Opt(StringChoices _) ->
                    match t.DefaultString.Value with 
                    | "None" -> sprintf "static member %sDefault : %s option = None" (capitalize a.Name) a.TypeString
                    | _ -> sprintf "static member %sDefault : %s option = %s.%s" (capitalize a.Name) a.TypeString a.TypeString t.DefaultString.Value
                | StringChoices _ ->
                    sprintf "static member %sDefault : %s = %s.%s" (capitalize a.Name) a.TypeString a.TypeString t.DefaultString.Value
                | _ -> sprintf "static member %sDefault : %s = %s" (capitalize a.Name) (t.TargetTypeString(true, "[]")) t.DefaultString.Value
            for a in ins do 
                yield! (stripParam a.Doc)
                match a.SymbolOrNDArray with
                | Some ManySymbolOrNDArray -> 
                    sprintf "member __.%s = operatorArguments.GetVarArg \"%s\"" (capitalize a.Name) a.Arg.ArgumentInfo.Name
                | _ ->
                    sprintf "member __.%s = operatorArguments.GetInput \"%s\"" (capitalize a.Name) a.Arg.ArgumentInfo.Name
            for a in req do 
                yield! (stripParam a.Doc)
                sprintf "member __.%s : %s = match operatorArguments.GetParameter \"%s\" with Some(v) -> unbox v | None -> failwithf \"Required parameter %s is missing\"" (capitalize a.Name) (a.TypeString) a.Arg.ArgumentInfo.Name a.Arg.ArgumentInfo.Name
            for a in opt do 
                yield! (stripParam a.Doc)
                sprintf "member __.%s = operatorArguments.GetParameter(\"%s\", %s.%sDefault)" (capitalize a.Name) a.Arg.ArgumentInfo.Name h.Name (capitalize a.Name)

        ]
    [
        sprintf "type %s private (operatorArguments) = " h.Name
        sprintf "    inherit SymbolOperator(\"%s\", operatorArguments)" h.AtomicSymbolInfo.Name
        sprintf "    static member CreateFromArguments(args : Arguments<Symbol>) = new %s(args)" h.Name
        sprintf "    override this.WithArguments(args : Arguments<Symbol>) = new %s(this.OperatorArguments.AddReplace(args)) :> Symbol" h.Name
        if req.Length = 0 && ins.Length = 1 && ctor3.Length > 0 then 
            yield! indent 1 ctor3
        else   
            if req.Length = 0 || ins.Length = 0 then 
                yield! indent 1 ctor2
            else
                yield! indent 1 ctor
                yield! indent 1 ctor2
            yield! indent 1 ctor3
        yield! indent 1 props
        yield! indent 1 withMethod
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
        [
            {h with 
                Args = replaceArg (fun a -> 
                    {a with 
                        TypeString = "float seq"
                        DefaultMode = Some(ReplaceNull "\"None\"")
                    })
            }
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

// **************************** _npi_multinomial *******************************
// Blank type for arg pvals


Mappings.Modify(fun (x : ProcessedArg) -> 
    if x.Arg.AtomicSymbolInfo.Name = "_npi_multinomial" then
        match x.Arg.ArgumentInfo.Name with 
        | "pvals" -> {x with TypeString = "double seq"}
        | _ -> x
    else    
        x
    )

// **************************** _npi_blackman *******************************
// **************************** _npi_hanning *******************************
// **************************** _npi_hamming *******************************
// Blank type for arg M


Mappings.Modify(fun (x : ProcessedArg) -> 
    if x.Arg.AtomicSymbolInfo.Name = "_npi_hanning" ||
       x.Arg.AtomicSymbolInfo.Name = "_npi_hamming" ||
       x.Arg.AtomicSymbolInfo.Name = "_npi_blackman" then
        match x.Arg.ArgumentInfo.Name with 
        | "M" -> {x with TypeString = "int"}
        | _ -> x
    else    
        x
    )

    
// **************************** _npi_choice *******************************
// Blank type for arg a and size


Mappings.Modify(fun (x : ProcessedArg) -> 
    if x.Arg.AtomicSymbolInfo.Name = "_npi_choice" then
        match x.Arg.ArgumentInfo.Name with 
        | "a" -> {x with TypeString = "int64"}
        | "size" -> {x with TypeString = "int seq"}
        | _ -> x
    else    
        x
    )
// **************************** _cvcopyMakeBorder *******************************

Mappings.Modify(fun (x : ProcessedArg) -> 
    if x.Arg.AtomicSymbolInfo.Name = "_cvcopyMakeBorder"  then
        match x.Name with 
        | "type" -> {x with Name = "fillingType"} |> argDoc
        | _ -> x
    else    
        x
    )

// **************************** "or None" handling *******************************


Mappings.Modify(fun (l : ProcessedAtomicSymbol list) ->
    l
    |> List.collect
        (fun x ->
            if x.Args |> Seq.exists (fun a -> a.TypeString.EndsWith "or None") then 
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
                                            DefaultMode = Some IgnoreNull
                                        }
                                    | "int or None" -> 
                                        {a with 
                                            TypeString = "int"
                                            DefaultMode = Some IgnoreNull
                                        }
                                    | "double or None"
                                    | "float or None" -> 
                                        {a with 
                                            TypeString = "float"
                                            DefaultMode = Some IgnoreNull
                                        }
                                    | "Shape or None" -> 
                                        {a with 
                                            TypeString = "int seq"
                                            DefaultMode = Some IgnoreNull
                                        }
                                    | _ -> a
                                )
                            |> Array.map 
                                (fun a ->
                                    match a.DefaultMode with 
                                    | Some(UseAttr d) 
                                    | Some(ReplaceNull d) ->
                                        {a with DefaultMode = Some(ReplaceOptionWithString (ensureQuote d))}
                                    | Some(IgnoreNull) ->
                                        {a with DefaultMode = Some(IgnoreNone)}
                                    | _ -> a
                                )
                        }
                [
                    optional
                ]
            else
                [x]

        )
)

// **************************** Simple Shaple(tuple) handling *******************************

Mappings.Modify(fun (x : ProcessedArg) -> 
    if x.TypeString = "Shape(tuple)" then 
        {x with 
            TypeString = "int seq"
        }   
    else    
        x
    )
       

// **************************** Sort parameters so required args appear first *******************************
// Note: this is intentionally near the end of the mappings and applied to 
// `ProcessedAtomicSymbol list` to ensure it happens on a "final" transform

Mappings.Modify(fun (l : ProcessedAtomicSymbol list) -> 
    l
    |> List.map 
        (fun x ->
            let optional, required = x.Args |> Array.partition (fun x -> x.DefaultMode.IsSome) 
            { x  with 
                Args = Array.append required optional
            }
        )
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
            yield! toNDArrayCode true x 
            yield! toSymbolCode true x 
        else    
            if ndArray then 
                yield! toNDArrayCode false x
            if symbol then 
                yield! toSymbolCode false x
    ]

let toCodeOld (x : ProcessedAtomicSymbol) =
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
            yield! toNDArrayCode true x 
            yield! toSymbolCode true x 
            ()
        else    
            if ndArray then 
                yield! toNDArrayCode false x
            if symbol then 
                yield! toSymbolCode false x
    ]


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
                let symbolDefs = (y.Head, toSymbolTypeCode y) |> List.singleton
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
                    SymbolTypeDefinitions = symbolDefs
                }
                |> Ok
            | _, Error(x,e) -> Error(x,e)
        )


// ******************************** Skip *************************************
let skipped = 
    [
        "Pooling_v1" //DEPRECATED
        "Crop" //DEPRECATED
        "_NDArray" // ptr type
        "_Native" // ptr type
    ] |> Set.ofSeq


let definedTypes = HashSet()
let types = ResizeArray()
let members = ResizeArray()
let symboltypes = ResizeArray()
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
        cb.SymbolTypeDefinitions
        |> List.map snd
        |> List.iter (symboltypes.Add)
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
    
    
System.IO.File.WriteAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__,"operators.fs"),     
    [
        """namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop
"""
        //yield! types |> breakBlocks
        ""
        //yield! symboltypes |> Seq.filter (List.isEmpty >> not) |> breakBlocks
        """
type Operators() =  
"""  
        yield! members |> breakBlocks
        yield! skip |> breakBlocks
        yield! errors |> breakBlocks
    ])
            


System.IO.File.WriteAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__,"genargtypes.fs"),     
    [
        """namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop

[<AutoOpen>]
module GeneratedArgumentTypes = 
"""
        yield! types |> Seq.filter (fun x -> Seq.isEmpty x |> not) |> Seq.map (fun x -> ["[<RequireQualifiedAccess>]"; yield! x]) |> breakBlocks |> indent 1
        ""
    ])

let symbolsFile = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"symbol.fs")
let lines = System.IO.File.ReadAllLines(symbolsFile)

let startTag = "(* GERNATED SYMBOL TYPES BEGIN *)//"
let endTag = "(* GERNATED SYMBOL TYPES END *)//"
System.IO.File.WriteAllLines(symbolsFile,
    [
        let mutable i = 0 
        while i < lines.Length && not (lines.[i].StartsWith(startTag)) do 
            yield lines.[i]
            i <- i + 1
        yield lines.[i]
        i <- i + 1
        yield! symboltypes |> Seq.filter (List.isEmpty >> not) |> breakBlocks
        while i < lines.Length && not (lines.[i].StartsWith(endTag)) do 
            i <- i + 1
        yield lines.[i]
        i <- i + 1
        while i < lines.Length do 
            yield lines.[i]
            i <- i + 1
    ]
) 



