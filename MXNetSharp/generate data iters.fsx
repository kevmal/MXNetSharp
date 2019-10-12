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
            
    
    
        

let creatorInfos = MXDataIter.list() |> Array.map MXDataIter.getInfo

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

type Arg = 
    {
        DataIterInfo : DataIterInfo
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
        ProcessedDataIter : ProcessedDataIter option
        Arg : Arg
        Name : string
        TypeString : string
        DefaultMode : DefaultMode option
        Doc : string []
        CodeGenerator : ArgCodeGen
        DefinedType : UnionType Option
    }

and ProcessedDataIter = 
    {
        DataIterInfo : DataIterInfo
        Doc : string []
        Name : string 
        Args : ProcessedArg []
    }

type CodeBlock = 
    {
        DataIterInfo : DataIterInfo
        MemberDefinitions : (ProcessedDataIter*(string list)) list
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
    {
        ProcessedDataIter = None
        Arg = a
        Name = a.Name
        TypeString = a.TypeString
        DefaultMode = None
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
                    | None -> Some(DefaultMode.ReplaceNull "\"None\"")
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
                | "Shape(tuple)" -> ReplaceNull (sprintf "\"%s\"" str) |> Some
                | t when t.StartsWith "{" -> 
                    Some(ReplaceNull(str.Replace(''','\"')))
                | _ -> None
            | _ -> None
        { x with DefaultMode = dmode }
    )

Mappings.Modify
    (fun (x : ProcessedDataIter) ->
        { x with Args = x.Args |> Array.map (fun a -> {a with ProcessedDataIter = Some x}) }
    )

let mapDataIterInfoBase (x : DataIterInfo) = 
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
                        DataIterInfo = x
                        ArgumentInfo = a
                        TypeString = tp
                        RequiredOrOptional = req
                        Name = fsName a.Name
                    }
                mapArgBase arg
        |]
    {
        DataIterInfo = x
        Doc = doc
        Name = mname
        Args = args
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
    | "string" -> str
    | _ -> sprintf "string %s" str

let toCode (x : ProcessedDataIter) =
    let args = 
        x.Args 
        |> Seq.filter (fun x -> match x.CodeGenerator with ConstantArg _ -> false | _ -> true)
        |> Seq.map 
            (fun x -> 
                let t = x.TypeString
                match x.DefaultMode with 
                | Some (ReplaceOptionWithString _) -> 
                    sprintf "?%s : %s" x.Name t
                | Some (ReplaceNull _)
                | Some IgnoreNull -> 
                    sprintf "[<Optional>] %s : %s" x.Name t
                | None -> sprintf "%s : %s" x.Name t
                | Some (UseAttr d)-> 
                    sprintf "[<Optional; DefaultParameterValue(%s)>] %s : %s" d x.Name t
                )
        |> Seq.toArray
    let define = 
        if args.Length < 5 then 
            let argStr = args |> String.concat ", "
            [sprintf "static member %s(%s) =" x.Name argStr]
        else
            let dstr = sprintf "static member %s(" x.Name
            let indent = String.replicate dstr.Length " "
            [
                dstr + args.[0] + ", "
                yield! args.[1 .. args.Length - 2] |> Seq.map (fun x -> indent + x + ", ")
                indent + args.[args.Length - 1] + ") ="
            ]
    let arr x = if Array.isEmpty x then "Array.empty" else sprintf "[|%s|]" (x |> String.concat "; ")
    let paramNamesStr = 
        x.Args 
        |> Array.filter (fun x -> match x.CodeGenerator with | SkipArg -> false | _ -> true)
        |> Array.map (fun a -> ("\"" + a.Arg.ArgumentInfo.Name + "\""))  
        |> arr
    let paramValuesStr = 
        x.Args 
        |> Array.choose
            (fun a ->
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
            ) 
        |> arr
    let invoke = 
        [
            sprintf "DataIter.FomName(%s)" x.DataIterInfo.Name
            sprintf "        .WithParamters(%s," paramNamesStr
            sprintf "                       %s)" paramValuesStr
        ]
        
    [
        yield! indent 1 x.Doc
        yield! indent 1 (x.Args |> Array.collect (fun x -> x.Doc))
        yield! indent 1 define 
        yield! indent 2 invoke 
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
       
// **************************** "or None" handling *******************************


Mappings.Modify(fun (l : ProcessedDataIter list) ->
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
        match x.Arg.DataIterInfo.Name with 
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
        | Name "ActType" & (_,a) when a.Arg.DataIterInfo.Name = "LeakyReLU" -> 
            {t with Name = "LeakyReLUType"}
        | (Name "Mode" | Name "OutType" | Name "Dtype" | Name "Layout" | Name "TransformType") & (_,{ProcessedDataIter = Some pas}) -> 
            {t with Name = pas.Name + t.Name}
        | _ -> t
    { arg with 
        DefinedType = Some newt
        TypeString = newt.Name
    }
        

Mappings.Modify(fun (x : ProcessedDataIter) ->
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
                    let y = mapDataIterInfoBase x
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
                    DataIterInfo = x
                    MemberDefinitions = memberDefs
                    TypeDefinitions = typeDefs
                }
                |> Ok
            | _, Error(x,e) -> Error(x,e)
        )


// ******************************** Skip *************************************
let skipped = 
    [
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
        | Ok(cb) when skipped.Contains cb.DataIterInfo.Name->     
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
    member x.NDArrayHandle = failwith "" 
"""  
        yield! members |> breakBlocks
        yield! skip |> breakBlocks
        yield! errors |> breakBlocks
    ]
            

System.IO.File.WriteAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__,"DataIter_gen.fs"),  generatedLines)
