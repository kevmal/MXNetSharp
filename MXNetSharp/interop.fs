namespace MXNetSharp.Interop

#nowarn "9"

open System
open System.Runtime.InteropServices
open CApi
open MXNetSharp

type KeyVarNumArgs = IntPtr
exception MXNetException of string*string with
    override x.Message = 
        match x :> Exception with 
        | MXNetException(call,msg) -> sprintf "%s: %s" call msg
        | _ -> failwith "unreachable"


[<Struct; StructLayout(LayoutKind.Sequential)>]
type LibFeature =
    val name : string
    val enabled : bool    

type ArgumentInfo = 
    {
        Name : string 
        Description : string 
        TypeInfo : string
    }

type FunctionInfo = 
    {
        Name : string
        Description : string
        Arguments : ArgumentInfo []
        ReturnTypeInfo : string
    }

type AtomicSymbolInfo = 
    {
        Name : string
        Description : string
        Arguments : ArgumentInfo []
        ReturnTypeInfo : string
        KeyVarNumArgs : KeyVarNumArgs
    }

type DataIterInfo = 
    {
        Name : string
        Description : string
        Arguments : ArgumentInfo []
    }

module Helper = 
    let length (a : 'a []) = 
        match a with 
        | null -> 0
        | _ -> a.Length
    let ulength (a : 'a []) = a |> length |> uint32
    let boolToInt b = if b then 1 else 0
    let intPtrSz = Marshal.SizeOf<IntPtr>()
    let un<'a> = Unchecked.defaultof<'a>
    let str x = 
        if x = IntPtr.Zero then 
            ""
        else 
            Marshal.PtrToStringAnsi x
    let readIntPtr index ptr = Marshal.ReadIntPtr(ptr, index*intPtrSz)
    let readString index ptr = Marshal.PtrToStringAnsi(readIntPtr index ptr)
    let readByteArray size (ptr : IntPtr) = 
        let b : byte[] = Array.zeroCreate size
        Marshal.Copy(ptr,b,0,size)
        b
    let inline readStringArray size ptr = Array.init (int size) (fun i -> readString i ptr)
    let inline readPtrArray size ptr = Array.init (int size) (fun i -> readIntPtr i ptr)
    let inline readStructArray size ptr : ^a[] = Array.init (int size) (fun i -> Marshal.PtrToStructure(ptr + IntPtr(i*sizeof< ^a>)))
    let throwOnError call (returnCode : int) = 
        if returnCode <> 0 then 
            let ptr = MXGetLastError()
            let errorMesseage  = str ptr
            raise (MXNetException(call, errorMesseage))
                

open Helper


module MXLib = 
    
    /// <summary>return str message of the last error
    /// all function in this file will return 0 when success
    /// and -1 when an error occured,
    /// MXGetLastError can be called to retrieve the error
    ///
    /// this function is threadsafe and can be called by different thread</summary>
    /// <returns>error info</returns>
    let getLastError() = MXGetLastError() |> str

    /// <summary>Load library dynamically</summary>
    /// <param name="path">to the library .so file</param>
    let loadLib(path : string) = MXLoadLib path |> throwOnError "MXLoadLib"

    /// Get list of features supported on the runtime
    let infoFeatures() : LibFeature [] =
        let mutable a = un
        let mutable sz = un
        MXLibInfoFeatures(&a, &sz) |> throwOnError "MXLibInfoFeatures"
        Array.init sz (fun i -> Marshal.PtrToStructure( a + IntPtr(i*sizeof<LibFeature>)))
        
    /// <summary>Seed all global random number generators in mxnet.</summary>
    /// <param name="seed">the random number seed.</param>
    let randomSeed (seed : int) = MXRandomSeed seed |> throwOnError "MXRandomSeed"

    /// <summary>Seed the global random number generator of the given device.</summary>
    /// <param name="seed">the random number seed.</param> 
    /// <param name="dev_type">device type</param> 
    /// <param name="dev_id">device id</param> 
    let randomSeedContext seed dev_type dev_id = MXRandomSeedContext(seed,dev_type,dev_id) |> throwOnError "MXRandomSeedContext"

    /// <summary>Notify the engine about a shutdown,
    /// This can help engine to print less messages into display.
    /// User do not have to call this function.</summary>
    let notifyShutdown() = MXNotifyShutdown() |> throwOnError "MXNotifyShutdown"


    /// <summary>Get the number of GPUs.</summary>
    let getGpuCount() = 
        let mutable out = un 
        MXGetGPUCount(&out) |> throwOnError "MXGetGPUCount"
        out

    /// <summary>get the MXNet library version as an integer</summary>
    let getVersion() = 
        let mutable out = un 
        MXGetVersion(&out) |> throwOnError "MXGetVersion"
        out
        


    /// list all the available functions handles
    /// most user can use it to list all the needed functions
    let listFunctions() : FunctionHandle[] =     
        let mutable outSize = un
        let mutable outArray = un
        MXListFunctions(&outSize, &outArray) |> throwOnError "MXListFunctions"
        readPtrArray outSize outArray

    /// <summary>Get the information of the function handle.</summary>
    /// <param name="functionHandle">The function handle.</param>
    let funcGetInfo functionHandle = 
        let mutable name = un
        let mutable description = un
        let mutable numArgs = un
        let mutable argNames = un
        let mutable arg_type_infos = un
        let mutable arg_descriptions = un
        let mutable return_type = un
        MXFuncGetInfo(functionHandle,&name,&description,&numArgs,&argNames,&arg_type_infos,&arg_descriptions,&return_type) |> throwOnError "MXFuncGetInfo"
        {
            Name = name
            Description = str description
            Arguments = 
                [|
                    for i = 0 to int numArgs - 1 do 
                        {
                            Name = readString i argNames
                            Description = readString i arg_descriptions
                            TypeInfo = readString i arg_type_infos
                        }
                |]
            ReturnTypeInfo = str return_type
        }
    
    /// list all the available operator names, include entries.
    let listAllOpNames() =
       let mutable out_size = un
       let mutable out_array = un
       MXListAllOpNames(&out_size, &out_array) |> throwOnError "MXListAllOpNames"
       readStringArray out_size out_array


type InferShapeResult<'a> = 
    {
        Complete : bool
        AuxShapes : 'a [] []
        InputShapes : 'a [] []
        OutputShapes : 'a [] []
    }

type InferTypeResult<'a> = 
    {
        Complete : bool
        AuxTypes : int[]
        InputTypes : int[]
        OutputTypes : int[]
    }

module MXSymbol = 

    /// <summary>list all the available AtomicSymbolEntry</summary>
    let listAtomicSymbolCreators() : AtomicSymbolCreatorHandle[] = 
        let mutable outSize = un
        let mutable outArray = un
        MXSymbolListAtomicSymbolCreators(&outSize, &outArray) |> throwOnError "MXSymbolListAtomicSymbolCreators"
        readPtrArray outSize outArray

    /// <summary>Get the name of an atomic symbol.</summary>
    /// <param name="creator">the AtomicSymbolCreator.</param>
    let getAtomicSymbolName (creator : AtomicSymbolCreatorHandle) : string = 
        let mutable name = un
        MXSymbolGetAtomicSymbolName(creator, &name) |> throwOnError "MXSymbolGetAtomicSymbolName"
        str name

    /// <summary>Get the detailed information about atomic symbol.</summary>
    /// <param name="creator">the AtomicSymbolCreator.</param>
    let getAtomicSymbolInfo (creator : AtomicSymbolCreatorHandle) = 
        let mutable name = un
        let mutable description = un
        let mutable numArgs = un
        let mutable argNames = un
        let mutable arg_type_infos = un
        let mutable arg_descriptions = un
        let mutable key_var_num_args = un
        let mutable return_type = un
        MXSymbolGetAtomicSymbolInfo(creator,&name,&description,&numArgs,&argNames,&arg_type_infos,&arg_descriptions,&key_var_num_args,&return_type) 
        |> throwOnError "MXSymbolGetAtomicSymbolInfo"
        {
            Name = str name
            Description = str description
            Arguments = 
                [|
                    for i = 0 to int numArgs - 1 do 
                        {
                            Name = readString i argNames
                            Description = readString i arg_descriptions
                            TypeInfo = readString i arg_type_infos
                        }
                |]
            ReturnTypeInfo = str return_type
            KeyVarNumArgs = readIntPtr 0 key_var_num_args
        }


    /// <summary>Create an AtomicSymbol.</summary>
    /// <param name="creator">the AtomicSymbolCreator</param>
    /// <param name="keys">the keys to the params</param>
    /// <param name="vals">the vals of the params</param>
    /// <returns>pointer to the created symbol handle</returns>
    let createAtomicSymbol creator keys vals : AtomicSymbolHandle = 
        let mutable out = un
        assert(Array.length keys = Array.length vals)
        MXSymbolCreateAtomicSymbol(creator, uint32(Array.length keys), keys, vals, &out) |> throwOnError "MXSymbolCreateAtomicSymbol"
        out

    /// <summary>Compose the symbol on other symbols.
    /// This function will change the sym hanlde.
    /// To achieve function apply behavior, copy the symbol first
    /// before apply.</summary>
    /// <param name="sym">the symbol to apply</param>
    /// <param name="name">the name of symbol</param>
    /// <param name="keys">the key of keyword args (optional)</param>
    /// <param name="args">arguments to sym</param>
    let compose sym name keys args = 
        assert(isNull keys || ulength keys = ulength args)
        MXSymbolCompose(sym, name, ulength args, keys, args) |> throwOnError "MXSymbolCompose"

    /// <summary>Save a symbol into a json string</summary>
    /// <param name="symbol">the input symbol.</param>
    let saveToJSON symbol = 
        let mutable out_json = un
        MXSymbolSaveToJSON(symbol, &out_json) |> throwOnError "MXSymbolSaveToJSON"
        str out_json

    /// <summary>Create a Variable Symbol.</summary>
    /// <param name="name">name of the variable</param>
    /// <returns>pointer to the created symbol handle</returns>
    let createVariable name = 
        let mutable out = un
        MXSymbolCreateVariable(name, &out) |> throwOnError "MXSymbolCreateVariable"
        out

    /// <summary>Free the symbol handle.</summary>
    /// <param name="symbol">the symbol</param>
    let free symbol = MXSymbolFree(symbol) |> throwOnError "MXSymbolFree"
        
    /// <summary>Generate atomic symbol (able to be composed) from a source symbol</summary>
    /// <param name="sym_handle">source symbol</param>
    let genAtomicSymbolFromSymbol sym_handle : AtomicSymbolHandle =
        let mutable ret_sym_handle = un
        MXGenAtomicSymbolFromSymbol(sym_handle, &ret_sym_handle) |> throwOnError "MXGenAtomicSymbolFromSymbol"
        ret_sym_handle
            

    /// <summary>Get the input symbols of the graph.</summary>
    /// <param name="sym">The graph.</param>
    /// <returns>The input symbols of the graph.</returns>
    let getInputSymbols sym  : SymbolHandle [] = 
        let mutable input_size = un
        let mutable inputs = 0n
        MXSymbolGetInputSymbols(sym, &inputs, &input_size) |> throwOnError "MXSymbolGetInputSymbols"
        readStructArray input_size inputs

    /// <summary>Cut a subgraph whose nodes are marked with a subgraph attribute.
    ///The input graph will be modified. A variable node will be created for each
    ///edge that connects to nodes outside the subgraph. The outside nodes that
    ///connect to the subgraph will be returned.</summary>
    /// <param name="sym">The graph.</param>
    /// <returns>The nodes that connect to the subgraph.</returns>
    let cutSubgraph sym = 
        let mutable input_size = un
        let mutable inputs = 0n
        MXSymbolCutSubgraph(sym, &inputs, &input_size) |> throwOnError "MXSymbolCutSubgraph"
        readStructArray input_size inputs

    /// <summary>Create a Symbol by grouping list of symbols together</summary>
    /// <param name="num_symbols">number of symbols to be grouped</param>
    /// <param name="symbols">array of symbol handles</param>
    /// <returns>pointer to the created symbol handle</returns>
    let createGroup symbols = 
        let mutable out = un
        MXSymbolCreateGroup(ulength symbols, symbols, &out) |> throwOnError "MXSymbolCreateGroup"
        out

    /// <summary>Load a symbol from a json file.</summary>
    /// <param name="fname">the file name.</param>
    /// <returns>the output symbol.</returns>
    let createFromFile fname = 
        let mutable out = un
        MXSymbolCreateFromFile(fname, &out) |> throwOnError "MXSymbolCreateFromFile"
        out

    /// <summary>Load a symbol from a json string.</summary>
    /// <param name="json">the json string.</param>
    /// <returns>the output symbol.</returns>
    let createFromJSON json = 
        let mutable out = un
        MXSymbolCreateFromJSON(json, &out) |> throwOnError "MXSymbolCreateFromJSON"
        out

    /// <summary>Remove the operators amp_cast and amp_multicast</summary>
    /// <param name="sym_handle">the input symbol.</param>
    /// <returns>the output symbol.</returns>
    let removeAmpCast sym_handle = 
        let mutable ret_sym_handle = 0n
        MXSymbolRemoveAmpCast(sym_handle, &ret_sym_handle) |> throwOnError "MXSymbolRemoveAmpCast"
        ret_sym_handle

    /// <summary>Save a symbol into a json file.</summary>
    /// <param name="symbol">the input symbol.</param>
    /// <param name="fname">the file name.</param>
    let saveToFile symbol fname = 
        MXSymbolSaveToFile(symbol, fname) |> throwOnError "MXSymbolSaveToFile"

    /// <summary>Copy the symbol to another handle</summary>
    /// <param name="symbol">the source symbol</param>
    /// <returns>used to hold the result of copy</returns>
    let copy symbol = 
        let mutable out = un
        MXSymbolCopy(symbol, &out) |> throwOnError "MXSymbolCopy"
        out

    /// <summary>Print the content of symbol, used for debug.</summary>
    /// <param name="symbol">the symbol</param>
    /// <returns>pointer to hold the output string of the printing.</returns>
    let print symbol = 
        let mutable out_str = un
        MXSymbolPrint(symbol, &out_str) |> throwOnError "MXSymbolPrint"
        str out_str

    /// <summary>Get string name from symbol</summary>
    /// <param name="symbol">the source symbol</param>
    /// <returns>The result name.</returns>
    let getName symbol = 
        let mutable out = un
        let mutable success = un
        MXSymbolGetName(symbol, &out, &success) |> throwOnError "MXSymbolGetName"
        if success = 0 then 
            None
        else
            Some (str out)

    /// <summary>Get string attribute from symbol</summary>
    /// <param name="symbol">the source symbol</param>
    /// <param name="key">The key of the symbol.</param>
    /// <returns>The result attribute, can be NULL if the attribute do not exist.</returns>
    let getAttr symbol key = 
        let mutable out = un
        let mutable success = un
        MXSymbolGetAttr(symbol, key, &out, &success) |> throwOnError "MXSymbolGetAttr"
        if success = 0 then 
            None
        else
            Some (str out)

    /// <summary>Set string attribute from symbol.
    /// NOTE: Setting attribute to a symbol can affect the semantics(mutable/immutable) of symbolic graph.
    ///
    /// Safe recommendaton: use  immutable graph
    /// - Only allow set attributes during creation of new symbol as optional parameter
    ///
    /// Mutable graph (be careful about the semantics):
    /// - Allow set attr at any point.
    /// - Mutating an attribute of some common node of two graphs can cause confusion from user.</summary>
    /// <param name="symbol">the source symbol</param>
    /// <param name="key">The key of the symbol.</param>
    /// <param name="value">The value to be saved.</param>
    let setAttr symbol key value = 
        MXSymbolSetAttr(symbol, key, value) |> throwOnError "MXSymbolSetAttr"

    /// <summary>Get all attributes from symbol, including all descendents.</summary>
    /// <param name="symbol">the source symbol</param>
    /// <returns>2*out_size strings representing key value pairs.</returns>
    let listAttr symbol = 
        let mutable out_size = un
        let mutable out = un
        MXSymbolListAttr(symbol, &out_size, &out) |> throwOnError "MXSymbolListAttr"
        readStringArray out_size out

    /// <summary>Get all attributes from symbol, excluding descendents.</summary>
    /// <param name="symbol">the source symbol</param>
    /// <returns>key value pairs.</returns>
    let listAttrShallow symbol = 
        let mutable out_size = un
        let mutable out = un
        MXSymbolListAttrShallow(symbol, &out_size, &out) |> throwOnError "MXSymbolListAttrShallow"
        readStringArray (2*int out_size) out
        |> Array.chunkBySize 2
        |> Array.map (fun a -> a.[0], a.[1])

    /// <summary>List arguments in the symbol.</summary>
    /// <param name="symbol">the symbol</param>
    let listArguments symbol = 
        let mutable out_size = un
        let mutable out_str_array = un
        MXSymbolListArguments(symbol, &out_size, &out_str_array) |> throwOnError "MXSymbolListArguments"
        readStringArray out_size out_str_array

    /// <summary>List returns in the symbol.</summary>
    /// <param name="symbol">the symbol</param>
    let listOutputs symbol = 
        let mutable out_size = un
        let mutable out_str_array = un
        MXSymbolListOutputs(symbol, &out_size, &out_str_array) |> throwOnError "MXSymbolListOutputs"
        readStringArray out_size out_str_array

    /// <summary>Get number of outputs of the symbol.</summary>
    /// <param name="symbol">The symbol</param>
    /// <returns>number of outputs</returns>
    let getNumOutputs symbol = 
        let mutable output_count = un
        MXSymbolGetNumOutputs(symbol, &output_count) |> throwOnError "MXSymbolGetNumOutputs"
        output_count

    /// <summary>Get a symbol that contains all the internals.</summary>
    /// <param name="symbol">The symbol</param>
    /// <returns>The output symbol whose outputs are all the internals.</returns>
    let getInternals symbol = 
        let mutable out = un
        MXSymbolGetInternals(symbol, &out) |> throwOnError "MXSymbolGetInternals"
        out

    /// <summary>Get a symbol that contains only direct children.</summary>
    /// <param name="symbol">The symbol</param>
    /// <returns>The output symbol whose outputs are the direct children.</returns>
    let getChildren symbol = 
        let mutable out = un
        MXSymbolGetChildren(symbol, &out) |> throwOnError "MXSymbolGetChildren"
        out

    /// <summary>Get index-th outputs of the symbol.</summary>
    /// <param name="symbol">The symbol</param>
    /// <param name="index">the Index of the output.</param>
    /// <returns>The output symbol whose outputs are the index-th symbol.</returns>
    let getOutput symbol index = 
        let mutable out = un
        MXSymbolGetOutput(symbol, uint32 index, &out) |> throwOnError "MXSymbolGetOutput"
        out

    /// <summary>List auxiliary states in the symbol.</summary>
    /// <param name="symbol">the symbol</param>
    let listAuxiliaryStates symbol = 
        let mutable out_size = un
        let mutable out_str_array = un
        MXSymbolListAuxiliaryStates(symbol, &out_size, &out_str_array) |> throwOnError "MXSymbolListAuxiliaryStates"
        readStringArray out_size out_str_array

    /// <summary>Get the gradient graph of the symbol</summary>
    /// <param name="sym">the symbol to get gradient</param>
    /// <param name="wrt">the name of the arguments to get gradient</param>
    /// <returns>the returned symbol that has gradient</returns>
    let grad sym wrt = 
        let mutable out = un
        MXSymbolGrad(sym, ulength wrt, wrt, &out) |> throwOnError "MXSymbolGrad"
        out

    /// key*shape to CSR from (argIndPtr,argShapeData) for inferShape call
    let inline internal keyShapeToCsrForm (argShapes : (string * ^a []) [])  = 
        let argIndPtr = Array.zeroCreate ((Array.length argShapes) + 1)
        for i = 1 to argIndPtr.Length - 1 do 
            argIndPtr.[i] <- argIndPtr.[i - 1] + (argShapes.[i - 1] |> snd |> Array.length)
        let argShapeData = Array.zeroCreate argIndPtr.[argIndPtr.Length - 1 |> int]
        let keys = argShapes |> Array.map fst
        let mutable j = 0
        for i = 0 to argShapes.Length - 1 do 
            let shapes = snd argShapes.[i] 
            for k = 0 to shapes.Length - 1 do
                argShapeData.[j] <- shapes.[k]
                j <- j + 1
        argIndPtr,argShapeData
        
    /// <summary>infer shape of unknown input shapes given the known one.
    /// The shapes are packed into a CSR matrix represented by arg_ind_ptr and arg_shape_data
    /// The call will be treated as a kwargs call if key != nullptr or num_args==0, otherwise it is positional.</summary>
    /// <param name="sym">symbol handle</param>
    /// <param name="keys">the key of keyword args (optional)</param>
    /// <param name="arg_ind_ptr">the head pointer of the rows in CSR</param>
    /// <param name="arg_shape_data">the content of the CSR</param>
    /// <returns>input, output and aux shape data in a InferShapeResult<uint32> record</returns>
    let inferShape sym keys arg_ind_ptr arg_shape_data = 
        let mutable in_shape_size = un
        let mutable in_shape_ndim = un
        let mutable in_shape_data = un
        let mutable aux_shape_size = un
        let mutable aux_shape_ndim = un
        let mutable aux_shape_data = un
        let mutable out_shape_size = un
        let mutable out_shape_ndim = un
        let mutable out_shape_data = un
        let mutable complete = un
        MXSymbolInferShapeEx(sym, ulength keys, keys, arg_ind_ptr, arg_shape_data, &in_shape_size, &in_shape_ndim, &in_shape_data, &out_shape_size, &out_shape_ndim, &out_shape_data, &aux_shape_size, &aux_shape_ndim, &aux_shape_data, &complete) |> throwOnError "MXSymbolInferShapeEx"
        let shapes sz ndim data =
            let dims = Helper.readStructArray sz ndim : uint32[]
            Helper.readPtrArray sz data
            |> Array.mapi (fun i ptr -> Helper.readStructArray dims.[i] ptr : uint32 [])
        {
            Complete = complete <> 0
            AuxShapes = shapes aux_shape_size aux_shape_ndim aux_shape_data
            InputShapes = shapes in_shape_size in_shape_ndim in_shape_data
            OutputShapes = shapes out_shape_size out_shape_ndim out_shape_data
        }

    /// <summary>infer shape of unknown input shapes given the known one.
    /// The shapes are packed into a CSR matrix represented by arg_ind_ptr and arg_shape_data
    /// The call will be treated as a kwargs call if key != nullptr or num_args==0, otherwise it is positional.</summary>
    /// <param name="sym">symbol handle</param>
    /// <param name="keys">the key of keyword args (optional)</param>
    /// <param name="arg_ind_ptr">the head pointer of the rows in CSR</param>
    /// <param name="arg_shape_data">the content of the CSR</param>
    /// <returns>input, output and aux shape data in a InferShapeResult<int64> record</returns>
    let inferShape64 sym keys arg_ind_ptr arg_shape_data = 
        let mutable in_shape_size = un
        let mutable in_shape_ndim = un
        let mutable in_shape_data = un
        let mutable aux_shape_size = un
        let mutable aux_shape_ndim = un
        let mutable aux_shape_data = un
        let mutable out_shape_size = un
        let mutable out_shape_ndim = un
        let mutable out_shape_data = un
        let mutable complete = un
        MXSymbolInferShapeEx64(sym, ulength keys, keys, arg_ind_ptr, arg_shape_data, &in_shape_size, &in_shape_ndim, &in_shape_data, &out_shape_size, &out_shape_ndim, &out_shape_data, &aux_shape_size, &aux_shape_ndim, &aux_shape_data, &complete) |> throwOnError "MXSymbolInferShapeEx64"
        let shapes sz ndim data =
            let dims = Helper.readStructArray sz ndim : int[]
            Helper.readPtrArray sz data
            |> Array.mapi (fun i ptr -> Helper.readStructArray dims.[i] ptr : int64 []) 
        {
            Complete = complete <> 0
            AuxShapes = shapes aux_shape_size aux_shape_ndim aux_shape_data
            InputShapes = shapes in_shape_size in_shape_ndim in_shape_data
            OutputShapes = shapes out_shape_size out_shape_ndim out_shape_data
        }
  
    /// <summary>partially infer shape of unknown input shapes given the known one.
    /// Return partially inferred results if not all shapes could be inferred.
    /// The shapes are packed into a CSR matrix represented by arg_ind_ptr and arg_shape_data
    /// The call will be treated as a kwargs call if key != nullptr or num_args==0, otherwise it is positional.</summary>
    /// <param name="sym">symbol handle</param>
    /// <param name="keys">the key of keyword args (optional)</param>
    /// <param name="arg_ind_ptr">the head pointer of the rows in CSR</param>
    /// <param name="arg_shape_data">the content of the CSR</param>
    /// <returns>input, output and aux shape data in a InferShapeResult<uint32> record</returns>
    let inferShapePartial sym keys arg_ind_ptr arg_shape_data = 
        let mutable in_shape_size = un
        let mutable in_shape_ndim = un
        let mutable in_shape_data = un
        let mutable aux_shape_size = un
        let mutable aux_shape_ndim = un
        let mutable aux_shape_data = un
        let mutable out_shape_size = un
        let mutable out_shape_ndim = un
        let mutable out_shape_data = un
        let mutable complete = un
        MXSymbolInferShapePartialEx(sym, ulength keys, keys, arg_ind_ptr, arg_shape_data, &in_shape_size, &in_shape_ndim, &in_shape_data, &out_shape_size, &out_shape_ndim, &out_shape_data, &aux_shape_size, &aux_shape_ndim, &aux_shape_data, &complete) |> throwOnError "MXSymbolInferShapePartialEx"
        let shapes sz ndim data =
            let dims = Helper.readStructArray sz ndim : uint32[]
            Helper.readPtrArray sz data
            |> Array.mapi (fun i ptr -> Helper.readStructArray dims.[i] ptr : uint32 [])
        {
            Complete = complete <> 0
            AuxShapes = shapes aux_shape_size aux_shape_ndim aux_shape_data
            InputShapes = shapes in_shape_size in_shape_ndim in_shape_data
            OutputShapes = shapes out_shape_size out_shape_ndim out_shape_data
        }

    /// <summary>partially infer shape of unknown input shapes given the known one.
    /// Return partially inferred results if not all shapes could be inferred.
    /// The shapes are packed into a CSR matrix represented by arg_ind_ptr and arg_shape_data
    /// The call will be treated as a kwargs call if key != nullptr or num_args==0, otherwise it is positional.</summary>
    /// <param name="sym">symbol handle</param>
    /// <param name="keys">the key of keyword args (optional)</param>
    /// <param name="arg_ind_ptr">the head pointer of the rows in CSR</param>
    /// <param name="arg_shape_data">the content of the CSR</param>
    /// <returns>input, output and aux shape data in a InferShapeResult<int64> record</returns>
    let inferShapePartial64 sym keys arg_ind_ptr arg_shape_data = 
        let mutable in_shape_size = un
        let mutable in_shape_ndim = un
        let mutable in_shape_data = un
        let mutable aux_shape_size = un
        let mutable aux_shape_ndim = un
        let mutable aux_shape_data = un
        let mutable out_shape_size = un
        let mutable out_shape_ndim = un
        let mutable out_shape_data = un
        let mutable complete = un
        MXSymbolInferShapePartialEx64(sym, ulength keys, keys, arg_ind_ptr, arg_shape_data, &in_shape_size, &in_shape_ndim, &in_shape_data, &out_shape_size, &out_shape_ndim, &out_shape_data, &aux_shape_size, &aux_shape_ndim, &aux_shape_data, &complete) |> throwOnError "MXSymbolInferShapePartialEx64"
        let shapes sz ndim data =
            let dims = Helper.readStructArray sz ndim : int[]
            Helper.readPtrArray sz data
            |> Array.mapi (fun i ptr -> Helper.readStructArray dims.[i] ptr : int64 []) 
        {
            Complete = complete <> 0
            AuxShapes = shapes aux_shape_size aux_shape_ndim aux_shape_data
            InputShapes = shapes in_shape_size in_shape_ndim in_shape_data
            OutputShapes = shapes out_shape_size out_shape_ndim out_shape_data
        }

    /// <summary>infer type of unknown input types given the known one.
    /// The types are packed into a CSR matrix represented by arg_ind_ptr and arg_type_data
    /// The call will be treated as a kwargs call if key != nullptr or num_args==0, otherwise it is positional.</summary>
    /// <param name="sym">symbol handle</param>
    /// <param name="keys">the key of keyword args (optional)</param>
    /// <param name="arg_type_data">the content of the CSR</param>
    /// <param name="in_type_size">sizeof the returning array of in_types</param>
    /// <param name="in_type_data">returning array of pointers to head of the input type.</param>
    /// <param name="out_type_size">sizeof the returning array of out_types</param>
    /// <param name="out_type_data">returning array of pointers to head of the input type.</param>
    /// <param name="aux_type_size">sizeof the returning array of aux_types</param>
    /// <param name="aux_type_data">returning array of pointers to head of the auxiliary type.</param>
    /// <param name="complete">whether infer type completes or more information is needed.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let inferType sym keys arg_type_data = 
        let mutable in_type_size = un
        let mutable in_type_data = un
        let mutable aux_type_size = un
        let mutable aux_type_data = un
        let mutable complete = un
        let mutable out_type_size = un
        let mutable out_type_data = un
        MXSymbolInferType(sym, ulength keys, keys, arg_type_data, &in_type_size, &in_type_data, &out_type_size, &out_type_data, &aux_type_size, &aux_type_data, &complete) |> throwOnError "MXSymbolInferType"
        {
            Complete = complete <> 0
            AuxTypes = readStructArray aux_type_size aux_type_data
            InputTypes = readStructArray in_type_size in_type_data
            OutputTypes = readStructArray out_type_size out_type_data
        }

    /// <summary>partially infer type of unknown input types given the known one.
    ///
    /// Return partially inferred results if not all types could be inferred.
    /// The types are packed into a CSR matrix represented by arg_ind_ptr and arg_type_data
    /// The call will be treated as a kwargs call if key != nullptr or num_args==0, otherwise it is positional.</summary>
    /// <param name="sym">symbol handle</param>
    /// <param name="keys">the key of keyword args (optional)</param>
    /// <param name="arg_type_data">the content of the CSR</param>
    /// <param name="in_type_size">sizeof the returning array of in_types</param>
    /// <param name="in_type_data">returning array of pointers to head of the input type.</param>
    /// <param name="out_type_size">sizeof the returning array of out_types</param>
    /// <param name="out_type_data">returning array of pointers to head of the input type.</param>
    /// <param name="aux_type_size">sizeof the returning array of aux_types</param>
    /// <param name="aux_type_data">returning array of pointers to head of the auxiliary type.</param>
    /// <param name="complete">whether infer type completes or more information is needed.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let inferTypePartial sym keys arg_type_data = 
        let mutable in_type_size = un
        let mutable in_type_data = un
        let mutable aux_type_size = un
        let mutable aux_type_data = un
        let mutable complete = un
        let mutable out_type_size = un
        let mutable out_type_data = un
        MXSymbolInferTypePartial(sym, ulength keys, keys, arg_type_data, &in_type_size, &in_type_data, &out_type_size, &out_type_data, &aux_type_size, &aux_type_data, &complete) |> throwOnError "MXSymbolInferTypePartial"
        {
            Complete = complete <> 0
            AuxTypes = readStructArray aux_type_size aux_type_data
            InputTypes = readStructArray in_type_size in_type_data
            OutputTypes = readStructArray out_type_size out_type_data
        }

    /// <summary>Convert a symbol into a quantized symbol where FP32 operators are replaced with INT8</summary>
    /// <param name="sym_handle">symbol to be converted</param>
    /// <param name="ret_sym_handle">quantized symbol result</param>
    /// <param name="dev_type">device type</param>
    /// <param name="num_excluded_sym_names">number of layers excluded from being quantized in the input symbol</param>
    /// <param name="excluded_sym_names">node names to be excluded from being quantized</param>
    /// <param name="num_excluded_op_names">number of operators excluded from being quantized in the input symbol</param>
    /// <param name="excluded_op_names">operator names to be excluded from being quantized</param>
    /// <param name="num_offline">number of parameters that are quantized offline</param>
    /// <param name="offline_params">array of c strings representing the names of params quantized offline</param>
    /// <param name="quantized_dtype">the quantized destination type for input data</param>
    /// <param name="calib_quantize">**Deprecated**. quantize op will always be calibrated if could</param>
    /// <param name="quantize_mode">quantize mode to be used in quantize pass</param>
    /// <param name="out_num_calib_names">return the number of nodes to be calibrated</param>
    /// <param name="out_calib_names">return the node names to be calibrated</param>
    let quantize sym_handle excluded_sym_names excluded_op_names offline_params quantized_dtype quantize_mode = 
        //TODO: Complete MXQuantizeSymbol
        let mutable dev_type = un
        let mutable ret_sym_handle = un
        let mutable out_num_calib_names = un
        let mutable out_calib_names = un
        MXQuantizeSymbol(sym_handle, &ret_sym_handle, &dev_type, ulength excluded_sym_names, excluded_sym_names, ulength excluded_op_names, excluded_op_names, ulength offline_params, offline_params, quantized_dtype, true, quantize_mode, &out_num_calib_names, &out_calib_names) |> throwOnError "MXQuantizeSymbol"

    /// <summary>Convert a symbol into a mixed precision symbol with cast operators for target dtype casting</summary>
    /// <param name="sym_handle">symbol to be converted</param>
    /// <param name="arg_type_data">arg types of the arguments</param>
    /// <param name="target_dtype">target_dtype for mixed precision symbol</param>
    /// <param name="cast_optional_params">whether to cast optional params to target_dtype</param>
    /// <param name="num_target_dtype_op_names">number of ops to be casted to target_dtype</param>
    /// <param name="num_fp32_op_names">number of ops to be casted to FP32</param>
    /// <param name="num_widest_dtype_op_names">number of ops to be casted to widest dtype</param>
    /// <param name="num_conditional_fp32_op_names">number of ops to be casted to FP32 based on a condition</param>
    /// <param name="num_excluded_symbols">number of symbols to be excluded from casting</param>
    /// <param name="num_model_params">number of model parameters</param>
    /// <param name="num_widest_dtype_op_names">number of ops to be casted to the widest dtype</param>
    /// <param name="num_conditional_fp32_op_names">number of ops to be cast to fp32 based on precision</param>
    /// <param name="target_dtype_op_names">op names to be casted to target_dtype</param>
    /// <param name="fp32_op_names">op names to be casted to fp32</param>
    /// <param name="widest_dtype_op_names">names to be casted to widest dtype</param>
    /// <param name="conditional_fp32_op_names">names to be casted to FP32 conditionally</param>
    /// <param name="excluded_symbols">symbol names to be excluded from casting</param>
    /// <param name="param_names">param names for conditional FP32 casting</param>
    /// <param name="param_values">param values for conditional FP32 casting</param>
    /// <param name="arg_names">argument names for which type information is provided</param>
    /// <param name="model_param_names">names for model parameters</param>
    let reducePrecision sym_handle arg_type_data ind_ptr target_dtype cast_optional_params num_target_dtype_op_names num_fp32_op_names num_widest_dtype_op_names num_conditional_fp32_op_names num_excluded_symbols num_model_params target_dtype_op_names fp32_op_names widest_dtype_op_names conditional_fp32_op_names excluded_symbols conditional_param_names conditional_param_vals model_param_names arg_names = 
        //TODO: Complete MXReducePrecisionSymbol
        let mutable ret_sym_handle = un
        MXReducePrecisionSymbol(sym_handle, &ret_sym_handle, ulength arg_type_data, arg_type_data, ulength ind_ptr, ind_ptr, target_dtype, cast_optional_params, num_target_dtype_op_names, num_fp32_op_names, num_widest_dtype_op_names, num_conditional_fp32_op_names, num_excluded_symbols, num_model_params, target_dtype_op_names, fp32_op_names, widest_dtype_op_names, conditional_fp32_op_names, excluded_symbols, conditional_param_names, conditional_param_vals, model_param_names, arg_names) |> throwOnError "MXReducePrecisionSymbol"

    /// <summary>Set calibration table to node attributes in the sym</summary>
    /// <param name="sym_handle">symbol whose node attributes are to be set by calibration table</param>
    /// <param name="num_layers">number of layers in the calibration table</param>
    /// <param name="layer">names stored as keys in the calibration table</param>
    /// <param name="low_quantiles">low quantiles of layers stored in the calibration table</param>
    /// <param name="high_quantiles">high quantiles of layers stored in the calibration table</param>
    /// <param name="ret_sym_handle">returned symbol</param>
    let setCalibTableToQuantizedSymbol qsym_handle layer_names low_quantiles high_quantiles = 
        let mutable ret_sym_handle = un
        MXSetCalibTableToQuantizedSymbol(qsym_handle, ulength layer_names, layer_names, low_quantiles, high_quantiles, &ret_sym_handle) |> throwOnError "MXSetCalibTableToQuantizedSymbol"
        ret_sym_handle

    /// <summary>Run subgraph pass based on the backend provided</summary>
    /// <param name="sym_handle">symbol to be converted</param>
    /// <param name="backend">backend names for subgraph pass</param>
    /// <param name="ret_sym_handle">returned symbol</param>
    let genBackendSubgraph sym_handle backend = 
        let mutable ret_sym_handle = un
        MXGenBackendSubgraph(sym_handle, backend, &ret_sym_handle) |> throwOnError "MXGenBackendSubgraph"
        ret_sym_handle

    /// <summary>Partitions symbol for given backend, potentially creating subgraphs</summary>
    /// <param name="sym_handle">symbol to be partitioned</param>
    /// <param name="dev_type">context device type</param>
    /// <param name="backend_name">backend name</param>
    /// <param name="ret_sym_handle">partitioned symbol returned</param>
    /// <param name="len">number of args</param>
    /// <param name="in_args_handle">args array</param>
    /// <param name="num_options">number of key value pairs</param>
    /// <param name="keys">keys for options</param>
    /// <param name="vals">values corresponding to keys</param>
    let optimizeForBackend sym_handle backend_name dev_type ret_sym_handle len in_args_handle num_options keys vals = 
        MXOptimizeForBackend(sym_handle, backend_name, dev_type, ret_sym_handle, len, in_args_handle, num_options, keys, vals) |> throwOnError "MXOptimizeForBackend"
        


module MXNDArray = 
    /// <summary>create a NDArray handle that is not initialized
    /// can be used to pass in as mutate variables
    /// to hold the result of NDArray</summary>
    let createNone() : NDArrayHandle = 
        let mutable out = IntPtr.Zero 
        MXNDArrayCreateNone(&out) |> throwOnError "MXNDArrayCreateNone"
        out
    
    /// <summary>create a NDArray with specified shape</summary>
    /// <param name="shape">shape</param>
    /// <param name="dev_type">device type, specify device we want to take</param>
    /// <param name="dev_id">the device id of the specific device</param>
    /// <param name="delay_alloc">whether to delay allocation until
    ///   the narray is first mutated</param>
    let inline create (shape : ^a[]) (dev_type : DeviceTypeEnum) dev_id (delay_alloc : bool) : NDArrayHandle = 
        let mutable out = IntPtr.Zero 
        MXNDArrayCreate(shape |> Array.map uint32, uint32 shape.Length, int dev_type, dev_id, boolToInt delay_alloc, &out) |> throwOnError "MXNDArrayCreate"
        out

    /// <summary>create a NDArray with specified shape and data type</summary>
    /// <param name="shape">the pointer to the shape</param>
    /// <param name="dev_type">device type, specify device we want to take</param>
    /// <param name="dev_id">the device id of the specific device</param>
    /// <param name="delay_alloc">whether to delay allocation until
    ///   the narray is first mutated</param>
    /// <param name="dtype">data type of created array</param>
    let inline createEx (shape : ^a[]) (dev_type : DeviceTypeEnum) dev_id (delay_alloc : bool) (dtype : TypeFlag) : NDArrayHandle = 
        let mutable out = IntPtr.Zero 
        MXNDArrayCreateEx(shape |> Array.map uint32, uint32 shape.Length, int dev_type, dev_id, boolToInt delay_alloc, int dtype, &out) |> throwOnError "MXNDArrayCreateEx"
        out

    /// <summary>create a NDArray with specified shape and data type</summary>
    /// <param name="shape">the pointer to the shape</param>
    /// <param name="dev_type">device type, specify device we want to take</param>
    /// <param name="dev_id">the device id of the specific device</param>
    /// <param name="delay_alloc">whether to delay allocation until
    ///   the narray is first mutated</param>
    /// <param name="dtype">data type of created array</param>
    let inline createEx64 (shape : ^a[]) dev_type dev_id delay_alloc dtype : NDArrayHandle = 
        let mutable out = IntPtr.Zero 
        MXNDArrayCreateEx64(shape |> Array.map int64, int shape.Length, dev_type, dev_id, delay_alloc, dtype, &out) |> throwOnError "MXNDArrayCreateEx64"
        out

    /// <summary>create a NDArray handle that is loaded from raw bytes.</summary>
    /// <param name="buf">the head of the raw bytes</param>
    let loadFromRawBytes (buf : byte[]) = 
        let mutable out = IntPtr.Zero 
        MXNDArrayLoadFromRawBytes(buf, int64 buf.Length, &out) |> throwOnError "MXNDArrayLoadFromRawBytes"
        out

    /// <summary>save the NDArray into raw bytes.</summary>
    /// <param name="handle">the NDArray handle</param>
    let saveRawBytes (handle : NDArrayHandle) = 
        let mutable out_size = un
        let mutable out_buf = IntPtr.Zero
        MXNDArraySaveRawBytes(handle, &out_size, &out_buf) |> throwOnError "MXNDArraySaveRawBytes"
        readByteArray (int out_size) out_buf


    /// <summary>Save list of narray into the file.</summary>
    /// <param name="fname">name of the file.</param>
    /// <param name="num_args">number of arguments to save.</param>
    /// <param name="args">the array of NDArrayHandles to be saved.</param>
    /// <param name="keys">the name of the NDArray, optional, can be NULL</param>
    let save fname (args : NDArrayHandle []) keys = 
        MXNDArraySave(fname, uint32 args.Length, args, keys) |> throwOnError "MXNDArraySave"
        
    /// <summary>Load list of narray from the file.</summary>
    /// <param name="fname">name of the file.</param>
    /// <returns>names, ndarrays</returns>
    let load fname = 
        let mutable out_size = un
        let mutable out_arr = IntPtr.Zero
        let mutable out_name_size = un
        let mutable out_names = IntPtr.Zero
        MXNDArrayLoad(fname, &out_size, &out_arr, &out_name_size, &out_names) |> throwOnError "MXNDArrayLoad"
        let arrs = readPtrArray out_size out_arr
        let names = readStringArray out_name_size out_names 
        names,arrs
        
    /// <summary>free the narray handle</summary>
    /// <param name="handle">the handle to be freed</param>
    let free (handle : NDArrayHandle) = MXNDArrayFree(handle) |> throwOnError "MXNDArrayFree"

    /// <summary>Slice the NDArray along axis 0.</summary>
    /// <param name="handle">the handle to the NDArray</param>
    /// <param name="slice_begin">The beginning index of slice</param>
    /// <param name="slice_end">The ending index of slice</param>
    /// <returns>The NDArrayHandle of sliced NDArray</returns>
    let inline slice (handle : NDArrayHandle) slice_begin slice_end =  
        let mutable out = IntPtr.Zero 
        MXNDArraySlice(handle, uint32 slice_begin, uint32 slice_end, &out) |> throwOnError "MXNDArraySlice"
        out
           
    /// <summary>Slice the NDArray along axis 0.</summary>
    /// <param name="handle">the handle to the NDArray</param>
    /// <param name="slice_begin">The beginning index of slice</param>
    /// <param name="slice_end">The ending index of slice</param>
    /// <returns>The NDArrayHandle of sliced NDArray</returns>
    let slice64 (handle : NDArrayHandle) slice_begin slice_end =  
        let mutable out = IntPtr.Zero 
        MXNDArraySlice64(handle, slice_begin, slice_end, &out) |> throwOnError "MXNDArraySlice64"
        out
           
    /// <summary>Index the NDArray along axis 0.</summary>
    /// <param name="handle">the handle to the NDArray</param>
    /// <param name="idx">the index</param>
    /// <returns>The NDArrayHandle of output NDArray</returns>
    let inline at (handle : NDArrayHandle) index =  
        let mutable out = IntPtr.Zero 
        MXNDArrayAt(handle, uint32 index, &out) |> throwOnError "MXNDArrayAt"
        out
        
    /// <summary>Index the NDArray along axis 0.</summary>
    /// <param name="handle">the handle to the NDArray</param>
    /// <param name="idx">the index</param>
    /// <returns>The NDArrayHandle of output NDArray</returns>
    let at64 (handle : NDArrayHandle) index =  
        let mutable out = IntPtr.Zero 
        MXNDArrayAt64(handle, index, &out) |> throwOnError "MXNDArrayAt64"
        out
    
    
    /// <summary>get the storage type of the array</summary>
    let getStorageType handle = 
        let mutable out = un 
        MXNDArrayGetStorageType(handle, &out) |> throwOnError "MXNDArrayGetStorageType"
        out

    /// <summary>Reshape the NDArray.</summary>
    /// <param name="handle">the handle to the narray</param>
    /// <param name="dims">new shape</param>
    /// <returns>the NDArrayHandle of reshaped NDArray</returns>    
    let reshape handle (dims : int []) = 
        let mutable out = un
        MXNDArrayReshape(handle, dims.Length, dims, &out) |> throwOnError "MXNDArrayReshape"
        out
       
    /// <summary>Reshape the NDArray.</summary>
    /// <param name="handle">the handle to the narray</param>
    /// <param name="dims">new shape</param>
    /// <returns>the NDArrayHandle of reshaped NDArray</returns>    
    let reshape64 handle (dims : int64 []) reverse = 
        let mutable out = un
        MXNDArrayReshape64(handle, dims.Length, dims, reverse, &out) |> throwOnError "MXNDArrayReshape64"
        out

    /// <summary>get the shape of the array</summary>
    /// <param name="handle">the handle to the narray</param>
    let getShape handle : int [] = 
        let mutable out_dim = un
        let mutable out_pdata = un
        MXNDArrayGetShapeEx(handle, &out_dim, &out_pdata) |> throwOnError "MXNDArrayGetShapeEx"
        if out_dim > 0 then 
            readStructArray out_dim out_pdata
        else    
            Array.empty
    
    /// <summary>get the shape of the array</summary>
    /// <param name="handle">the handle to the narray</param>
    let getShape64 handle : int64 [] = 
        let mutable out_dim = un
        let mutable out_pdata = un
        MXNDArrayGetShapeEx64(handle, &out_dim, &out_pdata) |> throwOnError "MXNDArrayGetShapeEx64"
        if out_dim > 0 then 
            readStructArray out_dim out_pdata
        else    
            Array.empty
        

    /// <summary>get the shape of the array</summary>
    /// <param name="handle">the handle to the narray</param>
    let inline syncCopyFromCPU handle (data : ^a[]) = 
        use ptr = fixed data
        let iptr = NativeInterop.NativePtr.toNativeInt ptr
        let sz = int64 data.Length
        MXNDArraySyncCopyFromCPU(handle, iptr, sz) |> throwOnError "MXNDArraySyncCopyFromCPU"
        
    /// <summary>get the context of the NDArray</summary>
    /// <param name="handle">the handle to the narray</param>
    /// <returns>NDArray context</returns>
    let getContext handle = 
        let mutable out_dev_type = un
        let mutable out_dev_id = un
        MXNDArrayGetContext(handle, &out_dev_type, &out_dev_id) |> throwOnError "MXNDArrayGetContext"
        struct((enum out_dev_type : DeviceTypeEnum), out_dev_id)
                
    /// <summary>get the type of the data in NDArray</summary>
    /// <param name="handle">the handle to the narray</param>
    let getDType handle : TypeFlag = 
        let mutable out_dtype = un
        MXNDArrayGetDType(handle, &out_dtype) |> throwOnError "MXNDArrayGetDType"
        enum out_dtype
        
    /// <summary>get the content of the data in NDArray</summary>
    /// <param name="handle">the handle to the ndarray</param>
    /// <param name="out_pdata">pointer holder to get pointer of data</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getData handle = 
        let mutable out_pdata = un
        MXNDArrayGetData(handle, &out_pdata) |> throwOnError "MXNDArrayGetDType"
        out_pdata

    /// <summary>invoke a nnvm op and imperative function</summary>
    /// <param name="creator">the op</param>
    /// <param name="inputs">input NDArrays</param>
    /// <param name="param_keys">keys for keyword parameters</param>
    /// <param name="param_vals">values for keyword parameters</param>
    let imperativeInvoke creator inputs parameterKeys parameterValues : NDArrayHandle [] = 
        let mutable num_outputs = un
        let mutable outputs = un
        assert(length parameterKeys = length parameterValues)
        MXImperativeInvoke(creator, length inputs, inputs, &num_outputs, &outputs, Array.length parameterKeys, parameterKeys, parameterValues) |> throwOnError "MXImperativeInvoke"
        readStructArray num_outputs outputs

    /// <summary>invoke a nnvm op and imperative function</summary>
    /// <param name="creator">the op</param>
    /// <param name="inputs">input NDArrays</param>
    /// <param name="outputs">output NDArrays</param>
    /// <param name="param_keys">keys for keyword parameters</param>
    /// <param name="param_vals">values for keyword parameters</param>
    /// <returns>number of outputs</returns>
    let imperativeInvokeInto creator inputs (outputs : NDArrayHandle []) parameterKeys parameterValues = 
        let mutable num_outputs = outputs.Length
        assert(length parameterKeys = length parameterValues)
        use outputsptr = fixed outputs
        let mutable ptr = NativeInterop.NativePtr.toNativeInt outputsptr
        MXImperativeInvoke(creator, length inputs, inputs, &num_outputs, &ptr, length parameterKeys, parameterKeys, parameterValues) |> throwOnError "MXImperativeInvoke"
        assert(ptr = NativeInterop.NativePtr.toNativeInt outputsptr)
        num_outputs

    /// <summary>wait until all delayed operations in
    ///  the system is completed</summary>
    let waitAll()  = 
        MXNDArrayWaitAll() |> throwOnError "MXNDArrayWaitAll"
        
    /// <summary>Perform a synchronize copyto a continugous CPU memory region.
    ///
    /// This function will call WaitToRead before the copy is performed.
    /// This is useful to copy data from existing memory region that are
    /// not wrapped by NDArray(thus dependency not being tracked).</summary>
    /// <param name="handle">the NDArray handle</param>
    /// <param name="data">the data source to copy into.</param>
    /// <param name="size">the memory size we want to copy into.</param>
    let syncCopyToCPU handle (a : 'a[]) = 
        use ptr = fixed a
        let size = int64 a.Length
        let data = NativeInterop.NativePtr.toNativeInt ptr
        MXNDArraySyncCopyToCPU(handle, data, size) |> throwOnError "MXNDArraySyncCopyToCPU"

    // TODO: Storag type enum?
    /// <summary>create an empty sparse NDArray with specified shape and data type</summary>
    /// <param name="storage_type">the storage type of the ndarray</param>
    /// <param name="shape">the pointer to the shape</param>
    /// <param name="dev_type">device type, specify device we want to take</param>
    /// <param name="dev_id">the device id of the specific device</param>
    /// <param name="delay_alloc">whether to delay allocation until
    ///       the narray is first mutated</param>
    /// <param name="dtype">data type of created array</param>
    /// <param name="aux_type">data type of the aux data for the created array</param>
    /// <param name="aux_ndims">the dimension of the shapes of aux data</param>
    /// <param name="aux_shape">the shapes of aux data</param>
    /// <param name="out">the returning handle</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let createSparseEx storage_type shape dev_type dev_id delay_alloc dtype aux_type (aux_shape : uint32 [][]) = 
        let mutable out = un
        let aux_ndims = aux_shape |> Array.map ulength
        let aux_shape = aux_shape |> Array.concat
        MXNDArrayCreateSparseEx(storage_type, shape, ulength shape, dev_type, dev_id, delay_alloc, dtype, ulength aux_type, aux_type, aux_ndims, aux_shape, &out) |> throwOnError "MXNDArrayCreateSparseEx"
        out

    /// <summary>create an empty sparse NDArray with specified shape and data type</summary>
    /// <param name="storage_type">the storage type of the ndarray</param>
    /// <param name="shape">the pointer to the shape</param>
    /// <param name="ndim">the dimension of the shape</param>
    /// <param name="dev_type">device type, specify device we want to take</param>
    /// <param name="dev_id">the device id of the specific device</param>
    /// <param name="delay_alloc">whether to delay allocation until
    ///       the narray is first mutated</param>
    /// <param name="dtype">data type of created array</param>
    /// <param name="num_aux">the number of aux data to support this ndarray</param>
    /// <param name="aux_type">data type of the aux data for the created array</param>
    /// <param name="aux_ndims">the dimension of the shapes of aux data</param>
    /// <param name="aux_shape">the shapes of aux data</param>
    /// <param name="out">the returning handle</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let createSparseEx64 storage_type shape dev_type dev_id delay_alloc dtype num_aux aux_type (aux_shape : int64 [][]) = 
        let mutable out = un
        let aux_ndims = aux_shape |> Array.map length
        let aux_shape = aux_shape |> Array.concat
        MXNDArrayCreateSparseEx64(storage_type, shape, length shape, dev_type, dev_id, delay_alloc, dtype, num_aux, aux_type, aux_ndims, aux_shape, &out) |> throwOnError "MXNDArrayCreateSparseEx64"
    
    /// <summary>Load list / dictionary of narrays from file content loaded into memory.
    ///This will load a list of ndarrays in a similar
    ///manner to MXNDArrayLoad, however, it loads from
    ///buffer containing the contents of a file, rather than
    ///from a specified file.</summary>
    /// <param name="ndarray_buffer">pointer to the start of the ndarray file content</param>
    /// <param name="size">size of the file</param>
    /// <param name="out_size">number of narray loaded.</param>
    /// <param name="out_arr">head of the returning narray handles.</param>
    /// <param name="out_name_size">size of output name arrray.</param>
    /// <param name="out_names">the names of returning NDArrays, can be NULL</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let loadFromBuffer ndarray_buffer size = 
        let mutable out_size = un
        let mutable out_arr = un
        let mutable out_name_size = un
        let mutable out_names = un
        MXNDArrayLoadFromBuffer(ndarray_buffer, size, &out_size, &out_arr, &out_name_size, &out_names) |> throwOnError "MXNDArrayLoadFromBuffer"
        let names = readStringArray out_name_size out_names
        let handles : NDArrayHandle [] = readStructArray out_size out_arr
        names, handles

    /// <summary>Copy src.data() to dst.data() if i = -1, else dst.aux_data(i) if i >= 0
    ///This function blocks. Do not use it in performance critical code.</summary>
    /// <param name="handle_dst">handle of a dst ndarray whose data/aux_data has been allocated</param>
    /// <param name="handle_src">handle of a src ndarray which has default storage type</param>
    /// <param name="i">dst data blob indicator</param>
    let syncCopyFromNDArray handle_dst handle_src i = 
        MXNDArraySyncCopyFromNDArray(handle_dst, handle_src, i) |> throwOnError "MXNDArraySyncCopyFromNDArray"

    /// <summary>check whether the NDArray format is valid</summary>
    /// <param name="full_check">if `True`, rigorous check, O(N) operations
    ///   Otherwise basic check, O(1) operations</param>
    let syncCheckFormat handle full_check = 
        MXNDArraySyncCheckFormat(handle, full_check) |> throwOnError "MXNDArraySyncCheckFormat"

    /// <summary>Wait until all the pending writes with respect NDArray are finished.
    /// Always call this before read data out synchronizely.</summary>
    /// <param name="handle">the NDArray handle</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let waitToRead handle = 
        MXNDArrayWaitToRead(handle) |> throwOnError "MXNDArrayWaitToRead"

    /// <summary>Wait until all the pending read/write with respect NDArray are finished.
    /// Always call this before write data into NDArray synchronizely.</summary>
    /// <param name="handle">the NDArray handle</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let waitToWrite handle = 
        MXNDArrayWaitToWrite(handle) |> throwOnError "MXNDArrayWaitToWrite"

    /// <summary>Create a reference view of NDArray that
    /// represents as DLManagedTensor
    /// Notice: MXNet uses asynchronous execution. Please call MXNDArrayWaitToRead or
    ///         MXNDArrayWaitToWrite before calling MXNDArrayToDLPack.</summary>
    /// <param name="handle">the handle to the ndarray</param>
    /// <param name="out_dlpack">pointer holder to get pointer of DLManagedTensor</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let toDLPack handle = 
        let mutable out_dlpack = un
        MXNDArrayToDLPack(handle, &out_dlpack) |> throwOnError "MXNDArrayToDLPack"
        out_dlpack

    /// <summary>Create a NDArray backed by a dlpack tensor.
    ///
    ///This allows us to create a NDArray using the memory
    ///allocated by an external deep learning framework
    ///that is DLPack compatible.
    ///
    ///The memory is retained until the NDArray went out of scope.</summary>
    /// <param name="dlpack">the pointer of the input DLManagedTensor</param>
    /// <param name="transient_handle">whether the handle will be destructed before calling the deleter</param>
    /// <param name="out_handle">pointer holder to get pointer of NDArray</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let fromDLPackEx dlpack transient_handle = 
        let mutable out_handle = un
        MXNDArrayFromDLPackEx(dlpack, transient_handle, &out_handle) |> throwOnError "MXNDArrayFromDLPackEx"
        out_handle

    /// <summary>Delete a dlpack tensor</summary>
    /// <param name="dlpack">the pointer of the input DLManagedTensor</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let callDLPackDeleter dlpack = 
        MXNDArrayCallDLPackDeleter(dlpack) |> throwOnError "MXNDArrayCallDLPackDeleter"

    /// <summary>get the type of the ith aux data in NDArray</summary>
    /// <param name="handle">the handle to the narray</param>
    /// <param name="i">the index of the aux data</param>
    /// <param name="out_type">pointer holder to get type of aux data</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getAuxType handle i : TypeFlag = 
        let mutable out_type = un
        MXNDArrayGetAuxType(handle, i, &out_type) |> throwOnError "MXNDArrayGetAuxType"
        enum out_type

    /// <summary>get the type of the ith aux data in NDArray</summary>
    /// <param name="handle">the handle to the narray</param>
    /// <param name="i">the index of the aux data</param>
    /// <param name="out_type">pointer holder to get type of aux data</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getAuxType64 handle i : TypeFlag = 
        let mutable out_type = un
        MXNDArrayGetAuxType64(handle, i, &out_type) |> throwOnError "MXNDArrayGetAuxType64"
        enum out_type

    /// <summary>Get a deep copy of the ith aux data blob
    ///in the form of an NDArray of default storage type.
    ///This function blocks. Do not use it in performance critical code.</summary>
    let getAuxNDArray handle i = 
        let mutable out = un
        MXNDArrayGetAuxNDArray(handle, i, &out) |> throwOnError "MXNDArrayGetAuxNDArray"
        out

    /// <summary>Get a deep copy of the ith aux data blob
    ///in the form of an NDArray of default storage type.
    ///This function blocks. Do not use it in performance critical code.</summary>
    let getAuxNDArray64 handle i = 
        let mutable out = un
        MXNDArrayGetAuxNDArray64(handle, i, &out) |> throwOnError "MXNDArrayGetAuxNDArray64"
        out

    /// <summary>Get a deep copy of the data blob
    ///in the form of an NDArray of default storage type.
    ///This function blocks. Do not use it in performance critical code.</summary>
    let getDataNDArray handle = 
        let mutable out = un
        MXNDArrayGetDataNDArray(handle, &out) |> throwOnError "MXNDArrayGetDataNDArray"
        out

    /// <summary>return gradient buffer attached to this NDArray</summary>
    /// <param name="handle">NDArray handle</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getGrad handle = 
        let mutable out = un
        MXNDArrayGetGrad(handle, &out) |> throwOnError "MXNDArrayGetGrad"
        out

    /// <summary>detach and ndarray from computation graph by clearing entry_</summary>
    /// <param name="handle">NDArray handle</param>
    /// <returns>A new NDArray detached from graph</returns>
    let detach handle = 
        let mutable out = un
        MXNDArrayDetach(handle, &out) |> throwOnError "MXNDArrayDetach"
        out

    /// <summary>set the flag for gradient array state.</summary>
    /// <param name="handle">NDArray handle</param>
    /// <param name="state">the new state.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let setGradState handle state = 
        MXNDArraySetGradState(handle, state) |> throwOnError "MXNDArraySetGradState"

    /// <summary>set the flag for gradient array state.</summary>
    /// <param name="handle">NDArray handle</param>
    /// <param name="state">the new state.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getGradState handle = 
        let mutable out = un
        MXNDArrayGetGradState(handle, &out) |> throwOnError "MXNDArrayGetGradState"
        out

module MXExecutor = 

    /// <summary>Delete the executor</summary>
    /// <param name="handle">the executor.</param>
    let free handle = 
        MXExecutorFree(handle) |> throwOnError "MXExecutorFree"

    /// <summary>Print the content of execution plan, used for debug.</summary>
    /// <param name="handle">the executor.</param>
    /// <returns>pointer to hold the output string of the printing.</returns>
    let print handle = 
        let mutable out_str = 0n
        MXExecutorPrint(handle, &out_str) |> throwOnError "MXExecutorPrint"
        str out_str

    /// <summary>Executor forward method</summary>
    /// <param name="handle">executor handle</param>
    /// <param name="is_train">int value to indicate whether the forward pass is for evaluation</param>
    let forward handle is_train = 
        MXExecutorForward(handle, is_train) |> throwOnError "MXExecutorForward"

    /// <summary>Excecutor run backward</summary>
    /// <param name="handle">execute handle</param>
    /// <param name="head_grads">NDArray handle for heads' gradient</param>
    let backward handle head_grads = 
        MXExecutorBackward(handle, ulength head_grads, head_grads) |> throwOnError "MXExecutorBackward"

    /// <summary>Excecutor run backward</summary>
    /// <param name="handle">execute handle</param>
    /// <param name="head_grads">NDArray handle for heads' gradient</param>
    /// <param name="is_train">int value to indicate whether the backward pass is for evaluation</param>
    let backwardEx handle head_grads is_train = 
        MXExecutorBackwardEx(handle, ulength head_grads, head_grads, is_train) |> throwOnError "MXExecutorBackwardEx"

    /// <summary>Get executor's head NDArray</summary>
    /// <param name="handle">executor handle</param>
    /// <returns>narray handles</returns>
    let outputs handle : NDArrayHandle[] = 
        let mutable out_size = un
        let mutable out = un
        MXExecutorOutputs(handle, &out_size, &out) |> throwOnError "MXExecutorOutputs"
        readStructArray out_size out

    /// <summary>Generate Executor from symbol</summary>
    /// <param name="symbol_handle">symbol handle</param>
    /// <param name="dev_type">device type</param>
    /// <param name="dev_id">device id</param>
    /// <param name="in_args">in args array</param>
    /// <param name="arg_grad_store">arg grads handle array</param>
    /// <param name="grad_req_type">grad req array</param>
    /// <param name="aux_states">auxiliary states array</param>
    /// <returns>output executor handle</returns>
    let bind symbol_handle dev_type dev_id in_args arg_grad_store grad_req_type aux_states = 
        let mutable out = un
        assert(length in_args = length arg_grad_store)
        assert(length in_args = length grad_req_type)
        MXExecutorBind(symbol_handle, dev_type, dev_id, ulength in_args, in_args, arg_grad_store, grad_req_type, ulength aux_states, aux_states, &out) |> throwOnError "MXExecutorBind"
        out

    /// <summary>Generate Executor from symbol,
    /// This is advanced function, allow specify group2ctx map.
    /// The user can annotate "ctx_group" attribute to name each group.</summary>
    /// <param name="symbol_handle">symbol handle</param>
    /// <param name="dev_type">device type of default context</param>
    /// <param name="dev_id">device id of default context</param>
    /// <param name="map_keys">keys of group2ctx map</param>
    /// <param name="map_dev_types">device type of group2ctx map</param>
    /// <param name="map_dev_ids">device id of group2ctx map</param>
    /// <param name="in_args">in args array</param>
    /// <param name="arg_grad_store">arg grads handle array</param>
    /// <param name="grad_req_type">grad req array</param>
    /// <param name="aux_states">auxiliary states array</param>
    /// <returns>output executor handle</returns>
    let bindX symbol_handle dev_type dev_id map_keys map_dev_types map_dev_ids in_args arg_grad_store grad_req_type aux_states = 
        let mutable out = un
        MXExecutorBindX(symbol_handle, dev_type, dev_id, ulength map_keys, map_keys, map_dev_types, map_dev_ids, ulength in_args, in_args, arg_grad_store, grad_req_type, ulength aux_states, aux_states, &out) |> throwOnError "MXExecutorBindX"
        out

    /// <summary>Generate Executor from symbol,
    /// This is advanced function, allow specify group2ctx map.
    /// The user can annotate "ctx_group" attribute to name each group.</summary>
    /// <param name="symbol_handle">symbol handle</param>
    /// <param name="dev_type">device type of default context</param>
    /// <param name="dev_id">device id of default context</param>
    /// <param name="map_keys">keys of group2ctx map</param>
    /// <param name="map_dev_types">device type of group2ctx map</param>
    /// <param name="map_dev_ids">device id of group2ctx map</param>
    /// <param name="in_args">in args array</param>
    /// <param name="arg_grad_store">arg grads handle array</param>
    /// <param name="grad_req_type">grad req array</param>
    /// <param name="aux_states">auxiliary states array</param>
    /// <param name="shared_exec">input executor handle for memory sharing</param>
    /// <returns>output executor handle</returns>
    let bindEX symbol_handle dev_type dev_id map_keys map_dev_types map_dev_ids in_args arg_grad_store grad_req_type aux_states shared_exec = 
        let mutable out = un
        MXExecutorBindEX(symbol_handle, dev_type, dev_id, ulength map_keys, map_keys, map_dev_types, map_dev_ids, ulength in_args, in_args, arg_grad_store, grad_req_type, ulength aux_states, aux_states, shared_exec, &out) |> throwOnError "MXExecutorBindEX"
        out

    // TODO: simpleBindEx wrapper
    //let simpleBindEx symbol_handle dev_type dev_id num_g2c_keys g2c_keys g2c_dev_types g2c_dev_ids provided_grad_req_list_len provided_grad_req_names provided_grad_req_types num_provided_arg_shapes provided_arg_shape_names provided_arg_shape_data provided_arg_shape_idx num_provided_arg_dtypes provided_arg_dtype_names provided_arg_dtypes num_provided_arg_stypes provided_arg_stype_names provided_arg_stypes num_shared_arg_names shared_arg_name_list shared_buffer_len shared_buffer_name_list shared_buffer_handle_list updated_shared_buffer_name_list updated_shared_buffer_handle_list num_in_args in_args arg_grads num_aux_states aux_states shared_exec_handle = 
    //    let mutable out = un
    //    MXExecutorSimpleBindEx(symbol_handle, dev_type, dev_id, num_g2c_keys, g2c_keys, g2c_dev_types, g2c_dev_ids, provided_grad_req_list_len, provided_grad_req_names, provided_grad_req_types, num_provided_arg_shapes, provided_arg_shape_names, provided_arg_shape_data, provided_arg_shape_idx, num_provided_arg_dtypes, provided_arg_dtype_names, provided_arg_dtypes, num_provided_arg_stypes, provided_arg_stype_names, provided_arg_stypes, num_shared_arg_names, shared_arg_name_list, shared_buffer_len, shared_buffer_name_list, shared_buffer_handle_list, updated_shared_buffer_name_list, updated_shared_buffer_handle_list, num_in_args, in_args, arg_grads, num_aux_states, aux_states, shared_exec_handle, &out) |> throwOnError "MXExecutorSimpleBindEx"

    /// <summary>Return a new executor with the same symbol and shared memory,
    /// but different input/output shapes.</summary>
    /// <param name="partial_shaping">Whether to allow changing the shape of unspecified arguments.</param>
    /// <param name="allow_up_sizing">Whether to allow allocating new ndarrays that's larger than the original.</param>
    /// <param name="dev_type">device type of default context</param>
    /// <param name="dev_id">device id of default context</param>
    /// <param name="map_keys">keys of group2ctx map</param>
    /// <param name="map_dev_types">device type of group2ctx map</param>
    /// <param name="map_dev_ids">device id of group2ctx map</param>
    /// <param name="in_args">in args array</param>
    /// <param name="arg_grads">arg grads handle array</param>
    /// <param name="aux_states">auxiliary states array</param>
    /// <param name="shared_exec">input executor handle for memory sharing</param>
    /// <param name="out">output executor handle</param>
    /// <returns>a new executor</returns>
    let reshapeEx partial_shaping allow_up_sizing dev_type dev_id map_keys map_dev_types map_dev_ids provided_arg_shape_names provided_arg_shape_data provided_arg_shape_idx in_args arg_grads aux_states shared_exec = 
        //REVIEW: num_in_args and num_aux_states should be a ptr?
        let mutable out = un
        MXExecutorReshapeEx(partial_shaping, allow_up_sizing, dev_type, dev_id, ulength map_keys, map_keys, map_dev_types, map_dev_ids, ulength provided_arg_shape_names, provided_arg_shape_names, provided_arg_shape_data, provided_arg_shape_idx, ulength in_args, in_args, arg_grads, ulength aux_states, aux_states, shared_exec, &out) |> throwOnError "MXExecutorReshapeEx"
    /// <summary>get optimized graph from graph executor</summary>
    let getOptimizedSymbol handle = 
        let mutable out = un
        MXExecutorGetOptimizedSymbol(handle, &out) |> throwOnError "MXExecutorGetOptimizedSymbol"
        out

    /// <summary>set a call back to notify the completion of operation</summary>
    let setMonitorCallback handle callback = 
        let mutable callback_handle = 0n
        MXExecutorSetMonitorCallback(handle, callback, &callback_handle) |> throwOnError "MXExecutorSetMonitorCallback"
        callback_handle

    /// <summary>set a call back to notify the completion of operation</summary>
    /// <param name="monitor_all">If true, monitor both input and output, otherwise monitor output only.</param>
    let setMonitorCallbackEX handle callback monitor_all = 
        let mutable callback_handle = 0n
        MXExecutorSetMonitorCallbackEX(handle, callback, &callback_handle, monitor_all) |> throwOnError "MXExecutorSetMonitorCallbackEX"
        callback_handle
        
module MXPred = 
    open CPredictApi
    /// <summary>create a predictor</summary>
    /// <param name="symbol_json_str">The JSON string of the symbol.</param>
    /// <param name="param_bytes">The in-memory raw bytes of parameter ndarray file.</param>
    /// <param name="param_size">The size of parameter ndarray file.</param>
    /// <param name="dev_type">The device type, 1: cpu, 2:gpu</param>
    /// <param name="dev_id">The device id of the predictor.</param>
    /// <param name="input_keys">The name of input argument.
    ///   For feedforward net, this is {"data"}</param>
    /// <param name="input_shape_indptr">Index pointer of shapes of each input node.
    ///   The length of this array = num_input_nodes + 1.
    ///   For feedforward net that takes 4 dimensional input, this is {0, 4}.</param>
    /// <param name="input_shape_data">A flattened data of shapes of each input node.
    ///   For feedforward net that takes 4 dimensional input, this is the shape data.</param>
    /// <returns>The created predictor handle.</returns>
    let create symbol_json_str param_bytes param_size dev_type dev_id input_keys input_shape_indptr input_shape_data = 
        let mutable out = un
        MXPredCreate(symbol_json_str, param_bytes, param_size, dev_type, dev_id, uint32(Array.length input_keys), input_keys, input_shape_indptr, input_shape_data, &out) |> throwOnError "MXPredCreate"
        out

    /// <summary>create a predictor</summary>
    /// <param name="symbol_json_str">The JSON string of the symbol.</param>
    /// <param name="param_bytes">The in-memory raw bytes of parameter ndarray file.</param>
    /// <param name="param_size">The size of parameter ndarray file.</param>
    /// <param name="dev_type">The device type, 1: cpu, 2: gpu</param>
    /// <param name="dev_id">The device id of the predictor.</param>
    /// <param name="num_input_nodes">Number of input nodes to the net.
    ///   For feedforward net, this is 1.</param>
    /// <param name="input_keys">The name of the input argument.
    ///   For feedforward net, this is {"data"}</param>
    /// <param name="input_shape_indptr">Index pointer of shapes of each input node.
    ///   The length of this array = num_input_nodes + 1.
    ///   For feedforward net that takes 4 dimensional input, this is {0, 4}.</param>
    /// <param name="input_shape_data">A flattened data of shapes of each input node.
    ///   For feedforward net that takes 4 dimensional input, this is the shape data.</param>
    /// <param name="num_provided_arg_dtypes">  The length of provided_arg_dtypes.</param>
    /// <param name="provided_arg_dtype_names">  The provided_arg_dtype_names the names of args for which dtypes are provided.</param>
    /// <param name="provided_arg_dtypes">  The provided_arg_dtypes the dtype provided</param>
    /// <returns>The created predictor handle.</returns>
    let createEx symbol_json_str param_bytes param_size dev_type dev_id input_keys input_shape_indptr input_shape_data provided_arg_dtype_names provided_arg_dtypes = 
        let mutable out = un
        MXPredCreateEx(symbol_json_str, param_bytes, param_size, dev_type, dev_id, uint32(Array.length input_keys), input_keys, input_shape_indptr, input_shape_data, uint32(Array.length provided_arg_dtype_names), provided_arg_dtype_names, provided_arg_dtypes, &out) 
        |> throwOnError "MXPredCreateEx"
        out


    /// <summary>create a predictor with customized outputs</summary>
    /// <param name="symbol_json_str">The JSON string of the symbol.</param>
    /// <param name="param_bytes">The in-memory raw bytes of parameter ndarray file.</param>
    /// <param name="param_size">The size of parameter ndarray file.</param>
    /// <param name="dev_type">The device type, 1: cpu, 2:gpu</param>
    /// <param name="dev_id">The device id of the predictor.</param>
    /// <param name="num_input_nodes">Number of input nodes to the net,
    ///   For feedforward net, this is 1.</param>
    /// <param name="input_keys">The name of input argument.
    ///   For feedforward net, this is {"data"}</param>
    /// <param name="input_shape_indptr">Index pointer of shapes of each input node.
    ///   The length of this array = num_input_nodes + 1.
    ///   For feedforward net that takes 4 dimensional input, this is {0, 4}.</param>
    /// <param name="input_shape_data">A flattened data of shapes of each input node.
    ///   For feedforward net that takes 4 dimensional input, this is the shape data.</param>
    /// <param name="num_output_nodes">Number of output nodes to the net,</param>
    /// <param name="output_keys">The name of output argument.
    ///   For example {"global_pool"}</param>
    /// <returns>The created predictor handle.</returns>
    let createPartialOut symbol_json_str param_bytes param_size dev_type dev_id input_keys input_shape_indptr input_shape_data output_keys = 
        let mutable out = un
        MXPredCreatePartialOut(symbol_json_str, param_bytes, param_size, dev_type, dev_id, input_keys |> Array.length |> uint32, input_keys, input_shape_indptr, input_shape_data, output_keys |> Array.length |> uint32, output_keys, &out) |> throwOnError "MXPredCreatePartialOut"
        out

    /// <summary>create predictors for multiple threads. One predictor for a thread.</summary>
    /// <param name="symbol_json_str">The JSON string of the symbol.</param>
    /// <param name="param_bytes">The in-memory raw bytes of parameter ndarray file.</param>
    /// <param name="param_size">The size of parameter ndarray file.</param>
    /// <param name="dev_type">The device type, 1: cpu, 2:gpu</param>
    /// <param name="dev_id">The device id of the predictor.</param>
    /// <param name="num_input_nodes">Number of input nodes to the net,
    ///   For feedforward net, this is 1.</param>
    /// <param name="input_keys">The name of input argument.
    ///   For feedforward net, this is {"data"}</param>
    /// <param name="input_shape_indptr">Index pointer of shapes of each input node.
    ///   The length of this array = num_input_nodes + 1.
    ///   For feedforward net that takes 4 dimensional input, this is {0, 4}.</param>
    /// <param name="input_shape_data">A flattened data of shapes of each input node.
    ///   For feedforward net that takes 4 dimensional input, this is the shape data.</param>
    /// <param name="num_threads">The number of threads that we'll run the predictors.</param>
    /// <returns>An array of created predictor handles. The array has to be large
    ///  enough to keep `num_threads` predictors.</returns>
    let createMultiThread symbol_json_str param_bytes param_size dev_type dev_id input_keys input_shape_indptr input_shape_data num_threads = 
        let mutable out = un
        MXPredCreateMultiThread(symbol_json_str, param_bytes, param_size, dev_type, dev_id, input_keys |> Array.length |> uint32, input_keys, input_shape_indptr, input_shape_data, num_threads, &out) |> throwOnError "MXPredCreateMultiThread"
        out

    /// <summary>Change the input shape of an existing predictor.</summary>
    /// <param name="input_keys">The name of input argument.
    ///   For feedforward net, this is {"data"}</param>
    /// <param name="input_shape_indptr">Index pointer of shapes of each input node.
    ///   The length of this array = num_input_nodes + 1.
    ///   For feedforward net that takes 4 dimensional input, this is {0, 4}.</param>
    /// <param name="input_shape_data">A flattened data of shapes of each input node.
    ///   For feedforward net that takes 4 dimensional input, this is the shape data.</param>
    /// <param name="handle">The original predictor handle.</param>
    /// <returns>The reshaped predictor handle.</returns>
    let reshape input_keys input_shape_indptr input_shape_data handle = 
        let mutable out = un
        MXPredReshape(input_keys |> Array.length |> uint32, input_keys, input_shape_indptr, input_shape_data, handle, &out) |> throwOnError "MXPredReshape"
        out

    /// <summary>Get the shape of output node.
    /// The returned shape_data and shape_ndim is only valid before next call to MXPred function.</summary>
    /// <param name="handle">The handle of the predictor.</param>
    /// <param name="index">The index of output node, set to 0 if there is only one output.</param>
    let getOutputShape handle index : uint32[] = 
        let mutable shape_data = 0n
        let mutable shape_ndim = un
        MXPredGetOutputShape(handle, index, &shape_data, &shape_ndim) |> throwOnError "MXPredGetOutputShape"
        readStructArray shape_ndim shape_data

        
    /// <summary>Get the dtype of output node.
    ///The returned data type is only valid before next call to MXPred function.</summary>
    /// <param name="handle">The handle of the predictor.</param>
    /// <param name="index">The index of the output node, set to 0 if there is only one output.</param>
    let getOutputType handle index = 
        let mutable out_dtype = un
        MXPredGetOutputType(handle, uint32 index, &out_dtype) |> throwOnError "MXPredGetOutputType"
        out_dtype

    /// <summary>Set the input data of predictor.</summary>
    /// <param name="handle">The predictor handle.</param>
    /// <param name="key">The name of input node to set.
    ///    For feedforward net, this is "data".</param>
    /// <param name="data">The pointer to the data to be set, with the shape specified in MXPredCreate.</param>
    /// <param name="size">The size of data array, used for safety check.</param>
    let setInput handle key data size = 
        MXPredSetInput(handle, key, data, size) |> throwOnError "MXPredSetInput"

    /// <summary>Run a forward pass to get the output.</summary>
    /// <param name="handle">The handle of the predictor.</param>
    let forward handle = 
        MXPredForward(handle) |> throwOnError "MXPredForward"

    /// <summary>Run a interactive forward pass to get the output.
    /// This is helpful for displaying progress of prediction which can be slow.
    /// User must call PartialForward from step=0, keep increasing it until step_left=0.</summary>
    /// <code>int step_left = 1;
    ///for (int step = 0; step_left != 0; ++step) {
    ///   MXPredPartialForward(handle, step, &step_left);
    ///   printf("Current progress [%d/%d]", step, step + step_left + 1);
    ///}</code>
    /// <param name="handle">The handle of the predictor.</param>
    /// <param name="step">The current step to run forward on.</param>
    /// <param name="step_left">The number of steps left</param>
    /// <returns>0 when success, -1 when failure.</returns>
    let partialForward handle step = 
        let mutable step_left = un
        MXPredPartialForward(handle, step, &step_left) |> throwOnError "MXPredPartialForward"

    /// <summary>Get the output value of prediction.</summary>
    /// <param name="handle">The handle of the predictor.</param>
    /// <param name="index">The index of output node, set to 0 if there is only one output.</param>
    /// <param name="data">User allocated data to hold the output.</param>
    /// <param name="size">The size of data array, used for safe checking.</param>
    let getOutput handle index data size = 
        MXPredGetOutput(handle, index, data, size) |> throwOnError "MXPredGetOutput"

    /// <summary>Free a predictor handle.</summary>
    /// <param name="handle">The handle of the predictor.</param>
    let free handle = 
        MXPredFree(handle) |> throwOnError "MXPredFree"

    /// <summary>set a call back to notify the completion of operation and allow for
    ///additional monitoring</summary>
    let setMonitorCallback handle callback monitor_all = 
        let mutable callback_handle = un
        MXPredSetMonitorCallback(handle, callback, &callback_handle, monitor_all) |> throwOnError "MXPredSetMonitorCallback"
        callback_handle

module MXNDList = 
    open CPredictApi
    /// <summary>Get an element from list</summary>
    /// <param name="handle">The handle to the NDArray</param>
    /// <param name="index">The index in the list</param>
    /// <param name="out_key">The output key of the item</param>
    /// <param name="out_data">The data region of the item</param>
    /// <param name="out_shape">The shape of the item.</param>
    /// <param name="out_ndim">The number of dimension in the shape.</param>
    let create nd_file_bytes nd_file_size : uint32 [] = 
        let mutable out = 0n
        let mutable out_length = un
        MXNDListCreate(nd_file_bytes, nd_file_size, &out, &out_length) |> throwOnError "MXNDListCreate"
        readStructArray out_length out

    
    /// <summary>Get an element from list</summary>
    /// <param name="handle">The handle to the NDArray</param>
    /// <param name="index">The index in the list</param>
    /// <param name="out_key">The output key of the item</param>
    /// <param name="out_data">The data region of the item</param>
    /// <param name="out_shape">The shape of the item.</param>
    /// <param name="out_ndim">The number of dimension in the shape.</param>
    let get handle index = 
        failwith "not implemented" //TODO: MXNDListGet
        let mutable out_key = un
        let mutable out_data = un
        let mutable out_shape = un
        let mutable out_ndim = un
        MXNDListGet(handle, index, &out_key, &out_data, &out_shape, &out_ndim) |> throwOnError "MXNDListGet"

        
    /// <summary>Free a MXAPINDList</summary>
    /// <param name="handle">The handle of the MXAPINDList.</param>
    let free handle = 
        MXNDListFree(handle) |> throwOnError "MXNDListFree"


open CNNVMApi

module NNVM = 
    
    /// <summary>Set the last error message needed by C API</summary>
    /// <param name="msg">The error message to set.</param>
    let apiSetLastError msg = NNAPISetLastError(msg) 

    /// <summary>return str message of the last error
    /// all function in this file will return 0 when success
    /// and -1 when an error occured,
    /// NNGetLastError can be called to retrieve the error
    ///
    /// this function is threadsafe and can be called by different thread</summary>
    /// <returns>error info</returns>
    /// <summary>list all the available operator names, include entries.</summary>
    /// <returns>the output operator name array.</returns>
    let listAllOpNames()  = 
        let mutable out_size = un
        let mutable out_array = un
        NNListAllOpNames(&out_size, &out_array) |> throwOnError "NNListAllOpNames"
        readStringArray out_size out_array

    /// <summary>Get operator handle given name.</summary>
    /// <param name="op_name">The name of the operator.</param>
    /// <returns>The returning op handle.</returns>
    let getOpHandle op_name = 
        let mutable op_out = un
        NNGetOpHandle(op_name, &op_out) |> throwOnError "NNGetOpHandle"
        op_out


    /// <summary>list all the available operators.
    /// This won't include the alias, use ListAllNames
    /// instead to get all alias names.</summary>
    /// <param name="out_size">the size of returned array</param>
    /// <param name="out_array">the output AtomicSymbolCreator array</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let listUniqueOps() : AtomicSymbolCreatorHandle[] = 
        let mutable out_size = un
        let mutable out_array = un
        NNListUniqueOps(&out_size, &out_array) |> throwOnError "NNListUniqueOps"
        readStructArray out_size out_array


    /// <summary>Get the detailed information about atomic symbol.</summary>
    /// <param name="op">The operator handle.</param>
    /// <param name="real_name">The returned name of the creator.
    ///  This name is not the alias name of the atomic symbol.</param>
    /// <param name="description">The returned description of the symbol.</param>
    /// <param name="num_doc_args">Number of arguments that contain documents.</param>
    /// <param name="arg_names">Name of the arguments of doc args</param>
    /// <param name="arg_type_infos">Type informations about the arguments.</param>
    /// <param name="arg_descriptions">Description information about the arguments.</param>
    /// <param name="return_type">Return type of the function, if any.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getOpInfo op =
        let mutable real_name = un
        let mutable description = un
        let mutable num_doc_args = un 
        let mutable arg_names = un 
        let mutable arg_type_infos = un 
        let mutable arg_descriptions = un 
        let mutable return_type = un
        NNGetOpInfo(op, &real_name, &description, &num_doc_args, &arg_names, &arg_type_infos, &arg_descriptions, &return_type) |> throwOnError "NNGetOpInfo"
        {
            Name = str real_name
            Description = str description
            Arguments = 
                [|
                    for i = 0 to int num_doc_args - 1 do 
                        {
                            Name = readString i arg_names
                            Description = readString i arg_descriptions
                            TypeInfo = readString i arg_type_infos
                        }
                |]
            ReturnTypeInfo = str return_type
            KeyVarNumArgs = 0n
        }

module NNSymbol = 
    /// <summary>Create an AtomicSymbol functor.</summary>
    /// <param name="op">The operator handle</param>
    /// <param name="num_param">the number of parameters</param>
    /// <param name="keys">the keys to the params</param>
    /// <param name="vals">the vals of the params</param>
    /// <param name="out">pointer to the created symbol handle</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let createAtomicSymbol op num_param keys vals = 
        let mutable out = un
        NNSymbolCreateAtomicSymbol(op, num_param, keys, vals, &out) |> throwOnError "NNSymbolCreateAtomicSymbol"
        out

    /// <summary>Create a Variable Symbol.</summary>
    /// <param name="name">name of the variable</param>
    /// <param name="out">pointer to the created symbol handle</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let createVariable name = 
        let mutable out = un
        NNSymbolCreateVariable(name, &out) |> throwOnError "NNSymbolCreateVariable"
        out

    /// <summary>Create a Symbol by grouping list of symbols together</summary>
    /// <param name="num_symbols">number of symbols to be grouped</param>
    /// <param name="symbols">array of symbol handles</param>
    /// <param name="out">pointer to the created symbol handle</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let createGroup num_symbols symbols = 
        let mutable out = un
        NNSymbolCreateGroup(num_symbols, symbols, &out) |> throwOnError "NNSymbolCreateGroup"
        out

    /// <summary>Add src_dep to the handle as control dep.</summary>
    /// <param name="handle">The symbol to add dependency edges on.</param>
    /// <param name="src_dep">the source handles.</param>
    let addControlDeps handle src_dep = 
        NNAddControlDeps(handle, src_dep) |> throwOnError "NNAddControlDeps"

    /// <summary>Free the symbol handle.</summary>
    /// <param name="symbol">the symbol</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let free symbol = 
        NNSymbolFree(symbol) |> throwOnError "NNSymbolFree"

    /// <summary>Copy the symbol to another handle</summary>
    /// <param name="symbol">the source symbol</param>
    /// <param name="out">used to hold the result of copy</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let copy symbol = 
        let mutable out = un
        NNSymbolCopy(symbol, &out) |> throwOnError "NNSymbolCopy"

    /// <summary>Print the content of symbol, used for debug.</summary>
    /// <param name="symbol">the symbol</param>
    /// <param name="out_str">pointer to hold the output string of the printing.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let print symbol = 
        let mutable out = un
        NNSymbolPrint(symbol, &out) |> throwOnError "NNSymbolPrint"

    /// <summary>Get string attribute from symbol</summary>
    /// <param name="symbol">the source symbol</param>
    /// <param name="key">The key of the symbol.</param>
    /// <param name="out">The result attribute, can be NULL if the attribute do not exist.</param>
    /// <param name="success">Whether the result is contained in out.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getAttr symbol key  = 
        let mutable out = un
        let mutable success = un
        NNSymbolGetAttr(symbol, key, &out, &success) |> throwOnError "NNSymbolGetAttr"
        if success = 0 || out <= 0n then 
            None
        else    
            Some(str out)

    /// <summary>Set string attribute from symbol.
    /// NOTE: Setting attribute to a symbol can affect the semantics(mutable/immutable) of symbolic graph.
    ///
    /// Safe recommendaton: use  immutable graph
    /// - Only allow set attributes during creation of new symbol as optional parameter
    ///
    /// Mutable graph (be careful about the semantics):
    /// - Allow set attr at any point.
    /// - Mutating an attribute of some common node of two graphs can cause confusion from user.</summary>
    /// <param name="symbol">the source symbol</param>
    /// <param name="num_param">Number of parameters to set.</param>
    /// <param name="keys">The keys of the attribute</param>
    /// <param name="values">The value to be set</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let setAttrs symbol num_param keys values = 
        NNSymbolSetAttrs(symbol, num_param, keys, values) |> throwOnError "NNSymbolSetAttrs"

    /// <summary>Get all attributes from symbol, including all descendents.</summary>
    /// <param name="symbol">the source symbol</param>
    /// <param name="recursive_option">0 for recursive, 1 for shallow.</param>
    /// <param name="out_size">The number of output attributes</param>
    /// <param name="out">2*out_size strings representing key value pairs.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let listAttrs symbol recursive_option = 
        let mutable out_size = un
        let mutable out = un
        NNSymbolListAttrs(symbol, recursive_option, &out_size, &out) |> throwOnError "NNSymbolListAttrs"
        readStringArray out_size out

    /// <summary>List inputs variables in the symbol.</summary>
    /// <param name="symbol">the symbol</param>
    /// <param name="option">The option to list the inputs
    ///  option=0 means list all arguments.
    ///  option=1 means list arguments that are readed only by the graph.
    ///  option=2 means list arguments that are mutated by the graph.</param>
    /// <param name="out_size">output size</param>
    /// <param name="out_sym_array">the output array.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let listInputVariables symbol option : SymbolHandle [] = 
        let mutable out_size = un
        let mutable out_sym_array = un
        NNSymbolListInputVariables(symbol, option, &out_size, &out_sym_array) |> throwOnError "NNSymbolListInputVariables"
        readStructArray out_size out_sym_array

    /// <summary>List input names in the symbol.</summary>
    /// <param name="symbol">the symbol</param>
    /// <param name="option">The option to list the inputs
    ///  option=0 means list all arguments.
    ///  option=1 means list arguments that are readed only by the graph.
    ///  option=2 means list arguments that are mutated by the graph.</param>
    /// <param name="out_size">output size</param>
    /// <param name="out_str_array">pointer to hold the output string array</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let listInputNames symbol option = 
        let mutable out_size = un
        let mutable out_sym_array = un
        NNSymbolListInputNames(symbol, option, &out_size, &out_sym_array) |> throwOnError "NNSymbolListInputNames"
        readStringArray out_size out_sym_array

    /// <summary>List returns names in the symbol.</summary>
    /// <param name="symbol">the symbol</param>
    /// <param name="out_size">output size</param>
    /// <param name="out_str_array">pointer to hold the output string array</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let listOutputNames symbol = 
        let mutable out_size = un
        let mutable out_str_array = un
        NNSymbolListOutputNames(symbol, &out_size, &out_str_array) |> throwOnError "NNSymbolListOutputNames"
        readStringArray out_size out_str_array

    /// <summary>Supply number of outputs of the symbol.</summary>
    /// <param name="symbol">the symbol</param>
    /// <param name="output_count">number of outputs</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getNumOutputs symbol = 
        let mutable output_count = un
        NNSymbolGetNumOutputs(symbol, &output_count) |> throwOnError "NNSymbolGetNumOutputs"
        output_count

    /// <summary>Get a symbol that contains all the internals.</summary>
    /// <param name="symbol">The symbol</param>
    /// <param name="out">The output symbol whose outputs are all the internals.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getInternals symbol = 
        let mutable out = un
        NNSymbolGetInternals(symbol, &out) |> throwOnError "NNSymbolGetInternals"
        out

    /// <summary>Get a symbol that contains only direct children.</summary>
    /// <param name="symbol">The symbol</param>
    /// <param name="out">The output symbol whose outputs are the direct children.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getChildren symbol = 
        let mutable out = un
        NNSymbolGetChildren(symbol, &out) |> throwOnError "NNSymbolGetChildren"
        out

    /// <summary>Get index-th outputs of the symbol.</summary>
    /// <param name="symbol">The symbol</param>
    /// <param name="index">the Index of the output.</param>
    /// <param name="out">The output symbol whose outputs are the index-th symbol.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getOutput symbol index = 
        let mutable out = un
        NNSymbolGetOutput(symbol, index, &out) |> throwOnError "NNSymbolGetOutput"
        out

    /// <summary>Compose the symbol on other symbols.
    ///
    /// This function will change the sym hanlde.
    /// To achieve function apply behavior, copy the symbol first
    /// before apply.</summary>
    /// <param name="sym">the symbol to apply</param>
    /// <param name="name">the name of symbol</param>
    /// <param name="num_args">number of arguments</param>
    /// <param name="keys">the key of keyword args (optional)</param>
    /// <param name="args">arguments to sym</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let compose sym name num_args keys args = 
        NNSymbolCompose(sym, name, num_args, keys, args) |> throwOnError "NNSymbolCompose"

module NNGraph = 
    /// <summary>create a graph handle from symbol</summary>
    /// <param name="symbol">The symbol representing the graph.</param>
    /// <param name="graph">The graph handle created.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let create symbol graph = 
        NNGraphCreate(symbol, graph) |> throwOnError "NNGraphCreate"

    /// <summary>free the graph handle</summary>
    /// <param name="handle">The handle to be freed.</param>
    let free handle = 
        NNGraphFree(handle) |> throwOnError "NNGraphFree"

    /// <summary>Get a new symbol from the graph.</summary>
    /// <param name="graph">The graph handle.</param>
    /// <param name="symbol">The corresponding symbol</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getSymbol graph symbol = 
        NNGraphGetSymbol(graph, symbol) |> throwOnError "NNGraphGetSymbol"

    /// <summary>Get Set a attribute in json format.
    ///This feature allows pass graph attributes back and forth in reasonable speed.</summary>
    /// <param name="handle">The graph handle.</param>
    /// <param name="key">The key to the attribute.</param>
    /// <param name="json_value">The value need to be in format [type_name, value],
    /// Where type_name is a registered type string in C++ side via DMLC_JSON_ENABLE_ANY.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let setJSONAttr handle key json_value = 
        NNGraphSetJSONAttr(handle, key, json_value) |> throwOnError "NNGraphSetJSONAttr"

    /// <summary>Get a serialized attrirbute from graph.
    ///This feature allows pass graph attributes back and forth in reasonable speed.</summary>
    /// <param name="handle">The graph handle.</param>
    /// <param name="key">The key to the attribute.</param>
    /// <param name="json_out">The result attribute, can be NULL if the attribute do not exist.
    /// The json_out is an array of [type_name, value].
    /// Where the type_name is a registered type string in C++ side via DMLC_JSON_ENABLE_ANY.</param>
    /// <param name="success">Whether the result is contained in out.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getJSONAttr handle key = 
        let mutable json_out = un
        let mutable success = un
        NNGraphGetJSONAttr(handle, key, &json_out, &success) |> throwOnError "NNGraphGetJSONAttr"
        if success = 0 || json_out <= 0n then 
            None 
        else 
            Some(str json_out)

    /// <summary>Set a attribute whose type is std::vector<NodeEntry> in c++
    ///This feature allows pass List of symbolic variables for gradient request.</summary>
    /// <remarks>This is beta feature only used for test purpos</remarks>
    /// <param name="handle">The graph handle.</param>
    /// <param name="key">The key to the attribute.</param>
    /// <param name="list">The symbol whose outputs represents the list of NodeEntry to be passed.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let setNodeEntryListAttr handle key list = 
        NNGraphSetNodeEntryListAttr_(handle, key, list) |> throwOnError "NNGraphSetNodeEntryListAttr"

    /// <summary>Apply passes on the src graph.</summary>
    /// <param name="src">The source graph handle.</param>
    /// <param name="num_pass">The number of pass to be applied.</param>
    /// <param name="pass_names">The names of the pass.</param>
    /// <param name="dst">The result graph.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let applyPasses src num_pass pass_names = 
        let mutable dst = un
        NNGraphApplyPasses(src, num_pass, pass_names, &dst) |> throwOnError "NNGraphApplyPasses"
        dst

module MXDataIter = 
    /// <summary>List all the available iterator entries</summary>
    /// <param name="out_size">the size of returned iterators</param>
    /// <param name="out_array">the output iteratos entries</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let list()  : DataIterCreator []= 
        let mutable out_size = un
        let mutable out_array = un
        MXListDataIters(&out_size, &out_array) |> throwOnError "MXListDataIters"
        readStructArray out_size out_array

    /// <summary>Init an iterator, init with parameters
    ///the array size of passed in arguments</summary>
    /// <param name="handle">of the iterator creator</param>
    /// <param name="keys">parameter keys</param>
    /// <param name="vals">parameter values</param>
    let create handle keys vals = 
        let mutable out = un
        assert(length keys = length vals)
        MXDataIterCreateIter(handle, ulength keys, keys, vals, &out) |> throwOnError "MXDataIterCreateIter"
        out

    /// <summary>Get the detailed information about data iterator.</summary>
    /// <param name="creator">the DataIterCreator.</param>
    /// <param name="name">The returned name of the creator.</param>
    /// <param name="description">The returned description of the symbol.</param>
    /// <param name="num_args">Number of arguments.</param>
    /// <param name="arg_names">Name of the arguments.</param>
    /// <param name="arg_type_infos">Type informations about the arguments.</param>
    /// <param name="arg_descriptions">Description information about the arguments.</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getInfo creator = 
        let mutable name = un
        let mutable description = un
        let mutable num_args = un
        let mutable arg_names = un
        let mutable arg_type_infos = un
        let mutable arg_descriptions = un
        MXDataIterGetIterInfo(creator, &name, &description, &num_args, &arg_names, &arg_type_infos, &arg_descriptions) |> throwOnError "MXDataIterGetIterInfo"
        {
            Name = str name
            Description = str description
            Arguments = 
                [|
                    for i = 0 to int num_args - 1 do 
                        {
                            Name = readString i arg_names
                            Description = readString i arg_descriptions
                            TypeInfo = readString i arg_type_infos
                        }
                |]
        }

    /// <summary>Free the handle to the IO module</summary>
    /// <param name="handle">the handle pointer to the data iterator</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let free handle = 
        MXDataIterFree(handle) |> throwOnError "MXDataIterFree"

    /// <summary>Move iterator to next position</summary>
    /// <param name="handle">the handle to iterator</param>
    /// <param name="out">return value of next</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let next handle = 
        let mutable out = un
        MXDataIterNext(handle, &out) |> throwOnError "MXDataIterNext"
        out

    /// <summary>Call iterator.Reset</summary>
    /// <param name="handle">the handle to iterator</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let beforeFirst handle = 
        MXDataIterBeforeFirst(handle) |> throwOnError "MXDataIterBeforeFirst"

    /// <summary>Get the handle to the NDArray of underlying data</summary>
    /// <param name="handle">the handle pointer to the data iterator</param>
    /// <param name="out">handle to underlying data NDArray</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getData handle = 
        let mutable out = un
        MXDataIterGetData(handle, &out) |> throwOnError "MXDataIterGetData"
        out

    /// <summary>Get the image index by array.</summary>
    /// <param name="handle">the handle pointer to the data iterator</param>
    /// <returns>output index of the array.</returns>
    let getIndex handle : uint64[] = 
        let mutable out_index = un
        let mutable out_size = un
        MXDataIterGetIndex(handle, &out_index, &out_size) |> throwOnError "MXDataIterGetIndex"
        readStructArray out_size out_index


    /// <summary>Get the padding number in current data batch</summary>
    /// <param name="handle">the handle pointer to the data iterator</param>
    /// <returns>pad number</returns>
    let getPadNum handle = 
        let mutable pad = un
        MXDataIterGetPadNum(handle, &pad) |> throwOnError "MXDataIterGetPadNum"
        un

    /// <summary>Get the handle to the NDArray of underlying label</summary>
    /// <param name="handle">the handle pointer to the data iterator</param>
    /// <param name="out">the handle to underlying label NDArray</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getLabel handle = 
        let mutable out = un
        MXDataIterGetLabel(handle, &out) |> throwOnError "MXDataIterGetLabel"
        out

module MXKVStore = 

    /// <summary>Initialized ps-lite environment variables</summary>
    /// <param name="num_vars">number of variables to initialize</param>
    /// <param name="keys">environment keys</param>
    /// <param name="vals">environment values</param>
    let mXInitPSEnv num_vars keys vals = 
        MXInitPSEnv(num_vars, keys, vals) |> throwOnError "MXInitPSEnv"

    /// <summary>Create a kvstore</summary>
    /// <param name="type">the type of KVStore</param>
    /// <param name="out">The output type of KVStore</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let create ``type`` = 
        let mutable out = un
        MXKVStoreCreate(``type``, &out) |> throwOnError "MXKVStoreCreate"

    /// <summary>Set parameters to use low-bit compressed gradients</summary>
    /// <param name="handle">handle to the kvstore</param>
    /// <param name="keys">keys for compression parameters</param>
    /// <param name="vals">values for compression parameters</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let setGradientCompression handle num_params keys vals = 
        MXKVStoreSetGradientCompression(handle, num_params, keys, vals) |> throwOnError "MXKVStoreSetGradientCompression"

    /// <summary>Delete a KVStore handle.</summary>
    /// <param name="handle">handle to the kvstore</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let free handle = 
        MXKVStoreFree(handle) |> throwOnError "MXKVStoreFree"

    /// <summary>Init a list of (key,value) pairs in kvstore</summary>
    /// <param name="handle">handle to the kvstore</param>
    /// <param name="num">the number of key-value pairs</param>
    /// <param name="keys">the list of keys</param>
    /// <param name="vals">the list of values</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let init handle num keys vals = 
        MXKVStoreInit(handle, num, keys, vals) |> throwOnError "MXKVStoreInit"

    /// <summary>Init a list of (key,value) pairs in kvstore, where each key is a string</summary>
    /// <param name="handle">handle to the kvstore</param>
    /// <param name="num">the number of key-value pairs</param>
    /// <param name="keys">the list of keys</param>
    /// <param name="vals">the list of values</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let initEx handle num keys vals = 
        MXKVStoreInitEx(handle, num, keys, vals) |> throwOnError "MXKVStoreInitEx"

    /// <summary>Push a list of (key,value) pairs to kvstore</summary>
    /// <param name="handle">handle to the kvstore</param>
    /// <param name="num">the number of key-value pairs</param>
    /// <param name="keys">the list of keys</param>
    /// <param name="vals">the list of values</param>
    /// <param name="priority">the priority of the action</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let push handle keys vals priority = 
        MXKVStorePush(handle, ulength keys, keys, vals, priority) |> throwOnError "MXKVStorePush"

    /// <summary>Push a list of (key,value) pairs to kvstore, where each key is a string</summary>
    /// <param name="handle">handle to the kvstore</param>
    /// <param name="keys">the list of keys</param>
    /// <param name="vals">the list of values</param>
    /// <param name="priority">the priority of the action</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let pushEx handle keys vals priority = 
        MXKVStorePushEx(handle, ulength keys, keys, vals, priority) |> throwOnError "MXKVStorePushEx"

    /// <summary>pull a list of (key, value) pairs from the kvstore</summary>
    /// <param name="handle">handle to the kvstore</param>
    /// <param name="keys">the list of keys</param>
    /// <param name="vals">the list of values</param>
    /// <param name="priority">the priority of the action</param>
    /// <param name="ignore_sparse">whether to ignore sparse arrays in the request</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let pullWithSparse handle keys priority ignore_sparse : NDArrayHandle [] = 
        let mutable vals = un //REVIEW is this right or should it be preallocated?
        MXKVStorePullWithSparse(handle, ulength keys, keys, &vals, priority, ignore_sparse) |> throwOnError "MXKVStorePullWithSparse"
        readStructArray (ulength keys) vals

    /// <summary>pull a list of (key, value) pairs from the kvstore, where each key is a string</summary>
    /// <param name="handle">handle to the kvstore</param>
    /// <param name="num">the number of key-value pairs</param>
    /// <param name="keys">the list of keys</param>
    /// <param name="vals">the list of values</param>
    /// <param name="priority">the priority of the action</param>
    /// <param name="ignore_sparse">whether to ignore sparse arrays in the request</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let pullWithSparseEx handle keys priority ignore_sparse : NDArrayHandle[] = 
        let mutable vals = un //REVIEW is this right or should it be preallocated?
        MXKVStorePullWithSparseEx(handle, ulength keys, keys, &vals, priority, ignore_sparse) |> throwOnError "MXKVStorePullWithSparseEx"
        readStructArray (ulength keys) vals

    /// <summary>pull a list of (key, value) pairs from the kvstore</summary>
    /// <param name="handle">handle to the kvstore</param>
    /// <param name="num">the number of key-value pairs</param>
    /// <param name="keys">the list of keys</param>
    /// <param name="vals">the list of values</param>
    /// <param name="priority">the priority of the action</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let pull handle keys priority : NDArrayHandle[] = 
        let mutable vals = un //REVIEW is this right or should it be preallocated?
        MXKVStorePull(handle, ulength keys, keys, &vals, priority) |> throwOnError "MXKVStorePull"
        readStructArray (ulength keys) vals

    /// <summary>pull a list of (key, value) pairs from the kvstore, where each key is a string</summary>
    /// <param name="handle">handle to the kvstore</param>
    /// <param name="num">the number of key-value pairs</param>
    /// <param name="keys">the list of keys</param>
    /// <param name="vals">the list of values</param>
    /// <param name="priority">the priority of the action</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let pullEx handle num keys vals priority : NDArrayHandle[] = 
        let mutable vals = un //REVIEW is this right or should it be preallocated?
        MXKVStorePullEx(handle, ulength keys, keys, &vals, priority) |> throwOnError "MXKVStorePullEx"
        readStructArray (ulength keys) vals

    /// <summary>pull a list of (key, value) pairs from the kvstore, where each key is an integer.
    ///       The NDArray pulled back will be in row_sparse storage with only the specified
    ///       row_ids present based row_ids (others rows are zeros).</summary>
    /// <param name="handle">handle to the kvstore</param>
    /// <param name="num">the number of key-value pairs</param>
    /// <param name="keys">the list of keys</param>
    /// <param name="vals">the list of values</param>
    /// <param name="row_ids">the list of row_id NDArrays</param>
    /// <param name="priority">the priority of the action</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let pullRowSparse handle keys priority = 
        let mutable vals = un //REVIEW is this right or should it be preallocated?
        let mutable row_ids = un
        MXKVStorePullRowSparse(handle, ulength keys, keys, &vals, &row_ids, priority) |> throwOnError "MXKVStorePullRowSparse"
        let vals = readStructArray (ulength keys) vals : NDArrayHandle[]
        let row_ids = readStructArray (ulength keys) row_ids : NDArrayHandle[]
        vals, row_ids

    /// <summary>pull a list of (key, value) pairs from the kvstore, where each key is a string.
    ///       The NDArray pulled back will be in row_sparse storage with only the specified
    ///       row_ids present based row_ids (others rows are zeros).</summary>
    /// <param name="handle">handle to the kvstore</param>
    /// <param name="num">the number of key-value pairs</param>
    /// <param name="keys">the list of keys</param>
    /// <param name="vals">the list of values</param>
    /// <param name="row_ids">the list of row_id NDArrays</param>
    /// <param name="priority">the priority of the action</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let pullRowSparseEx handle keys priority = 
        let mutable vals = un //REVIEW is this right or should it be preallocated?
        let mutable row_ids = un
        MXKVStorePullRowSparseEx(handle, ulength keys, keys, &vals, &row_ids, priority) |> throwOnError "MXKVStorePullRowSparseEx"
        let vals = readStructArray (ulength keys) vals : NDArrayHandle[]
        let row_ids = readStructArray (ulength keys) row_ids : NDArrayHandle[]
        vals, row_ids

    /// <summary>push and pull a list of (key, value) pairs from the kvstore</summary>
    /// <param name="handle">handle to the kvstore</param>
    /// <param name="vkeys">the list of keys for the values to be pushed</param>
    /// <param name="okeys">the list of keys for the values to be pulled</param>
    /// <param name="vals">the list of values</param>
    /// <param name="priority">the priority of the action</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let pushPull handle vkeys okeys vals priority : NDArrayHandle[] = 
        let mutable outs = un
        MXKVStorePushPull(handle, ulength vkeys, vkeys, ulength okeys, okeys, vals, &outs, priority) |> throwOnError "MXKVStorePushPull"
        readStructArray (ulength okeys) outs

    /// <summary>push and pull a list of (key, value) pairs from the kvstore,
    ///where each key is a string</summary>
    /// <param name="handle">handle to the kvstore</param>
    /// <param name="vnum">the number of key-value pairs corresponding to vkeys</param>
    /// <param name="vkeys">the list of keys for the values to be pushed</param>
    /// <param name="onum">the number of key-value pairs corresponding to okeys</param>
    /// <param name="okeys">the list of keys for the values to be pulled</param>
    /// <param name="vals">the list of values</param>
    /// <param name="outs">the list of outputs</param>
    /// <param name="priority">the priority of the action</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let pushPullEx handle vkeys okeys vals priority = 
        let mutable outs = un
        MXKVStorePushPullEx(handle, ulength vkeys, vkeys, ulength okeys, okeys, vals, &outs, priority) |> throwOnError "MXKVStorePushPullEx"
        readStructArray (ulength okeys) outs

    /// <summary>register a push updater</summary>
    /// <param name="handle">handle to the KVStore</param>
    /// <param name="updater">udpater function</param>
    /// <param name="updater_handle">The additional handle used to invoke the updater</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let setUpdater handle updater updater_handle = 
        MXKVStoreSetUpdater(handle, updater, updater_handle) |> throwOnError "MXKVStoreSetUpdater"

    /// <summary>register a push updater with int keys and one with string keys</summary>
    /// <param name="handle">handle to the KVStore</param>
    /// <param name="updater">updater function with int keys</param>
    /// <param name="str_updater">updater function with string keys</param>
    /// <param name="updater_handle">The additional handle used to invoke the updater</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let setUpdaterEx handle updater str_updater updater_handle = 
        MXKVStoreSetUpdaterEx(handle, updater, str_updater, updater_handle) |> throwOnError "MXKVStoreSetUpdaterEx"

    /// <summary>get the type of the kvstore</summary>
    /// <param name="handle">handle to the KVStore</param>
    /// <param name="type">a string type</param>
    /// <returns>0 when success, -1 when failure happens</returns>
    let getType handle = 
        let mutable tp = un
        MXKVStoreGetType(handle, &tp) |> throwOnError "MXKVStoreGetType"
        str tp
