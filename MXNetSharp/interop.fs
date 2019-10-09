namespace MXNetSharp.Interop

#nowarn "9"

open System
open System.Runtime.InteropServices
open CApi

type KeyVarNumArgs = IntPtr
exception MXNetException of string*string with
    override x.Message = 
        match x :> Exception with 
        | MXNetException(call,msg) -> sprintf "%s: %s" call msg
        | _ -> failwith "unreachable"

// defined in cpp-package/include/mxnet-cpp/ndarray.h
// https://github.com/apache/incubator-mxnet/blob/745a41ca1a6d74a645911de8af46dece03db93ea/cpp-package/include/mxnet-cpp/ndarray.h#L41
type DeviceType = 
    | CPU = 1
    | GPU = 2
    | CPUPinned = 3 

// defined in mshadow/base.h
// https://github.com/apache/incubator-mxnet/blob/618c4811e417fb86cbb3fc0f7f38d55972eeb2af/3rdparty/mshadow/mshadow/base.h#L306
type TypeFlag = 
    | None = -1
    | Float32 = 0
    | Float64 = 1
    | Float16 = 2
    | Uint8 = 3
    | Int32 = 4
    | Int8  = 5
    | Int64 = 6

type Context = 
    {
        DeviceType : DeviceType
        DeviceId : int
    }

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

module Helper = 
    let ulength (a : 'a []) = a.Length |> uint32
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

    //TODO: Fix Doc string
    /// <summary>Seed the global random number generator of the given device.</summary>
    /// <param name="seed">the random number seed.</param> 
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
        assert(Array.length keys = Array.length args)
        MXSymbolCompose(sym, name, uint32(Array.length keys), keys, args) |> throwOnError "MXSymbolCompose"

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
        str out

    /// <summary>Free the symbol handle.</summary>
    /// <param name="symbol">the symbol</param>
    let free symbol = MXSymbolFree(symbol) |> throwOnError "MXSymbolFree"
        
    /// <summary>Generate atomic symbol (able to be composed) from a source symbol</summary>
    /// <param name="sym_handle">source symbol</param>
    let genAtomicSymbolFromSymbol sym_handle : AtomicSymbolHandle =
        let mutable ret_sym_handle = un
        MXGenAtomicSymbolFromSymbol(sym_handle, &ret_sym_handle) |> throwOnError "MXGenAtomicSymbolFromSymbol"
        ret_sym_handle
        

        

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
    let inline create (shape : ^a[]) (dev_type : DeviceType) dev_id (delay_alloc : bool) : NDArrayHandle = 
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
    let inline createEx (shape : ^a[]) (dev_type : DeviceType) dev_id (delay_alloc : bool) (dtype : TypeFlag) : NDArrayHandle = 
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
        {
            DeviceType = enum out_dev_type
            DeviceId = out_dev_id
        }
                

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
    /// <param name="outputs">output NDArrays</param>
    /// <param name="param_keys">keys for keyword parameters</param>
    /// <param name="param_vals">values for keyword parameters</param>
    let imperativeInvoke creator inputs parameterKeys parameterValues : NDArrayHandle [] = 
        let mutable num_outputs = un
        let mutable outputs = un
        assert(Array.length parameterKeys = Array.length parameterValues)
        MXImperativeInvoke(creator, Array.length inputs, inputs, &num_outputs, &outputs, Array.length parameterKeys, parameterKeys, parameterValues) |> throwOnError "MXImperativeInvoke"
        readStructArray num_outputs outputs

        
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
        MXExecutorBackward(handle, head_grads |> Array.length |> uint32, head_grads) |> throwOnError "MXExecutorBackward"

    /// <summary>Excecutor run backward</summary>
    /// <param name="handle">execute handle</param>
    /// <param name="head_grads">NDArray handle for heads' gradient</param>
    /// <param name="is_train">int value to indicate whether the backward pass is for evaluation</param>
    let backwardEx handle head_grads is_train = 
        MXExecutorBackwardEx(handle, head_grads |> Array.length |> uint32, head_grads, is_train) |> throwOnError "MXExecutorBackwardEx"

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

    // TODO: simpleBindEx wrapper
    //let simpleBindEx symbol_handle dev_type dev_id num_g2c_keys g2c_keys g2c_dev_types g2c_dev_ids provided_grad_req_list_len provided_grad_req_names provided_grad_req_types num_provided_arg_shapes provided_arg_shape_names provided_arg_shape_data provided_arg_shape_idx num_provided_arg_dtypes provided_arg_dtype_names provided_arg_dtypes num_provided_arg_stypes provided_arg_stype_names provided_arg_stypes num_shared_arg_names shared_arg_name_list shared_buffer_len shared_buffer_name_list shared_buffer_handle_list updated_shared_buffer_name_list updated_shared_buffer_handle_list num_in_args in_args arg_grads num_aux_states aux_states shared_exec_handle = 
    //    let mutable out = un
    //    MXExecutorSimpleBindEx(symbol_handle, dev_type, dev_id, num_g2c_keys, g2c_keys, g2c_dev_types, g2c_dev_ids, provided_grad_req_list_len, provided_grad_req_names, provided_grad_req_types, num_provided_arg_shapes, provided_arg_shape_names, provided_arg_shape_data, provided_arg_shape_idx, num_provided_arg_dtypes, provided_arg_dtype_names, provided_arg_dtypes, num_provided_arg_stypes, provided_arg_stype_names, provided_arg_stypes, num_shared_arg_names, shared_arg_name_list, shared_buffer_len, shared_buffer_name_list, shared_buffer_handle_list, updated_shared_buffer_name_list, updated_shared_buffer_handle_list, num_in_args, in_args, arg_grads, num_aux_states, aux_states, shared_exec_handle, &out) |> throwOnError "MXExecutorSimpleBindEx"

    (* TODO: MXExecutorReshapeEx
    /// <summary>Return a new executor with the same symbol and shared memory,
    ///but different input/output shapes.</summary>
    /// <param name="partial_shaping">Whether to allow changing the shape of unspecified arguments.</param>
    /// <param name="allow_up_sizing">Whether to allow allocating new ndarrays that's larger than the original.</param>
    /// <param name="dev_type">device type of default context</param>
    /// <param name="dev_id">device id of default context</param>
    /// <param name="num_map_keys">size of group2ctx map</param>
    /// <param name="map_keys">keys of group2ctx map</param>
    /// <param name="map_dev_types">device type of group2ctx map</param>
    /// <param name="map_dev_ids">device id of group2ctx map</param>
    /// <param name="num_in_args">length of in_args</param>
    /// <param name="in_args">in args array</param>
    /// <param name="arg_grads">arg grads handle array</param>
    /// <param name="num_aux_states">length of auxiliary states</param>
    /// <param name="aux_states">auxiliary states array</param>
    /// <param name="shared_exec">input executor handle for memory sharing</param>
    /// <param name="out">output executor handle</param>
    /// <returns>a new executor</returns>
    let reshapeEx partial_shaping allow_up_sizing dev_type dev_id num_map_keys map_keys map_dev_types map_dev_ids num_provided_arg_shapes provided_arg_shape_names provided_arg_shape_data provided_arg_shape_idx num_in_args in_args arg_grads num_aux_states aux_states shared_exec = 
        let mutable out = un
        MXExecutorReshapeEx(partial_shaping, allow_up_sizing, dev_type, dev_id, num_map_keys, map_keys, map_dev_types, map_dev_ids, num_provided_arg_shapes, provided_arg_shape_names, provided_arg_shape_data, provided_arg_shape_idx, num_in_args, in_args, arg_grads, num_aux_states, aux_states, shared_exec, &out) |> throwOnError "MXExecutorReshapeEx"
    *)

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
