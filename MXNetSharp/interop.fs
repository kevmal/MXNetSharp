namespace MXNetSharp.Interop

open System
open System.Runtime.InteropServices
open CApi

type KeyVarNumArgs = IntPtr
exception MXNetException of string

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
    let throwOnError (returnCode : int) = 
        if returnCode <> 0 then 
            raise (MXNetException(MXGetLastError() |> str))
                

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
    let loadLib(path : string) = MXLoadLib path |> throwOnError

    /// Get list of features supported on the runtime
    let infoFeatures() : LibFeature [] =
        let mutable a = un
        let mutable sz = un
        MXLibInfoFeatures(&a, &sz) |> throwOnError
        Array.init sz (fun i -> Marshal.PtrToStructure( a + IntPtr(i*sizeof<LibFeature>)))
        
    /// <summary>Seed all global random number generators in mxnet.</summary>
    /// <param name="seed">the random number seed.</param>
    let randomSeed (seed : int) = MXRandomSeed seed |> throwOnError

    //TODO: Fix Doc string
    /// <summary>Seed the global random number generator of the given device.</summary>
    /// <param name="seed">the random number seed.</param> 
    let randomSeedContext seed dev_type dev_id = MXRandomSeedContext(seed,dev_type,dev_id) |> throwOnError

    /// <summary>Notify the engine about a shutdown,
    /// This can help engine to print less messages into display.
    /// User do not have to call this function.</summary>
    let notifyShutdown() = MXNotifyShutdown() |> throwOnError


    /// <summary>Get the number of GPUs.</summary>
    let getGpuCount() = 
        let mutable out = un 
        MXGetGPUCount(&out) |> throwOnError
        out

    /// <summary>get the MXNet library version as an integer</summary>
    let getVersion() = 
        let mutable out = un 
        MXGetVersion(&out) |> throwOnError
        out
        


    /// list all the available functions handles
    /// most user can use it to list all the needed functions
    let listFunctions() : FunctionHandle[] =     
        let mutable outSize = un
        let mutable outArray = un
        MXListFunctions(&outSize, &outArray) |> throwOnError
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
        MXFuncGetInfo(functionHandle,&name,&description,&numArgs,&argNames,&arg_type_infos,&arg_descriptions,&return_type) |> throwOnError
        {
            Name = name
            Description = str description
            Arguments = 
                [|
                    for i = 0 to int numArgs - 1 do 
                        {
                            Name = readString i argNames
                            Description = readString i description
                            TypeInfo = readString i arg_type_infos
                        }
                |]
            ReturnTypeInfo = str return_type
        }
    
    /// list all the available operator names, include entries.
    let listAllOpNames() =
       let mutable out_size = un
       let mutable out_array = un
       MXListAllOpNames(&out_size, &out_array) |> throwOnError
       readStringArray out_size out_array

module MXSymbol = 

    /// <summary>list all the available AtomicSymbolEntry</summary>
    let listAtomicSymbolCreators() = 
        let mutable outSize = un
        let mutable outArray = un
        MXSymbolListAtomicSymbolCreators(&outSize, &outArray) |> throwOnError
        readPtrArray outSize outArray

    /// <summary>Get the name of an atomic symbol.</summary>
    /// <param name="creator">the AtomicSymbolCreator.</param>
    let getAtomicSymbolName (creator : AtomicSymbolCreator) : string = 
        let mutable name = un
        MXSymbolGetAtomicSymbolName(creator, &name) |> throwOnError
        str name

    /// <summary>Get the detailed information about atomic symbol.</summary>
    /// <param name="creator">the AtomicSymbolCreator.</param>
    let getAtomicSymbolInfo (creator : AtomicSymbolCreator) = 
        let mutable name = un
        let mutable description = un
        let mutable numArgs = un
        let mutable argNames = un
        let mutable arg_type_infos = un
        let mutable arg_descriptions = un
        let mutable key_var_num_args = un
        let mutable return_type = un
        MXSymbolGetAtomicSymbolInfo(creator,&name,&description,&numArgs,&argNames,&arg_type_infos,&arg_descriptions,&key_var_num_args,&return_type) |> throwOnError
        {
            Name = str name
            Description = str description
            Arguments = 
                [|
                    for i = 0 to int numArgs - 1 do 
                        {
                            Name = readString i argNames
                            Description = readString i description
                            TypeInfo = readString i arg_type_infos
                        }
                |]
            ReturnTypeInfo = str return_type
            KeyVarNumArgs = readIntPtr 0 key_var_num_args
        }


module MXNDArray = 
    /// <summary>create a NDArray handle that is not initialized
    /// can be used to pass in as mutate variables
    /// to hold the result of NDArray</summary>
    let createNone() : NDArrayHandle = 
        let mutable out = IntPtr.Zero 
        MXNDArrayCreateNone(&out) |> throwOnError
        out
    
    /// <summary>create a NDArray with specified shape</summary>
    /// <param name="shape">shape</param>
    /// <param name="dev_type">device type, specify device we want to take</param>
    /// <param name="dev_id">the device id of the specific device</param>
    /// <param name="delay_alloc">whether to delay allocation until
    ///   the narray is first mutated</param>
    let inline create (shape : ^a[]) dev_type dev_id delay_alloc : NDArrayHandle = 
        let mutable out = IntPtr.Zero 
        MXNDArrayCreate(shape |> Array.map uint32, uint32 shape.Length, dev_type, dev_id, delay_alloc, &out) |> throwOnError
        out

    /// <summary>create a NDArray with specified shape and data type</summary>
    /// <param name="shape">the pointer to the shape</param>
    /// <param name="dev_type">device type, specify device we want to take</param>
    /// <param name="dev_id">the device id of the specific device</param>
    /// <param name="delay_alloc">whether to delay allocation until
    ///   the narray is first mutated</param>
    /// <param name="dtype">data type of created array</param>
    let inline createEx (shape : ^a[]) dev_type dev_id delay_alloc dtype : NDArrayHandle = 
        let mutable out = IntPtr.Zero 
        MXNDArrayCreateEx(shape |> Array.map uint32, uint32 shape.Length, dev_type, dev_id, delay_alloc, dtype, &out) |> throwOnError
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
        MXNDArrayCreateEx64(shape |> Array.map int64, int shape.Length, dev_type, dev_id, delay_alloc, dtype, &out) |> throwOnError
        out

    /// <summary>create a NDArray handle that is loaded from raw bytes.</summary>
    /// <param name="buf">the head of the raw bytes</param>
    let loadFromRawBytes (buf : byte[]) = 
        let mutable out = IntPtr.Zero 
        MXNDArrayLoadFromRawBytes(buf, int64 buf.Length, &out) |> throwOnError
        out

    /// <summary>save the NDArray into raw bytes.</summary>
    /// <param name="handle">the NDArray handle</param>
    let saveRawBytes (handle : NDArrayHandle) = 
        let mutable out_size = un
        let mutable out_buf = IntPtr.Zero
        MXNDArraySaveRawBytes(handle, &out_size, &out_buf) |> throwOnError
        readByteArray (int out_size) out_buf


    /// <summary>Save list of narray into the file.</summary>
    /// <param name="fname">name of the file.</param>
    /// <param name="num_args">number of arguments to save.</param>
    /// <param name="args">the array of NDArrayHandles to be saved.</param>
    /// <param name="keys">the name of the NDArray, optional, can be NULL</param>
    let save fname (args : NDArrayHandle []) keys = 
        MXNDArraySave(fname, uint32 args.Length, args, keys) |> throwOnError
        
    /// <summary>Load list of narray from the file.</summary>
    /// <param name="fname">name of the file.</param>
    /// <returns>names, ndarrays</returns>
    let load fname = 
        let mutable out_size = un
        let mutable out_arr = IntPtr.Zero
        let mutable out_name_size = un
        let mutable out_names = IntPtr.Zero
        MXNDArrayLoad(fname, &out_size, &out_arr, &out_name_size, &out_names) |> throwOnError
        let arrs = readPtrArray out_size out_arr
        let names = readStringArray out_name_size out_names 
        names,arrs
        
    /// <summary>free the narray handle</summary>
    /// <param name="handle">the handle to be freed</param>
    let free (handle : NDArrayHandle) = MXNDArrayFree(handle) |> throwOnError

    /// <summary>Slice the NDArray along axis 0.</summary>
    /// <param name="handle">the handle to the NDArray</param>
    /// <param name="slice_begin">The beginning index of slice</param>
    /// <param name="slice_end">The ending index of slice</param>
    /// <returns>The NDArrayHandle of sliced NDArray</returns>
    let inline slice (handle : NDArrayHandle) slice_begin slice_end =  
        let mutable out = IntPtr.Zero 
        MXNDArraySlice(handle, uint32 slice_begin, uint32 slice_end, &out) |> throwOnError
        out
           
    /// <summary>Slice the NDArray along axis 0.</summary>
    /// <param name="handle">the handle to the NDArray</param>
    /// <param name="slice_begin">The beginning index of slice</param>
    /// <param name="slice_end">The ending index of slice</param>
    /// <returns>The NDArrayHandle of sliced NDArray</returns>
    let slice64 (handle : NDArrayHandle) slice_begin slice_end =  
        let mutable out = IntPtr.Zero 
        MXNDArraySlice64(handle, slice_begin, slice_end, &out) |> throwOnError
        out
           
    /// <summary>Index the NDArray along axis 0.</summary>
    /// <param name="handle">the handle to the NDArray</param>
    /// <param name="idx">the index</param>
    /// <returns>The NDArrayHandle of output NDArray</returns>
    let inline at (handle : NDArrayHandle) index =  
        let mutable out = IntPtr.Zero 
        MXNDArrayAt(handle, uint32 index, &out) |> throwOnError
        out
        
    /// <summary>Index the NDArray along axis 0.</summary>
    /// <param name="handle">the handle to the NDArray</param>
    /// <param name="idx">the index</param>
    /// <returns>The NDArrayHandle of output NDArray</returns>
    let at64 (handle : NDArrayHandle) index =  
        let mutable out = IntPtr.Zero 
        MXNDArrayAt64(handle, index, &out) |> throwOnError
        out
    
    
    /// <summary>get the storage type of the array</summary>
    let getStorageType handle = 
        let mutable out = un 
        MXNDArrayGetStorageType(handle, &out) |> throwOnError
        out

    /// <summary>Reshape the NDArray.</summary>
    /// <param name="handle">the handle to the narray</param>
    /// <param name="dims">new shape</param>
    /// <returns>the NDArrayHandle of reshaped NDArray</returns>    
    let reshape handle (dims : int []) = 
        let mutable out = un
        MXNDArrayReshape(handle, dims.Length, dims, &out) |> throwOnError
        out
       
    /// <summary>Reshape the NDArray.</summary>
    /// <param name="handle">the handle to the narray</param>
    /// <param name="dims">new shape</param>
    /// <returns>the NDArrayHandle of reshaped NDArray</returns>    
    let reshape64 handle (dims : int64 []) reverse = 
        let mutable out = un
        MXNDArrayReshape64(handle, dims.Length, dims, reverse, &out) |> throwOnError
        out

    /// <summary>get the shape of the array</summary>
    /// <param name="handle">the handle to the narray</param>
    let getShape handle : int [] = 
        let mutable out_dim = un
        let mutable out_pdata = un
        MXNDArrayGetShapeEx(handle, &out_dim, &out_pdata) |> throwOnError
        if out_dim > 0 then 
            readStructArray out_dim out_pdata
        else    
            Array.empty
    
    /// <summary>get the shape of the array</summary>
    /// <param name="handle">the handle to the narray</param>
    let getShape64 handle : int64 [] = 
        let mutable out_dim = un
        let mutable out_pdata = un
        MXNDArrayGetShapeEx64(handle, &out_dim, &out_pdata) |> throwOnError
        if out_dim > 0 then 
            readStructArray out_dim out_pdata
        else    
            Array.empty
        
        
         