module MXNetSharp.Interop.CApi

open System
open System.Runtime.InteropServices
type size_t = int64
[<Literal>]
let MXNETLIB = "libmxnet"
(*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *)
/// \file c_api.h
/// <summary>C API of mxnet</summary>
/// <summary>Inhibit C++ name-mangling for MXNet functions.</summary>
/// <summary>Keep the default value in C++</summary>
/// <summary>MXNET_DLL prefix for windows</summary>
/// <summary>manually define unsigned int</summary>
type mx_uint = uint32
/// <summary>manually define float</summary>
type mx_float = float32
/// <summary>data type to store dim size</summary>
type dim_t = int64
// all the handles are simply void *
// will be casted internally to specific pointers types
// these typedefs are mainly used for readablity reasons
/// <summary>handle to NDArray</summary>
type NDArrayHandle = IntPtr
/// <summary>handle to a mxnet narray function that changes NDArray</summary>
type FunctionHandle = IntPtr
/// <summary>handle to a function that takes param and creates symbol</summary>
type AtomicSymbolCreatorHandle = IntPtr
/// <summary>handle to cached operator</summary>
type CachedOpHandle = IntPtr
/// <summary>handle to a symbol that can be bind as operator</summary>
type SymbolHandle = IntPtr
/// <summary>handle to a AtomicSymbol</summary>
type AtomicSymbolHandle = IntPtr
/// <summary>handle to an Executor</summary>
type ExecutorHandle = IntPtr
/// <summary>handle a dataiter creator</summary>
type DataIterCreator = IntPtr
/// <summary>handle to a DataIterator</summary>
type DataIterHandle = IntPtr
/// <summary>handle to KVStore</summary>
type KVStoreHandle = IntPtr
/// <summary>handle to RecordIO</summary>
type RecordIOHandle = IntPtr
/// <summary>handle to MXRtc</summary>
type RtcHandle = IntPtr
/// <summary>handle to rtc cuda module</summary>
type CudaModuleHandle = IntPtr
/// <summary>handle to rtc cuda kernel</summary>
type CudaKernelHandle = IntPtr
/// <summary>handle to a Profile object (domain, duration, counter, etc.)</summary>
type ProfileHandle = IntPtr
/// <summary>handle to DLManagedTensor</summary>
type DLManagedTensorHandle = IntPtr
/// <summary>handle to Context</summary>
type ContextHandle = IntPtr
/// <summary>handle to Engine FnProperty</summary>
type EngineFnPropertyHandle = IntPtr
/// <summary>handle to Engine VarHandle</summary>
type EngineVarHandle = IntPtr
/// <summary>Engine asynchronous operation</summary>
type EngineAsyncFunc = delegate of IntPtr * IntPtr * IntPtr -> unit
/// <summary>Engine synchronous operation</summary>
type EngineSyncFunc = delegate of IntPtr * IntPtr -> unit
/// <summary>Callback to free the param for EngineAsyncFunc/EngineSyncFunc</summary>
type EngineFuncParamDeleter = delegate of IntPtr -> unit
type ExecutorMonitorCallback = delegate of string * NDArrayHandle * IntPtr -> unit
/// <summary>Monitor callback called at operator level for cached op</summary>
type CachedOpMonitorCallback = delegate of string * string * NDArrayHandle -> unit
type NativeOpInfo_forward = delegate of int * float32[] byref * int[] * int[] byref * int[] * IntPtr -> unit
type NativeOpInfo_backward = delegate of int * float32[] byref * int[] * int[] byref * int[] * IntPtr -> unit
type NativeOpInfo_infer_shape = delegate of int * int[] * int[] byref * IntPtr -> unit
type NativeOpInfo_list_outputs = delegate of string[] byref * IntPtr -> unit
type NativeOpInfo_list_arguments = delegate of string[] byref * IntPtr -> unit
[<Struct; StructLayout(LayoutKind.Sequential); NoComparison>]
type NativeOpInfo =
    val forward : NativeOpInfo_forward
    val backward : NativeOpInfo_backward
    val infer_shape : NativeOpInfo_infer_shape
    val list_outputs : NativeOpInfo_list_outputs
    val list_arguments : NativeOpInfo_list_arguments
// all functions also pass a payload void* pointer
    val p_forward : IntPtr
    val p_backward : IntPtr
    val p_infer_shape : IntPtr
    val p_list_outputs : IntPtr
    val p_list_arguments : IntPtr

type NDArrayOpInfo_forward = delegate of int * IntPtr * int[] * IntPtr -> bool
type NDArrayOpInfo_backward = delegate of int * IntPtr * int[] * IntPtr -> bool
type NDArrayOpInfo_infer_shape = delegate of int * int[] * int[] byref * IntPtr -> bool
type NDArrayOpInfo_list_outputs = delegate of string[] byref * IntPtr -> bool
type NDArrayOpInfo_list_arguments = delegate of string[] byref * IntPtr -> bool
type NDArrayOpInfo_declare_backward_dependency = delegate of int[] * int[] * int[] * int[] * int[] byref * IntPtr -> bool
[<Struct; StructLayout(LayoutKind.Sequential); NoComparison>]
type NDArrayOpInfo =
    val forward : NDArrayOpInfo_forward
    val backward : NDArrayOpInfo_backward
    val infer_shape : NDArrayOpInfo_infer_shape
    val list_outputs : NDArrayOpInfo_list_outputs
    val list_arguments : NDArrayOpInfo_list_arguments
    val declare_backward_dependency : NDArrayOpInfo_declare_backward_dependency
// all functions also pass a payload void* pointer
    val p_forward : IntPtr
    val p_backward : IntPtr
    val p_infer_shape : IntPtr
    val p_list_outputs : IntPtr
    val p_list_arguments : IntPtr
    val p_declare_backward_dependency : IntPtr

type MXGenericCallback = delegate of unit -> int
type MXCallbackList_callbacks = delegate of unit -> int
[<Struct; StructLayout(LayoutKind.Sequential, Pack = 8)>]
type MXCallbackList =
    val mutable num_callbacks : int
    val mutable callbacks : IntPtr
    val mutable contexts : IntPtr

[<Struct; StructLayout(LayoutKind.Sequential)>]
type LibFeature =
    val name : string
    val enabled : bool

type CustomOpCallbacks =
    | kCustomOpDelete = 0
    | kCustomOpForward = 1
    | kCustomOpBackward = 2
type CustomOpPropCallbacks =
    | kCustomOpPropDelete = 0
    | kCustomOpPropListArguments = 1
    | kCustomOpPropListOutputs = 2
    | kCustomOpPropListAuxiliaryStates = 3
    | kCustomOpPropInferShape = 4
    | kCustomOpPropDeclareBackwardDependency = 5
    | kCustomOpPropCreateOperator = 6
    | kCustomOpPropInferType = 7
    | kCustomOpPropInferStorageType = 8
    | kCustomOpPropBackwardInferStorageType = 9
type CustomOpFBFunc = delegate of size : int * ptrs : IntPtr * tags : IntPtr * reqs : IntPtr * isTrain  : bool * state : IntPtr -> bool
type CustomOpDelFunc = delegate of state : IntPtr -> bool
type CustomOpListFunc = delegate of args : IntPtr byref * state : IntPtr -> bool
type CustomOpInferShapeFunc = delegate of numInput : int * ndims : IntPtr * shapes : IntPtr * state : IntPtr -> bool
type CustomOpInferStorageTypeFunc = delegate of numInput : int * stypes : IntPtr * state : IntPtr -> bool
type CustomOpBackwardInferStorageTypeFunc = delegate of numInput : IntPtr * stypes : IntPtr * tags : IntPtr * state : IntPtr -> bool
type CustomOpInferTypeFunc = delegate of numInput : int * types : IntPtr * state : IntPtr -> bool
type CustomOpBwdDepFunc = delegate of outGrad : IntPtr * inData : IntPtr * outData :  IntPtr * numDeps : int byref * rdeps : IntPtr byref * state : IntPtr -> bool
type CustomOpCreateFunc = delegate of context : string * numInputs : int * shapes : IntPtr * ndims : IntPtr * dtypes : IntPtr * ret :  MXCallbackList byref * state : IntPtr -> bool
type CustomOpPropCreator = delegate of op_type : string * numArgs : int * keys : IntPtr * values : IntPtr * ret : MXCallbackList byref -> bool
type CustomFunctionCallbacks =
    | kCustomFunctionBackward = 0
    | kCustomFunctionDelete = 1
type CustomFunctionBwdFunc = delegate of int * int * IntPtr * int[] * int * IntPtr -> bool
type CustomFunctionDelFunc = delegate of IntPtr -> bool
/// <summary>return str message of the last error
/// all function in this file will return 0 when success
/// and -1 when an error occured,
/// MXGetLastError can be called to retrieve the error
///
/// this function is threadsafe and can be called by different thread</summary>
/// <returns>error info</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern IntPtr MXGetLastError()

//-------------------------------------
// Part 0: Global State setups
//-------------------------------------
/// <summary>Load library dynamically</summary>
/// <param name="path">to the library .so file</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXLoadLib(string path)

/// <summary>Get list of features supported on the runtime</summary>
/// <param name="libFeature">pointer to array of LibFeature</param>
/// <param name="size">of the array</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXLibInfoFeatures( [<Out>] IntPtr& libFeature, [<Out>] int& size)

/// <summary>Seed all global random number generators in mxnet.</summary>
/// <param name="seed">the random number seed.</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRandomSeed(int seed)

/// <summary>Seed the global random number generator of the given device.</summary>
/// <param name="seed">the random number seed.</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRandomSeedContext(int seed, int dev_type, int dev_id)

/// <summary>Notify the engine about a shutdown,
/// This can help engine to print less messages into display.
///
/// User do not have to call this function.</summary>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNotifyShutdown()

/// <summary>Set up configuration of profiler for the process passed as profile_process in keys</summary>
/// <param name="num_params">Number of parameters</param>
/// <param name="keys">array of parameter keys</param>
/// <param name="vals">array of parameter values</param>
/// <param name="kvstoreHandle">handle to kvstore</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSetProcessProfilerConfig__(int num_params, string keys, string vals, KVStoreHandle kvstoreHandle)

/// <summary>Set up configuration of profiler for worker/current process</summary>
/// <param name="num_params">Number of parameters</param>false
/// <param name="keys">array of parameter keys</param>
/// <param name="vals">array of parameter values</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSetProfilerConfig__(int num_params, string keys, string vals)

/// <summary>Set up state of profiler for either worker or server process</summary>
/// <param name="state">indicate the working state of profiler,
/// profiler not running when state == 0,
/// profiler running when state == 1</param>
/// <param name="profile_process">an int,
///when 0 command is for worker/current process,
///when 1 command is for server process</param>
/// <param name="kvstoreHandle">handle to kvstore, needed for server process profiling</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSetProcessProfilerState__(int state, int profile_process, KVStoreHandle kvStoreHandle)

/// <summary>Set up state of profiler for current process</summary>
/// <param name="state">indicate the working state of profiler,
/// profiler not running when state == 0,
/// profiler running when state == 1</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSetProfilerState__(int state)

/// <summary>Save profile and stop profiler</summary>
/// <param name="finished">true if stat output should stop after this point</param>
/// <param name="profile_process">an int,
///when 0 command is for worker/current process,
///when 1 command is for server process</param>
/// <param name="kvstoreHandle">handle to kvstore</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXDumpProcessProfile__(int finished, int profile_process, KVStoreHandle kvStoreHandle)

/// <summary>Save profile and stop profiler for worker/current process</summary>
/// <param name="finished">true if stat output should stop after this point</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXDumpProfile__(int finished)

/// <summary>Deprecated, use MXAggregateProfileStatsPrintEx instead.</summary>
/// <param name="out_str">Will receive a pointer to the output string</param>
/// <param name="reset">Clear the aggregate stats after printing</param>
/// <returns>0 when success, -1 when failure happens.</returns>
/// <remarks></remarks>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXAggregateProfileStatsPrint__(string[] out_str, int reset)

/// <summary>Print sorted aggregate stats to the a string
///       How aggregate stats are stored will not change</summary>
/// <param name="out_str">will receive a pointer to the output string</param>
/// <param name="reset">clear the aggregate stats after printing</param>
/// <param name="format">whether to return in tabular or json format</param>
/// <param name="sort_by">sort by total, avg, min, max, or count</param>
/// <param name="ascending">whether to sort ascendingly</param>
/// <returns>0 when success, -1 when failure happens.</returns>
/// <remarks></remarks>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXAggregateProfileStatsPrintEx__(string[] out_str, int reset, int format, int sort_by, int ascending)

/// <summary>Pause profiler tuning collection</summary>
/// <param name="paused">If nonzero, profiling pauses. Otherwise, profiling resumes/continues</param>
/// <param name="profile_process">integer which denotes whether to process worker or server process</param>
/// <param name="kvstoreHandle">handle to kvstore</param>
/// <returns>0 when success, -1 when failure happens.</returns>
/// <remarks>pausing and resuming is global and not recursive</remarks>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXProcessProfilePause__(int paused, int profile_process, KVStoreHandle kvStoreHandle)

/// <summary>Pause profiler tuning collection for worker/current process</summary>
/// <param name="paused">If nonzero, profiling pauses. Otherwise, profiling resumes/continues</param>
/// <returns>0 when success, -1 when failure happens.</returns>
/// <remarks>pausing and resuming is global and not recursive</remarks>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXProfilePause__(int paused)

/// <summary>Create profiling domain</summary>
/// <param name="domain">String representing the domain name to create</param>
/// <param name="out">Return domain object</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXProfileCreateDomain__(string domain, ProfileHandle[] out)

/// <summary>Create profile task</summary>
/// <param name="name">Name of the task</param>
/// <param name="domain">Domain of the task</param>
/// <param name="out">Output handle</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXProfileCreateTask__(ProfileHandle domain, string task_name, ProfileHandle[] out)

/// <summary>Create profile frame</summary>
/// <param name="name">Name of the frame</param>
/// <param name="domain">Domain of the frame</param>
/// <param name="out">Output handle</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXProfileCreateFrame__(ProfileHandle domain, string frame_name, ProfileHandle[] out)

/// <summary>Create profile event</summary>
/// <param name="name">Name of the event</param>
/// <param name="out">Output handle</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXProfileCreateEvent__(string event_name, ProfileHandle[] out)

/// <summary>Create profile counter</summary>
/// <param name="name">Name of the counter</param>
/// <param name="domain">Domain of the counter</param>
/// <param name="out">Output handle</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXProfileCreateCounter__(ProfileHandle domain, string counter_name, ProfileHandle[] out)

/// <summary>Destroy a frame</summary>
/// <param name="frame_handle">Handle to frame to destroy</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXProfileDestroyHandle__(ProfileHandle frame_handle)

/// <summary>Start timing the duration of a profile duration object such as an event, task or frame</summary>
/// <param name="duration_handle">handle to the duration object</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXProfileDurationStart__(ProfileHandle duration_handle)

/// <summary>Stop timing the duration of a profile duration object such as an event, task or frame</summary>
/// <param name="duration_handle">handle to the duration object</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXProfileDurationStop__(ProfileHandle duration_handle)

/// <summary>Set a counter, given its handle</summary>
/// <param name="counter_handle">Handle to counter to set</param>
/// <param name="value">Value to set the counter to (64-bit unsigned integer)</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXProfileSetCounter__(ProfileHandle counter_handle, uint64 value)

/// <summary>Adjust a counter by the given amount, given its handle</summary>
/// <param name="counter_handle">Handle to counter to adjust</param>
/// <param name="value">Value to adjust the counter by (64-bit signed integer)</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXProfileAdjustCounter__(ProfileHandle counter_handle, int64 value)

/// <summary>Mark a single instant in time</summary>
/// <param name="domain">Domain of the marker</param>
/// <param name="instant_marker_name">Name of the marker</param>
/// <param name="scope">Scope of marker ('global', 'process', 'thread', 'task', 'marker')</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXProfileSetMarker__(ProfileHandle domain, string instant_marker_name, string scope)

/// <summary>Set the number of OMP threads to use</summary>
/// <param name="thread_num">Number of OMP threads desired</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSetNumOMPThreads__(int thread_num)

/// <summary>set bulk execution limit</summary>
/// <param name="bulk_size">new bulk_size</param>
/// <param name="prev_bulk_size">previous bulk_size</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXEngineSetBulkSize__(int bulk_size, int[] prev_bulk_size)

/// <summary>Get the number of GPUs.</summary>
/// <param name="pointer">to int that will hold the number of GPUs available.</param>
/// <returns>0 when success, -1 when failure happens.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXGetGPUCount([<Out>] int& out)

/// <summary>get the free and total available memory on a GPU
/// Note: Deprecated, use MXGetGPUMemoryInformation64 instead.</summary>
/// <param name="dev">the GPU number to query</param>
/// <param name="free_mem">pointer to the integer holding free GPU memory</param>
/// <param name="total_mem">pointer to the integer holding total GPU memory</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXGetGPUMemoryInformation__(int dev, int[] free_mem, int[] total_mem)

/// <summary>get the free and total available memory on a GPU</summary>
/// <param name="dev">the GPU number to query</param>
/// <param name="free_mem">pointer to the uint64_t holding free GPU memory</param>
/// <param name="total_mem">pointer to the uint64_t holding total GPU memory</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXGetGPUMemoryInformation64__(int dev, uint64[] free_mem, uint64[] total_mem)

/// <summary>get the MXNet library version as an integer</summary>
/// <param name="pointer">to the integer holding the version number</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXGetVersion([<Out>] int& out)

/// <summary>Load TVM operator from the binary library</summary>
/// <param name="libpath">TVM operators lib file</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXLoadTVMOp__(string libpath)

//-------------------------------------
// Part 1: NDArray creation and deletion
//-------------------------------------
/// <summary>create a NDArray handle that is not initialized
/// can be used to pass in as mutate variables
/// to hold the result of NDArray</summary>
/// <param name="out">the returning handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayCreateNone([<Out>] NDArrayHandle& out)

/// <summary>create a NDArray with specified shape</summary>
/// <param name="shape">the pointer to the shape</param>
/// <param name="ndim">the dimension of the shape</param>
/// <param name="dev_type">device type, specify device we want to take</param>
/// <param name="dev_id">the device id of the specific device</param>
/// <param name="delay_alloc">whether to delay allocation until
///   the narray is first mutated</param>
/// <param name="out">the returning handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayCreate(uint32[] shape, uint32 ndim, int dev_type, int dev_id, int delay_alloc, [<Out>] NDArrayHandle& out)

/// <summary>create a NDArray with specified shape and data type</summary>
/// <param name="shape">the pointer to the shape</param>
/// <param name="ndim">the dimension of the shape</param>
/// <param name="dev_type">device type, specify device we want to take</param>
/// <param name="dev_id">the device id of the specific device</param>
/// <param name="delay_alloc">whether to delay allocation until
///   the narray is first mutated</param>
/// <param name="dtype">data type of created array</param>
/// <param name="out">the returning handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayCreateEx(uint32[] shape, uint32 ndim, int dev_type, int dev_id, int delay_alloc, int dtype, [<Out>] NDArrayHandle& out)

/// <summary>create a NDArray with specified shape and data type</summary>
/// <param name="shape">the pointer to the shape</param>
/// <param name="ndim">the dimension of the shape</param>
/// <param name="dev_type">device type, specify device we want to take</param>
/// <param name="dev_id">the device id of the specific device</param>
/// <param name="delay_alloc">whether to delay allocation until
///   the narray is first mutated</param>
/// <param name="dtype">data type of created array</param>
/// <param name="out">the returning handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayCreateEx64(int64[] shape, int ndim, int dev_type, int dev_id, int delay_alloc, int dtype, [<Out>] NDArrayHandle& out)

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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayCreateSparseEx(int storage_type, uint32[] shape, uint32 ndim, int dev_type, int dev_id, int delay_alloc, int dtype, uint32 num_aux, int[] aux_type, uint32[] aux_ndims, uint32[] aux_shape, [<Out>] NDArrayHandle& out)

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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayCreateSparseEx64(int storage_type, int64[] shape, int ndim, int dev_type, int dev_id, int delay_alloc, int dtype, uint32 num_aux, int[] aux_type, int[] aux_ndims, int64[] aux_shape, [<Out>] NDArrayHandle& out)

/// <summary>create a NDArray handle that is loaded from raw bytes.</summary>
/// <param name="buf">the head of the raw bytes</param>
/// <param name="size">size of the raw bytes</param>
/// <param name="out">the returning handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayLoadFromRawBytes(byte[] buf, size_t size, [<Out>] NDArrayHandle& out)

/// <summary>save the NDArray into raw bytes.</summary>
/// <param name="handle">the NDArray handle</param>
/// <param name="out_size">size of the raw bytes</param>
/// <param name="out_buf">the head of returning memory bytes.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArraySaveRawBytes(NDArrayHandle handle, [<Out>] size_t& out_size, [<Out>] IntPtr& out_buf)

/// <summary>Save list of narray into the file.</summary>
/// <param name="fname">name of the file.</param>
/// <param name="num_args">number of arguments to save.</param>
/// <param name="args">the array of NDArrayHandles to be saved.</param>
/// <param name="keys">the name of the NDArray, optional, can be NULL</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArraySave(string fname, uint32 num_args, NDArrayHandle[] args, string[] keys)

/// <summary>Load list of narray from the file.</summary>
/// <param name="fname">name of the file.</param>
/// <param name="out_size">number of narray loaded.</param>
/// <param name="out_arr">head of the returning narray handles.</param>
/// <param name="out_name_size">size of output name arrray.</param>
/// <param name="out_names">the names of returning NDArrays, can be NULL</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayLoad(string fname, [<Out>] uint32& out_size, [<Out>] IntPtr& out_arr, [<Out>] uint32& out_name_size, [<Out>] IntPtr& out_names)

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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayLoadFromBuffer(IntPtr ndarray_buffer, size_t size, [<Out>] uint32& out_size, [<Out>] NDArrayHandle& out_arr, [<Out>] uint32& out_name_size, [<Out>] IntPtr& out_names)

/// <summary>Perform a synchronize copy from a continugous CPU memory region.
/// This function will call WaitToWrite before the copy is performed.
/// This is useful to copy data from existing memory region that are
/// not wrapped by NDArray(thus dependency not being tracked).</summary>
/// <param name="handle">the NDArray handle</param>
/// <param name="data">the data source to copy from.</param>
/// <param name="size">the memory size we want to copy from.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArraySyncCopyFromCPU(NDArrayHandle handle, IntPtr data, size_t size)

/// <summary>Perform a synchronize copyto a continugous CPU memory region.
///
/// This function will call WaitToRead before the copy is performed.
/// This is useful to copy data from existing memory region that are
/// not wrapped by NDArray(thus dependency not being tracked).</summary>
/// <param name="handle">the NDArray handle</param>
/// <param name="data">the data source to copy into.</param>
/// <param name="size">the memory size we want to copy into.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArraySyncCopyToCPU(NDArrayHandle handle, IntPtr data, size_t size)

/// <summary>Copy src.data() to dst.data() if i = -1, else dst.aux_data(i) if i >= 0
///This function blocks. Do not use it in performance critical code.</summary>
/// <param name="handle_dst">handle of a dst ndarray whose data/aux_data has been allocated</param>
/// <param name="handle_src">handle of a src ndarray which has default storage type</param>
/// <param name="i">dst data blob indicator</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArraySyncCopyFromNDArray(NDArrayHandle handle_dst, NDArrayHandle handle_src, int i)

/// <summary>check whether the NDArray format is valid</summary>
/// <param name="full_check">if `True`, rigorous check, O(N) operations
///   Otherwise basic check, O(1) operations</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArraySyncCheckFormat(NDArrayHandle handle, bool full_check)

/// <summary>Wait until all the pending writes with respect NDArray are finished.
/// Always call this before read data out synchronizely.</summary>
/// <param name="handle">the NDArray handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayWaitToRead(NDArrayHandle handle)

/// <summary>Wait until all the pending read/write with respect NDArray are finished.
/// Always call this before write data into NDArray synchronizely.</summary>
/// <param name="handle">the NDArray handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayWaitToWrite(NDArrayHandle handle)

/// <summary>wait until all delayed operations in
///  the system is completed</summary>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayWaitAll()

/// <summary>free the narray handle</summary>
/// <param name="handle">the handle to be freed</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayFree(NDArrayHandle handle)

/// <summary>Slice the NDArray along axis 0.</summary>
/// <param name="handle">the handle to the NDArray</param>
/// <param name="slice_begin">The beginning index of slice</param>
/// <param name="slice_end">The ending index of slice</param>
/// <param name="out">The NDArrayHandle of sliced NDArray</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArraySlice(NDArrayHandle handle, uint32 slice_begin, uint32 slice_end, [<Out>] NDArrayHandle& out)

[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArraySlice64(NDArrayHandle handle, int64 slice_begin, int64 slice_end, [<Out>] NDArrayHandle& out)

/// <summary>Index the NDArray along axis 0.</summary>
/// <param name="handle">the handle to the NDArray</param>
/// <param name="idx">the index</param>
/// <param name="out">The NDArrayHandle of output NDArray</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayAt(NDArrayHandle handle, uint32 idx, [<Out>] NDArrayHandle& out)

[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayAt64(NDArrayHandle handle, int64 idx, [<Out>] NDArrayHandle& out)

/// <summary>get the storage type of the array</summary>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetStorageType(NDArrayHandle handle, [<Out>] int& out_storage_type)

/// <summary>Reshape the NDArray.</summary>
/// <param name="handle">the handle to the narray</param>
/// <param name="ndim">number of dimensions of new shape</param>
/// <param name="dims">new shape</param>
/// <param name="out">the NDArrayHandle of reshaped NDArray</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayReshape(NDArrayHandle handle, int ndim, int[] dims, [<Out>] NDArrayHandle& out)

/// <summary>Reshape the NDArray.</summary>
/// <param name="handle">the handle to the narray</param>
/// <param name="ndim">number of dimensions of new shape</param>
/// <param name="dims">new shape</param>
/// <param name="out">the NDArrayHandle of reshaped NDArray</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayReshape64(NDArrayHandle handle, int ndim, dim_t[] dims, bool reverse, [<Out>] NDArrayHandle& out)


(* DEPRECATED and excluded
/// <summary>DEPRECATED. Use MXNDArrayGetShapeEx instead.
///get the shape of the array</summary>
/// <param name="handle">the handle to the narray</param>
/// <param name="out_dim">the output dimension</param>
/// <param name="out_pdata">pointer holder to get data pointer of the shape</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetShape__(NDArrayHandle handle, uint32[] out_dim, uint32[]& out_pdata)

[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetShape64__(NDArrayHandle handle, int[] out_dim, int64[]& out_pdata)
*)

/// <summary>get the shape of the array</summary>
/// <param name="handle">the handle to the narray</param>
/// <param name="out_dim">the output dimension</param>
/// <param name="out_pdata">pointer holder to get data pointer of the shape</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetShapeEx(NDArrayHandle handle, [<Out>] int& out_dim, [<Out>] IntPtr& out_pdata)

[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetShapeEx64(NDArrayHandle handle, [<Out>] int& out_dim, [<Out>] IntPtr& out_pdata)

/// <summary>get the content of the data in NDArray</summary>
/// <param name="handle">the handle to the ndarray</param>
/// <param name="out_pdata">pointer holder to get pointer of data</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetData(NDArrayHandle handle, [<Out>] IntPtr& out_pdata)

/// <summary>Create a reference view of NDArray that
/// represents as DLManagedTensor
/// Notice: MXNet uses asynchronous execution. Please call MXNDArrayWaitToRead or
///         MXNDArrayWaitToWrite before calling MXNDArrayToDLPack.</summary>
/// <param name="handle">the handle to the ndarray</param>
/// <param name="out_dlpack">pointer holder to get pointer of DLManagedTensor</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayToDLPack(NDArrayHandle handle, [<Out>] DLManagedTensorHandle& out_dlpack)



(* DEPRECATED and excluded
/// <summary>DEPRECATED. Use MXNDArrayFromDLPackEx instead.
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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayFromDLPack__(DLManagedTensorHandle dlpack, [<Out>] NDArrayHandle& out_handle)
*)


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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayFromDLPackEx(DLManagedTensorHandle dlpack, bool transient_handle, [<Out>] NDArrayHandle& out_handle)

/// <summary>Delete a dlpack tensor</summary>
/// <param name="dlpack">the pointer of the input DLManagedTensor</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayCallDLPackDeleter(DLManagedTensorHandle dlpack)

/// <summary>get the type of the data in NDArray</summary>
/// <param name="handle">the handle to the narray</param>
/// <param name="out_dtype">pointer holder to get type of data</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetDType(NDArrayHandle handle, [<Out>] int& out_dtype)

/// <summary>get the type of the ith aux data in NDArray</summary>
/// <param name="handle">the handle to the narray</param>
/// <param name="i">the index of the aux data</param>
/// <param name="out_type">pointer holder to get type of aux data</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetAuxType(NDArrayHandle handle, uint32 i, [<Out>] int& out_type)

/// <summary>get the type of the ith aux data in NDArray</summary>
/// <param name="handle">the handle to the narray</param>
/// <param name="i">the index of the aux data</param>
/// <param name="out_type">pointer holder to get type of aux data</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetAuxType64(NDArrayHandle handle, int64 i, [<Out>] int& out_type)

/// <summary>Get a deep copy of the ith aux data blob
///in the form of an NDArray of default storage type.
///This function blocks. Do not use it in performance critical code.</summary>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetAuxNDArray(NDArrayHandle handle, uint32 i, [<Out>] NDArrayHandle& out)

/// <summary>Get a deep copy of the ith aux data blob
///in the form of an NDArray of default storage type.
///This function blocks. Do not use it in performance critical code.</summary>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetAuxNDArray64(NDArrayHandle handle, int64 i, [<Out>] NDArrayHandle& out)

/// <summary>Get a deep copy of the data blob
///in the form of an NDArray of default storage type.
///This function blocks. Do not use it in performance critical code.</summary>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetDataNDArray(NDArrayHandle handle, [<Out>] NDArrayHandle& out)

/// <summary>get the context of the NDArray</summary>
/// <param name="handle">the handle to the narray</param>
/// <param name="out_dev_type">the output device type</param>
/// <param name="out_dev_id">the output device id</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetContext(NDArrayHandle handle, [<Out>] int& out_dev_type, [<Out>] int& out_dev_id)

/// <summary>return gradient buffer attached to this NDArray</summary>
/// <param name="handle">NDArray handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetGrad(NDArrayHandle handle, [<Out>] NDArrayHandle& out)

/// <summary>detach and ndarray from computation graph by clearing entry_</summary>
/// <param name="handle">NDArray handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayDetach(NDArrayHandle handle, [<Out>] NDArrayHandle& out)

/// <summary>set the flag for gradient array state.</summary>
/// <param name="handle">NDArray handle</param>
/// <param name="state">the new state.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArraySetGradState(NDArrayHandle handle, int state)

/// <summary>set the flag for gradient array state.</summary>
/// <param name="handle">NDArray handle</param>
/// <param name="state">the new state.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetGradState(NDArrayHandle handle, [<Out>] int& out)

//--------------------------------
// Part 2: functions on NDArray
//--------------------------------
/// <summary>list all the available functions handles
///  most user can use it to list all the needed functions</summary>
/// <param name="out_size">the size of returned array</param>
/// <param name="out_array">the output function array</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXListFunctions([<Out>] uint32& out_size, [<Out>] IntPtr& out_array)

/// <summary>get the function handle by name</summary>
/// <param name="name">the name of the function</param>
/// <param name="out">the corresponding function handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXGetFunction__(string name, FunctionHandle[] out)

/// <summary>Get the information of the function handle.</summary>
/// <param name="fun">The function handle.</param>
/// <param name="name">The returned name of the function.</param>
/// <param name="description">The returned description of the function.</param>
/// <param name="num_args">Number of arguments.</param>
/// <param name="arg_names">Name of the arguments.</param>
/// <param name="arg_type_infos">Type information about the arguments.</param>
/// <param name="arg_descriptions">Description information about the arguments.</param>
/// <param name="return_type">Return type of the function.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXFuncGetInfo(
    FunctionHandle ``fun``, 
    [<Out>] IntPtr& name, 
    [<Out>] IntPtr& description, 
    [<Out>] uint32& num_args,
    [<Out>] IntPtr& arg_names, 
    [<Out>] IntPtr& arg_type_infos, 
    [<Out>] IntPtr& arg_descriptions, 
    [<Out>] IntPtr& return_type (*=NULL*))

/// <summary>get the argument requirements of the function</summary>
/// <param name="fun">input function handle</param>
/// <param name="num_use_vars">how many NDArrays to be passed in as used_vars</param>
/// <param name="num_scalars">scalar variable is needed</param>
/// <param name="num_mutate_vars">how many NDArrays to be passed in as mutate_vars</param>
/// <param name="type_mask">the type mask of this function</param>
/// <returns>0 when success, -1 when failure happens</returns>
/// <seealso cref="MXFuncInvoke"/>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXFuncDescribe__(FunctionHandle ``fun``, uint32[] num_use_vars, uint32[] num_scalars, uint32[] num_mutate_vars, int[] type_mask)

/// <summary>invoke a function, the array size of passed in arguments
///  must match the values in the</summary>
/// <param name="fun">the function</param>
/// <param name="use_vars">the normal arguments passed to function</param>
/// <param name="scalar_args">the scalar qarguments</param>
/// <param name="mutate_vars">the mutate arguments</param>
/// <returns>0 when success, -1 when failure happens</returns>
/// <seealso cref="MXFuncDescribeArgs"/>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXFuncInvoke__(FunctionHandle ``fun``, NDArrayHandle[] use_vars, float* scalar_args, NDArrayHandle[] mutate_vars)

/// <summary>invoke a function, the array size of passed in arguments
///  must match the values in the</summary>
/// <param name="fun">the function</param>
/// <param name="use_vars">the normal arguments passed to function</param>
/// <param name="scalar_args">the scalar qarguments</param>
/// <param name="mutate_vars">the mutate arguments</param>
/// <param name="num_params">number of keyword parameters</param>
/// <param name="param_keys">keys for keyword parameters</param>
/// <param name="param_vals">values for keyword parameters</param>
/// <returns>0 when success, -1 when failure happens</returns>
/// <seealso cref="MXFuncDescribeArgs"/>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXFuncInvokeEx__(FunctionHandle ``fun``, NDArrayHandle[] use_vars, float* scalar_args, NDArrayHandle[] mutate_vars, int num_params, string[] param_keys, string[] param_vals)

/// <summary>invoke a nnvm op and imperative function</summary>
/// <param name="creator">the op</param>
/// <param name="num_inputs">number of input NDArrays</param>
/// <param name="inputs">input NDArrays</param>
/// <param name="num_outputs">number of output NDArrays</param>
/// <param name="outputs">output NDArrays</param>
/// <param name="num_params">number of keyword parameters</param>
/// <param name="param_keys">keys for keyword parameters</param>
/// <param name="param_vals">values for keyword parameters</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXImperativeInvoke(AtomicSymbolCreatorHandle creator, int num_inputs, NDArrayHandle[] inputs, int& num_outputs, IntPtr& outputs, int num_params, string[] param_keys, string[] param_vals)

/// <summary>invoke a nnvm op and imperative function</summary>
/// <param name="creator">the op</param>
/// <param name="num_inputs">number of input NDArrays</param>
/// <param name="inputs">input NDArrays</param>
/// <param name="num_outputs">number of output NDArrays</param>
/// <param name="outputs">output NDArrays</param>
/// <param name="num_params">number of keyword parameters</param>
/// <param name="param_keys">keys for keyword parameters</param>
/// <param name="param_vals">values for keyword parameters</param>
/// <param name="out_stypes">output ndarrays' stypes</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXImperativeInvokeEx__(AtomicSymbolCreatorHandle creator, int num_inputs, NDArrayHandle[] inputs, int[] num_outputs, NDArrayHandle[]& outputs, int num_params, string[] param_keys, string[] param_vals, int[]& out_stypes)

/// <summary>set whether to record operator for autograd</summary>
/// <param name="is_recording">1 when recording, 0 when not recording.</param>
/// <param name="prev">returns the previous status before this set.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXAutogradSetIsRecording(int is_recording, [<Out>] int& prev)

/// <summary>set whether to record operator for autograd</summary>
/// <param name="is_training">1 when training, 0 when testing</param>
/// <param name="prev">returns the previous status before this set.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXAutogradSetIsTraining(int is_training, [<Out>] int& prev)

/// <summary>get whether autograd recording is on</summary>
/// <param name="curr">returns the current status.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXAutogradIsRecording([<Out>] bool& curr)

/// <summary>get whether training mode is on</summary>
/// <param name="curr">returns the current status.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXAutogradIsTraining([<Out>] bool& curr)

/// <summary>get whether numpy compatibility is on</summary>
/// <param name="curr">returns the current status</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXIsNumpyShape([<Out>] bool& curr)

/// <summary>set numpy compatibility switch</summary>
/// <param name="is_np_shape">1 when numpy shape semantics is on, 0 when off</param>
/// <param name="prev">returns the previous status before this set</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSetIsNumpyShape(int is_np_shape, [<Out>] int& prev)

/// <summary>mark NDArrays as variables to compute gradient for autograd</summary>
/// <param name="num_var">number of variable NDArrays</param>
/// <param name="var_handles">variable NDArrays</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXAutogradMarkVariables(uint32 num_var, NDArrayHandle[] var_handles, uint32[] reqs_array, NDArrayHandle[] grad_handles)

/// <summary>compute the gradient of outputs w.r.t variabels</summary>
/// <param name="num_output">number of output NDArray</param>
/// <param name="output_handles">output NDArrays</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXAutogradComputeGradient(uint32 num_output, NDArrayHandle[] output_handles)

/// <summary>compute the gradient of outputs w.r.t variabels</summary>
/// <param name="num_output">number of output NDArray</param>
/// <param name="output_handles">output NDArrays</param>
/// <param name="ograd_handles">head gradient for NDArrays</param>
/// <param name="retain_graph">whether to keep the graph after backward</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXAutogradBackward(uint32 num_output, NDArrayHandle[] output_handles, NDArrayHandle[] ograd_handles, int retain_graph)

/// <summary>compute the gradient of outputs w.r.t variabels</summary>
/// <param name="num_output">number of output NDArray</param>
/// <param name="output_handles">output NDArrays</param>
/// <param name="ograd_handles">head gradient for NDArrays</param>
/// <param name="num_variables">number of variables</param>
/// <param name="retain_graph">whether to keep the graph after backward</param>
/// <param name="is_train">whether to do backward for training or inference</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXAutogradBackwardEx(uint32 num_output, NDArrayHandle[] output_handles, NDArrayHandle[] ograd_handles, uint32 num_variables, NDArrayHandle[] var_handles, int retain_graph, int create_graph, int is_train, [<Out>] IntPtr& grad_handles, [<Out>] IntPtr& grad_stypes)

/// <summary>get the graph constructed by autograd.</summary>
/// <param name="handle">ndarray handle</param>
/// <param name="out">output symbol handle</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXAutogradGetSymbol(NDArrayHandle handle, [<Out>] SymbolHandle& out)

/// <summary>create cached operator</summary>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXCreateCachedOp(SymbolHandle handle, [<Out>] CachedOpHandle& out)

/// <summary>create cached operator</summary>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXCreateCachedOpEx(SymbolHandle handle, int num_flags, string[] keys, string[] vals, [<Out>]CachedOpHandle& out)

/// <summary>free cached operator</summary>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXFreeCachedOp(CachedOpHandle handle)

/// <summary>invoke cached operator</summary>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXInvokeCachedOp(CachedOpHandle handle, int num_inputs, NDArrayHandle[] inputs, [<Out>] int& num_outputs, [<Out>] IntPtr& outputs)

/// <summary>invoke a cached op</summary>
/// <param name="handle">the handle to the cached op</param>
/// <param name="num_inputs">number of input NDArrays</param>
/// <param name="inputs">input NDArrays</param>
/// <param name="num_outputs">number of output NDArrays</param>
/// <param name="outputs">output NDArrays</param>
/// <param name="out_stypes">output ndarrays' stypes</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXInvokeCachedOpEx(CachedOpHandle handle, int num_inputs, NDArrayHandle[] inputs, [<Out>] int& num_outputs, [<Out>] IntPtr& outputs, [<Out>] IntPtr& out_stypes)

/// <summary>cached op set monitor callback</summary>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXCachedOpRegisterOpHook(NDArrayHandle handle, CachedOpMonitorCallback callback, bool monitor_all)

//--------------------------------------------
// Part 3: symbolic configuration generation
//--------------------------------------------
/// <summary>list all the available operator names, include entries.</summary>
/// <param name="out_size">the size of returned array</param>
/// <param name="out_array">the output operator name array.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXListAllOpNames(uint32& out_size, IntPtr& out_array)

/// <summary>list all the available AtomicSymbolEntry</summary>
/// <param name="out_size">the size of returned array</param>
/// <param name="out_array">the output AtomicSymbolCreator array</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolListAtomicSymbolCreators([<Out>] uint32& out_size, [<Out>] IntPtr& out_array)

/// <summary>Get the name of an atomic symbol.</summary>
/// <param name="creator">the AtomicSymbolCreator.</param>
/// <param name="name">The returned name of the creator.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolGetAtomicSymbolName(AtomicSymbolCreatorHandle creator, [<Out>] IntPtr& name)

/// <summary>Get the input symbols of the graph.</summary>
/// <param name="sym">The graph.</param>
/// <param name="inputs">The input symbols of the graph.</param>
/// <param name="input_size">the number of input symbols returned.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolGetInputSymbols(SymbolHandle sym, [<Out>] IntPtr& inputs, [<Out>] int& input_size)

/// <summary>Cut a subgraph whose nodes are marked with a subgraph attribute.
///The input graph will be modified. A variable node will be created for each
///edge that connects to nodes outside the subgraph. The outside nodes that
///connect to the subgraph will be returned.</summary>
/// <param name="sym">The graph.</param>
/// <param name="inputs">The nodes that connect to the subgraph.</param>
/// <param name="input_size">The number of such nodes.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolCutSubgraph(SymbolHandle sym, [<Out>] IntPtr& inputs, [<Out>] int& input_size)

/// <summary>Get the detailed information about atomic symbol.</summary>
/// <param name="creator">the AtomicSymbolCreator.</param>
/// <param name="name">The returned name of the creator.</param>
/// <param name="description">The returned description of the symbol.</param>
/// <param name="num_args">Number of arguments.</param>
/// <param name="arg_names">Name of the arguments.</param>
/// <param name="arg_type_infos">Type informations about the arguments.</param>
/// <param name="arg_descriptions">Description information about the arguments.</param>
/// <param name="key_var_num_args">The keyword argument for specifying variable number of arguments.
///           When this parameter has non-zero length, the function allows variable number
///           of positional arguments, and will need the caller to pass it in in
///           MXSymbolCreateAtomicSymbol,
///           With key = key_var_num_args, and value = number of positional arguments.</param>
/// <param name="return_type">Return type of the function, can be Symbol or Symbol[]</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolGetAtomicSymbolInfo(
    AtomicSymbolCreatorHandle creator, 
    [<Out>] IntPtr& name, 
    [<Out>] IntPtr& description, 
    [<Out>] uint32& num_args,
    [<Out>] IntPtr& arg_names, 
    [<Out>] IntPtr& arg_type_infos, 
    [<Out>] IntPtr& arg_descriptions, 
    [<Out>] IntPtr& key_var_num_args, 
    [<Out>] IntPtr& return_type (*=NULL*))

/// <summary>Create an AtomicSymbol.</summary>
/// <param name="creator">the AtomicSymbolCreator</param>
/// <param name="num_param">the number of parameters</param>
/// <param name="keys">the keys to the params</param>
/// <param name="vals">the vals of the params</param>
/// <param name="out">pointer to the created symbol handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolCreateAtomicSymbol(AtomicSymbolCreatorHandle creator, uint32 num_param, string[] keys, string[] vals, [<Out>] SymbolHandle& out)

/// <summary>Create a Variable Symbol.</summary>
/// <param name="name">name of the variable</param>
/// <param name="out">pointer to the created symbol handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolCreateVariable(string name, [<Out>] SymbolHandle& out)

/// <summary>Create a Symbol by grouping list of symbols together</summary>
/// <param name="num_symbols">number of symbols to be grouped</param>
/// <param name="symbols">array of symbol handles</param>
/// <param name="out">pointer to the created symbol handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolCreateGroup(uint32 num_symbols, SymbolHandle[] symbols, [<Out>] SymbolHandle& out)

/// <summary>Load a symbol from a json file.</summary>
/// <param name="fname">the file name.</param>
/// <param name="out">the output symbol.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolCreateFromFile(string fname, [<Out>] SymbolHandle& out)

/// <summary>Load a symbol from a json string.</summary>
/// <param name="json">the json string.</param>
/// <param name="out">the output symbol.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolCreateFromJSON(string json, [<Out>] SymbolHandle& out)

/// <summary>Remove the operators amp_cast and amp_multicast</summary>
/// <param name="sym_handle">the input symbol.</param>
/// <param name="ret_sym_handle">the output symbol.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolRemoveAmpCast(SymbolHandle sym_handle, [<Out>] SymbolHandle& ret_sym_handle)

/// <summary>Save a symbol into a json file.</summary>
/// <param name="symbol">the input symbol.</param>
/// <param name="fname">the file name.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolSaveToFile(SymbolHandle symbol, string fname)

/// <summary>Save a symbol into a json string</summary>
/// <param name="symbol">the input symbol.</param>
/// <param name="out_json">output json string.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolSaveToJSON(SymbolHandle symbol, [<Out>] IntPtr& out_json)

/// <summary>Free the symbol handle.</summary>
/// <param name="symbol">the symbol</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolFree(SymbolHandle symbol)

/// <summary>Copy the symbol to another handle</summary>
/// <param name="symbol">the source symbol</param>
/// <param name="out">used to hold the result of copy</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolCopy(SymbolHandle symbol, [<Out>] SymbolHandle& out)

/// <summary>Print the content of symbol, used for debug.</summary>
/// <param name="symbol">the symbol</param>
/// <param name="out_str">pointer to hold the output string of the printing.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolPrint(SymbolHandle symbol, [<Out>] IntPtr& out_str)

/// <summary>Get string name from symbol</summary>
/// <param name="symbol">the source symbol</param>
/// <param name="out">The result name.</param>
/// <param name="success">Whether the result is contained in out.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolGetName(SymbolHandle symbol, [<Out>] IntPtr& out, [<Out>] int& success)

/// <summary>Get string attribute from symbol</summary>
/// <param name="symbol">the source symbol</param>
/// <param name="key">The key of the symbol.</param>
/// <param name="out">The result attribute, can be NULL if the attribute do not exist.</param>
/// <param name="success">Whether the result is contained in out.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolGetAttr(SymbolHandle symbol, string key, [<Out>] IntPtr& out, [<Out>] int& success)

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
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolSetAttr(SymbolHandle symbol, string key, string value)

/// <summary>Get all attributes from symbol, including all descendents.</summary>
/// <param name="symbol">the source symbol</param>
/// <param name="out_size">The number of output attributes</param>
/// <param name="out">2*out_size strings representing key value pairs.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolListAttr(SymbolHandle symbol, [<Out>] uint32& out_size, [<Out>] IntPtr& out)

/// <summary>Get all attributes from symbol, excluding descendents.</summary>
/// <param name="symbol">the source symbol</param>
/// <param name="out_size">The number of output attributes</param>
/// <param name="out">2*out_size strings representing key value pairs.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolListAttrShallow(SymbolHandle symbol, [<Out>] uint32& out_size, [<Out>] IntPtr& out)

/// <summary>List arguments in the symbol.</summary>
/// <param name="symbol">the symbol</param>
/// <param name="out_size">output size</param>
/// <param name="out_str_array">pointer to hold the output string array</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolListArguments(SymbolHandle symbol, [<Out>] uint32& out_size, [<Out>] IntPtr& out_str_array)

/// <summary>List returns in the symbol.</summary>
/// <param name="symbol">the symbol</param>
/// <param name="out_size">output size</param>
/// <param name="out_str_array">pointer to hold the output string array</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolListOutputs(SymbolHandle symbol, [<Out>] uint32& out_size, [<Out>] IntPtr& out_str_array)

/// <summary>Get number of outputs of the symbol.</summary>
/// <param name="symbol">The symbol</param>
/// <param name="out_size">number of outputs</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolGetNumOutputs(SymbolHandle symbol, [<Out>] uint32& output_count)

/// <summary>Get a symbol that contains all the internals.</summary>
/// <param name="symbol">The symbol</param>
/// <param name="out">The output symbol whose outputs are all the internals.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolGetInternals(SymbolHandle symbol, [<Out>] SymbolHandle& out)

/// <summary>Get a symbol that contains only direct children.</summary>
/// <param name="symbol">The symbol</param>
/// <param name="out">The output symbol whose outputs are the direct children.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolGetChildren(SymbolHandle symbol, [<Out>] SymbolHandle& out)

/// <summary>Get index-th outputs of the symbol.</summary>
/// <param name="symbol">The symbol</param>
/// <param name="index">the Index of the output.</param>
/// <param name="out">The output symbol whose outputs are the index-th symbol.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolGetOutput(SymbolHandle symbol, uint32 index, [<Out>] SymbolHandle& out)

/// <summary>List auxiliary states in the symbol.</summary>
/// <param name="symbol">the symbol</param>
/// <param name="out_size">output size</param>
/// <param name="out_str_array">pointer to hold the output string array</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolListAuxiliaryStates(SymbolHandle symbol, [<Out>] uint32& out_size, [<Out>] IntPtr& out_str_array)

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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolCompose(SymbolHandle sym, string name, uint32 num_args, string[] keys, SymbolHandle[] args)

/// <summary>Get the gradient graph of the symbol</summary>
/// <param name="sym">the symbol to get gradient</param>
/// <param name="num_wrt">number of arguments to get gradient</param>
/// <param name="wrt">the name of the arguments to get gradient</param>
/// <param name="out">the returned symbol that has gradient</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolGrad(SymbolHandle sym, uint32 num_wrt, string[] wrt, [<Out>] SymbolHandle& out)

(* DEPRECATED
/// <summary>DEPRECATED. Use MXSymbolInferShapeEx instead.
///infer shape of unknown input shapes given the known one.
/// The shapes are packed into a CSR matrix represented by arg_ind_ptr and arg_shape_data
/// The call will be treated as a kwargs call if key != nullptr or num_args==0, otherwise it is positional.</summary>
/// <param name="sym">symbol handle</param>
/// <param name="num_args">numbe of input arguments.</param>
/// <param name="keys">the key of keyword args (optional)</param>
/// <param name="arg_ind_ptr">the head pointer of the rows in CSR</param>
/// <param name="arg_shape_data">the content of the CSR</param>
/// <param name="in_shape_size">sizeof the returning array of in_shapes</param>
/// <param name="in_shape_ndim">returning array of shape dimensions of eachs input shape.</param>
/// <param name="in_shape_data">returning array of pointers to head of the input shape.</param>
/// <param name="out_shape_size">sizeof the returning array of out_shapes</param>
/// <param name="out_shape_ndim">returning array of shape dimensions of eachs input shape.</param>
/// <param name="out_shape_data">returning array of pointers to head of the input shape.</param>
/// <param name="aux_shape_size">sizeof the returning array of aux_shapes</param>
/// <param name="aux_shape_ndim">returning array of shape dimensions of eachs auxiliary shape.</param>
/// <param name="aux_shape_data">returning array of pointers to head of the auxiliary shape.</param>
/// <param name="complete">whether infer shape completes or more information is needed.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolInferShape(SymbolHandle sym, uint32 num_args, string[] keys, uint32[] arg_ind_ptr, uint32[] arg_shape_data, uint32[] in_shape_size, uint32[]& in_shape_ndim, IntPtr[]& in_shape_data, uint32[] out_shape_size, uint32[]& out_shape_ndim, IntPtr[]& out_shape_data, uint32[] aux_shape_size, uint32[]& aux_shape_ndim, IntPtr[]& aux_shape_data, int[] complete)

[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolInferShape64__(SymbolHandle sym, uint32 num_args, string[] keys, int64[] arg_ind_ptr, int64[] arg_shape_data, size_t[] in_shape_size, int[]& in_shape_ndim, IntPtr[]& in_shape_data, size_t[] out_shape_size, int[]& out_shape_ndim, IntPtr[]& out_shape_data, size_t[] aux_shape_size, int[]& aux_shape_ndim, IntPtr[]& aux_shape_data, int[] complete)
*)

/// <summary>infer shape of unknown input shapes given the known one.
/// The shapes are packed into a CSR matrix represented by arg_ind_ptr and arg_shape_data
/// The call will be treated as a kwargs call if key != nullptr or num_args==0, otherwise it is positional.</summary>
/// <param name="sym">symbol handle</param>
/// <param name="num_args">numbe of input arguments.</param>
/// <param name="keys">the key of keyword args (optional)</param>
/// <param name="arg_ind_ptr">the head pointer of the rows in CSR</param>
/// <param name="arg_shape_data">the content of the CSR</param>
/// <param name="in_shape_size">sizeof the returning array of in_shapes</param>
/// <param name="in_shape_ndim">returning array of shape dimensions of eachs input shape.</param>
/// <param name="in_shape_data">returning array of pointers to head of the input shape.</param>
/// <param name="out_shape_size">sizeof the returning array of out_shapes</param>
/// <param name="out_shape_ndim">returning array of shape dimensions of eachs input shape.</param>
/// <param name="out_shape_data">returning array of pointers to head of the input shape.</param>
/// <param name="aux_shape_size">sizeof the returning array of aux_shapes</param>
/// <param name="aux_shape_ndim">returning array of shape dimensions of eachs auxiliary shape.</param>
/// <param name="aux_shape_data">returning array of pointers to head of the auxiliary shape.</param>
/// <param name="complete">whether infer shape completes or more information is needed.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolInferShapeEx(SymbolHandle sym, uint32 num_args, string[] keys, uint32[] arg_ind_ptr, int[] arg_shape_data, [<Out>] uint32& in_shape_size, [<Out>] IntPtr& in_shape_ndim, [<Out>] IntPtr& in_shape_data, [<Out>] uint32& out_shape_size, [<Out>] IntPtr& out_shape_ndim, [<Out>] IntPtr& out_shape_data, [<Out>] uint32& aux_shape_size, [<Out>] IntPtr& aux_shape_ndim, [<Out>] IntPtr& aux_shape_data, [<Out>] int& complete)

[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolInferShapeEx64(SymbolHandle sym, uint32 num_args, string[] keys, int64[] arg_ind_ptr, int64[] arg_shape_data, [<Out>] size_t& in_shape_size, [<Out>] IntPtr& in_shape_ndim, [<Out>] IntPtr& in_shape_data, [<Out>] size_t& out_shape_size, [<Out>] IntPtr& out_shape_ndim, [<Out>] IntPtr& out_shape_data, [<Out>] size_t& aux_shape_size, [<Out>] IntPtr& aux_shape_ndim, [<Out>] IntPtr& aux_shape_data, [<Out>] int& complete)

(* DEPRECATED
/// <summary>DEPRECATED. Use MXSymbolInferShapePartialEx instead.
///partially infer shape of unknown input shapes given the known one.
///
/// Return partially inferred results if not all shapes could be inferred.
/// The shapes are packed into a CSR matrix represented by arg_ind_ptr and arg_shape_data
/// The call will be treated as a kwargs call if key != nullptr or num_args==0, otherwise it is positional.</summary>
/// <param name="sym">symbol handle</param>
/// <param name="num_args">numbe of input arguments.</param>
/// <param name="keys">the key of keyword args (optional)</param>
/// <param name="arg_ind_ptr">the head pointer of the rows in CSR</param>
/// <param name="arg_shape_data">the content of the CSR</param>
/// <param name="in_shape_size">sizeof the returning array of in_shapes</param>
/// <param name="in_shape_ndim">returning array of shape dimensions of eachs input shape.</param>
/// <param name="in_shape_data">returning array of pointers to head of the input shape.</param>
/// <param name="out_shape_size">sizeof the returning array of out_shapes</param>
/// <param name="out_shape_ndim">returning array of shape dimensions of eachs input shape.</param>
/// <param name="out_shape_data">returning array of pointers to head of the input shape.</param>
/// <param name="aux_shape_size">sizeof the returning array of aux_shapes</param>
/// <param name="aux_shape_ndim">returning array of shape dimensions of eachs auxiliary shape.</param>
/// <param name="aux_shape_data">returning array of pointers to head of the auxiliary shape.</param>
/// <param name="complete">whether infer shape completes or more information is needed.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolInferShapePartial(SymbolHandle sym, uint32 num_args, string[] keys, uint32[] arg_ind_ptr, uint32[] arg_shape_data, uint32[] in_shape_size, uint32[]& in_shape_ndim, IntPtr[]& in_shape_data, uint32[] out_shape_size, uint32[]& out_shape_ndim, IntPtr[]& out_shape_data, uint32[] aux_shape_size, uint32[]& aux_shape_ndim, IntPtr[]& aux_shape_data, int[] complete)

[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolInferShapePartial64(SymbolHandle sym, uint32 num_args, string[] keys, int64[] arg_ind_ptr, int64[] arg_shape_data, size_t[] in_shape_size, int[]& in_shape_ndim, IntPtr[]& in_shape_data, size_t[] out_shape_size, int[]& out_shape_ndim, IntPtr[]& out_shape_data, size_t[] aux_shape_size, int[]& aux_shape_ndim, IntPtr[]& aux_shape_data, int[] complete)
*)

/// <summary>partially infer shape of unknown input shapes given the known one.
///
/// Return partially inferred results if not all shapes could be inferred.
/// The shapes are packed into a CSR matrix represented by arg_ind_ptr and arg_shape_data
/// The call will be treated as a kwargs call if key != nullptr or num_args==0, otherwise it is positional.</summary>
/// <param name="sym">symbol handle</param>
/// <param name="num_args">numbe of input arguments.</param>
/// <param name="keys">the key of keyword args (optional)</param>
/// <param name="arg_ind_ptr">the head pointer of the rows in CSR</param>
/// <param name="arg_shape_data">the content of the CSR</param>
/// <param name="in_shape_size">sizeof the returning array of in_shapes</param>
/// <param name="in_shape_ndim">returning array of shape dimensions of eachs input shape.</param>
/// <param name="in_shape_data">returning array of pointers to head of the input shape.</param>
/// <param name="out_shape_size">sizeof the returning array of out_shapes</param>
/// <param name="out_shape_ndim">returning array of shape dimensions of eachs input shape.</param>
/// <param name="out_shape_data">returning array of pointers to head of the input shape.</param>
/// <param name="aux_shape_size">sizeof the returning array of aux_shapes</param>
/// <param name="aux_shape_ndim">returning array of shape dimensions of eachs auxiliary shape.</param>
/// <param name="aux_shape_data">returning array of pointers to head of the auxiliary shape.</param>
/// <param name="complete">whether infer shape completes or more information is needed.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolInferShapePartialEx(SymbolHandle sym, uint32 num_args, string[] keys, uint32[] arg_ind_ptr, int[] arg_shape_data, [<Out>] uint32& in_shape_size, [<Out>] IntPtr& in_shape_ndim, [<Out>] IntPtr& in_shape_data, [<Out>] uint32& out_shape_size, [<Out>] IntPtr& out_shape_ndim, [<Out>] IntPtr& out_shape_data, [<Out>] uint32& aux_shape_size, [<Out>] IntPtr& aux_shape_ndim, [<Out>] IntPtr& aux_shape_data, [<Out>] int& complete)

[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolInferShapePartialEx64(SymbolHandle sym, uint32 num_args, string[] keys, int64[] arg_ind_ptr, int64[] arg_shape_data, [<Out>] size_t& in_shape_size, [<Out>] IntPtr& in_shape_ndim, [<Out>] IntPtr& in_shape_data, [<Out>] size_t& out_shape_size, [<Out>] IntPtr& out_shape_ndim, [<Out>] IntPtr& out_shape_data, [<Out>] size_t& aux_shape_size, [<Out>] IntPtr& aux_shape_ndim, [<Out>] IntPtr& aux_shape_data, [<Out>] int& complete)

/// <summary>infer type of unknown input types given the known one.
/// The types are packed into a CSR matrix represented by arg_ind_ptr and arg_type_data
/// The call will be treated as a kwargs call if key != nullptr or num_args==0, otherwise it is positional.</summary>
/// <param name="sym">symbol handle</param>
/// <param name="num_args">numbe of input arguments.</param>
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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolInferType(SymbolHandle sym, uint32 num_args, string[] keys, int[] arg_type_data, [<Out>] uint32& in_type_size, [<Out>] IntPtr& in_type_data, [<Out>] uint32& out_type_size, [<Out>] IntPtr& out_type_data, [<Out>] uint32& aux_type_size, [<Out>] IntPtr& aux_type_data, [<Out>] int& complete)

/// <summary>partially infer type of unknown input types given the known one.
///
/// Return partially inferred results if not all types could be inferred.
/// The types are packed into a CSR matrix represented by arg_ind_ptr and arg_type_data
/// The call will be treated as a kwargs call if key != nullptr or num_args==0, otherwise it is positional.</summary>
/// <param name="sym">symbol handle</param>
/// <param name="num_args">numbe of input arguments.</param>
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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSymbolInferTypePartial(SymbolHandle sym, uint32 num_args, string[] keys, int[] arg_type_data, [<Out>] uint32& in_type_size, [<Out>] IntPtr& in_type_data, [<Out>] uint32& out_type_size, [<Out>] IntPtr& out_type_data, [<Out>] uint32& aux_type_size, [<Out>] IntPtr& aux_type_data, [<Out>] int& complete)

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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXQuantizeSymbol(SymbolHandle sym_handle, [<Out>] SymbolHandle& ret_sym_handle, [<Out>] int& dev_type, uint32 num_excluded_sym_names, string[] excluded_sym_names, uint32 num_excluded_op_names, string[] excluded_op_names, uint32 num_offline, string[] offline_params, string quantized_dtype, bool calib_quantize, string quantize_mode, [<Out>] uint32& out_num_calib_names, [<Out>] IntPtr& out_calib_names)

/// <summary>Convert a symbol into a mixed precision symbol with cast operators for target dtype casting</summary>
/// <param name="sym_handle">symbol to be converted</param>
/// <param name="ret_sym_handle">mixed precision symbol result</param>
/// <param name="num_args">number of arguments for known dtypes</param>
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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXReducePrecisionSymbol(SymbolHandle sym_handle, [<Out>] SymbolHandle& ret_sym_handle, uint32 num_args, int[] arg_type_data, uint32 num_ind_ptr, int[] ind_ptr, int[] target_dtype, int cast_optional_params, uint32 num_target_dtype_op_names, uint32 num_fp32_op_names, uint32 num_widest_dtype_op_names, uint32 num_conditional_fp32_op_names, uint32 num_excluded_symbols, uint32 num_model_params, string[] target_dtype_op_names, string[] fp32_op_names, string[] widest_dtype_op_names, string[] conditional_fp32_op_names, string[] excluded_symbols, string[] conditional_param_names, string[] conditional_param_vals, string[] model_param_names, string[] arg_names)

/// <summary>Set calibration table to node attributes in the sym</summary>
/// <param name="sym_handle">symbol whose node attributes are to be set by calibration table</param>
/// <param name="num_layers">number of layers in the calibration table</param>
/// <param name="layer">names stored as keys in the calibration table</param>
/// <param name="low_quantiles">low quantiles of layers stored in the calibration table</param>
/// <param name="high_quantiles">high quantiles of layers stored in the calibration table</param>
/// <param name="ret_sym_handle">returned symbol</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXSetCalibTableToQuantizedSymbol(SymbolHandle qsym_handle, uint32 num_layers, string[] layer_names, float32[] low_quantiles, float32[] high_quantiles, [<Out>] SymbolHandle& ret_sym_handle)

/// <summary>Run subgraph pass based on the backend provided</summary>
/// <param name="sym_handle">symbol to be converted</param>
/// <param name="backend">backend names for subgraph pass</param>
/// <param name="ret_sym_handle">returned symbol</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXGenBackendSubgraph(SymbolHandle sym_handle, string backend, [<Out>] SymbolHandle& ret_sym_handle)

/// <summary>Generate atomic symbol (able to be composed) from a source symbol</summary>
/// <param name="sym_handle">source symbol</param>
/// <param name="ret_sym_handle">returned atomic symbol</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXGenAtomicSymbolFromSymbol(SymbolHandle sym_handle, [<Out>] SymbolHandle& ret_sym_handle)

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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXOptimizeForBackend(SymbolHandle sym_handle, string backend_name, int dev_type, SymbolHandle[] ret_sym_handle, uint32 len, NDArrayHandle[] in_args_handle, uint32 num_options, string[] keys, string[] vals)

//--------------------------------------------
// Part 4: Executor interface
//--------------------------------------------
/// <summary>Delete the executor</summary>
/// <param name="handle">the executor.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorFree(ExecutorHandle handle)

/// <summary>Print the content of execution plan, used for debug.</summary>
/// <param name="handle">the executor.</param>
/// <param name="out_str">pointer to hold the output string of the printing.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorPrint(ExecutorHandle handle, [<Out>] IntPtr& out_str)

/// <summary>Executor forward method</summary>
/// <param name="handle">executor handle</param>
/// <param name="is_train">int value to indicate whether the forward pass is for evaluation</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorForward(ExecutorHandle handle, int is_train)

/// <summary>Excecutor run backward</summary>
/// <param name="handle">execute handle</param>
/// <param name="len">lenth</param>
/// <param name="head_grads">NDArray handle for heads' gradient</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorBackward(ExecutorHandle handle, uint32 len, NDArrayHandle[] head_grads)

/// <summary>Excecutor run backward</summary>
/// <param name="handle">execute handle</param>
/// <param name="len">lenth</param>
/// <param name="head_grads">NDArray handle for heads' gradient</param>
/// <param name="is_train">int value to indicate whether the backward pass is for evaluation</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorBackwardEx(ExecutorHandle handle, uint32 len, NDArrayHandle[] head_grads, int is_train)

/// <summary>Get executor's head NDArray</summary>
/// <param name="handle">executor handle</param>
/// <param name="out_size">output narray vector size</param>
/// <param name="out">out put narray handles</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorOutputs(ExecutorHandle handle, [<Out>] uint32& out_size, [<Out>] IntPtr& out)

/// <summary>Generate Executor from symbol</summary>
/// <param name="symbol_handle">symbol handle</param>
/// <param name="dev_type">device type</param>
/// <param name="dev_id">device id</param>
/// <param name="len">length</param>
/// <param name="in_args">in args array</param>
/// <param name="arg_grad_store">arg grads handle array</param>
/// <param name="grad_req_type">grad req array</param>
/// <param name="aux_states_len">length of auxiliary states</param>
/// <param name="aux_states">auxiliary states array</param>
/// <param name="out">output executor handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorBind(SymbolHandle symbol_handle, int dev_type, int dev_id, uint32 len, NDArrayHandle[] in_args, NDArrayHandle[] arg_grad_store, uint32[] grad_req_type, uint32 aux_states_len, NDArrayHandle[] aux_states, [<Out>] ExecutorHandle& out)

/// <summary>Generate Executor from symbol,
/// This is advanced function, allow specify group2ctx map.
/// The user can annotate "ctx_group" attribute to name each group.</summary>
/// <param name="symbol_handle">symbol handle</param>
/// <param name="dev_type">device type of default context</param>
/// <param name="dev_id">device id of default context</param>
/// <param name="num_map_keys">size of group2ctx map</param>
/// <param name="map_keys">keys of group2ctx map</param>
/// <param name="map_dev_types">device type of group2ctx map</param>
/// <param name="map_dev_ids">device id of group2ctx map</param>
/// <param name="len">length</param>
/// <param name="in_args">in args array</param>
/// <param name="arg_grad_store">arg grads handle array</param>
/// <param name="grad_req_type">grad req array</param>
/// <param name="aux_states_len">length of auxiliary states</param>
/// <param name="aux_states">auxiliary states array</param>
/// <param name="out">output executor handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorBindX(SymbolHandle symbol_handle, int dev_type, int dev_id, uint32 num_map_keys, string[] map_keys, int[] map_dev_types, int[] map_dev_ids, uint32 len, NDArrayHandle[] in_args, NDArrayHandle[] arg_grad_store, uint32[] grad_req_type, uint32 aux_states_len, NDArrayHandle[] aux_states, [<Out>] ExecutorHandle& out)

/// <summary>Generate Executor from symbol,
/// This is advanced function, allow specify group2ctx map.
/// The user can annotate "ctx_group" attribute to name each group.</summary>
/// <param name="symbol_handle">symbol handle</param>
/// <param name="dev_type">device type of default context</param>
/// <param name="dev_id">device id of default context</param>
/// <param name="num_map_keys">size of group2ctx map</param>
/// <param name="map_keys">keys of group2ctx map</param>
/// <param name="map_dev_types">device type of group2ctx map</param>
/// <param name="map_dev_ids">device id of group2ctx map</param>
/// <param name="len">length</param>
/// <param name="in_args">in args array</param>
/// <param name="arg_grad_store">arg grads handle array</param>
/// <param name="grad_req_type">grad req array</param>
/// <param name="aux_states_len">length of auxiliary states</param>
/// <param name="aux_states">auxiliary states array</param>
/// <param name="shared_exec">input executor handle for memory sharing</param>
/// <param name="out">output executor handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorBindEX(SymbolHandle symbol_handle, int dev_type, int dev_id, uint32 num_map_keys, string[] map_keys, int[] map_dev_types, int[] map_dev_ids, uint32 len, NDArrayHandle[] in_args, NDArrayHandle[] arg_grad_store, uint32[] grad_req_type, uint32 aux_states_len, NDArrayHandle[] aux_states, ExecutorHandle shared_exec, [<Out>] ExecutorHandle& out)

(* Exclude DEPRECATED
/// <summary>DEPRECATED. Use MXExecutorSimpleBindEx instead.</summary>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorSimpleBind(SymbolHandle symbol_handle, int dev_type, int dev_id, uint32 num_g2c_keys, string[] g2c_keys, int[] g2c_dev_types, int[] g2c_dev_ids, uint32 provided_grad_req_list_len, string[] provided_grad_req_names, string[] provided_grad_req_types, uint32 num_provided_arg_shapes, string[] provided_arg_shape_names, uint32[] provided_arg_shape_data, uint32[] provided_arg_shape_idx, uint32 num_provided_arg_dtypes, string[] provided_arg_dtype_names, int[] provided_arg_dtypes, uint32 num_provided_arg_stypes, string[] provided_arg_stype_names, int[] provided_arg_stypes, uint32 num_shared_arg_names, string[] shared_arg_name_list, int[] shared_buffer_len, string[] shared_buffer_name_list, NDArrayHandle[] shared_buffer_handle_list, string[]& updated_shared_buffer_name_list, NDArrayHandle[]& updated_shared_buffer_handle_list, uint32[] num_in_args, NDArrayHandle[]& in_args, NDArrayHandle[]& arg_grads, uint32[] num_aux_states, NDArrayHandle[]& aux_states, ExecutorHandle shared_exec_handle, ExecutorHandle[] out)
*)

[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorSimpleBindEx(SymbolHandle symbol_handle, int dev_type, int dev_id, uint32 num_g2c_keys, string[] g2c_keys, int[] g2c_dev_types, int[] g2c_dev_ids, uint32 provided_grad_req_list_len, string[] provided_grad_req_names, string[] provided_grad_req_types, uint32 num_provided_arg_shapes, string[] provided_arg_shape_names, int[] provided_arg_shape_data, uint32[] provided_arg_shape_idx, uint32 num_provided_arg_dtypes, string[] provided_arg_dtype_names, int[] provided_arg_dtypes, uint32 num_provided_arg_stypes, string[] provided_arg_stype_names, int[] provided_arg_stypes, uint32 num_shared_arg_names, string[] shared_arg_name_list, int[] shared_buffer_len, string[] shared_buffer_name_list, NDArrayHandle[] shared_buffer_handle_list, string[]& updated_shared_buffer_name_list, NDArrayHandle[]& updated_shared_buffer_handle_list, uint32[] num_in_args, NDArrayHandle[]& in_args, NDArrayHandle[]& arg_grads, uint32[] num_aux_states, NDArrayHandle[]& aux_states, ExecutorHandle shared_exec_handle, [<Out>] ExecutorHandle& out)

(* Exclude DEPRECATED
/// <summary>DEPRECATED. Use MXExecutorReshapeEx instead.
///Return a new executor with the same symbol and shared memory,
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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorReshape(int partial_shaping, int allow_up_sizing, int dev_type, int dev_id, uint32 num_map_keys, string[] map_keys, int[] map_dev_types, int[] map_dev_ids, uint32 num_provided_arg_shapes, string[] provided_arg_shape_names, uint32[] provided_arg_shape_data, uint32[] provided_arg_shape_idx, uint32[] num_in_args, NDArrayHandle[]& in_args, NDArrayHandle[]& arg_grads, uint32[] num_aux_states, NDArrayHandle[]& aux_states, ExecutorHandle shared_exec, ExecutorHandle[] out)
*)

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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorReshapeEx(int partial_shaping, int allow_up_sizing, int dev_type, int dev_id, uint32 num_map_keys, string[] map_keys, int[] map_dev_types, int[] map_dev_ids, uint32 num_provided_arg_shapes, string[] provided_arg_shape_names, int[] provided_arg_shape_data, uint32[] provided_arg_shape_idx, uint32 num_in_args, NDArrayHandle[] in_args, NDArrayHandle[] arg_grads, uint32 num_aux_states, NDArrayHandle[] aux_states, ExecutorHandle shared_exec, [<Out>]ExecutorHandle& out)

/// <summary>get optimized graph from graph executor</summary>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorGetOptimizedSymbol(ExecutorHandle handle, [<Out>] SymbolHandle& out)

/// <summary>set a call back to notify the completion of operation</summary>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorSetMonitorCallback(ExecutorHandle handle, ExecutorMonitorCallback callback, [<Out>] IntPtr& callback_handle)

/// <summary>set a call back to notify the completion of operation</summary>
/// <param name="monitor_all">If true, monitor both input and output, otherwise monitor output only.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXExecutorSetMonitorCallbackEX(ExecutorHandle handle, ExecutorMonitorCallback callback, [<Out>] IntPtr& callback_handle, bool monitor_all)

//--------------------------------------------
// Part 5: IO Interface
//--------------------------------------------
/// <summary>List all the available iterator entries</summary>
/// <param name="out_size">the size of returned iterators</param>
/// <param name="out_array">the output iteratos entries</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXListDataIters([<Out>] uint32& out_size, [<Out>] DataIterCreator& out_array)

/// <summary>Init an iterator, init with parameters
///the array size of passed in arguments</summary>
/// <param name="handle">of the iterator creator</param>
/// <param name="num_param">number of parameter</param>
/// <param name="keys">parameter keys</param>
/// <param name="vals">parameter values</param>
/// <param name="out">resulting iterator</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXDataIterCreateIter(DataIterCreator handle, uint32 num_param, string[] keys, string[] vals, [<Out>] DataIterHandle& out)

/// <summary>Get the detailed information about data iterator.</summary>
/// <param name="creator">the DataIterCreator.</param>
/// <param name="name">The returned name of the creator.</param>
/// <param name="description">The returned description of the symbol.</param>
/// <param name="num_args">Number of arguments.</param>
/// <param name="arg_names">Name of the arguments.</param>
/// <param name="arg_type_infos">Type informations about the arguments.</param>
/// <param name="arg_descriptions">Description information about the arguments.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXDataIterGetIterInfo(DataIterCreator creator, [<Out>] IntPtr& name, [<Out>] IntPtr& description, [<Out>] uint32& num_args, [<Out>] IntPtr& arg_names, [<Out>] IntPtr& arg_type_infos, [<Out>] IntPtr& arg_descriptions)

/// <summary>Free the handle to the IO module</summary>
/// <param name="handle">the handle pointer to the data iterator</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXDataIterFree(DataIterHandle handle)

/// <summary>Move iterator to next position</summary>
/// <param name="handle">the handle to iterator</param>
/// <param name="out">return value of next</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXDataIterNext(DataIterHandle handle, [<Out>]int& out)

/// <summary>Call iterator.Reset</summary>
/// <param name="handle">the handle to iterator</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXDataIterBeforeFirst(DataIterHandle handle)

/// <summary>Get the handle to the NDArray of underlying data</summary>
/// <param name="handle">the handle pointer to the data iterator</param>
/// <param name="out">handle to underlying data NDArray</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXDataIterGetData(DataIterHandle handle, [<Out>]NDArrayHandle& out)

/// <summary>Get the image index by array.</summary>
/// <param name="handle">the handle pointer to the data iterator</param>
/// <param name="out_index">output index of the array.</param>
/// <param name="out_size">output size of the array.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXDataIterGetIndex(DataIterHandle handle, [<Out>]IntPtr& out_index, [<Out>]uint64& out_size)

/// <summary>Get the padding number in current data batch</summary>
/// <param name="handle">the handle pointer to the data iterator</param>
/// <param name="pad">pad number ptr</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXDataIterGetPadNum(DataIterHandle handle, [<Out>]int& pad)

/// <summary>Get the handle to the NDArray of underlying label</summary>
/// <param name="handle">the handle pointer to the data iterator</param>
/// <param name="out">the handle to underlying label NDArray</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXDataIterGetLabel(DataIterHandle handle, [<Out>] NDArrayHandle& out)

//--------------------------------------------
// Part 6: basic KVStore interface
//--------------------------------------------
/// <summary>Initialized ps-lite environment variables</summary>
/// <param name="num_vars">number of variables to initialize</param>
/// <param name="keys">environment keys</param>
/// <param name="vals">environment values</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXInitPSEnv(uint32 num_vars, string[] keys, string[] vals)

/// <summary>Create a kvstore</summary>
/// <param name="type">the type of KVStore</param>
/// <param name="out">The output type of KVStore</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreCreate(string ``type``, [<Out>] KVStoreHandle& out)

/// <summary>Set parameters to use low-bit compressed gradients</summary>
/// <param name="handle">handle to the kvstore</param>
/// <param name="keys">keys for compression parameters</param>
/// <param name="vals">values for compression parameters</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreSetGradientCompression(KVStoreHandle handle, uint32 num_params, string[] keys, string[] vals)

/// <summary>Delete a KVStore handle.</summary>
/// <param name="handle">handle to the kvstore</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreFree(KVStoreHandle handle)

/// <summary>Init a list of (key,value) pairs in kvstore</summary>
/// <param name="handle">handle to the kvstore</param>
/// <param name="num">the number of key-value pairs</param>
/// <param name="keys">the list of keys</param>
/// <param name="vals">the list of values</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreInit(KVStoreHandle handle, uint32 num, int[] keys, NDArrayHandle[] vals)

/// <summary>Init a list of (key,value) pairs in kvstore, where each key is a string</summary>
/// <param name="handle">handle to the kvstore</param>
/// <param name="num">the number of key-value pairs</param>
/// <param name="keys">the list of keys</param>
/// <param name="vals">the list of values</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreInitEx(KVStoreHandle handle, uint32 num, string[] keys, NDArrayHandle[] vals)

/// <summary>Push a list of (key,value) pairs to kvstore</summary>
/// <param name="handle">handle to the kvstore</param>
/// <param name="num">the number of key-value pairs</param>
/// <param name="keys">the list of keys</param>
/// <param name="vals">the list of values</param>
/// <param name="priority">the priority of the action</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStorePush(KVStoreHandle handle, uint32 num, int[] keys, NDArrayHandle[] vals, int priority)

/// <summary>Push a list of (key,value) pairs to kvstore, where each key is a string</summary>
/// <param name="handle">handle to the kvstore</param>
/// <param name="num">the number of key-value pairs</param>
/// <param name="keys">the list of keys</param>
/// <param name="vals">the list of values</param>
/// <param name="priority">the priority of the action</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStorePushEx(KVStoreHandle handle, uint32 num, string[] keys, NDArrayHandle[] vals, int priority)

/// <summary>pull a list of (key, value) pairs from the kvstore</summary>
/// <param name="handle">handle to the kvstore</param>
/// <param name="num">the number of key-value pairs</param>
/// <param name="keys">the list of keys</param>
/// <param name="vals">the list of values</param>
/// <param name="priority">the priority of the action</param>
/// <param name="ignore_sparse">whether to ignore sparse arrays in the request</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStorePullWithSparse(KVStoreHandle handle, uint32 num, int[] keys, [<Out>] IntPtr& vals, int priority, bool ignore_sparse)

/// <summary>pull a list of (key, value) pairs from the kvstore, where each key is a string</summary>
/// <param name="handle">handle to the kvstore</param>
/// <param name="num">the number of key-value pairs</param>
/// <param name="keys">the list of keys</param>
/// <param name="vals">the list of values</param>
/// <param name="priority">the priority of the action</param>
/// <param name="ignore_sparse">whether to ignore sparse arrays in the request</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStorePullWithSparseEx(KVStoreHandle handle, uint32 num, string[] keys, [<Out>] NDArrayHandle& vals, int priority, bool ignore_sparse)

/// <summary>pull a list of (key, value) pairs from the kvstore</summary>
/// <param name="handle">handle to the kvstore</param>
/// <param name="num">the number of key-value pairs</param>
/// <param name="keys">the list of keys</param>
/// <param name="vals">the list of values</param>
/// <param name="priority">the priority of the action</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStorePull(KVStoreHandle handle, uint32 num, int[] keys, [<Out>] NDArrayHandle& vals, int priority)

/// <summary>pull a list of (key, value) pairs from the kvstore, where each key is a string</summary>
/// <param name="handle">handle to the kvstore</param>
/// <param name="num">the number of key-value pairs</param>
/// <param name="keys">the list of keys</param>
/// <param name="vals">the list of values</param>
/// <param name="priority">the priority of the action</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStorePullEx(KVStoreHandle handle, uint32 num, string[] keys, [<Out>] NDArrayHandle& vals, int priority)

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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStorePullRowSparse(KVStoreHandle handle, uint32 num, int[] keys, [<Out>] NDArrayHandle& vals, [<Out>] NDArrayHandle& row_ids, int priority)

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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStorePullRowSparseEx(KVStoreHandle handle, uint32 num, string[] keys, [<Out>] NDArrayHandle& vals, [<Out>] NDArrayHandle& row_ids, int priority)

/// <summary>push and pull a list of (key, value) pairs from the kvstore</summary>
/// <param name="handle">handle to the kvstore</param>
/// <param name="vnum">the number of key-value pairs corresponding to vkeys</param>
/// <param name="vkeys">the list of keys for the values to be pushed</param>
/// <param name="onum">the number of key-value pairs corresponding to okeys</param>
/// <param name="okeys">the list of keys for the values to be pulled</param>
/// <param name="vals">the list of values</param>
/// <param name="outs">the list of outputs</param>
/// <param name="priority">the priority of the action</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStorePushPull(KVStoreHandle handle, mx_uint vnum, int[] vkeys, mx_uint onum, int[] okeys, NDArrayHandle[] vals, [<Out>] NDArrayHandle& outs, int priority)

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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStorePushPullEx(KVStoreHandle handle, mx_uint vnum, string[] vkeys, mx_uint onum, string[] okeys, NDArrayHandle[] vals, [<Out>] NDArrayHandle& outs, int priority)

/// <summary>user-defined updater for the kvstore
///It's this updater's responsibility to delete\a recv and\a local</summary>
/// <param name="the">key</param>
/// <param name="recv">the pushed value on this key</param>
/// <param name="local">the value stored on local on this key</param>
/// <param name="handle">The additional handle to the updater</param>
type MXKVStoreUpdater = delegate of int * NDArrayHandle * NDArrayHandle * IntPtr -> unit
/// <summary>user-defined updater for the kvstore with string keys
///It's this updater's responsibility to delete\a recv and\a local</summary>
/// <param name="the">key</param>
/// <param name="recv">the pushed value on this key</param>
/// <param name="local">the value stored on local on this key</param>
/// <param name="handle">The additional handle to the updater</param>
type MXKVStoreStrUpdater = delegate of string * NDArrayHandle * NDArrayHandle * IntPtr -> unit
/// <summary>register a push updater</summary>
/// <param name="handle">handle to the KVStore</param>
/// <param name="updater">udpater function</param>
/// <param name="updater_handle">The additional handle used to invoke the updater</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreSetUpdater(KVStoreHandle handle, MXKVStoreUpdater updater, IntPtr updater_handle)

/// <summary>register a push updater with int keys and one with string keys</summary>
/// <param name="handle">handle to the KVStore</param>
/// <param name="updater">updater function with int keys</param>
/// <param name="str_updater">updater function with string keys</param>
/// <param name="updater_handle">The additional handle used to invoke the updater</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreSetUpdaterEx(KVStoreHandle handle, MXKVStoreUpdater updater, MXKVStoreStrUpdater str_updater, IntPtr updater_handle)

/// <summary>get the type of the kvstore</summary>
/// <param name="handle">handle to the KVStore</param>
/// <param name="type">a string type</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreGetType(KVStoreHandle handle, [<Out>] IntPtr& ``type``)

//--------------------------------------------
// Part 6: advanced KVStore for multi-machines
//--------------------------------------------
/// <summary> return The rank of this node in its group, which is in [0, GroupSize).</summary>
/// <param name="handle"> handle to the KVStore</param>
/// <param name="ret">the node rank</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreGetRank(KVStoreHandle handle, [<Out>] int& ret)

/// <summary>return The number of nodes in this group, which is 
/// - number of workers if if `IsWorkerNode() == true`,
/// - number of servers if if `IsServerNode() == true`,
/// - 1 if `IsSchedulerNode() == true`, </summary>
/// <param name="handle"> handle to the KVStore</param>
/// <param name="ret"> the group size</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreGetGroupSize(KVStoreHandle handle, [<Out>] int& ret)

/// <summary> return whether or not this process is a worker node.</summary>
/// <param name="ret"> 1 for yes, 0 for no</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreIsWorkerNode([<Out>] int& ret)


/// <summar>return whether or not this process is a server node.</summary>
/// <param name="ret"> 1 for yes, 0 for no</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreIsServerNode([<Out>] int& ret)

/// <summary>return whether or not this process is a scheduler node.</summary>
/// <param name="ret"> 1 for yes, 0 for no</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreIsSchedulerNode([<Out>] int& ret)

/// <summary>global barrier among all worker machines</summary>
/// <param name="handle"> handle to the KVStore</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreBarrier(KVStoreHandle handle)

/// <summary>rwhether to do barrier when finalize</summary>
/// <param name="handle"> handle to the KVStore</param>
/// <param name="barrier_before_exit"> whether to do barrier when kvstore finalize</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreSetBarrierBeforeExit(KVStoreHandle handle, int barrier_before_exit)

/// <summary>rthe prototype of a server controller</summary>
/// <param name="head"> the head of the command</param>
/// <param name="body"> the body of the command</param>
/// <param name="controller_handle"> helper handle for implementing controller</param>
type MXKVStoreServerController = delegate of int * string * IntPtr -> unit

/// <summary>rRun as server (or scheduler)</summary>
/// <param name="handle"> handle to the KVStore</param>
/// <param name="controller"> the user-defined server controller</param>
/// <param name="controller_handle"> helper handle for implementing controller</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreRunServer(KVStoreHandle handle, MXKVStoreServerController controller, IntPtr controller_handle)

/// <summary>rSend a command to all server nodes</summary>
/// <param name="handle"> handle to the KVStore</param>
/// <param name="cmd_id"> the head of the command</param>
/// <param name="cmd_body"> the body of the command</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreSendCommmandToServers(KVStoreHandle handle, int cmd_id, string cmd_body)

/// <summary>rGet the number of ps dead node(s) specified by {node_id}</summary>
/// <param name="handle"> handle to the KVStore</param>
/// <param name="node_id"> Can be a node group or a single node.
///    kScheduler = 1, kServerGroup = 2, kWorkerGroup = 4</param>
/// <param name="number"> Ouptut number of dead nodes</param>
/// <param name="timeout_sec"> A node fails to send heartbeart in {timeout_sec} seconds
/// will be presumed as 'dead'</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXKVStoreGetNumDeadNode(KVStoreHandle handle, int node_id, [<Out>] int& number, int timeout_sec (*=60*))

/// <summary>rCreate a RecordIO writer object</summary>
/// <param name="uri"> path to file</param>
/// <param name="out"> handle pointer to the created object</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRecordIOWriterCreate(string uri, [<Out>] RecordIOHandle& out)

/// <summary>rDelete a RecordIO writer object</summary>
/// <param name="handle"> handle to RecordIO object</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRecordIOWriterFree(RecordIOHandle handle)

/// <summary>rWrite a record to a RecordIO object</summary>
/// <param name="handle"> handle to RecordIO object</param>
/// <param name="buf"> buffer to write</param>
/// <param name="size"> size of buffer</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRecordIOWriterWriteRecord(RecordIOHandle handle, string buf, size_t size)

/// <summary>rGet the current writer pointer position</summary>
/// <param name="handle"> handle to RecordIO object</param>
/// <param name="pos"> handle to output position</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRecordIOWriterTell(RecordIOHandle handle, [<Out>]size_t& pos)

/// <summary>rCreate a RecordIO reader object</summary>
/// <param name="uri"> path to file</param>
/// <param name="out"> handle pointer to the created object</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRecordIOReaderCreate(string uri, [<Out>] RecordIOHandle& out)

/// <summary>rDelete a RecordIO reader object</summary>
/// <param name="handle"> handle to RecordIO object</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRecordIOReaderFree(RecordIOHandle handle)

/// <summary>rWrite a record to a RecordIO object</summary>
/// <param name="handle"> handle to RecordIO object</param>
/// <param name="buf"> pointer to return buffer</param>
/// <param name="size"> point to size of buffer</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRecordIOReaderReadRecord(RecordIOHandle handle, [<Out>] IntPtr buf, [<Out>]size_t& size)

/// <summary>rSet the current reader pointer position</summary>
/// <param name="handle"> handle to RecordIO object</param>
/// <param name="pos"> target position</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRecordIOReaderSeek(RecordIOHandle handle, size_t pos)

/// <summary>rGet the current writer pointer position</summary>
/// <param name="handle"> handle to RecordIO object</param>
/// <param name="pos"> handle to output position</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRecordIOReaderTell(RecordIOHandle handle, [<Out>]size_t& size)

/// <summary>rCreate a MXRtc object</summary>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRtcCreate__(string name, uint32 num_input, uint32 num_output, string[] input_names, string[] output_names, NDArrayHandle[] inputs, [<Out>] NDArrayHandle& outputs, string kernel, RtcHandle[] out)

/// <summary>rRun cuda kernel</summary>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRtcPush__(RtcHandle handle, uint32 num_input, uint32 num_output, NDArrayHandle[] inputs, [<Out>] NDArrayHandle& outputs, uint32 gridDimX, uint32 gridDimY, uint32 gridDimZ, uint32 blockDimX, uint32 blockDimY, uint32 blockDimZ)

/// Delete a MXRtc object
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRtcFree__(RtcHandle handle)

/// <summary> register custom operators from frontend.</summary>
/// <param name = op_type>name of custom op</param>
/// <param name = creator>creator</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXCustomOpRegister(string op_type, CustomOpPropCreator creator)

/// <summary>rrecord custom function for backward later.</summary>
/// <param name="num_inputs"> number of input NDArrays.</param>
/// <param name="inputs"> handle to input NDArrays.</param>
/// <param name="num_outputs"> number of output NDArrays.</param>
/// <param name="outputs"> handle to output NDArrays.</param>
/// <param name="callbacks"> callbacks for backward function.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXCustomFunctionRecord__(int num_inputs, NDArrayHandle[] inputs, int num_outputs, [<Out>] NDArrayHandle& outputs,  MXCallbackList[] callbacks)

/// <summary>rcreate cuda rtc module</summary>
/// <param name="source"> cuda source code</param>
/// <param name="num_options"> number of compiler flags</param>
/// <param name="options"> compiler flags</param>
/// <param name="num_exports"> number of exported function names</param>
/// <param name="exported"> function names</param>
/// <param name="out"> handle to created module</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRtcCudaModuleCreate(string source, int num_options, string[] options, int num_exports, string[] exports, [<Out>] CudaModuleHandle& out)

/// <summary>rdelete cuda rtc module</summary>
/// <param name="handle"> handle to cuda module</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRtcCudaModuleFree(CudaModuleHandle handle)

/// <summary>rget kernel from module</summary>
/// <param name="handle"> handle to cuda module</param>
/// <param name="name"> name of kernel function</param>
/// <param name="num_args"> number of arguments</param>
/// <param name="is_ndarray"> whether argument is ndarray</param>
/// <param name="is_const"> whether argument is constant</param>
/// <param name="arg_types"> data type of arguments</param>
/// <param name="out"> created kernel</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRtcCudaKernelCreate(CudaModuleHandle handle, string name, int num_args, int[] is_ndarray, int[] is_const, int[] arg_types, [<Out>] CudaModuleHandle& out)

/// <summary>rdelete kernel</summary>
/// <param name="handle"> handle to previously created kernel</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRtcCudaKernelFree(CudaKernelHandle handle)

/// <summary>rlaunch cuda kernel</summary>
/// <param name="handle"> handle to kernel</param>
/// <param name="dev_id"> (GPU) device id</param>
/// <param name="args"> pointer to arguments</param>
/// <param name="grid_dim_x"> grid dimension x</param>
/// <param name="grid_dim_y"> grid dimension y</param>
/// <param name="grid_dim_z"> grid dimension z</param>
/// <param name="block_dim_x"> block dimension x</param>
/// <param name="block_dim_y"> block dimension y</param>
/// <param name="block_dim_z"> block dimension z</param>
/// <param name="shared_mem"> size of dynamically allocated shared memory</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXRtcCudaKernelCall(CudaKernelHandle handle, int dev_id, IntPtr[] args, uint32 grid_dim_x, uint32 grid_dim_y, uint32 grid_dim_z, uint32 block_dim_x, uint32 block_dim_y, uint32 block_dim_z, uint32 shared_mem)

/// <summary>Get shared memory handle from NDArray</summary>
/// <param name="handle">NDArray handle.</param>
/// <param name="shared_pid">output PID</param>
/// <param name="shared_id">output shared memory id.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayGetSharedMemHandle__(NDArrayHandle handle, int[] shared_pid, int[] shared_id)

/// <summary>DEPRECATED. Use MXNDArrayCreateFromSharedMemEx instead.
///Reconstruct NDArray from shared memory handle</summary>
/// <param name="shared_pid">shared PID</param>
/// <param name="shared_id">shared memory id</param>
/// <param name="shape">pointer to NDArray dimensions</param>
/// <param name="ndim">number of NDArray dimensions</param>
/// <param name="dtype">data type of NDArray</param>
/// <param name="out">constructed NDArray</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayCreateFromSharedMem__(int shared_pid, int shared_id, uint32[] shape, uint32 ndim, int dtype, [<Out>] NDArrayHandle& out)

/// <summary>Release all unreferenced memory from the devices storage managers memory pool</summary>
/// <param name="dev_type">device type, specify device we want to take</param>
/// <param name="dev_id">the device id of the specific device</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXStorageEmptyCache__(int dev_type, int dev_id)

/// <summary>Reconstruct NDArray from shared memory handle</summary>
/// <param name="shared_pid">shared PID</param>
/// <param name="shared_id">shared memory id</param>
/// <param name="shape">pointer to NDArray dimensions</param>
/// <param name="ndim">number of NDArray dimensions</param>
/// <param name="dtype">data type of NDArray</param>
/// <param name="out">constructed NDArray</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXNDArrayCreateFromSharedMemEx__(int shared_pid, int shared_id, int[] shape, int ndim, int dtype, [<Out>] NDArrayHandle& out)

/// <summary>Push an asynchronous operation to the engine.</summary>
/// <param name="async_func">Execution function whici takes a parameter on_complete
///                  that must be called when the execution ompletes.</param>
/// <param name="func_param">The parameter set on calling async_func, can be NULL.</param>
/// <param name="deleter">The callback to free func_param, can be NULL.</param>
/// <param name="ctx_handle">Execution context.</param>
/// <param name="const_vars_handle">The variables that current operation will use
///                         but not mutate.</param>
/// <param name="num_const_vars">The number of const_vars_handle.</param>
/// <param name="mutable_vars_handle">The variables that current operation will mutate.</param>
/// <param name="num_mutable_vars">The number of mutable_vars_handle.</param>
/// <param name="prop_handle">Property of the function.</param>
/// <param name="priority">Priority of the action, as hint to the engine.</param>
/// <param name="opr_name">The operation name.</param>
/// <param name="wait">Whether this is a WaitForVar operation.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXEnginePushAsync(EngineAsyncFunc async_func, IntPtr func_param, EngineFuncParamDeleter deleter, ContextHandle ctx_handle, EngineVarHandle _vars_handle, int num_const_vars, EngineVarHandle mutable_vars_handle, int num_mutable_vars, EngineFnPropertyHandle prop_handle (*=NULL*), int priority (*=0*), string opr_name (*=NULL*), bool wait (*=false*))

/// <summary>Push a synchronous operation to the engine.</summary>
/// <param name="sync_func">Execution function that executes the operation.</param>
/// <param name="func_param">The parameter set on calling sync_func, can be NULL.</param>
/// <param name="deleter">The callback to free func_param, can be NULL.</param>
/// <param name="ctx_handle">Execution context.</param>
/// <param name="const_vars_handle">The variables that current operation will use
///                         but not mutate.</param>
/// <param name="num_const_vars">The number of const_vars_handle.</param>
/// <param name="mutable_vars_handle">The variables that current operation will mutate.</param>
/// <param name="num_mutable_vars">The number of mutable_vars_handle.</param>
/// <param name="prop_handle">Property of the function.</param>
/// <param name="priority">Priority of the action, as hint to the engine.</param>
/// <param name="opr_name">The operation name.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXEnginePushSync(EngineSyncFunc sync_func, IntPtr func_param, EngineFuncParamDeleter deleter, ContextHandle ctx_handle, EngineVarHandle _vars_handle, int num_const_vars, EngineVarHandle mutable_vars_handle, int num_mutable_vars, EngineFnPropertyHandle prop_handle (*=NULL*), int priority (*=0*), string opr_name (*=NULL*))

/// <summary>Push an asynchronous operation to the engine.</summary>
/// <param name="async_func">Execution function whici takes a parameter on_complete
///                  that must be called when the execution ompletes.</param>
/// <param name="func_param">The parameter set on calling async_func, can be NULL.</param>
/// <param name="deleter">The callback to free func_param, can be NULL.</param>
/// <param name="ctx_handle">Execution context.</param>
/// <param name="const_nds_handle">The NDArrays that current operation will use
///                         but not mutate.</param>
/// <param name="num_const_nds">The number of const_nds_handle.</param>
/// <param name="mutable_nds_handle">The NDArrays that current operation will mutate.</param>
/// <param name="num_mutable_nds">The number of mutable_nds_handle.</param>
/// <param name="prop_handle">Property of the function.</param>
/// <param name="priority">Priority of the action, as hint to the engine.</param>
/// <param name="opr_name">The operation name.</param>
/// <param name="wait">Whether this is a WaitForVar operation.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXEnginePushAsyncND(EngineAsyncFunc async_func, IntPtr func_param, EngineFuncParamDeleter deleter, ContextHandle ctx_handle, NDArrayHandle[] _nds_handle, int num_const_nds, NDArrayHandle[] mutable_nds_handle, int num_mutable_nds, EngineFnPropertyHandle prop_handle (*=NULL*), int priority (*=0*), string opr_name (*=NULL*), bool wait (*=false*))

/// <summary>Push a synchronous operation to the engine.</summary>
/// <param name="sync_func">Execution function that executes the operation.</param>
/// <param name="func_param">The parameter set on calling sync_func, can be NULL.</param>
/// <param name="deleter">The callback to free func_param, can be NULL.</param>
/// <param name="ctx_handle">Execution context.</param>
/// <param name="const_nds_handle">The NDArrays that current operation will use
///                         but not mutate.</param>
/// <param name="num_const_nds">The number of const_nds_handle.</param>
/// <param name="mutable_nds_handle">The NDArrays that current operation will mutate.</param>
/// <param name="num_mutable_nds">The number of mutable_nds_handle.</param>
/// <param name="prop_handle">Property of the function.</param>
/// <param name="priority">Priority of the action, as hint to the engine.</param>
/// <param name="opr_name">The operation name.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXEnginePushSyncND(EngineSyncFunc sync_func, IntPtr func_param, EngineFuncParamDeleter deleter, ContextHandle ctx_handle, NDArrayHandle[] _nds_handle, int num_const_nds, NDArrayHandle[] mutable_nds_handle, int num_mutable_nds, EngineFnPropertyHandle prop_handle (*=NULL*), int priority (*=0*), string opr_name (*=NULL*))

/// <summary>Create an NDArray from source sharing the same data chunk.</summary>
/// <param name="src">source NDArray</param>
/// <param name="out">new NDArray sharing the same data chunck with src</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXShallowCopyNDArray__(NDArrayHandle src, [<Out>] NDArrayHandle& out)

/// <summary>Create an Symbol from source sharing the same graph structure.</summary>
/// <param name="src">source Symbol</param>
/// <param name="out">new Symbol sharing the same graph structure with src</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int MXShallowCopySymbol__(SymbolHandle src, SymbolHandle[] out)

