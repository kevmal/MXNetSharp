module MXNetSharp.Interop.CPredictApi

open MXNetSharp.Interop.CApi
open System
open System.Runtime.InteropServices
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
/// <summary>handle to Predictor</summary>
type PredictorHandle = IntPtr
/// <summary>handle to NDArray list</summary>
type NDListHandle = IntPtr
/// <summary>handle to NDArray</summary>
type NDArrayHandle = IntPtr
/// <summary>callback used for add monitoring to nodes in the graph</summary>
type PredMonitorCallback = delegate of string * NDArrayHandle * IntPtr -> unit

/// <summary>create a predictor</summary>
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
/// <param name="out">The created predictor handle.</param>
/// <returns>0 when success, -1 when failure.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXPredCreate(string symbol_json_str, IntPtr param_bytes, int param_size, int dev_type, int dev_id, uint32 num_input_nodes, string[] input_keys, uint32[] input_shape_indptr, uint32[] input_shape_data, [<Out>] PredictorHandle& out)

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
/// <param name="out">The created predictor handle.</param>
/// <returns>0 when success, -1 when failure.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXPredCreateEx(string symbol_json_str, IntPtr param_bytes, int param_size, int dev_type, int dev_id, uint32 num_input_nodes, string[] input_keys, uint32[] input_shape_indptr, uint32[] input_shape_data, uint32 num_provided_arg_dtypes, string[] provided_arg_dtype_names, int[] provided_arg_dtypes, [<Out>] PredictorHandle& out)

/// <summary>create a predictor wicth customized outputs</summary>
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
/// <param name="out">The created predictor handle.</param>
/// <returns>0 when success, -1 when failure.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXPredCreatePartialOut(string symbol_json_str, IntPtr param_bytes, int param_size, int dev_type, int dev_id, uint32 num_input_nodes, string[] input_keys, uint32[] input_shape_indptr, uint32[] input_shape_data, uint32 num_output_nodes, string[] output_keys, [<Out>] PredictorHandle& out)

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
/// <param name="out">An array of created predictor handles. The array has to be large
///  enough to keep `num_threads` predictors.</param>
/// <returns>0 when success, -1 when failure.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXPredCreateMultiThread(string symbol_json_str, IntPtr param_bytes, int param_size, int dev_type, int dev_id, uint32 num_input_nodes, string[] input_keys, uint32[] input_shape_indptr, uint32[] input_shape_data, int num_threads, [<Out>] PredictorHandle& out)

/// <summary>Change the input shape of an existing predictor.</summary>
/// <param name="num_input_nodes">Number of input nodes to the net,
///   For feedforward net, this is 1.</param>
/// <param name="input_keys">The name of input argument.
///   For feedforward net, this is {"data"}</param>
/// <param name="input_shape_indptr">Index pointer of shapes of each input node.
///   The length of this array = num_input_nodes + 1.
///   For feedforward net that takes 4 dimensional input, this is {0, 4}.</param>
/// <param name="input_shape_data">A flattened data of shapes of each input node.
///   For feedforward net that takes 4 dimensional input, this is the shape data.</param>
/// <param name="handle">The original predictor handle.</param>
/// <param name="out">The reshaped predictor handle.</param>
/// <returns>0 when success, -1 when failure.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXPredReshape(uint32 num_input_nodes, string[] input_keys, uint32[] input_shape_indptr, uint32[] input_shape_data, PredictorHandle handle, [<Out>] PredictorHandle& out)

/// <summary>Get the shape of output node.
/// The returned shape_data and shape_ndim is only valid before next call to MXPred function.</summary>
/// <param name="handle">The handle of the predictor.</param>
/// <param name="index">The index of output node, set to 0 if there is only one output.</param>
/// <param name="shape_data">Used to hold pointer to the shape data</param>
/// <param name="shape_ndim">Used to hold shape dimension.</param>
/// <returns>0 when success, -1 when failure.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXPredGetOutputShape(PredictorHandle handle, uint32 index, [<Out>] IntPtr& shape_data, [<Out>] uint32& shape_ndim)

/// <summary>Get the dtype of output node.
///The returned data type is only valid before next call to MXPred function.</summary>
/// <param name="handle">The handle of the predictor.</param>
/// <param name="out_index">The index of the output node, set to 0 if there is only one output.</param>
/// <param name="out_dtype">The dtype of the output node</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXPredGetOutputType(PredictorHandle handle, uint32 out_index, [<Out>] int& out_dtype)

/// <summary>Set the input data of predictor.</summary>
/// <param name="handle">The predictor handle.</param>
/// <param name="key">The name of input node to set.
///    For feedforward net, this is "data".</param>
/// <param name="data">The pointer to the data to be set, with the shape specified in MXPredCreate.</param>
/// <param name="size">The size of data array, used for safety check.</param>
/// <returns>0 when success, -1 when failure.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXPredSetInput(PredictorHandle handle, string key, float32[] data, uint32 size)

/// <summary>Run a forward pass to get the output.</summary>
/// <param name="handle">The handle of the predictor.</param>
/// <returns>0 when success, -1 when failure.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXPredForward(PredictorHandle handle)

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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXPredPartialForward(PredictorHandle handle, int step, int[] step_left)

/// <summary>Get the output value of prediction.</summary>
/// <param name="handle">The handle of the predictor.</param>
/// <param name="index">The index of output node, set to 0 if there is only one output.</param>
/// <param name="data">User allocated data to hold the output.</param>
/// <param name="size">The size of data array, used for safe checking.</param>
/// <returns>0 when success, -1 when failure.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXPredGetOutput(PredictorHandle handle, uint32 index, IntPtr data, uint32 size)

/// <summary>Free a predictor handle.</summary>
/// <param name="handle">The handle of the predictor.</param>
/// <returns>0 when success, -1 when failure.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXPredFree(PredictorHandle handle)

/// <summary>Create a NDArray List by loading from ndarray file.
///    This can be used to load mean image file.</summary>
/// <param name="nd_file_bytes">The byte contents of nd file to be loaded.</param>
/// <param name="nd_file_size">The size of the nd file to be loaded.</param>
/// <param name="out">The output NDListHandle</param>
/// <param name="out_length">Length of the list.</param>
/// <returns>0 when success, -1 when failure.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXNDListCreate(string nd_file_bytes, int nd_file_size, [<Out>] NDListHandle& out, [<Out>] uint32& out_length)

/// <summary>Get an element from list</summary>
/// <param name="handle">The handle to the NDArray</param>
/// <param name="index">The index in the list</param>
/// <param name="out_key">The output key of the item</param>
/// <param name="out_data">The data region of the item</param>
/// <param name="out_shape">The shape of the item.</param>
/// <param name="out_ndim">The number of dimension in the shape.</param>
/// <returns>0 when success, -1 when failure.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXNDListGet(NDListHandle handle, uint32 index, [<Out>] IntPtr& out_key, [<Out>] IntPtr& out_data, [<Out>] IntPtr& out_shape, [<Out>] uint32& out_ndim)

/// <summary>set a call back to notify the completion of operation and allow for
///additional monitoring</summary>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXPredSetMonitorCallback(PredictorHandle handle, PredMonitorCallback callback, IntPtr callback_handle, bool monitor_all)

/// <summary>Free a MXAPINDList</summary>
/// <param name="handle">The handle of the MXAPINDList.</param>
/// <returns>0 when success, -1 when failure.</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int MXNDListFree(NDListHandle handle)


