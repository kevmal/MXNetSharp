module MXNetSharp.Interop.CNNVMApi

open MXNetSharp.Interop.CApi

open System
open System.Runtime.InteropServices
type size_t = int64
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
/// \file nnvm/c_api.h
/// <summary>C API of NNVM symbolic construction and pass.
/// Enables construction and transformation of Graph
/// in any other host languages.</summary>
/// <summary>NNVM_DLL prefix for windows</summary>
/// <summary>manually define unsigned int</summary>
/// <summary>handle to a function that takes param and creates symbol</summary>
type OpHandle = IntPtr
/// <summary>handle to Graph</summary>
type GraphHandle = IntPtr
/// <summary>Set the last error message needed by C API</summary>
/// <param name="msg">The error message to set.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern unit NNAPISetLastError(string msg)

/// <summary>return str message of the last error
/// all function in this file will return 0 when success
/// and -1 when an error occured,
/// NNGetLastError can be called to retrieve the error
///
/// this function is threadsafe and can be called by different thread</summary>
/// <returns>error info</returns>
/// <summary>list all the available operator names, include entries.</summary>
/// <param name="out_size">the size of returned array</param>
/// <param name="out_array">the output operator name array.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNListAllOpNames([<Out>] uint32& out_size, [<Out>] IntPtr& out_array)

/// <summary>Get operator handle given name.</summary>
/// <param name="op_name">The name of the operator.</param>
/// <param name="op_out">The returnning op handle.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNGetOpHandle(string op_name, [<Out>] OpHandle& op_out)

/// <summary>list all the available operators.
/// This won't include the alias, use ListAllNames
/// instead to get all alias names.</summary>
/// <param name="out_size">the size of returned array</param>
/// <param name="out_array">the output AtomicSymbolCreator array</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNListUniqueOps([<Out>] uint32& out_size, [<Out>] IntPtr& out_array)

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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNGetOpInfo(OpHandle op, [<Out>] IntPtr& real_name, [<Out>] IntPtr& description, [<Out>] uint32& num_doc_args, [<Out>] IntPtr& arg_names, [<Out>] IntPtr& arg_type_infos, [<Out>] IntPtr& arg_descriptions, [<Out>] IntPtr& return_type)

/// <summary>Create an AtomicSymbol functor.</summary>
/// <param name="op">The operator handle</param>
/// <param name="num_param">the number of parameters</param>
/// <param name="keys">the keys to the params</param>
/// <param name="vals">the vals of the params</param>
/// <param name="out">pointer to the created symbol handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolCreateAtomicSymbol(OpHandle op, uint32 num_param, string[] keys, string[] vals, [<Out>] SymbolHandle& out)

/// <summary>Create a Variable Symbol.</summary>
/// <param name="name">name of the variable</param>
/// <param name="out">pointer to the created symbol handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolCreateVariable(string name, [<Out>] SymbolHandle& out)

/// <summary>Create a Symbol by grouping list of symbols together</summary>
/// <param name="num_symbols">number of symbols to be grouped</param>
/// <param name="symbols">array of symbol handles</param>
/// <param name="out">pointer to the created symbol handle</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolCreateGroup(uint32 num_symbols, SymbolHandle[] symbols, [<Out>] SymbolHandle& out)

/// <summary>Add src_dep to the handle as control dep.</summary>
/// <param name="handle">The symbol to add dependency edges on.</param>
/// <param name="src_dep">the source handles.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNAddControlDeps(SymbolHandle handle, SymbolHandle src_dep)

/// <summary>Free the symbol handle.</summary>
/// <param name="symbol">the symbol</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolFree(SymbolHandle symbol)

/// <summary>Copy the symbol to another handle</summary>
/// <param name="symbol">the source symbol</param>
/// <param name="out">used to hold the result of copy</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolCopy(SymbolHandle symbol, [<Out>] SymbolHandle& out)

/// <summary>Print the content of symbol, used for debug.</summary>
/// <param name="symbol">the symbol</param>
/// <param name="out_str">pointer to hold the output string of the printing.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolPrint(SymbolHandle symbol, [<Out>] IntPtr& out)

/// <summary>Get string attribute from symbol</summary>
/// <param name="symbol">the source symbol</param>
/// <param name="key">The key of the symbol.</param>
/// <param name="out">The result attribute, can be NULL if the attribute do not exist.</param>
/// <param name="success">Whether the result is contained in out.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolGetAttr(SymbolHandle symbol, string key, [<Out>] IntPtr& out, [<Out>] int& success)

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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolSetAttrs(SymbolHandle symbol, uint32 num_param, string[] keys, string[] values)

/// <summary>Get all attributes from symbol, including all descendents.</summary>
/// <param name="symbol">the source symbol</param>
/// <param name="recursive_option">0 for recursive, 1 for shallow.</param>
/// <param name="out_size">The number of output attributes</param>
/// <param name="out">2*out_size strings representing key value pairs.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolListAttrs(SymbolHandle symbol, int recursive_option, [<Out>] uint32& out_size, [<Out>] IntPtr& out)

/// <summary>List inputs variables in the symbol.</summary>
/// <param name="symbol">the symbol</param>
/// <param name="option">The option to list the inputs
///  option=0 means list all arguments.
///  option=1 means list arguments that are readed only by the graph.
///  option=2 means list arguments that are mutated by the graph.</param>
/// <param name="out_size">output size</param>
/// <param name="out_sym_array">the output array.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolListInputVariables(SymbolHandle symbol, int option, [<Out>] uint32& out_size, [<Out>] IntPtr& out_sym_array)

/// <summary>List input names in the symbol.</summary>
/// <param name="symbol">the symbol</param>
/// <param name="option">The option to list the inputs
///  option=0 means list all arguments.
///  option=1 means list arguments that are readed only by the graph.
///  option=2 means list arguments that are mutated by the graph.</param>
/// <param name="out_size">output size</param>
/// <param name="out_str_array">pointer to hold the output string array</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolListInputNames(SymbolHandle symbol, int option, [<Out>] uint32& out_size, [<Out>] IntPtr& out_sym_array)

/// <summary>List returns names in the symbol.</summary>
/// <param name="symbol">the symbol</param>
/// <param name="out_size">output size</param>
/// <param name="out_str_array">pointer to hold the output string array</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolListOutputNames(SymbolHandle symbol, [<Out>] uint32& out_size, [<Out>] IntPtr& out_str_array)

/// <summary>Supply number of outputs of the symbol.</summary>
/// <param name="symbol">the symbol</param>
/// <param name="output_count">number of outputs</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolGetNumOutputs(SymbolHandle symbol, [<Out>] uint32& output_count)

/// <summary>Get a symbol that contains all the internals.</summary>
/// <param name="symbol">The symbol</param>
/// <param name="out">The output symbol whose outputs are all the internals.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolGetInternals(SymbolHandle symbol, [<Out>] SymbolHandle& out)

/// <summary>Get a symbol that contains only direct children.</summary>
/// <param name="symbol">The symbol</param>
/// <param name="out">The output symbol whose outputs are the direct children.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolGetChildren(SymbolHandle symbol, [<Out>] SymbolHandle& out)

/// <summary>Get index-th outputs of the symbol.</summary>
/// <param name="symbol">The symbol</param>
/// <param name="index">the Index of the output.</param>
/// <param name="out">The output symbol whose outputs are the index-th symbol.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolGetOutput(SymbolHandle symbol, uint32 index, [<Out>] SymbolHandle& out)

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
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNSymbolCompose(SymbolHandle sym, string name, uint32 num_args, string[] keys, SymbolHandle[] args)

// Graph IR API

/// <summary>create a graph handle from symbol</summary>
/// <param name="symbol">The symbol representing the graph.</param>
/// <param name="graph">The graph handle created.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNGraphCreate(SymbolHandle symbol, GraphHandle[] graph)

/// <summary>free the graph handle</summary>
/// <param name="handle">The handle to be freed.</param>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNGraphFree(GraphHandle handle)

/// <summary>Get a new symbol from the graph.</summary>
/// <param name="graph">The graph handle.</param>
/// <param name="symbol">The corresponding symbol</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNGraphGetSymbol(GraphHandle graph, SymbolHandle[] symbol)

/// <summary>Get Set a attribute in json format.
///This feature allows pass graph attributes back and forth in reasonable speed.</summary>
/// <param name="handle">The graph handle.</param>
/// <param name="key">The key to the attribute.</param>
/// <param name="json_value">The value need to be in format [type_name, value],
/// Where type_name is a registered type string in C++ side via DMLC_JSON_ENABLE_ANY.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNGraphSetJSONAttr(GraphHandle handle, string key, string json_value)

/// <summary>Get a serialized attrirbute from graph.
///This feature allows pass graph attributes back and forth in reasonable speed.</summary>
/// <param name="handle">The graph handle.</param>
/// <param name="key">The key to the attribute.</param>
/// <param name="json_out">The result attribute, can be NULL if the attribute do not exist.
/// The json_out is an array of [type_name, value].
/// Where the type_name is a registered type string in C++ side via DMLC_JSON_ENABLE_ANY.</param>
/// <param name="success">Whether the result is contained in out.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNGraphGetJSONAttr(GraphHandle handle, string key, [<Out>] IntPtr& json_out, [<Out>] int& success)

/// <summary>Set a attribute whose type is std::vector<NodeEntry> in c++
///This feature allows pass List of symbolic variables for gradient request.</summary>
/// <remarks>This is beta feature only used for test purpos</remarks>
/// <param name="handle">The graph handle.</param>
/// <param name="key">The key to the attribute.</param>
/// <param name="list">The symbol whose outputs represents the list of NodeEntry to be passed.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNGraphSetNodeEntryListAttr_(GraphHandle handle, string key, SymbolHandle list)

/// <summary>Apply passes on the src graph.</summary>
/// <param name="src">The source graph handle.</param>
/// <param name="num_pass">The number of pass to be applied.</param>
/// <param name="pass_names">The names of the pass.</param>
/// <param name="dst">The result graph.</param>
/// <returns>0 when success, -1 when failure happens</returns>
[<DllImport(MXNETLIB, CallingConvention = CallingConvention.Cdecl)>]
extern int NNGraphApplyPasses(GraphHandle src, uint32 num_pass, string[] pass_names, [<Out>] GraphHandle& dst)

