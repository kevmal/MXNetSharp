#### MASTER
* Issue #20: Added Constant:>Variable type
* Issue #19: Empty NDArray error on ToString
* Issue #18: Add NDArray.CopyFrom(_:NDArray) method
* Issue #17: Add NDArray index setter
* Additional Context.CopyFrom convenience methods
* Issue #22: NDArray.MutFull method fixed
* Symbol.OutputCount property
* Symbol.Reshape method
* Compose op .>> now flows through Symbol.Outputs.[index]
* Symbol.Bind now finds Parameter types and uses them as default bindings
* Issue #25: opening PrimitiveOperators breaks ** operator
* Executor indexer on Variable for NDArray
* Bindings.mapNDArray now named Bindings.fillNDArray
* Symbol.Bind(ctx) will call inferShapes on symbol before creating Executor
* Run-Time Compilation (RTC) support for custom CUDA kernels in C/C++

#### 0.0.2 (2019-12-13)
* OpOutCount.txt and OpOutCountRemainder.txt are no longer packaged
* Fix num_var_arg to include count of all Symbol arguments (not just the vararg param) in SymbolOperator
* Fix MXSymbol.cutSubgraph return type
* Issue #8: fix MXFuncGetInfo parameter types causing mem error
* Issue #5: Add ToString override for SymbolOperator
* Autograd.pause and thread lock on Autograd.record
* NDArray.AsType instance method
* Use provided NDArray shape for parameter default shape
* Executor.BindMap renamed to Executor.Bindings
* Issue #12: Executor outputs need to be updated
* Issue #5: Float16 support
* NDArray.CopyFrom supports multidimensional .net Array type

#### 0.0.1 (2019-12-03)
* Move generated operators to MX and generated Symbol types to SymbolOperators
* Type conversion on NDArray.ToArray as well as specific ToArray methods (ie ToFloat32Array)
* NDArray.ToScalar (NDArray.ToFloat32Scalar) methods
* Basic indexing for NDArray and Symbol
* Cleaned up generated Argument types. DType arguments were consolidated where only one/two options were missing. For example, if all dtypes but uint8 are valid then it just uses DataType regardless.
* (.>>) and (.|>) operators for Symbol composition
* Context extension methods for creating NDArrays on a context (zeros, ones, linspace, ...)
* Fix ctx arg type on generated NDArray ops (was previously a string)
* Context is not optional for NDArray operators that have it as an arg
* NDArray initializing constructors were removed in favor of NDArray.CopyFrom static methods
* BindMap now Bindings and ArgBinding is now Bind. Additional functions added to the corresponding modules.
* Feature struct fix (incorrectly reporting "enabled" flag on MXLib features) with LibFeature module


#### 0.0.1-alpha (2019-11-07)
* Generated types for MXNet symbol api in MXNetSharp namespace
* Generated static methods for both NDArray and Symbol operators in MXNetSharp.Operators namespace
* Symbol, NDArray, Autograd, Executor and more low level interop in MXNetSharp.Interop.CApi 
* Low level F# friendly interop in MXNetSharp.Interop with modules
  - MXLib
  - MXSymbol
  - MXNDArray
  - MXExecutor
  - MXNDList
  - NNVM
  - NNSymbol
  - NNGraph
  - MXDataIter
  - MXKVStore
  - MXAutograd
* MNIST and CSV data iterators
* Untested custom operator support in MXNetSharp.Operator

