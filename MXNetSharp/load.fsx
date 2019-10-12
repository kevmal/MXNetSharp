
#load "capi.fs"
#load "cpredictapi.fs"
#load "cnnvmapi.fs"
#load "interop.fs"
#load "AtomicSymbolMap.fs"
#load "symbol.fs"
#load "ndarray.fs"
#load "executor.fs"
#load "operators.fs"

type Bleh() = 
    member x.Yield(a) = [a]
    member x.Combine(a,b) = a@b
let b = Bleh()


