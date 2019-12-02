module MXNetSharp.LibFeature

open MXNetSharp.Interop

let internal fs = lazy MXLib.infoFeatures()
let lookup = fs.Value |> Array.map (fun x -> x.Name, x.Enabled) |> dict

let isEnabled name = 
    let scc,v = lookup.TryGetValue(name)
    scc && v

let CUDNN = isEnabled "CUDNN"
let NCCL = isEnabled "NCCL"
let CUDA_RTC = isEnabled "CUDA_RTC"
let TENSORRT = isEnabled "TENSORRT"
let CPU_SSE = isEnabled "CPU_SSE"
let CPU_SSE2 = isEnabled "CPU_SSE2"
let CPU_SSE3 = isEnabled "CPU_SSE3"
let CPU_SSE4_1 = isEnabled "CPU_SSE4_1"
let CPU_SSE4_2 = isEnabled "CPU_SSE4_2"
let CPU_SSE4A = isEnabled "CPU_SSE4A"
let CPU_AVX = isEnabled "CPU_AVX"
let CPU_AVX2 = isEnabled "CPU_AVX2"
let OPENMP = isEnabled "OPENMP"
let SSE = isEnabled "SSE"
let F16C = isEnabled "F16C"
let JEMALLOC = isEnabled "JEMALLOC"
let BLAS_OPEN = isEnabled "BLAS_OPEN"
let BLAS_ATLAS = isEnabled "BLAS_ATLAS"
let BLAS_MKL = isEnabled "BLAS_MKL"
let BLAS_APPLE = isEnabled "BLAS_APPLE"
let LAPACK = isEnabled "LAPACK"
let MKLDNN = isEnabled "MKLDNN"
let OPENCV = isEnabled "OPENCV"
let CAFFE = isEnabled "CAFFE"
let PROFILER = isEnabled "PROFILER"
let DIST_KVSTORE = isEnabled "DIST_KVSTORE"
let CXX14 = isEnabled "CXX14"
let INT64_TENSOR_SIZE = isEnabled "INT64_TENSOR_SIZE"
let SIGNAL_HANDLER = isEnabled "SIGNAL_HANDLER"
let DEBUG = isEnabled "DEBUG"
let TVM_OP = isEnabled "TVM_OP"


