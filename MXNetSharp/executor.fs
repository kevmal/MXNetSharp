namespace MXNetSharp
open System
open System.Runtime.InteropServices
open MXNetSharp.Interop
 
 
type Executor(handle : CApi.ExecutorHandle) =   
    new(symbol : Symbol, context, inArgs, argGrad, gradReqType, auxStates) = 
        let inArgs = inArgs |> Seq.map (fun (x : NDArray) -> x.NDArrayHandle.UnsafeHandle) |> Seq.toArray
        let argGrad = argGrad |> Seq.map (fun (x : NDArray) -> x.NDArrayHandle.UnsafeHandle) |> Seq.toArray
        let gradReqType = gradReqType |> Seq.map (fun (x : OpReqType) -> uint32 x) |> Seq.toArray
        let h = MXExecutor.bindEX symbol.SymbolHandle (int context.DeviceType) context.DeviceId null null null inArgs argGrad gradReqType auxStates 0n
        Executor(h)
    member x.Forward(isTraining : bool) = 
        let isTrain = if isTraining then 1 else 0
        MXExecutor.forward handle isTrain
        MXExecutor.outputs handle
        |> Array.map NDArray
    member x.Backward() = 
        MXExecutor.backward handle null



