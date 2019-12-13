namespace MXNetSharp

open MXNetSharp.Interop

module Autograd = 
    let internal lck = obj()
    let isRecording() = MXAutograd.isRecording()
    let isTraining() = MXAutograd.isTraining()
    let setIsRecording recording = MXAutograd.setIsRecording recording
    let setIsTraining training = MXAutograd.setIsTraining training
    let pause f = 
        lock lck
            (fun _ ->
                MXAutograd.setIsRecording false |> ignore
                let result = f()
                MXAutograd.setIsRecording true |> ignore
                result
            )
    let record f = 
        lock lck
            (fun _ ->
                MXAutograd.setIsRecording true |> ignore
                let result = f()
                MXAutograd.setIsRecording false |> ignore
                result
            )
    let symbol (ndarray : NDArray) = 
        let rec makeSymbol handle = 
            let s = 
                {new Symbol() with 
                    override x.Initialize() = ()
                    override x.Copy() = handle |> MXSymbol.copy |> makeSymbol
                }
            s.InternalHandle <- Some (new SafeSymbolHandle(handle,true))
            s
        MXAutograd.getSymbol ndarray.UnsafeHandle
        |> makeSymbol
    let computeGradient (ndarrays : NDArray seq) = 
        ndarrays
        |> Seq.map (fun x -> x.UnsafeHandle)
        |> Seq.toArray 
        |> MXAutograd.computeGradient 

    let markVariables vars = 
        let a,req,g = vars |> Seq.toArray |> Array.unzip3
        let a = a |> Array.map (fun (a : NDArray) -> a.UnsafeHandle)
        let req = req |> Array.map (fun (x : OpReqType) -> x.OpReqTypeInt |> uint32)
        let g = g |> Array.map (fun (g : NDArray) -> g.UnsafeHandle)
        MXAutograd.markVariables a req g

