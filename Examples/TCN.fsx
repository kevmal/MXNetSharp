// ********************** Incomplete "Example" *****************************

// Adapted from https://github.com/locuslab/TCN

open System.Collections.Generic
open System.Runtime.InteropServices

#load "load.fsx"
open MXNetSharp
open MXNetSharp.Interop
open MXNetSharp.IO
open System
open System.Net
open System.IO
open System.IO.Compression

open MXNetSharp.PrimitiveOperators
open MXNetSharp.Interop.CApi
    

let withName name (symbol : #Symbol) = symbol.Name <- name; symbol

let wnorm name = 
    let g = new Variable(name + "_g")
    let v = new Variable(name + "_v")
    let w = (g / sqrt(new Sum(new Square(v)))) * v |> withName (name + "_w")
    w :> Symbol
    
let temporalBlock name inCount outputCount kernelSize stride dilation padding dropout (x : Symbol) = 
    let conv1 = new Convolution(x, 
                                wnorm (name + "_conv1_weight"),
                                wnorm (name + "_conv1_bias"),
                                [kernelSize],  
                                outputCount, 
                                stride = [stride], 
                                dilate = [dilation], 
                                pad = [padding],  
                                noBias = false,
                                Name = name + "_conv1")
    let conv1Sliced = new Slice(conv1, [None; None; None], [None; None; Some -padding])
    let relu1 = new Relu(conv1Sliced, Name = name + "_relu1")
    let dropout1 = new Dropout(relu1, dropout, DropoutMode.Training)
    let conv2 = new Convolution(dropout1,
                                wnorm (name + "_conv2_weight"),
                                wnorm (name + "_conv2_bias"),
                                [kernelSize],  
                                outputCount, 
                                stride = [stride], 
                                dilate = [dilation], 
                                pad = [padding],  
                                noBias = false,
                                Name = name + "_conv2")
    let conv2Sliced = new Slice(conv2, [None; None; None], [None; None; Some -padding])
    let relu2 = new Relu(conv2Sliced, Name = name + "_relu2")
    let dropout2 = new Dropout(relu2, dropout, DropoutMode.Training)
    let final = 
        if inCount <> outputCount then 
            new Convolution(data = dropout2, numFilter = outputCount, kernel = [1], Name = name + "_downsample") :> Symbol
        else 
            dropout2 :> Symbol
    let relu = new Relu(final, Name = name + "_relu")
    relu :> Symbol

let make numInputs outCount numChannels kernelSize dropout = 
    let x = new Variable("xIn")
    ((0,numInputs,x :> Symbol),numChannels)
    ||> Seq.fold
        (fun (i, lastN, last : Symbol) outCount ->
            let d = pown 2 i
            let padding = (kernelSize - 1) * d
            let y = temporalBlock (sprintf "L%d" i) lastN outCount kernelSize 1 d padding dropout last
            i + 1, outCount, y
        )
    |> (fun (_,_,x) -> 
        new FullyConnected(data = x, numHidden = outCount, Name = "final_fc")
    )

let tcn = make 1 10 (Array.create 8 30) 7 0.00

let batchSize = 32
let seqLength = 400



    


