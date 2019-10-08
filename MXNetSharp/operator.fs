namespace MXNetSharp.Interop
open MXNetSharp.Interop
open System.Collections.Generic
type Node = class end



type Operator(name : string, positionalInputs : Node seq, namedInputs) = 
    let positionalInputs = ResizeArray<Node>(positionalInputs)
    let namedInputs = Dictionary<string,Node>(dict namedInputs)
    new(name) = Operator(name,Array.empty,Array.empty)
    member x.SetParam(value : Node) = 
        positionalInputs.Add value
        x
    member x.SetInput(name, value) = 
        namedInputs.[name] <- value
        x
    member x.Invoke() = ()
        