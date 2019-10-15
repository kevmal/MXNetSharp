namespace MXNetSharp


type Operator(name) =   
    member x.Name = name

type OperatorArgument<'a> = 
    {
        Name : string
        Key : string
        TypeString : string
        Description : string
        DefaultValue : 'a option
    }

    

type RandomUniform private () = 
    static let valueString = string
    //inherit Operator("RandomUniform")
    static let creator = AtomicSymbolCreator.FromName "_random_uniform"
    static let keys = [|"low"; "high"; "shape"; "context"; "dtype"|]
    static let defaultValueStrings = [|"0"; "1"; "None"; ""; "None"|]
    static member Low = 
        {
            Name = "Low"
            Key = "low"
            TypeString = "float, optional, default=0"
            Description = "Lower bound of the distribution."
            DefaultValue = Some 0
        }
    static member GetValueArray(?low,?high,?shape,?context,?dtype) = 
        let values = Array.copy defaultValueStrings
        if low.IsSome then values.[0] <- valueString low
        if high.IsSome then values.[1] <- valueString high
        if shape.IsSome then values.[1] <- valueString shape
        values

