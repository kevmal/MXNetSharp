namespace MXNETSharp.Tests.Interop

open MXNetSharp.Interop
open Xunit

module MXLib =
    [<Fact>]
    let ``basic MXLib calls``() =
        // Mainly looking for no exceptions
        Assert.NotEqual(0, MXLib.getVersion())
        Assert.NotEqual(-1, MXLib.getGpuCount())
        let features = MXLib.infoFeatures()
        Assert.NotEqual(0, features.Length)
        MXLib.randomSeed(32432)
        let opNames = MXLib.listAllOpNames()
        Assert.NotEmpty opNames

    [<Fact>]
    let ``functions``() = 
        let fs = MXLib.listFunctions()
        Assert.NotEmpty(fs)
        for h in fs do 
            let info = MXLib.funcGetInfo h
            Assert.NotEqual(0, info.Name.Length)
            for arg in info.Arguments do 
                Assert.NotEqual(0, arg.Name.Length)
                Assert.NotEqual(0, arg.TypeInfo.Length)
            
    [<Fact>]
    let ``atomic symbols``() = 
        let xs = MXSymbol.listAtomicSymbolCreators()
        Assert.NotEmpty(xs)
        for h in xs do 
            let name = MXSymbol.getAtomicSymbolName h
            let info = MXSymbol.getAtomicSymbolInfo h
            Assert.Equal(info.Name, name)
            for arg in info.Arguments do 
                Assert.NotEqual(0, arg.Name.Length)
                Assert.NotEqual(0, arg.TypeInfo.Length)
                
            







