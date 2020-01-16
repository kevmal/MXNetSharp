// .NET core
(*
#I @"bin/Debug/netcoreapp3.0"
// Libraries in bin/Debug/netcoreapp3.0/runtimes/RID/native (where RID is the appropriate runtime id) need to be accessible
// The following works for windows however setting LD_LIBRARY_PATH at runtime in 'dotnet fsi' does not seem to work
// on linux based systems. LD_LIBRARY_PATH can be set prior to running 'dotnet fsi' or the required native library
// should be copied next to the Avalonia binaries.

open System.IO
let path = Environment.GetEnvironmentVariable("PATH")
let libsearch = 
    Directory.EnumerateDirectories(Path.Combine(__SOURCE_DIRECTORY__, "bin", "Debug", "netcoreapp3.0", "runtimes"))
    |> Seq.map (fun d -> Path.Combine(d,"native"))
    |> String.concat (string Path.PathSeparator)
Environment.SetEnvironmentVariable("PATH", libsearch + string Path.PathSeparator + path)
*)
////
// .NET Framework
#I @"bin/Debug/net48"
////


#r "System.Reactive.dll" 
#r "Avalonia.Animation.dll" 
#r "Avalonia.Base.dll"
#r "Avalonia.Controls.dll"
#r "Avalonia.DesignerSupport.dll"
#r "Avalonia.Desktop.dll"
#r "Avalonia.Diagnostics.dll"
#r "Avalonia.Direct2D1.dll"
#r "Avalonia.dll"
#r "Avalonia.Input.dll"
#r "Avalonia.Interactivity.dll"
#r "Avalonia.Layout.dll"
#r "Avalonia.Logging.Serilog.dll"
#r "Avalonia.Markup.dll"
#r "Avalonia.Markup.Xaml.dll"
#r "Avalonia.Native.dll"
#r "Avalonia.OpenGL.dll"
#r "Avalonia.Remote.Protocol.dll"
#r "Avalonia.Skia.dll"
#r "Avalonia.Styling.dll"
#r "Avalonia.Themes.Default.dll"
#r "Avalonia.Visuals.dll"
#r "Avalonia.Win32.dll"
#r "Avalonia.X11.dll"
#r "Examples.dll"
#r @"../MXNetSharp/bin/Debug/netstandard2.0/MXNetSharp.dll"

#r "Avalonia.DesktopRuntime.dll"

open System
open System.Threading
open FSIApp
open Avalonia
open Avalonia.Threading

let createApp() = 
    let ctSource = new CancellationTokenSource()
    async {
        let app = AppBuilder.Configure<App>().UsePlatformDetect().SetupWithoutStarting()
        let application = app.Instance
        application.Run(ctSource.Token)
    } |> Async.Start

createApp()

module UI = 
    let uido f = Dispatcher.UIThread.Post(Action(fun () -> f()),DispatcherPriority.Send)
    let invoke f = 
        let mutable a = Unchecked.defaultof<'a>
        let e = new ManualResetEventSlim()
        uido 
            (fun () -> 
                a <- f()
                e.Set()
            )
        async {
            let! _ = Async.AwaitWaitHandle e.WaitHandle
            e.Dispose()
            return a
        }
    let ui f = invoke f |> Async.RunSynchronously

