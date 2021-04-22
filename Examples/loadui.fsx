﻿#I @"bin/Debug/net5.0"


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

