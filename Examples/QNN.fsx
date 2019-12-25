open System.Collections.Concurrent

// Conditional Generative Adversarial Nets (https://arxiv.org/abs/1411.1784) Mehdi Mirza, Simon Osindero
// Adapted from https://github.com/eriklindernoren/Keras-GAN/tree/master/cgan


#load "loadui.fsx"
open Loadui

open MXNetSharp
open MXNetSharp.Interop
open MXNetSharp.IO
open System
open System.Net
open System.IO
open System.Collections.Generic
open System.IO.Compression
open Microsoft.FSharp.NativeInterop

open MXNetSharp.PrimitiveOperators
open MXNetSharp.SymbolOperators


open Avalonia
open Avalonia.Controls
open Avalonia.Media.Imaging

let rng = Random(2323)

let boardWidth = 16
let boardHeight = 16

let obstableProb = 0.0
let rewardProb = 0.3
let initSpeed = 300.0
let maxSpeed = 200.0
let movesPerUpdate = 4


type GameBoard = 
    {
        Player : int
        Obstacles : (int*int) list
        Rewards : (int*int) list
        Score : int
        Missed : int
        Speed : double
        UpdateTime : DateTime
    }

type Move = 
    | Left
    | Right
    | Stay

let gameStep (s : GameBoard) (now : DateTime) getmove  = 
    let player = 
        match getmove s with 
        | Left -> s.Player - 1 |> max 0
        | Right -> s.Player + 1 |> min (boardWidth - 1)
        | Stay -> s.Player
    let update = now.Ticks >= s.UpdateTime.Ticks
    let obstacles =
        if update then 
            let o = 
                if rng.NextDouble() < obstableProb then 
                    (rng.Next(boardWidth), boardHeight) :: s.Obstacles
                else 
                    s.Obstacles
            o |> List.map (fun (x,y) -> (x, y - 1)) |> List.filter (fun (x,y) -> y >= 0)
        else
            s.Obstacles
    let rewards,missed =
        if update then 
            let o = 
                if rng.NextDouble() < rewardProb then 
                    (rng.Next(boardWidth), boardHeight) :: s.Rewards
                else 
                    s.Rewards
            let missed = o |> List.map (fun (x,y) -> (x, y - 1)) |> List.filter (fun (x,y) -> y < 0) |> List.length
            let o = o |> List.map (fun (x,y) -> (x, y - 1)) |> List.filter (fun (x,y) -> y >= 0)
            o, missed + s.Missed
        else
            s.Rewards,s.Missed
    let hitObs = obstacles |> List.filter (fun (x,y) -> y = 0)|> List.exists (fun (x,y) -> x = player)
    if hitObs then 
        //if s.Score - 100 < 0 then 
        None
        //else 
        //    let s = 
        //        { 
        //            Player = player 
        //            Obstacles = obstacles |> List.filter (fun (x,y) -> y = 0)
        //            Rewards = rewards |> List.filter (fun (x,y) -> y = 0) 
        //            Score = s.Score - 100
        //            Speed = if update then max (s.Speed*0.95) maxSpeed else s.Speed
        //            UpdateTime = if update then now.AddMilliseconds (double s.Speed) else s.UpdateTime
        //            Missed = missed
        //        }
        //    Some s
    else
        let hitReward = rewards |> List.filter (fun (x,y) -> y = 0) |> List.exists (fun (x,y) -> x = player)
        let s = 
            { 
                Player = player 
                Obstacles = obstacles
                Rewards = if hitReward then rewards |> List.filter (fun (x,y) -> y <> 0 || x <> player) else rewards
                Score = s.Score + (if hitReward then 25 else 0) //  + (if update then 1 else 0) 
                Speed = if update then max (s.Speed*0.95) maxSpeed else s.Speed
                UpdateTime = if update then now.AddMilliseconds (double s.Speed) else s.UpdateTime
                Missed = missed
            }
        if missed < 10 then 
            Some s
        else
            None
        //Some s

let rec gameLoop s getmove reset writeBoard =
    writeBoard s
    match gameStep s DateTime.Now getmove with 
    | None -> gameLoop (reset s) getmove reset writeBoard
    | Some s ->gameLoop s getmove reset writeBoard

let initialBoard = 
    {
        Player = boardWidth / 2
        Obstacles = []
        Rewards = []
        Score = 0
        Speed = initSpeed
        UpdateTime = DateTime.MinValue
        Missed = 0
    }

let reset _ = initialBoard
open System

let bitmap = new WriteableBitmap(PixelSize(boardWidth,boardHeight),Vector(90.,90.), Nullable(Platform.PixelFormat.Bgra8888))
   
let moveq = ConcurrentQueue()
let mutable currentBoard = initialBoard
let wnd = 
    UI.ui (fun () ->
        let f = Window()
        f.KeyDown
        |> Observable.add
            (fun k ->
                match k.Key with 
                | Input.Key.Left -> moveq.Enqueue Left
                | Input.Key.Right -> moveq.Enqueue Right
                | _ -> ()
            )
        let p = Image()
        p.Source <- bitmap
        f.Content <- p
        f.Show()
        {|
            Window = f
            Image = p
        |}
    )
         
let updateBitmap (s : GameBoard) = 
    currentBoard <- s
    use fb = bitmap.Lock()
    let ptr =  fb.Address |> NativePtr.ofNativeInt<uint32>
    let put r g b x y = 
        let y = boardHeight - y - 1
        let r = 255.0*r |> round |> int |> max 0 |> min 255 |> uint32
        let g = 255.0*g |> round |> int |> max 0 |> min 255 |> uint32
        let b = 255.0*b |> round |> int |> max 0 |> min 255 |> uint32
        let pixel = (r <<< 16) ||| (g <<< 8) ||| b  ||| (0xFFu <<< 24)
        let ix = y*(fb.RowBytes / 4) + x
        NativePtr.set ptr ix pixel
    for x = 0 to boardWidth - 1 do
        for y = 0 to boardHeight - 1 do 
            put 1.0 1.0 1.0 x y
    put 0.0 0.0 0.6 s.Player 0
    for (x,y) in s.Rewards do 
        put 0.0 0.5 0.0 x y 
    for (x,y) in s.Obstacles do 
        put 0.5 0.0 0.0 x y 
    UI.uido 
        (fun () ->
            wnd.Window.Title <- sprintf "Score: %10d Missed: %10d" s.Score s.Missed
            wnd.Image.InvalidateVisual()
        )



type Sample = 
    {
        State : GameBoard
        Action : int
        Reward : double 
        NextState : GameBoard option
    }
let memory = ResizeArray<Sample>()
let memoryLength = 10000

let context = GPU 0
let batchSz = 32
let maxEp = 1.0
let minEp = 0.01
let lambda = 0.00006 //speed of decay
let gamma = 0.95
let actionCount = 3
let updateTargetStepCount = 3000


let mutable ep = maxEp

let mainRand = Random(231)
let mutable steps = 0
let mutable lasti = 0
let observe s = 
    if memory.Count >= memoryLength then
        lasti <- (lasti + 1) % memory.Count
        memory.[lasti] <- s
    else
        lasti <- memory.Count
        memory.Add(s)

    steps <- steps + 1
    ep <- minEp + (maxEp - minEp)*(exp (-lambda*(double steps)))

let o = Input("state", [0; 3; boardWidth; boardHeight])
let t = Input("qa", [0; actionCount])
let qmodel = 
    o 
    .>> Convolution(numFilter = 32, kernel = [3;3], stride = [1;1], pad = [1;1]) .>> Relu()
    .>> Convolution(numFilter = 32, kernel = [3;3], stride = [2;2], pad = [1;1]) .>> Relu()
    .>> Convolution(numFilter = 32, kernel = [3;3], stride = [1;1], pad = [1;1]) .>> Relu()
    //.>> Reshape(shape = [-1;64*7*7])
    //.>> Flatten()
    //.>> Convolution(numFilter = 32, kernel = [3;3], stride = [1;1]) .>> Relu()
    //.>> Convolution(numFilter = 1, kernel = [3;3], stride = [1;1]) .>> Relu()
    //.>> Convolution(numFilter = 32, kernel = [4;4], stride = [2;2]) .>> Relu()
    //.>> Convolution(numFilter = 1, kernel = [4;4], stride = [2;2]) .>> Relu()
    //.>> Convolution(numFilter = 64, kernel = [3;3], stride = [1;1]) .>> Relu()
    //.>> FullyConnected(1024) .>> Relu()
    .>> FullyConnected(20, flatten = true) .>> Relu()
    //.>> FullyConnected(32) .>> Relu()
    //.>> FullyConnected(32) .>> Relu()
    .>> FullyConnected(actionCount)

let loss = (qmodel - t) .>> Square() .>> Mean() .>> MakeLoss()


Bindings.inputs [o] |> Bindings.inferShapes qmodel |> Seq.last

let bindings = 
    Bindings.inputs [o] 
    |> Bindings.inferShapes qmodel

let evalBindings = 
    bindings 
    |> Bindings.batchSize 1
    //|> Bindings.init (fun a shape ->  MX.RandomUniformNDArray(context, -0.1, 0.1, shape)
    |> Bindings.init 
        (fun a shape ->  
            let fanin = 
                if shape.Length = 3 then    
                    double a.Shape.Value.[1]*double a.Shape.Value.[2]
                elif shape.Length = 1 then 
                    double a.Shape.Value.[0]
                else
                    double a.Shape.Value.[1]
            let alpha = sqrt 5.0
            let gain = sqrt(2.0 / (1.0 + alpha*alpha))
            let stdv = sqrt(3.0) * (gain) / sqrt(fanin)
            MX.RandomUniformNDArray(context, -stdv, stdv, a.Shape.Value)
        )

let targetBindings = 
    bindings 
    |> Bindings.batchSize batchSz
    |> Bindings.init 
        (fun a shape ->  
            let scc,v = evalBindings.TryGetValue(a.Name)
            if scc && shape = v.Shape.Value then 
                v.NDArray.Value
            else
                context.Zeros shape
        )

let trainBindings = 
    bindings 
    |> Bindings.inferShapes loss
    |> Bindings.batchSize batchSz
    |> Bindings.init 
        (fun a shape ->  
            let scc,v = evalBindings.TryGetValue(a.Name)
            if scc && shape = v.Shape.Value then 
                v.NDArray.Value.CopyTo(context)
            else
                context.Zeros shape
        )


let runBindings = 
    bindings 
    |> Bindings.batchSize 1
    |> Bindings.init 
        (fun a shape ->  
            let scc,v = evalBindings.TryGetValue(a.Name)
            if scc && shape = v.Shape.Value then 
                v.NDArray.Value.CopyTo(CPU 0)
            else
                context.Zeros shape
        )

let copyToTarget() = 
    for a in targetBindings do 
        match a with 
        | ArgBinding(a) -> 
            let scc,b = trainBindings.TryGetValue(a.Name)
            if scc && a.Shape = b.Shape then 
                b.NDArray.Value.CopyTo(a.NDArray.Value)
        | _ -> ()


let playLck = obj()
let copyToRun() = 
    lock playLck
        (fun _ ->
            for a in runBindings do 
                match a with 
                | ArgBinding(a) -> 
                    let scc,b = trainBindings.TryGetValue(a.Name)
                    if scc && a.Shape = b.Shape then 
                        b.NDArray.Value.CopyTo(a.NDArray.Value)
                | _ -> ()
        )

let evalin = evalBindings.NDArray(o)
let evalBatchin = targetBindings.NDArray(o)
let trainin = trainBindings.NDArray(o)
let labelin = trainBindings.NDArray(t)

let execEval = qmodel.Bind(context, evalBindings)
let execBatch = qmodel.Bind(context, targetBindings)
let execTrain = loss.Bind(context, trainBindings)
let execRun = qmodel.Bind(CPU 0, runBindings)

let makeBatch (states : GameBoard []) = 
    let inp = Array.CreateInstance(typeof<float32>,states.Length, 3, boardWidth, boardHeight)
    let put (b : int) (x : int) (y : int) (i : int) = 
        let sz = boardWidth*boardHeight
        inp.SetValue(1.f, b, i, x, y)
        //inp.[b * states.Length + i * sz + y * boardWidth + x] <- 1.f
    for i = 0 to states.Length - 1 do
        let s = states.[i]
        put i s.Player 0 0
        for (x,y) in s.Rewards do 
            put i x y 1
        for (x,y) in s.Obstacles do 
            put i x y 2
    inp
    

let act (s : GameBoard) = 
    if mainRand.NextDouble() < ep then 
        MX.Shuffle(MX.ArangeNDArray(start = 0.0, stop = double actionCount, ctx = CPU 0)).ToIntArray()
    else
        let inp = Array.CreateInstance(typeof<float32>, 1, 3, boardWidth, boardHeight)
        let put (x : int) (y : int) (i : int) = 
            let sz = boardWidth*boardHeight
            inp.SetValue(1.f, 0, i, x, y)
            //inp.[i * sz + y * boardWidth + x] <- 1.f
        put s.Player 0 0
        for (x,y) in s.Rewards do 
            put x y 1
        for (x,y) in s.Obstacles do 
            put x y 2
        evalin.CopyFrom(inp)
        execEval.Forward(false)
        //MX.Argmax(execEval.Outputs.[0], axis = 1).ToFloat32Scalar() |> int
        MX.Argsort(execEval.Outputs.[0], axis = 1, isAscend=false).ToIntArray()

let mutable lr = 0.004

let optUpdate (e : Executor) = 
    //let lr = 0.001
    let beta1 = 0.9
    let beta2 = 0.999
    let mutable updateCount = 0
    let lu = 
        let d = Dictionary<string, NDArray*NDArray>()
        fun (s : String) (a : NDArray) ->
            let scc,v = d.TryGetValue(s)
            if scc then 
                v
            else
                let v = MX.ZerosLike(a),MX.ZerosLike(a)
                d.[s] <- v
                v
    fun () -> 
        updateCount <- updateCount + 1
        let t = double updateCount
        let lr = lr*sqrt(1.0 - Math.Pow(beta2,t)) / (1.0 - Math.Pow(beta1,t))
        e.Bindings
        |> Seq.iter
            (fun a ->
                match a with 
                | ArgBinding ({Name = name; OpReqType = Some WriteTo; Grad = Some grad; NDArray = Some weight}) -> 
                    let m,v = lu name grad
                    MX.AdamUpdate([weight], weight, grad, m, v, lr, beta1, beta2)
                    //MX.SgdUpdate([weight], weight, grad, lr)
                | _ -> ()
            )
let opt = optUpdate execTrain

let mutable prevLoss = 0.0
let mutable lossCount = 0.0
let replay() = 
    if steps % updateTargetStepCount = 0 then 
        //printfn "Updating target..."
        copyToTarget()
        copyToRun()
        //()
    let batch = 
        if memory.Count = 0 then
            [||]
        else
            [|
                yield memory.[lasti]
                yield! Seq.init (batchSz - 1) (fun _ -> memory.[mainRand.Next(memory.Count)])
            |]
    let len = batch.Length
    let p = 
        let a = makeBatch (batch |> Array.map (fun x -> x.State))
        evalBatchin.CopyFrom a
        execBatch.Forward(false)
        execBatch.Outputs.[0].ToFloat32Array() |> Array.splitInto len
    let p_ = 
        let a = makeBatch (batch |> Array.map (fun x -> defaultArg x.NextState initialBoard))
        evalBatchin.CopyFrom a
        execBatch.Forward(false)
        execBatch.Outputs.[0].ToFloat32Array() |> Array.splitInto len
    let x = Array.zeroCreate len
    let y = Array.zeroCreate len
    for i = 0 to len - 1 do
        let b = batch.[i]
        let t = p.[i] |> Array.copy
        t.[b.Action] <- 
            match b.NextState with
            | Some _ -> b.Reward + gamma*(double(Array.max p_.[i])) |> float32
            | None -> b.Reward |> float32
        //printfn "%A -> %A" p.[i] t
        x.[i] <- b.State
        y.[i] <- t
    labelin.CopyFrom(y |> Array.concat)
    trainin.CopyFrom(makeBatch x)
    execTrain.Forward(true)
    prevLoss <- prevLoss + execTrain.Outputs.[0].ToScalar()
    lossCount <- lossCount + 1.0
    execTrain.Backward()
    opt()



let run() = 
    let rec loop frameMove (s : GameBoard) = 
        let actionInt = 
            let a = act s 
            let j = 
                if s.Player = 0 && a.[0] = 1 then 
                    1
                elif s.Player = (boardWidth - 1) && a.[0] = 2 then 
                    1
                else
                    0
            a.[j]
        let action, frameMove = 
            match actionInt with
            | _ when frameMove = movesPerUpdate -> Stay, 0
            | 0 -> Stay, 0
            | 1 -> Left, frameMove + 1
            | 2 -> Right, frameMove + 1
            | x -> failwithf "invalid action %d" x
        let s2 =
            match action with 
            | Stay -> 
                //gameStep {s with UpdateTime = DateTime.Now} DateTime.MinValue (fun _ -> action) |> Option.map (fun s2 -> {s2 with UpdateTime = DateTime.MinValue})
                gameStep {s with UpdateTime = DateTime.MinValue} DateTime.Now (fun _ -> action) |> Option.map (fun s2 -> {s2 with UpdateTime = DateTime.MinValue})
            | _ -> 
                gameStep {s with UpdateTime = DateTime.Now} DateTime.MinValue (fun _ -> action) |> Option.map (fun s2 -> {s2 with UpdateTime = DateTime.MinValue})
        let reward = 
            match s2 with 
            | None -> 0.0
            | Some s2 -> (s2.Score - s.Score |> double) / 25.0
        let reward = reward 
        observe 
            { State = s
              NextState = s2
              Action = actionInt
              Reward = reward }
        replay()
        match s2 with 
        | None -> s.Score |> double
        | Some s -> loop frameMove s
    loop 0 initialBoard


let mutable gameMode = 0
let game = 
    let evalin = runBindings.NDArray(o)
    let act (s : GameBoard) = 
        let inp = Array.CreateInstance(typeof<float32>,1,3,boardWidth,boardHeight)
        let put x y i = inp.SetValue(1.f,0,i,x,y)
        put s.Player 0 0
        for (x,y) in s.Rewards do 
            put x y 1
        for (x,y) in s.Obstacles do 
            put x y 2
        evalin.CopyFrom(inp)
        execRun.Forward(false)
        let moves = MX.Argsort(execRun.Outputs.[0], axis = 1, isAscend=false).ToIntArray()
        printfn "%A" moves
        moves.[0]
    let act x = lock playLck (fun _ -> act x)
    let mutable l = DateTime.MinValue
    let mutable m = 0
    let move(b : GameBoard) = 
        if m = movesPerUpdate then 
            Stay
        else
            m <- m + 1
            l <- b.UpdateTime
            match act b with
            | 0 -> 
                m <- movesPerUpdate
                Stay
            | 1 -> Left 
            | 2 -> Right
            | x -> failwithf "invalid action %d" x
    let getmove (s : GameBoard) = 
        if s.UpdateTime.Ticks <> l.Ticks then 
            l <- s.UpdateTime
            m <- 0
        if gameMode = 0 then 
            let scc,v = moveq.TryDequeue()
            if scc then 
                v
            else 
                Stay
        else
            move s
    async {
        gameLoop initialBoard getmove reset updateBitmap
    } |> Async.StartAsTask

let human() = 
    gameMode <- 0
    while moveq.Count > 0 do 
        let scc,_ = moveq.TryDequeue()
        ()
let comp() = 
    gameMode <- 1

let mutable epNum = 0
let mutable rsum = 0.0
let total_eps = 100000

let trainTask = 
    async {
        while epNum < total_eps do
            rsum <- rsum + run()
            epNum <- epNum + 1
            if epNum % 5 = 0 then
                printfn "%10f Steps %d , Episode: %d, Average reward for episode %f. Count %f Loss %f. LR: %f"  ep steps epNum (rsum / 5.0) lossCount (prevLoss/lossCount) lr
                lossCount <- 0.0
                prevLoss <- 0.0
                rsum <- 0.0
            //if epNum % 5 = 0 then 
           //     copyToTarget()
            //    copyToRun()
            if epNum % 100 = 0 && lr > 0.0005 then
                lr <- lr*0.75
            //if epNum % 50 = 0 then 
            //    printfn "Playing"
            //    for i = 0 to 5 do 
            //        printfn "%d" i
            //        play()
    } |> Async.StartAsTask


comp()
human()


targetBindings |> Seq.iter (fun a -> printfn "%-30s %A" a.Name a.NDArray.Value.UnsafeHandle)
trainBindings |> Seq.iter (fun a -> printfn "%-30s %A" a.Name a.NDArray.Value.UnsafeHandle)


trainBindings |> Seq.iter (fun a -> printfn "%-30s %A" a.Name ((a.NDArray.Value |> MX.Mean).ToDoubleScalar()))
evalBindings |> Seq.iter (fun a -> printfn "%-30s %A" a.Name ((a.NDArray.Value |> MX.Mean).ToDoubleScalar()))
runBindings |> Seq.iter (fun a -> printfn "%-30s %A" a.Name ((a.NDArray.Value |> MX.Mean).ToDoubleScalar()))



for a in targetBindings do 
    match a with 
    | ArgBinding(a) -> 
        let scc,b = trainBindings.TryGetValue(a.Name)
        printfn "%A" (scc, a.Shape, b.Shape)
        if scc && a.Shape = b.Shape then 
            printfn "copy"
            b.NDArray.Value.CopyTo(a.NDArray.Value)
    | _ -> ()


