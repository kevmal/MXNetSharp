
#load "../loadui.fsx"
open Loadui

#nowarn "9" // Unsafe code to update bitmap

open System
open System.Collections.Concurrent
open System.Collections.Generic
open Microsoft.FSharp.NativeInterop

open MXNetSharp
open MXNetSharp.PrimitiveOperators
open MXNetSharp.SymbolOperators

open Avalonia
open Avalonia.Controls
open Avalonia.Media.Imaging



// ********************************** Parameters ***********************************************
// Game Parameters
let boardWidth = 9
let boardHeight = 9
let maxMiss = 10
let rewardValue = 25

let obstableProb = 0.0
let rewardProb = 0.3
let initSpeed = 300.0
let maxSpeed = 200.0

let movesPerUpdate = 4      // This only applies to the agent, not a game rule

// Number of state, action, reward tuples to remember for training
let memoryLength = 10000

let context = GPU 0         // Where to train
let runContext = CPU 0      // Where to run live model

let batchSz = 32
let updateTargetStepCount = 3000    // Number of steps between updating target model parameters
let printUpdateEpCount = 25         // Console print frequency in #episodes
let initialLearningReate = 0.001
let minLearningRate = 0.001 
let learningRateDacay = 0.75
let learningRateUpdateCount = 100   // every `learningRateUpdateCount` episodes:  lr = max minLearningRate (lr*learningRateDacay)

// Epsilon range parameters where epsilon is the chance of random action 
let maxEp = 1.0
let minEp = 0.01
let lambda = 0.00006 //speed of epsilon decay
let gamma = 0.95     //q(state, action) = reward + gamma(max q(state',_))



// ********************************** Model ***********************************************
let actionCount = 3
// Three 'channels'. reward/ obstacle/ player
let boardInput = Input("state", [0; 3; boardWidth; boardHeight])
let labels = Input("qa", [0; actionCount])
let qmodel = 
    let h = 64
    let n1 = boardInput .>> FullyConnected(h, flatten = true) .>> Relu()
    let c1 = boardInput .>> Convolution(numFilter = 32, kernel = [3;3], stride = [1;1], pad = [1;1]) .>> Relu()
    let n2 = c1 .>> FullyConnected(h, flatten = true) .>> Relu()
    let n3 = 
        c1
        .>> Convolution(numFilter = 32, kernel = [3;3], stride = [2;2], pad = [1;1]) .>> Relu()
        .>> FullyConnected(h, flatten = true) .>> Relu()
    (n1 + n2 + n3) .>> FullyConnected(actionCount)

let loss = (qmodel - labels) .>> Square() .>> Mean() .>> MakeLoss()


// ********************************** Game logic / Display ***********************************************
[<AutoOpen>]
module Game = 
    type GameBoard = 
        {
            Random : Random
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
        let rng = s.Random
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
            None //Hitting an obstacle ends the game
        else
            let hitReward = rewards |> List.filter (fun (x,y) -> y = 0) |> List.exists (fun (x,y) -> x = player)
            let s = 
                { 
                    Random = s.Random
                    Player = player 
                    Obstacles = obstacles
                    Rewards = if hitReward then rewards |> List.filter (fun (x,y) -> y <> 0 || x <> player) else rewards
                    Score = s.Score + (if hitReward then rewardValue else 0) 
                    Speed = if update then max (s.Speed*0.95) maxSpeed else s.Speed
                    UpdateTime = if update then now.AddMilliseconds (double s.Speed) else s.UpdateTime
                    Missed = missed
                }
            if missed < maxMiss then 
                Some s
            else
                None

    let rec gameLoop s getmove reset writeBoard =
        writeBoard s
        match gameStep s DateTime.Now getmove with 
        | None -> gameLoop (reset s) getmove reset writeBoard
        | Some s ->gameLoop s getmove reset writeBoard

    let initialBoard = 
        {
            Random = Random()
            Player = boardWidth / 2
            Obstacles = []
            Rewards = []
            Score = 0
            Speed = initSpeed
            UpdateTime = DateTime.MinValue
            Missed = 0
        }

    let reset _ = initialBoard
    
    type GameMode = 
        | KeyboardControl
        | FunctionControl of (GameBoard -> int)

    type GameWindow =
        {
            Bitmap : WriteableBitmap
            MoveQueue : Move ConcurrentQueue
            Window : Window
            Image : Image
            ControlButton : Button
            UpdateButton : Button
            TextBlock : TextBox
            mutable GameMode : GameMode
            Left : TextBlock
            Right : TextBlock
            Stay : TextBlock
        }   

    let makeGameWindow() = 
        let bitmap = new WriteableBitmap(PixelSize(boardWidth,boardHeight),Vector(90.,90.), Nullable(Platform.PixelFormat.Bgra8888))
        let moveq = ConcurrentQueue()
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
            let dpanel = DockPanel()
            let grid = Grid()
            grid.ColumnDefinitions.Add(ColumnDefinition(GridLength.Parse "220"))
            grid.ColumnDefinitions.Add(ColumnDefinition(GridLength.Parse "*"))
            grid.Children.Add(dpanel)
            dpanel.SetValue(Grid.ColumnProperty, 0)
            grid.Children.Add(p)
            p.SetValue(Grid.ColumnProperty, 1)
            let txt = TextBox()
            txt.IsReadOnly <- true
            txt.TextWrapping <- Media.TextWrapping.Wrap
            let controlbtn = Button()
            controlbtn.Content <- "Use DQN"
            let updatebtn = Button()
            updatebtn.Content <- "Update Model"
            let spanel = StackPanel()
            let probpanel = StackPanel()
            probpanel.Orientation <- Orientation.Horizontal
            spanel.Orientation <- Orientation.Vertical
            spanel.Children.Add(controlbtn)
            spanel.Children.Add(updatebtn)
            dpanel.Children.Add(spanel)
            dpanel.Children.Add(probpanel)
            DockPanel.SetDock(spanel,Dock.Top)
            DockPanel.SetDock(probpanel,Dock.Bottom)
            dpanel.Children.Add(txt)
            let left = TextBlock()
            let right = TextBlock()
            let stay = TextBlock()
            left.Width <- 220.0 /3.0
            left.Text <- "Left"
            left.TextAlignment <- Media.TextAlignment.Center
            right.Width <- 220.0 /3.0
            right.Text <- "Right"
            right.TextAlignment <- Media.TextAlignment.Center
            stay.Width <- 220.0 /3.0
            stay.Text <- "Stay"
            stay.TextAlignment <- Media.TextAlignment.Center
            probpanel.Children.Add left
            probpanel.Children.Add stay
            probpanel.Children.Add right
            p.Source <- bitmap
            f.Content <- grid
            f.Show()
            {
                Bitmap = bitmap
                MoveQueue = moveq
                Window = f
                Image = p
                GameMode = KeyboardControl
                ControlButton = controlbtn
                TextBlock = txt
                UpdateButton = updatebtn
                Left = left
                Right = right
                Stay = stay
            }
        )
             
    let updateBitmap (wnd : GameWindow) (s : GameBoard) = 
        use fb = wnd.Bitmap.Lock()
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

    type Game = 
        {
            Window : GameWindow
            Task : System.Threading.Tasks.Task<unit>
        }

    let game() = 
        let wnd = makeGameWindow()
        let moveq = wnd.MoveQueue
        let mutable l = DateTime.MinValue
        let mutable m = 0
        let move act (b : GameBoard) = 
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
            match wnd.GameMode with 
            | KeyboardControl -> 
                let scc,v = moveq.TryDequeue()
                if scc then v else Stay
            | FunctionControl act -> move act s
        {
            Window = wnd
            Task =
                async {
                    gameLoop initialBoard getmove reset (updateBitmap wnd)
                } |> Async.StartAsTask
        }

    
/// Type for state, action and reward
type Sample = 
    {
        State : GameBoard
        Action : int
        Reward : double 
        NextState : GameBoard option
    }

type Memory(memoryLength : int) = 
    let memory = ResizeArray<Sample>()
    let mutable lasti = 0
    member x.Add(s : Sample) = 
        if memory.Count >= memoryLength then
            lasti <- (lasti + 1) % memory.Count
            memory.[lasti] <- s
        else
            lasti <- memory.Count
            memory.Add(s)
    member x.Count = memory.Count
    member x.Last = memory.[lasti]
    member x.Sample(rng : Random) = memory.[rng.Next(memory.Count)]


type TrainState = 
    {
        Random : Random
        Memory : Memory
        InputVariable : Variable
        LabelVariable : Variable
        TrainExecutor : Executor
        EvalExecutor : Executor
        BatchEvalExecutor : Executor
        UpdateStep : double -> unit
        LossSum : double 
        LossCount : double
        Steps : int
        Epsilon : double
        LearningRate : double
        Episode : int
        RewardSum : double
    }
    static member Create(memoryLength, input, label, trainExecutor, evalExecutor, batchEvalExecutor, update) = 
        {
            Random = Random()
            Memory = Memory(memoryLength)
            LossSum = 0.0
            LossCount = 0.0
            Steps = 0
            Epsilon = maxEp
            LearningRate = initialLearningReate
            TrainExecutor = trainExecutor
            EvalExecutor = evalExecutor
            BatchEvalExecutor = batchEvalExecutor
            UpdateStep  =  update
            InputVariable = input
            LabelVariable = label
            Episode = 0
            RewardSum = 0.0
        }
        


/// Gameboard array of length n to [n; 3; w; h] .net array
let makeBatch (states : GameBoard []) = 
    let inp = Array.CreateInstance(typeof<float32>,states.Length, 3, boardWidth, boardHeight)
    let put (b : int) (x : int) (y : int) (i : int) = 
        inp.SetValue(1.f, b, i, x, y)
    for i = 0 to states.Length - 1 do
        let s = states.[i]
        put i s.Player 0 0
        for (x,y) in s.Rewards do 
            put i x y 1
        for (x,y) in s.Obstacles do 
            put i x y 2
    inp
    
/// train using a single batch consisting of last sample and random samples from play memory
let replay (ts : TrainState) = 
    let memory = ts.Memory
    if memory.Count = 0 then failwith "memory empty"
    let inputVar = ts.InputVariable
    let labelVar = ts.LabelVariable
    let execBatch = ts.BatchEvalExecutor
    let execTrain = ts.TrainExecutor
    let batch = 
        [|
            yield memory.Last   // We'll ensure last action is a part of this batch
            yield! Seq.init (batchSz - 1) (fun _ -> memory.Sample(ts.Random))
        |]
    let len = batch.Length
    // evaluate network on each sample state
    let p = 
        let a = makeBatch (batch |> Array.map (fun x -> x.State))
        execBatch.[inputVar].CopyFrom a
        execBatch.Forward(false)
        execBatch.Outputs.[0].ToFloat32Array() |> Array.splitInto len
    // evaluate network on each sample "next state"
    let p_ = 
        let a = makeBatch (batch |> Array.map (fun x -> defaultArg x.NextState initialBoard))
        execBatch.[inputVar].CopyFrom a
        execBatch.Forward(false)
        execBatch.Outputs.[0].ToFloat32Array() |> Array.splitInto len
    // Generate x,y for training
    // With `p` we update the value for the action in which we know the reward
    // p.[actionTaken] <- reward + gamma*bestFutureActionValue
    // where 'bestFutureActionValue' is assumed from the max of given p_
    let x = Array.zeroCreate len
    let y = Array.zeroCreate len
    for i = 0 to len - 1 do
        let b = batch.[i]
        let t = p.[i] |> Array.copy
        t.[b.Action] <- 
            match b.NextState with
            | Some _ -> b.Reward + gamma*(double(Array.max p_.[i])) |> float32
            | None -> b.Reward |> float32
        x.[i] <- b.State
        y.[i] <- t
    execTrain.[labelVar].CopyFrom(y |> Array.concat)
    execTrain.[inputVar].CopyFrom(makeBatch x)
    execTrain.Forward(true)
    execTrain.Backward()
    ts.UpdateStep(ts.LearningRate)
    {ts with 
        LossSum = ts.LossSum + execTrain.Outputs.[0].ToScalar()
        LossCount = ts.LossCount + 1.0
    }

let bindings = 
    Bindings.inputs [boardInput] 
    |> Bindings.inferShapes qmodel


// We need an excutor to evaluate single decisions during training "game play"
let evalBindings = 
    bindings 
    |> Bindings.batchSize 1
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

// Target executor maintains copies of the model parameters and updates at deliberate times
// Keeping the parameters fixed for a number of steps allows the network train against a fixed target during that time
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

// Train executor will receive paramter updates every training step
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

/// Copy trained parameters to target network
let copyToTarget() = 
    for a in targetBindings do 
        match a with 
        | ArgBinding(a) -> 
            let scc,b = trainBindings.TryGetValue(a.Name)
            if scc && a.Shape = b.Shape then 
                b.NDArray.Value.CopyTo(a.NDArray.Value)
        | _ -> ()


// Create the needed executors for training
let execEval = qmodel.Bind(context, evalBindings)
let execBatch = qmodel.Bind(context, targetBindings)
let execTrain = loss.Bind(context, trainBindings)

/// Evaluate executor on GameBoard. epislon > 0.0 alows random moves with probability epsilon
let act epsilon (evalin : NDArray) (exe : Executor) (s : GameBoard) = 
    if epsilon > 0.0 && s.Random.NextDouble() < epsilon then 
        MX.Shuffle(MX.ArangeNDArray(start = 0.0, stop = double actionCount, ctx = CPU 0)).ToIntArray()
    else
        let inp = Array.CreateInstance(typeof<float32>, 1, 3, boardWidth, boardHeight)
        let put (x : int) (y : int) (i : int) = 
            inp.SetValue(1.f, 0, i, x, y)
        put s.Player 0 0
        for (x,y) in s.Rewards do 
            put x y 1
        for (x,y) in s.Obstacles do 
            put x y 2
        evalin.CopyFrom(inp)
        exe.Forward(false)
        MX.Argsort(exe.Outputs.[0], axis = 1, isAscend=false).ToIntArray()

type AdamOptimizer(e : Executor, ?beta1, ?beta2) =
    let beta1 = defaultArg beta1 0.9
    let beta2 = defaultArg beta2 0.999
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
    member x.Update(learningRate) = 
        updateCount <- updateCount + 1
        let t = double updateCount
        let lr = learningRate*sqrt(1.0 - Math.Pow(beta2,t)) / (1.0 - Math.Pow(beta1,t))
        e.Bindings
        |> Seq.iter
            (fun a ->
                match a with 
                | {Name = name; BindType = ArgBind(Some WriteTo, Some grad); NDArray = Some weight} -> 
                    let m,v = lu name grad
                    MX.AdamUpdate([weight], weight, grad, m, v, lr, beta1, beta2)
                | _ -> ()
            )

/// Run a single episode for training        
let run onStep (ts : TrainState)  = 
    let rec loop frameMove (s : GameBoard) (ts : TrainState) = 
        let evalin = ts.EvalExecutor.[ts.InputVariable]
        // mask out invalid moves taking the next best action
        let actionInt = 
            let a = act ts.Epsilon evalin ts.EvalExecutor s 
            let j = 
                if s.Player = 0 && a.[0] = 1 then 
                    1
                elif s.Player = (boardWidth - 1) && a.[0] = 2 then 
                    1
                else
                    0
            a.[j]
        // when movesPerUpdate is hit we force action 'Stay'
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
                // Let the game continue
                gameStep {s with UpdateTime = DateTime.MinValue} DateTime.Now (fun _ -> action) |> Option.map (fun s2 -> {s2 with UpdateTime = DateTime.MinValue})
            | _ -> 
                // Move the player only
                gameStep {s with UpdateTime = DateTime.Now} DateTime.MinValue (fun _ -> action) |> Option.map (fun s2 -> {s2 with UpdateTime = DateTime.MinValue})
        let reward = 
            match s2 with 
            | None -> 0.0
            | Some s2 -> (s2.Score - s.Score |> double) / double rewardValue
        ts.Memory.Add 
            { State = s
              NextState = s2
              Action = actionInt
              Reward = reward }
        let ts = 
            let steps = ts.Steps + 1
            replay 
                {ts with 
                    Steps = steps
                    Epsilon = minEp + (maxEp - minEp)*(exp (-lambda*(double steps)))
                }
        let ts = onStep ts
        match s2 with 
        | None -> ts, s.Score |> double
        | Some s -> loop frameMove s ts
    loop 0 initialBoard ts


type TrainerMessage = 
    | Pause of AsyncReplyChannel<TrainState>
    | Resume of TrainState option

let makeTrainer onStep onEpisode = 
    let opt = AdamOptimizer(execTrain)
    let ts = TrainState.Create(memoryLength, boardInput, labels, execTrain, execEval, execBatch, opt.Update)
    MailboxProcessor.Start
        (fun inbox -> 
            let rec loop paused (ts : TrainState) = 
                async {
                    try
                        let! msg = 
                            if paused then 
                                async {
                                    let! m = inbox.Receive()
                                    return Some m
                                }
                            else
                                inbox.TryReceive(1)
                        match msg with 
                        | None -> 
                            let ts, score = run onStep ts
                            let rewardSum = ts.RewardSum + score
                            let ts = {ts with Episode = ts.Episode + 1; RewardSum = rewardSum} |> onEpisode
                            return! loop paused ts
                        | Some(Pause(c)) -> 
                            c.Reply(ts)
                            return! loop true ts
                        | Some(Resume(Some ts))-> return! loop false ts
                        | Some(Resume(_)) -> return! loop false ts
                    with 
                    | e -> 
                        printfn "Trainer Exception: %s" (e.GetType().Name)
                        printfn "%A" e.Message
                        printfn "%A" e.StackTrace
                }
            loop false ts
        )




// ********************************** Create our live model ***********************************************

/// Lock to syncronize updates of parameters for our live model
let playLock = obj()

/// bindings for live game play created on runContext
let runBindings = 
    bindings 
    |> Bindings.batchSize 1
    |> Bindings.init 
        (fun a shape ->  
            let scc,v = evalBindings.TryGetValue(a.Name)
            if scc && shape = v.Shape.Value then 
                v.NDArray.Value.CopyTo(runContext)
            else
                context.Zeros shape
        )

/// update parameters of the live model
let copyToRun() = 
    lock playLock
        (fun _ ->
            for a in runBindings do 
                match a with 
                | ArgBinding(a) -> 
                    let scc,b = trainBindings.TryGetValue(a.Name)
                    if scc && a.Shape = b.Shape then 
                        b.NDArray.Value.CopyTo(a.NDArray.Value)
                | _ -> ()
        )

/// Live executor
let execRun = qmodel.Bind(runContext, runBindings)


// ********************************** Create game display ***********************************************
let gameWindow = game()

let appendText str = 
    UI.uido 
        (fun () ->
            let c = gameWindow.Window.TextBlock
            c.Text <- c.Text + str
            while c.Text.Length > 10000 do
                c.Text <- 
                    let a = c.Text.Split([|'\n'|],2)
                    if a.Length = 2 then 
                        a.[1]
                    else
                        c.Text
            c.CaretIndex <- Int32.MaxValue    
        )

/// Keyboard / DQN toggle
gameWindow.Window.ControlButton.Click
|> Observable.add 
    (fun _ ->
        let b = gameWindow.Window.ControlButton
        let useDqnTxt = "Use DQN"
        let useKeysTxt = "Keyboard Control"
        match b.Content with 
        | :? string as s when s = useDqnTxt -> 
            b.Content <- useKeysTxt
            let autoPlayAct (b : GameBoard) = 
                lock playLock
                    (fun _ ->
                        let evalin = runBindings.NDArray(boardInput)
                        let moves = act -1.0 evalin execRun b
                        let probs = 
                            let a = execRun.Outputs.[0].ToDoubleArray() |> Array.map exp
                            let sum = a |> Array.sum
                            a |> Array.map (fun x -> x / sum)
                        let str = sprintf "L: %5f S: %5f R: %5f\r\n" probs.[1] probs.[0] probs.[2] 
                        UI.uido
                            (fun () -> 
                                let w = gameWindow.Window
                                let color (c : TextBlock) (p : double) = 
                                    let r = 255.0 * (1.0 - p) |> round |> max 0.0 |> min 255.0 |> byte
                                    let g = 255.0 * p |> round |> max 0.0 |> min 255.0 |> byte
                                    let b = 100uy
                                    c.Background <- Media.SolidColorBrush(Media.Color.FromRgb(r,g,b))
                                color w.Stay probs.[0]
                                color w.Left probs.[1]
                                color w.Right probs.[2]
                                appendText str
                            )
                        moves.[0]
                    )
            gameWindow.Window.GameMode <- GameMode.FunctionControl autoPlayAct
        | _ -> 
            b.Content <- useDqnTxt 
            gameWindow.Window.GameMode <- GameMode.KeyboardControl
    )


/// Update parameters of live model
let updateModel ts = 
    copyToRun()
    appendText "--------------------\r\n"
    appendText "Model Updated\r\n"
    appendText (sprintf "Train steps: %d\r\n" ts.Steps)
    appendText "--------------------\r\n"


// ********************************** Create trainer ***********************************************

let trainer = 
    makeTrainer
        (fun ts -> 
            if ts.Steps % updateTargetStepCount = 0 then 
                copyToTarget()
                updateModel ts
            ts
        )
        (fun ts -> 
            let ts = 
                if ts.Episode % printUpdateEpCount = 0 then
                    printfn "Epsilon %10f Steps %d, Episode: %d, Average reward for episode %f, Count %f, LossSum %f, Loss %f, LR: %f"  ts.Epsilon ts.Steps ts.Episode (ts.RewardSum / double printUpdateEpCount) ts.LossCount ts.LossSum (ts.LossSum/ts.LossCount ) ts.LearningRate
                    {ts with 
                        LossCount = 0.0
                        LossSum = 0.0
                        RewardSum = 0.0}
                else
                    ts
            if ts.Episode % learningRateUpdateCount = 0 && ts.LearningRate > minLearningRate then
                {ts with 
                    LearningRate = ts.LearningRate * learningRateDacay |> max minLearningRate}
            else
                ts
        )

// Wire up "Update Model" button to copy parameters on demand
do 
    let mutable updating = false
    gameWindow.Window.UpdateButton.Click
    |> Observable.add 
        (fun _ ->
            if not updating then 
                updating <- true
                async {
                    let! ts = trainer.PostAndAsyncReply Pause
                    updateModel ts
                    trainer.Post(Resume None)
                    updating <- false
                } |> Async.Start
        )

