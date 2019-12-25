# Description 

F# deep reinforcement learning example. Simple QNN trained on a toy game using epsilon-greedy based decisions with memory replay. The task is simple, player (blue) can move left/right to collect rewards (green) and avoid obstacles (red). Once 10 (default setting) green rewards are missed the game resets.

QNN after training 1000 episodes:
![](play1000.gif)


# Running

- "libmxnet" and CUDA libraries must be available for loading
- configure [loadui.fsx](../loadui.fsx) to desired framework (needs editing for dotnet core)
- execute [QNN.fsx](QNN.fsx) in F# interactive.

# Details 
Player is blue, rewards are green and obstacles (not on by default) are red. The gameboard is set to 9x9 pixels with it's representation simply a [3;w;h] array where rgb values are 0.f or 1.f.

## Model
``` fsharp
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

```

## Training process

### Executors
We need to maintain 3 executors for training:
1. As the game is played we need to evaluate our q-network at each step to determine the best action.
2. We need to evaluate the "target" network on `batchSize`
3. We need to train the parameters on `batchSize`

The difference between the training network and the target network can be seen here 
``` fsharp 
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
```

Above notice the x,y training pairs are generated using the target network (`execBatch`) to then train the training network (`execTrain`). The target network parameters are copied with 
```fsharp 
/// Copy trained parameters to target network
let copyToTarget() = 
    for a in targetBindings do 
        match a with 
        | ArgBinding(a) -> 
            let scc,b = trainBindings.TryGetValue(a.Name)
            if scc && a.Shape = b.Shape then 
                b.NDArray.Value.CopyTo(a.NDArray.Value)
        | _ -> ()
```
every `updateTargetStepCount` steps. More spefically this is done by passing an `onStep` function to `makeTrainer : onStep -> onEpisode -> TrainerAgent`:
```fsharp 
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
```

With the above executors we can then also have a fourth executor for "live play". This network runs on the `CPU 0` context while training happens on `GPU 0`

### Iteration

A single episode is one play of the game till reset (10 rewards missed). On each step (`movesPerUpdate` per reward move) either the best action is taken with probability `1.0 - epsilon` or a random move is made (probability `epsilon`). The resulting state, nextState and reward (a 'sample') is stored in memory (capacity set by `memoryLength`). This new sample along with `batchSize - 1` random samples from memory are then used to train (`replay` function from above). The `run` function is where a single episode takes place:

```fsharp

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


```




