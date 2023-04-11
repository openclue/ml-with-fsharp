#load "../Data/Data.fsx"
#r "nuget: Plotly.NET"
#r "nuget: Plotly.NET.Interactive"

open Data
open Plotly.NET

let allData = loadData ()

let training, validation = allData |> splitData 0.7


let cost (obs: Obs seq) (model: Model) =
    obs |> Seq.sumBy (fun obs -> (model obs - float obs.Cnt) ** 2.) |> sqrt

let trainingCost = cost training

let validationCost = cost validation

let overallCost = cost allData.Rows

let model (theta0, theta1) (obs: Obs) = theta0 + theta1 * float obs.Instant

let updateThetas (alpha: float) (theta0, theta1) (obs: Obs) =
    let x = float obs.Instant
    let y = float obs.Cnt
    let theta0' = theta0 - 2. * alpha * (theta0 + theta1 * x - y)
    let theta1' = theta1 - 2. * alpha * x * (theta0 + theta1 * x - y)
    (theta0', theta1')

let batchUpdateThetas rate (theta0, theta1) (data: Obs seq) =
    let thetas = data |> Seq.map (updateThetas rate (theta0, theta1))

    let theta0' = thetas |> Seq.averageBy fst
    let theta1' = thetas |> Seq.averageBy snd
    (theta0', theta1')

let stochasticEstimation rate (theta0, theta1) =
    training |> Seq.fold (updateThetas rate) (theta0, theta1)

let batchEstimation rate iters (data: Obs seq) =
    let rec search (theta0, theta1) i =
        if i = 0 then
            (theta0, theta1)
        else
            search (batchUpdateThetas rate (theta0, theta1) data) (i - 1)

    search (0., 0.) iters

let rate =
    [ for r in 1..30 -> (pown 0.1 r), stochasticEstimation (pown 0.1 r) (0., 0.) |> model |> trainingCost ]
    |> Seq.filter (fun (rate, error) -> error > 0)
    |> Seq.minBy snd
    |> fst

let baselineModel =
    fun (_: Obs) -> allData.Rows |> Seq.averageBy (fun x -> float x.Cnt)

let thetas1 = stochasticEstimation rate (0., 0.)
let model1 = thetas1 |> model

let thetas2 = batchEstimation (pown 0.1 6) 15_000_000 training
let model2 = thetas2 |> model

evaluateModel baselineModel |> printfn "Naive model average error: %f"
thetas1 |> printfn "Model 1 thetas: %A"
evaluateModel model1 |> printfn "Model 1 average error: %f"
thetas2 |> printfn "Model 2 thetas: %A"
evaluateModel model2 |> printfn "Model 2 average error: %f"

let chart =
    allData.Rows
    |> Seq.map (fun obs -> (float obs.Instant, float obs.Cnt))
    |> Chart.Point

let createModelChart (model: Model) =
    training |> Array.map (fun obs -> (float obs.Instant, model obs)) |> Chart.Line

let baselineChart = createModelChart baselineModel

let model1Chart = createModelChart model1

let model2Chart = createModelChart model2

[ chart; baselineChart; model1Chart; model2Chart]
|> Chart.combine
|> Chart.show

model2Chart |> Chart.show
