#load "../Data/Data.fsx"

#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET"

open Data
open FSharp.Stats
open FSharp.Stats.Algebra.LinearAlgebra

type Vec = Vector<float>

type Mat = Matrix<float>

let allData = loadData ()

let training, validation = allData |> splitData 0.7


let estimateTheta (X: Mat) (Y: Vec) =
    let XT = X |> Matrix.transpose

    ((XT * X) |> Inverse) * XT * Y



let train (f: Featurizer) (data: Obs seq) =
    let Yt, Xt =
        data |> Seq.toList |> List.map (fun obs -> float obs.Cnt, f obs) |> List.unzip

    let thetas = estimateTheta (matrix Xt) (vector Yt)

    thetas, (fun obs -> Vector.dot (f obs |> vector) thetas)

let evaluate (model: Model) (data: Obs seq) =
    data |> Seq.averageBy (fun obs -> abs (model obs - float obs.Cnt))

let f1 (obs: Obs) = [ 1.0; obs.Instant |> float ]

let f2 (obs: Obs) = [ 1.0; obs.Instant; float obs.Temp ]

let f3 (obs: Obs) =
    [ 1.0; obs.Instant; float obs.Temp; float obs.Windspeed ]


let f4 (obs: Obs) =
    [ 1.0; obs.Instant; float obs.Temp; float obs.Windspeed; float obs.Hum ]


let model0 (_: Obs) =
    allData.Rows |> Seq.averageBy (fun o -> float o.Cnt)

let thetas1, model1 = train f1 training
let thetas2, model2 = train f2 training
let thetas3, model3 = train f3 training
let thetas4, model4 = train f4 training

let averageCount = allData.Rows |> Seq.averageBy (fun o -> float o.Cnt)

let minPrice = allData.Rows |> Seq.minBy (fun o -> o.Cnt) |> (fun x -> x.Cnt)
let maxPrice = allData.Rows |> Seq.maxBy (fun o -> o.Cnt) |> (fun x -> x.Cnt)

evaluate model0 validation |> printfn "Model0: %A \n"
thetas1 |> printfn "Thetas1: %A"
evaluate model1 validation |> printfn "Model1: %A \n"
thetas2 |> printfn "Thetas2: %A"
evaluate model2 validation |> printfn "Model2: %A \n"
thetas3 |> printfn "Thetas3: %A"
evaluate model3 validation |> printfn "Model3: %A \n"
thetas4 |> printfn "Thetas4: %A"
evaluate model4 validation |> printfn "Model4: %A \n"
