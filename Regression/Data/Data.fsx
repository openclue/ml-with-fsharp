#r "nuget: FSharp.Data"

open FSharp.Data
open System

type Data = CsvProvider<"day.csv", ResolutionFolder=__SOURCE_DIRECTORY__>

// single observation
type Obs = Data.Row

type Feature = float

type Features = Feature list

type Featurizer = Obs -> Features

type Model = Obs -> float

let rng = Random(314159)

let shuffle (arr: 'a[]) =
    let arr = Array.copy arr
    let l = arr.Length

    for i in (l - 1) .. -1 .. 1 do
        let tmp = arr[i]
        let j = rng.Next(0, i + 1)
        arr[i] <- arr[j]
        arr[j] <- tmp

    arr
let path = $"{__SOURCE_DIRECTORY__}/../Data"
let loadData () =
    Data.Load($"{path}/day.csv")

let splitData (rate: float) (data: Data) =
    let shuffled = data.Rows |> Seq.toArray |> shuffle
    let size = rate * float shuffled.Length |> int
    shuffled[..size], shuffled[size + 1 ..]
