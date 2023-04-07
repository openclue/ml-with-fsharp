#load "../Data/Data.fsx"

open Data

let allData = loadData()

let training, validation = allData |> splitData 0.7