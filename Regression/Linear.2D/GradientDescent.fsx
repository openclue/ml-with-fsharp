#r "nuget: Plotly.NET"

open System
open Plotly.NET

// f(x) = (x - 3)^2 + 5
let f x = (x - 3.) ** 2. + 5.

// f'(x) = 2(x - 3)
let dx_f x = 2. * (x - 3.)

// learning rate
let alfa = 0.001

// iterations
let i = 10_000

// initial guess
let rnd = Random(532123)
let x = rnd.NextDouble() * 100.

// gradient descent
let update alfa x =
    let dx = dx_f x
    printfn $"x = %.20f{x}, dx = %.20f{dx}"
    x - alfa * dx

let gradientDescent (alfa: float) (i: int) (x: float) =
    let rec search x i =
        if i = 0 then x else search (update alfa x) (i - 1)

    search x i

gradientDescent alfa i x |> printfn "'f(x) = (x-3)^2 + 5' reaches a minimum at 'x'  = %.20f"

[ -12 .. 18 ]
|> List.map (fun x -> (float x, f (float x)))
|> Chart.Line
|> Chart.withTitle "f(x) = (x - 3)^2 + 5"
|> Chart.show
