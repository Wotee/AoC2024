#time
open System.Text.RegularExpressions
open System

let input = System.IO.File.ReadAllText("inputs/03.txt")

let solve inp =
    Regex.Matches(inp, "mul\(\d+,\d+\)")
    |> Seq.sumBy (fun x -> x.Value.Split([|'m'; 'u'; 'l'; '('; ','; ')'|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> Array.reduce (*))

// P1
input
|> solve
|> printfn "P1: %A"

// P2
input
|> fun inp -> Regex.Split(inp, "do(?!n\'t)") |> Array.map (fun start -> start.Split("don't")[0]) |> String.concat ""
|> solve
|> printfn "P2: %A"