#time
let input = System.IO.File.ReadAllLines("inputs/05.txt")
let emptyIndex = input |> Seq.findIndex (fun x -> x = "")

let rules =
    input.[0..emptyIndex-1]
    |> Array.map (fun x -> x.Split('|') |> Array.map int |> fun [|x;y|] -> (x,y))

let sorter a b =
    if rules |> Array.contains (a,b) then -1
    elif rules |> Array.contains (b,a) then 1
    else 0

let instructions = input.[emptyIndex+1..] |> Array.map (fun x -> x.Split(',') |> Array.map int)

let correct, incorrect =
    instructions
    |> Array.map (fun instruction ->
        let sortedInstruction = Array.sortWith sorter instruction
        sortedInstruction, (instruction = sortedInstruction))
    |> Array.partition snd

correct
|> Array.sumBy (fun (x,_) -> x[x.Length/2])
|> printfn "Part 1: %d"

incorrect
|> Array.sumBy (fun (x,_) -> x[x.Length/2])
|> printfn "Part 2: %d"