#time
let lhs, rhs =
    System.IO.File.ReadAllLines "inputs/01.txt"
    |> Array.map (fun line -> line.Split "   " |> Array.map int |> fun [|x;y|]-> (x,y) )
    |> Array.unzip

(Array.sort lhs, Array.sort rhs)
||> Array.zip
|> Array.sumBy (fun (x,y) -> abs (x-y)) 
|> printfn "Part 1: %d"

let counts = rhs |> Array.countBy id |> Map.ofArray

lhs
|> Array.sumBy (fun x -> x * (Map.tryFind x counts |> Option.defaultValue 0))
|> printfn "Part 2: %d"
