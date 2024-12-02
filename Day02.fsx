#time
let input = System.IO.File.ReadAllLines("inputs/02.txt")

let levels = input |> Array.map (fun x -> x.Split(" ") |> Array.map int)

let allIncreasing arr = arr |> Array.forall (fun (x, y) -> x < y)
let allDecreasing arr = arr |> Array.forall (fun (x, y) -> x > y)

let isSafe (x, y) = 
    match x - y with
    | z when z > 3 -> false
    | z when z < -3 -> false
    | _ -> true

let isOk arr =
    let pairs = arr |> Array.pairwise
    (allIncreasing pairs || allDecreasing pairs) && pairs |> Array.forall isSafe

let safe, unsafe = levels |> Array.partition isOk

let safeLen = safe |> Array.length
safeLen |> printfn "P1: %A"

let bruteForce array =
    let len = array |> Array.length
    [|
        for i in 0..len-1 do
            yield array |> Array.removeAt i
    |]

let newSafe =
    unsafe
    |> Array.sumBy (fun arr -> if bruteForce arr |> Array.exists isOk then 1 else 0)

safeLen + newSafe |> printfn "P2: %A"



