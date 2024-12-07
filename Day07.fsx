#time
let input =
    System.IO.File.ReadAllLines("inputs/07.txt")
    |> Array.map (fun x -> x.Split([|' ';':'|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int64 |> Array.toList)

let handleRow ((total::head::rest): int64 list) =
    let totals1, totals2 = 
        rest
        |> List.fold (fun (acc1, acc2) elem -> 
            let plus1 = acc1 |> List.map (fun x -> x + elem)
            let product1 = acc1 |> List.map (fun x -> x * elem)
            let plus2 = acc2 |> List.map (fun x -> x + elem)
            let product2 = acc2 |> List.map (fun x -> x * elem)
            let concat2 = acc2 |> List.map (fun x -> int64(string x + string elem))
            plus1 @ product1, plus2 @ product2 @ concat2
        ) ([head], [head])
    let first = if totals1 |> List.contains total then total else 0
    let second = if totals2 |> List.contains total then total else 0
    first, second

let results = input |> Array.map handleRow

results |> Array.sumBy fst |> printfn "Part 1: %d"
results |> Array.sumBy snd |> printfn "Part 2: %d"