#time
let input =
    System.IO.File.ReadAllLines("inputs/06.txt")
    |> Seq.mapi (fun row -> Seq.mapi (fun col a -> ((col, row), a)))
    |> Seq.concat
    |> Map

let ahead (x,y) d =
    match d with
    | 0 -> (x,y-1)
    | 1 -> (x+1,y)
    | 2 -> (x,y+1)
    | 3 -> (x-1,y)

let patrol map =
    let rec step map dir loc visited =
        let turn = (dir+1) % 4
        let track = Set.add (dir,loc) visited
        if Set.contains (dir,loc) visited then Seq.empty
        else 
            let n = ahead loc dir
            match Map.tryFind n map with
            | Some '#' -> step map turn loc visited
            | Some _  -> step map dir n track
            | None -> visited |> Set.map snd |> Set.toSeq
    match Map.tryFindKey (fun _ v -> v = '^') map with
    | Some sp -> step map 0 sp Set.empty
    | _ -> Seq.empty

input
|> patrol
|> Seq.length
|> (+) 1
|> printfn "Part 1: %d"

input
|> patrol
|> Seq.map (fun loc -> patrol (Map.change loc (Option.map (fun _ -> '#')) input))
|> Seq.filter ((=) Seq.empty) |> Seq.length
|> printfn "Part 2: %d"