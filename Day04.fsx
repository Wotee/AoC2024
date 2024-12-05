open System
open System.Text.RegularExpressions
let input = System.IO.File.ReadAllLines("inputs/04.txt")
// let input = System.IO.File.ReadAllLines("inputs/temp.txt")

let normal =
    input
    |> Array.sumBy (fun str -> Regex.Count(str, "(?=(XMAS|SAMX))"))

let transpose (input: char array array) =
    let array = input |> array2D
    [| for i in 0..(Array2D.length1 array) - 1 do
        yield [|
            for j in 0..(Array2D.length2 array) - 1 do
                yield array.[j, i] |]
                |]

let getDiagonalIndices x =
    [|
        for d in 0 .. 2 * (x - 1) do
            yield
                [|
                    for i in 0 .. x - 1 do
                        let j = d - (x - 1 - i)
                        if j >= 0 && j < x then
                            yield (i, j)
                |]
    |]


let getOtherDiagonalIndices x =
    [|
        for d in 0 .. 2 * (x - 1) do
            yield
                [|
                    for i in 0 .. x - 1 do
                        let j = d - i
                        if j >= 0 && j < x then
                            yield (i, j)
                |]
    |]

let transposed = transpose (input |> Array.map (fun str -> str.ToCharArray())) |> Array.sumBy (fun str -> Regex.Count(str, "(?=(XMAS|SAMX))"))

let arr2d = input |> array2D

let len = Array2D.length1 arr2d
let otherDiagonalIndices = getOtherDiagonalIndices len

let diagonalIndices = getDiagonalIndices len

let data =
    diagonalIndices
    |> Array.map (fun indices -> indices |> Array.map (fun (a, b) -> arr2d.[a, b]) |> String.Concat)
    |> Array.sumBy (fun str -> Regex.Count(str, "(?=(XMAS|SAMX))")) 

let data2 =
    otherDiagonalIndices
    |> Array.map (fun indices -> indices |> Array.map (fun (a, b) -> arr2d.[a, b]) |> String.Concat)
    |> Array.sumBy (fun str -> Regex.Count(str, "(?=(XMAS|SAMX))"))


data + data2 + transposed + normal |> printfn "Part1: %d"

let getCrossIndices x y =
    let max = Array2D.length1 arr2d
    let min = 0
    if x - 1 >= min && y - 1 >= min && x + 1 < max && y + 1 < max then
        [|(x - 1, y + 1), (x + 1, y - 1); (x - 1, y - 1), (x + 1, y + 1)|]
    else [||]

arr2d
|> Array2D.mapi (fun a b char ->
    if char = 'A' then
        let crossIndices = getCrossIndices a b |> Array.map (fun ((a, b), (c, d)) -> [|arr2d.[a, b]; arr2d.[c, d]|] |> Array.sort |> String.Concat)
        match crossIndices with
        | [|"MS"; "MS"|] -> 1
        | _ -> 0
    else
        0
) |> Seq.cast<int> |> Seq.sum |> printfn "Part2: %d"


