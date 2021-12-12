#load "Advent.fsx"
open Advent

let parse = Input.toMultiline >> fun input ->
    {|
        Depths =
            input
            |> Seq.map int
    |}

let countIncrements =
    Seq.pairwise
    >> Seq.sumBy (fun (a, b) -> if a < b then 1 else 0)

let part1 = parse >> fun input ->
    input.Depths
    |> countIncrements

let part2 = parse >> fun input ->
    input.Depths
    |> Seq.windowed 3
    |> Seq.map Seq.sum
    |> countIncrements

///////////////////

let input1 = """
199
200
208
210
200
207
240
269
260
263
"""

AoC.Day 1 [
    AoC.Part part1 [
        input1, 7
    ]
    AoC.Part part2 [
        input1, 5
    ]
]
