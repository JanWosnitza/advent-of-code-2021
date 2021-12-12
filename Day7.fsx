// https://adventofcode.com/2021/day/7
#load "Advent.fsx"
open Advent

let parse = Input.toMultiline >> fun input ->
    let hPositions =
        input.[0]
        |> Input.split [","]
        |> Array.map int
        |> Array.sort

    {|
        HPositions = hPositions
        MedianPosition = hPositions.[hPositions.Length / 2]
    |}

let part1 = parse >> fun input ->
    input.HPositions
    |> Array.sumBy ((-) input.MedianPosition >> abs)

let part2 = parse >> fun input ->
    let averagePosition =
        input.HPositions
        |> Seq.averageBy float
        |> round
        |> int

    // With fuel consumtption "n * n" average would be the best position but the "n" term skews it of a little.
    let direction = sign (input.MedianPosition - averagePosition)
    Seq.initInfinite (fun x -> averagePosition + direction * x)
    |> Seq.map (fun (position) ->
        input.HPositions
        |> Array.sumBy (fun p ->
            let n = abs (p - position)
            (n * n + n) / 2 // == n * (n + 1) / 2
        )
    )
    |> Seq.pairwise
    |> Seq.find (fun (a, b) -> a < b)
    |> fst

//////////////////
let testInput1 = """
16,1,2,0,4,2,7,1,2,14
"""

AoC.Day 7 [
    AoC.Part part1 [
        testInput1, 37
    ]
    AoC.Part part2 [
        testInput1, 168
    ]
]
