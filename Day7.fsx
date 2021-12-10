// https://adventofcode.com/2021/day/7
#load "Advent.fsx"
open Advent

Day 7 {
Parse =
    fun input ->
    let hPositions =
        input.[0]
        |> Util.stringSplit [","]
        |> Array.map int
        |> Array.sort

    {|
        HPositions = hPositions
        MedianPosition = hPositions.[hPositions.Length / 2]
    |}

Part1 =
    37, fun input ->
    input.HPositions
    |> Array.sumBy ((-) input.MedianPosition >> abs)

Part2 =
    168, fun input ->
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
}

<| """
16,1,2,0,4,2,7,1,2,14
"""
