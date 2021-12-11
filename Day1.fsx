// https://adventofcode.com/2021/day/1
#load "Advent.fsx"
open Advent

let countIncrements =
    Seq.pairwise
    >> Seq.sumBy (fun (a, b) -> if a < b then 1 else 0)

Day 1 {
Parse =
    fun input ->
    {|
        Depths =
            input
            |> Seq.map int
    |}

Part1 =
    7, fun input ->
    input.Depths
    |> countIncrements

Part2 =
    5, fun input ->
    input.Depths
    |> Seq.windowed 3
    |> Seq.map Seq.sum
    |> countIncrements

TestInput = """
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
}
