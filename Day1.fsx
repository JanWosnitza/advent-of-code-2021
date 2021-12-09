// https://adventofcode.com/2021/day/1
#load "Advent.fsx"
open Advent

solution 1 """
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
<| fun input ->

let depths =
    input
    |> Seq.map int

{
    Part1 = 7, fun () ->
        depths
        |> Seq.pairwise
        |> Seq.sumBy (fun (a, b) -> if a < b then 1 else 0)

    Part2 = 5, fun () ->
        depths
        |> Seq.windowed 3
        |> Seq.map Seq.sum
        |> Seq.pairwise
        |> Seq.sumBy (fun (a, b) -> if a < b then 1 else 0)
}
