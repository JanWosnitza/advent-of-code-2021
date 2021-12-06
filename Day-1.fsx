// https://adventofcode.com/2021/day/1
#load "Util.fsx"

Util.useTestInput ()
let input = Util.getInput "https://adventofcode.com/2021/day/1/input" """
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

module Part1 =
    input
    |> Seq.map int
    |> Seq.pairwise
    |> Seq.sumBy (fun (a, b) -> if a < b then 1 else 0)
    |> printfn "Part 1 = %i"

module Part2 =
    input
    |> Seq.map int
    |> Seq.windowed 3
    |> Seq.map Seq.sum
    |> Seq.pairwise
    |> Seq.sumBy (fun (a, b) -> if a < b then 1 else 0)
    |> printfn "Part 2 = %i"
