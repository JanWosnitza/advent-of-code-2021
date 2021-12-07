// https://adventofcode.com/2021/day/1
#load "Util.fsx"

let adventDay = Util.adventDay 1 """
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

let depths =
    adventDay.RawInput
    |> Seq.map int

module Part1 =
    let increases =
        depths
        |> Seq.pairwise
        |> Seq.sumBy (fun (a, b) -> if a < b then 1 else 0)

module Part2 =
    let increases =
        depths
        |> Seq.windowed 3
        |> Seq.map Seq.sum
        |> Seq.pairwise
        |> Seq.sumBy (fun (a, b) -> if a < b then 1 else 0)

adventDay.Answer(
    part1 = Part1.increases,
    part2 = Part2.increases
)
