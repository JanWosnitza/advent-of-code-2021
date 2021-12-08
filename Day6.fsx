// https://adventofcode.com/2021/day/6
#load "Util.fsx"

let day = Util.adventDay 6 """
3,4,3,1,2
"""

let fishes =
    day.RawInput.[0]
    |> Util.stringSplit [","]
    |> Array.map int

let fishLife =
    let rec mem = Util.memoize ()
    let rec recurse (daysLeft) =
        if daysLeft <= 0 then
            1L
        else
            mem recurse (daysLeft - 7) + mem recurse (daysLeft - 9)

    fun (days) (fish) ->
    mem recurse (days - fish)

module Part1 =
    let count =
        fishes
        |> Seq.sumBy (fishLife 80)

module Part2 =
    let count =
        fishes
        |> Seq.countBy id
        |> Seq.sumBy (fun (fish, count) -> fishLife 256 fish * int64 count)

day.Answer(
    part1 = Part1.count,
    part2 = Part2.count
)
