// https://adventofcode.com/2021/day/6
#load "Advent.fsx"
open Advent

let fishLife =
    let rec mem = Util.memoize ()
    let rec recurse (daysLeft) =
        if daysLeft <= 0 then
            1L
        else
            mem recurse (daysLeft - 7) + mem recurse (daysLeft - 9)

    fun (days) (fish) ->
    mem recurse (days - fish)

Day 6 {
Parse =
    fun input ->

    {|
        AllFish =
            input.[0]
            |> Util.stringSplit [","]
            |> Array.map int
    |}

Part1 =
    5934L, fun input ->
    input.AllFish
    |> Seq.sumBy (fishLife 80)

Part2 =
    26984457539L, fun input ->
    input.AllFish
    |> Seq.countBy id
    |> Seq.sumBy (fun (fish, count) -> fishLife 256 fish * int64 count)

TestInput =  """
3,4,3,1,2
"""
}
