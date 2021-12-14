// https://adventofcode.com/2021/day/6
#load "Advent.fsx"
open Advent

let parse = Input.toMultiline >> fun input ->
    {|
        AllFish =
            input.[0]
            |> Input.split [","]
            |> Array.map int
    |}

let fishLife () =
    let toTree (fish) =
        if fish <= 0 then
            TreeLeaf 1I
        else
            TreeBranch [fish - 9; fish - 7]

    let fold = Util.treeFold toTree (+) 0I

    fun (days) (fish) ->
    (days - fish)
    |> fold

let part1 = parse >> fun input ->
    let fishLife = fishLife ()
    input.AllFish
    |> Seq.sumBy (fishLife 80)

let part2 = parse >> fun input ->
    let fishLife = fishLife ()
    input.AllFish
    |> Seq.countBy id
    |> Seq.sumBy (fun (fish, count) -> fishLife 256 fish * bigint count)

/////////////////////////////
let testInput1 =  """
3,4,3,1,2
"""

AoC.Day 6 [
    AoC.Part part1 [
        testInput1, 5934I
    ]
    AoC.Part part2 [
        testInput1, 26984457539I
    ]
]
