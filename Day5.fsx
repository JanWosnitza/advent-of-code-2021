// https://adventofcode.com/2021/day/5
#load "Advent.fsx"
#r "nuget: FSharp.Text.RegexProvider"
open Advent

type Regex = FSharp.Text.RegexProvider.Regex< @"^(?<X1>\d+),(?<Y1>\d+) -> (?<X2>\d+),(?<Y2>\d+)$" >

type Vent = {X1 : int; Y1 : int; X2 : int; Y2 : int}

solution 5 """
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
"""
<| fun input ->

let vents =
    input
    |> Array.map (fun x ->
        let tm = Regex().TypedMatch(x)
        {
            X1 = tm.X1.Value |> int
            Y1 = tm.Y1.Value |> int
            X2 = tm.X2.Value |> int
            Y2 = tm.Y2.Value |> int
        }
    )

let getPositions (vent:Vent) =
    let signX = sign (vent.X2 - vent.X1)
    let signY = sign (vent.Y2 - vent.Y1)
    let size = if vent.X2 <> vent.X1 then abs (vent.X2 - vent.X1) else abs (vent.Y2 - vent.Y1)
    seq { for i = 0 to size do yield (vent.X1 + i * signX, vent.Y1 + i * signY) }

let overlaps (vents) =
    vents
    |> Seq.collect getPositions
    |> Seq.countBy id
    |> Seq.filter (fun (_, count) -> count >= 2)
    |> Seq.length

{
    Part1 = 5, fun () ->
        vents
        |> Seq.filter (fun vent -> vent.X1 = vent.X2 || vent.Y1 = vent.Y2)
        |> overlaps

    Part2 = 12, fun () ->
        vents
        |> overlaps
}
