// https://adventofcode.com/2021/day/17
#load "Advent.fsx"
#r "nuget: FSharp.Text.RegexProvider"
open Advent

type Regex = FSharp.Text.RegexProvider.Regex< @"^target area: x=(?<X1>-?\d+)\.\.(?<X2>-?\d+), y=(?<Y1>-?\d+)\.\.(?<Y2>-?\d+)$" >

let parse = Input.toMultiline >> fun (input) ->
    let matches = Regex().TypedMatch(input[0])
    let x1 = matches.X1.Value |> int
    let x2 = matches.X2.Value |> int
    let y1 = matches.Y1.Value |> int
    let y2 = matches.Y2.Value |> int
    {|
        Source = {|
            X = 0
            Y = 0
        |}
        Target = {|
            XMin = min x1 x2
            XMax = max x1 x2
            YMin = min y1 y2
            YMax = max y1 y2
        |}
    |}

let updateXVelocity (vx) = vx - sign vx
let updateYVelocity (vy) = vy - 1

let getValues (updateVelocity) (sourcePosition) (sourceVelocity) =
    (sourcePosition, sourceVelocity)
    |> Seq.unfold (fun (pos, vel) ->
        let pos' = pos + vel
        Some (pos', (pos', updateVelocity vel))
    )

let part1 = parse >> fun input ->
    [0 .. 1000]
    |> Seq.map (getValues updateYVelocity input.Source.Y)
    |> Seq.map (Seq.takeWhile (fun pos -> input.Target.YMin <= pos))
    |> Seq.filter (Seq.exists (fun pos -> pos <= input.Target.YMax))
    |> Seq.last
    |> Seq.max

let part2 = parse >> fun input ->
    let xss = // indices inside
        [0 .. 1000]
        |> Seq.map (fun xVelocity ->
            xVelocity
            |> getValues updateXVelocity input.Source.X
            |> Seq.takeWhile (fun pos -> pos <= input.Target.XMax)
            |> Seq.cache
        )
        |> Seq.toList

    let yss = // indices inside
        [-1000 .. 1000]
        |> Seq.map (fun yVelocity ->
            yVelocity
            |> getValues updateYVelocity input.Source.Y
            |> Seq.takeWhile (fun pos -> pos >= input.Target.YMin)
            |> Seq.cache
        )
        |> Seq.toList

    Seq.allPairs xss yss
    |> Seq.filter (fun (xs, ys) ->
        Seq.zip xs ys
        |> Seq.exists (fun (x, y) -> x >= input.Target.XMin && y <= input.Target.YMax)
    )
    |> Seq.length

///////////////////

let testInput1 = """
target area: x=20..30, y=-10..-5
"""

AoC.Day 17 [
    AoC.Part part1 [
        testInput1, 45
    ]
    AoC.Part part2 [
        testInput1, 112
    ]
]
