// https://adventofcode.com/2021/day/12
#load "Advent.fsx"
open Advent

type Point = Point of (int * int)
type FoldCommand = FoldX of int | FoldY of int

let parse = Input.toMultiline >> fun input ->
    let idx = input |> Array.findIndex ((=)"")
    {|
        Points =
            input.[.. idx - 1]
            |> Array.map (fun row ->
                let xy = row |> Input.split [","]
                Point (int xy.[0], int xy.[1])
            )
    
        Folds =
            input.[idx + 1 ..]
            |> Array.map (fun row ->
                if row.StartsWith("fold along y=") then
                    row.Substring(13)
                    |> int
                    |> FoldY
                elif row.StartsWith("fold along x=") then
                    row.Substring(13)
                    |> int
                    |> FoldX
                else
                    failwith $"Invalid Input {row}"
            )
    |}

let fold (foldCommand) =
    let inline mirror (axis) (x) = axis * 2 - x
    match foldCommand with
    | FoldX foldX ->  
        fun (Point (x, y)) ->      
        if x > foldX then
            Point (mirror foldX x, y)
        else
            Point (x, y) 
    | FoldY foldY ->
        fun (Point (x, y)) ->
        if y > foldY then
            Point (x, mirror foldY y)
        else
            Point (x, y)

let part1 = parse >> fun input ->
    input.Points
    |> Seq.map (fold input.Folds.[0])
    |> Seq.distinct
    |> Seq.length

let part2 = parse >> fun input ->
    let points =
        (input.Points |> Array.toSeq, input.Folds)
        ||> Seq.fold (fun points foldCommand ->
            points
            |> Seq.map (fold foldCommand)
        )
        |> Set.ofSeq

    let maxX = points |> Set.toSeq |> Seq.map (fun (Point (x, _)) -> x) |> Seq.max
    let maxY = points |> Set.toSeq |> Seq.map (fun (Point (_, y)) -> y) |> Seq.max

    let rows =
        [0 .. maxY]
        |> Seq.map (fun y ->
            [0 .. maxX]
            |> Seq.map (fun x ->
                if Set.contains (Point (x,y)) points then '#' else ' '
            )
            |> System.String.Concat
        )
    System.String.Join('\n', rows)

////////////
let testInput1 = """
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
"""

let testOutput1_Part2 =
    System.String.Join('\n', [
        "#####"
        "#   #"
        "#   #"
        "#   #"
        "#####"
    ])

AoC.Day 13 [
    AoC.Part part1 [
        testInput1, 17
    ]
    AoC.Part part2 [
        testInput1, testOutput1_Part2
    ]
]