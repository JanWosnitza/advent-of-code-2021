// https://adventofcode.com/2021/day/20
#load "Advent.fsx"
open Advent

type Enhancement = Map<int, bool>
type Image = Map<int * int, bool>

let parse = Input.toMultiline >> fun input ->
    {|
        Enhance =
            input[0]
            |> Seq.mapi (fun i ->
                function
                | '.' -> (i, false)
                | '#' -> (i, true)
                | c -> failwith $"Invalid input {c}")
            |> Map.ofSeq
            : Enhancement

        Image =
            input
            |> Seq.skip 2
            |> Seq.mapi (fun y ->
                Seq.mapi (fun x -> function
                    | '.' -> ((x, y), false)
                    | '#' -> ((x, y), true)
                    | c -> failwith $"Invalid input {c}"
                )
            )
            |> Seq.collect id
            |> Map.ofSeq
            : Image
    |}

let binaryToInteger (binary:bool seq) =
    (0, binary)
    ||> Seq.fold (fun value -> function true -> value * 2 + 1 | false -> value * 2)

type ImageEx =
    {
        OutsidePixels : bool
        Pixels : Image
    }

module ImageEx =
    let getPixels (image:ImageEx) = image.Pixels

    let getBounds (image) =
        let xMin, xMax =
            let x = getPixels image |> Map.toSeq |> Seq.map (fst >> fst)
            Seq.min x, Seq.max x

        let yMin, yMax =
            let y = getPixels image |> Map.toSeq |> Seq.map (fst >> snd)
            Seq.min y, Seq.max y

        (xMin, xMax), (yMin, yMax)

    let getAt (position) (image) =
        getPixels image
        |> Map.tryFind position
        |> Option.defaultValue image.OutsidePixels

    let enhance (enhancement:Enhancement) (image:ImageEx) =
        let (xMin, xMax), (yMin, yMax) = getBounds image

        let pixels =
            seq {
                for x = xMin - 1 to xMax + 1 do
                for y = yMin - 1 to yMax + 1 do
                let enhanceIdx =
                    [
                        for y = -1 to 1 do
                        for x = -1 to 1 do
                        yield (x, y)
                    ]
                    |> Seq.map (fun (dx, dy) -> image |> getAt (x + dx, y + dy))
                    |> binaryToInteger
                yield (x, y), enhancement |> Map.find enhanceIdx
            }
            |> Map.ofSeq

        {
            OutsidePixels = image.OutsidePixels <> (enhancement |> Map.find 0)
            Pixels = pixels
        }

    let rec enhanceN (count) (enhancement) (image) =
        if count <= 0 then
            image
        else
            image
            |> enhance enhancement
            |> enhanceN (count - 1) (enhancement)

let countLitPixels (image:Image) =
    image
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.filter id
    |> Seq.length

let part1 = parse >> fun input ->
    {
        OutsidePixels = false
        Pixels = input.Image
    }
    |> ImageEx.enhanceN 2 input.Enhance
    |> ImageEx.getPixels
    |> countLitPixels

let part2 = parse >> fun input ->
    {
        OutsidePixels = false
        Pixels = input.Image
    }
    |> ImageEx.enhanceN 50 input.Enhance
    |> ImageEx.getPixels
    |> countLitPixels

///////////////////////////////

let testInput1 = """
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
"""

AoC.Day 20 [
    AoC.Part part1 [
        testInput1, 35
    ]
    AoC.Part part2 [
        testInput1, 3351
    ]
]
