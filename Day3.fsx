// https://adventofcode.com/2021/day/3
#load "Advent.fsx"
open Advent

let parse = Input.toMultiline >> fun input ->
    {|
        Rows =
            input
            |> Array.map (fun x ->
                x
                |> Seq.map (function '1' -> true | '0' -> false | c -> failwith $"Unexpectec char {c}")
                |> Seq.toArray
            )
    |}

let binaryToInteger (binary:bool seq) =
    (0, binary)
    ||> Seq.fold (fun value -> function true -> value * 2 + 1 | false -> value * 2)

let part1 = parse >> fun input ->
    let colCounts =
        input.Rows
        |> Seq.transpose
        |> Seq.map (Seq.countBy id)

    let GammaRate =
        colCounts
        |> Seq.map (Seq.maxBy snd >> fst)
        |> binaryToInteger

    let EpsilonRate =
        colCounts
        |> Seq.map (Seq.minBy snd >> fst)
        |> binaryToInteger

    GammaRate * EpsilonRate

let part2 = parse >> fun input ->
    let find (selector) (input) =
        let rec loop (index) (input) =
            let ones, zeros =
                input
                |> Array.partition (Array.item index)
            match selector zeros ones with
            | [||] -> failwith "No match found. Input?"
            | [|x|] -> x
            | xs -> loop (index + 1) xs
        loop 0 input

    let OxigenGeneratorRating =
        input.Rows
        |> find (fun zeros ones -> if ones.Length >= zeros.Length then ones else zeros)
        |> binaryToInteger

    let Co2ScrubberRating =
        input.Rows
        |> find (fun zeros ones -> if zeros.Length <= ones.Length then zeros else ones)
        |> binaryToInteger

    OxigenGeneratorRating * Co2ScrubberRating

///////////////////////////
let testInput1 =  """
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
"""

AoC.Day 3 [
    AoC.Part part1 [
        testInput1, 198
    ]
    AoC.Part part2 [
        testInput1, 230
    ]
]
