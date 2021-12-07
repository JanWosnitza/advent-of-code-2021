//https://adventofcode.com/2021/day/3
#load "Util.fsx"

let adventDay = Util.adventDay 3 """
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

let rows =
    adventDay.RawInput
    |> Array.map (fun x ->
        x
        |> Seq.map (function '1' -> true | '0' -> false | c -> failwith $"Unexpectec char {c}")
        |> Seq.toArray
    )

let binaryToInteger (binary:bool seq) =
    (0, binary)
    ||> Seq.fold (fun value -> function true -> value * 2 + 1 | false -> value * 2)

module Part1 =
    let colCounts =
        rows
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

module Part2 =
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
        rows
        |> find (fun zeros ones -> if ones.Length >= zeros.Length then ones else zeros)
        |> binaryToInteger

    let Co2ScrubberRating =
        rows
        |> find (fun zeros ones -> if zeros.Length <= ones.Length then zeros else ones)
        |> binaryToInteger

adventDay.Answer(
    part1 = Part1.GammaRate * Part1.EpsilonRate,
    part2 = Part2.OxigenGeneratorRating * Part2.Co2ScrubberRating
)