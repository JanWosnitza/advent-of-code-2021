//https://adventofcode.com/2021/day/3
#load "Util.fsx"

Util.useTestInput ()
let input = Util.getInput "https://adventofcode.com/2021/day/3/input" """
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

let binaryToInteger (binary:bool seq) =
    (0, binary)
    ||> Seq.fold (fun value -> function true -> value * 2 + 1 | false -> value * 2)

module Part1 =
    type State =
        {}
    
    let input =
        let matrix =
            input
            |> Seq.map (fun x ->
                x
                |> Seq.map (function '1' -> true | '0' -> false | c -> failwith $"Unexpectec char {c}")
                |> Seq.toArray
            )
            |> Seq.toArray

        Seq.init matrix.[0].Length (fun col -> matrix |> Seq.map (fun row -> row.[col]))
        |> Seq.map (Seq.countBy id)

    let GammaRate =
        input
        |> Seq.map (Seq.maxBy snd >> fst)
        |> binaryToInteger

    let EpsilonRate =
        input
        |> Seq.map (Seq.minBy snd >> fst)
        |> binaryToInteger

    (GammaRate * EpsilonRate)
    |> printfn "Part 1 = %O"

module Part2 =
    let input =
        input
        |> Seq.map (fun x ->
            x
            |> Seq.map (function '1' -> true | '0' -> false | c -> failwith $"Unexpectec char {c}")
            |> Seq.toArray
        )
        |> Seq.toArray

    let find (selector) (input) =
        let rec loop (index) (input) =
            let ones, zeros =
                input
                |> Array.partition (Array.item index)
            match selector zeros ones with
            | [|x|] -> x
            | xs -> loop (index + 1) xs
        loop 0 input

    let OxigenGeneratorRating =
        input
        |> find (fun zeros ones -> if ones.Length >= zeros.Length then ones else zeros)
        |> binaryToInteger

    let Co2ScrubberRating =
        input
        |> find (fun zeros ones -> if zeros.Length <= ones.Length then zeros else ones)
        |> binaryToInteger
    
    (OxigenGeneratorRating * Co2ScrubberRating)
    |> printfn "Part 2 = %O"
