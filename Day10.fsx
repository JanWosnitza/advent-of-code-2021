// https://adventofcode.com/2021/day/10
#load "Advent.fsx"
open Advent

type BracketType = Round | Square | Curly | Angle
type BracketKind = Open | Close

let parse = Input.toMultiline >> fun input ->
    let convert line =
        line
        |> Seq.map (function
            | '(' -> Open,  Round | ')' -> Close,  Round
            | '[' -> Open, Square | ']' -> Close, Square
            | '{' -> Open,  Curly | '}' -> Close,  Curly
            | '<' -> Open,  Angle | '>' -> Close,  Angle
            | c -> failwith $"Unexptected character {c}"
        )
        |> Seq.toList

    {|
        Lines =
            input
            |> Seq.map convert
            |> Seq.toList
    |}

type TestResult =
    | Valid
    | InvalidClose of BracketType
    | MissingCloses of BracketType list

let rec test (expectedClose) (rest) =
    match expectedClose, rest with
    | _, (Open, btype) :: rest' ->
        test (btype :: expectedClose) rest'

    | expectedBtype :: expectedClose', (Close, btype) :: rest'
        when btype = expectedBtype ->
        test expectedClose' rest'

    | [], [] -> Valid
    | _, (_, btype) :: _ -> InvalidClose btype
    | missing, [] -> MissingCloses missing

let part1 = parse >> fun input ->
    input.Lines
    |> Seq.map (test [])
    |> Seq.choose (function | InvalidClose btype -> Some btype | _ -> None)
    |> Seq.sumBy (function
        |  Round ->     3
        | Square ->    57
        |  Curly ->  1197
        |  Angle -> 25137
    )

let part2 = parse >> fun input ->
    let scores =
        input.Lines
        |> Seq.map (test [])
        |> Seq.choose (function | MissingCloses missing -> Some missing | _ -> None)
        |> Seq.map (fun missing ->
            missing
            |> Seq.map (function
                |  Round -> 1L
                | Square -> 2L
                |  Curly -> 3L
                |  Angle -> 4L
            )
            |> Seq.fold (fun s d -> s * 5L + d) 0L
        )
        |> Seq.sort
        |> Seq.toList
    
    scores.[scores.Length / 2]

///////////////////////
let testInput1 =  """
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
"""

AoC.Day 10 [
    AoC.Part part1 [
        testInput1, 26397
    ]
    AoC.Part part2 [
        testInput1, 288957L
    ]
]
