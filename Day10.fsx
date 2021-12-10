// https://adventofcode.com/2021/day/10
#load "Advent.fsx"
open Advent

type BracketType = Round | Square | Curly | Angle
type BracketKind = Open | Close

type TestResult =
    | Valid
    | InvalidClose of BracketType
    | MissingCloses of BracketType list

Day 10 {
Parse =
    fun input ->
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

    {|
        TestResults =
            input
            |> Seq.map (convert >> test [])
            |> Seq.toList
    |}

Part1 =
    26397, fun input ->
    input.TestResults
    |> Seq.choose (function | InvalidClose btype -> Some btype | _ -> None)
    |> Seq.sumBy (function
        |  Round ->     3
        | Square ->    57
        |  Curly ->  1197
        |  Angle -> 25137
    )

Part2 =
    288957L, fun input ->
    let scores =
        input.TestResults
        |> Seq.choose (function | MissingCloses missing -> Some missing | _ -> None)
        |> Seq.map (fun missing ->
            let res =
                missing
                |> Seq.map (function
                    |  Round -> 1L
                    | Square -> 2L
                    |  Curly -> 3L
                    |  Angle -> 4L
                )
                |> Seq.fold (fun s d -> s * 5L + d) 0L
            res
        )
        |> Seq.sort
        |> Seq.toList
    
    scores.[scores.Length / 2]

TestInput =  """
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
}
