// https://adventofcode.com/2021/day/4
#load "Advent.fsx"
open Advent

type Board = Board of int[][]

let parse = Input.toMultiline >> fun input ->
    let drawnNumbers = 
        input
        |> Array.head
        |> Input.split [","]
        |> Seq.map int
        |> Seq.toList

    let boards =
        input
        |> Seq.tail
        |> Seq.chunkBySize 6
        |> Seq.map (fun x ->
            x
            |> Seq.tail // skip empty row
            |> Seq.map (
                Input.split ["  "; " "]
                >> Seq.map (Input.trim >> int)
                >> Seq.toArray
            )
            |> Seq.toArray
            |> Board
        )
        |> Seq.toList

    {|
        DrawnNumbers = drawnNumbers
        Boards = boards
    |}

type Round = Round of int list

let toRounds (drawnNumbers:int list) =
    drawnNumbers
    |> List.rev
    |> List.unfold (function [] -> None | round -> Some (Round round, List.tail round))
    |> List.rev

let isWin (Board board) (Round round) =
    let test (get) = [0 .. 4] |> List.exists (fun i -> [0 .. 4] |> List.forall (fun j -> round |> List.contains (get i j)))
    test (fun i j -> board.[i].[j])
    || test (fun i j -> board.[j].[i])

let getWinningRound (rounds) (board) =
    let round = rounds |> List.findIndex (isWin board)
    (round, (rounds[round], board))

let getScore (Round drawList) (Board board) =
    let lastDrawnNumber = List.head drawList

    let sum =
        board
        |> Seq.collect id
        |> Seq.filter (fun x -> List.contains x drawList |> not)
        |> Seq.sum

    (lastDrawnNumber * sum)

let part1 = parse >> fun input ->
    let rounds = toRounds input.DrawnNumbers
    input.Boards
    |> List.map (getWinningRound rounds)
    |> List.minBy fst
    |> snd
    ||> getScore

let part2 = parse >> fun input ->
    let rounds = toRounds input.DrawnNumbers
    input.Boards
    |> List.map (getWinningRound rounds)
    |> List.maxBy fst
    |> snd
    ||> getScore

///////////////////////////////
let testInput1 = """
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
"""

AoC.Day 4 [
    AoC.Part part1 [
        testInput1, 4512
    ]
    AoC.Part part2 [
        testInput1, 1924
    ]
]
