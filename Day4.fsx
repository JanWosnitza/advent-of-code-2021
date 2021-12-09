// https://adventofcode.com/2021/day/4
#load "Advent.fsx"
open Advent

type DrawList = DrawList of int list

type Board = Board of int[][]


solution 4 """
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
<| fun input ->

let rounds =
    input
    |> Array.head
    |> Util.stringSplit [","]
    |> Seq.map int
    |> Seq.toList
    |> List.rev
    |> List.unfold (function [] -> None | drawList -> Some (DrawList drawList, List.tail drawList))
    |> List.rev

let boards =
    input
    |> Seq.tail
    |> Seq.chunkBySize 6
    |> Seq.map (fun x ->
        x
        |> Seq.tail // skip empty row
        |> Seq.map (
            Util.stringSplit ["  "; " "]
            >> Seq.map (Util.stringTrim >> int)
            >> Seq.toArray
        )
        |> Seq.toArray
        |> Board
    )
    |> Seq.toList

let isWin (Board board) (DrawList drawList) =
    let test (get) = [0 .. 4] |> List.exists (fun i -> [0 .. 4] |> List.forall (fun j -> drawList |> List.contains (get i j)))
    test (fun i j -> board.[i].[j])
    || test (fun i j -> board.[j].[i])

let boardsWithWinRound =
    boards
    |> List.map (fun board ->
        let round = rounds |> List.findIndex (isWin board)
        (round, board)
    )

let getScore (round, Board board) =
    let (DrawList drawList) = rounds.[round]
    let lastDrawnNumber = List.head drawList

    let sum =
        board
        |> Seq.collect id
        |> Seq.filter (fun x -> List.contains x drawList |> not)
        |> Seq.sum

    (lastDrawnNumber * sum)

{
    Part1 = 4512, fun () ->
        boardsWithWinRound
        |> List.minBy fst
        |> getScore

    Part2 = 1924, fun () ->
        boardsWithWinRound
        |> List.maxBy fst
        |> getScore
}
