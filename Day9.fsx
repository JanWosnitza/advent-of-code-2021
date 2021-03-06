// https://adventofcode.com/2021/day/9
#load "Advent.fsx"
open Advent

let parse = Input.toMultiline >> fun input ->
    {|
        Heightmap =
            input
            |> Seq.mapi (fun x row ->
                row
                |> Seq.mapi (fun y c -> ((x, y), int c - int '0'))
                |> Seq.toList
            )
            |> Seq.collect id
            |> Map.ofSeq
    |}

let getNeighbours (x, y) =
    [
        x - 1, y; x + 1, y
        x, y - 1; x, y + 1
    ]

let part1 = parse >> fun input ->
    let isLowest (position, height) =
        getNeighbours position
        |> Seq.choose (fun position -> Map.tryFind position input.Heightmap)
        |> Seq.forall (fun neighbourHeight -> height < neighbourHeight)

    input.Heightmap
    |> Map.toSeq
    |> Seq.filter isLowest
    |> Seq.map (snd >> (+)1)
    |> Seq.toList
    |> Seq.sum

let part2 = parse >> fun input ->
    let rec findNeighboursRecursive (validPositions) (visited) (position) =
        if Set.contains position visited then
            visited
        elif not <| Set.contains position validPositions then
            visited
        else
            (Set.add position visited, getNeighbours position)
            ||> Seq.fold (findNeighboursRecursive validPositions)

    let groupConnected (positions:Set<int * int>) =
        positions
        |> List.unfold (fun positions ->
            if positions.IsEmpty then None else
            let position = Set.minElement positions
            let set = findNeighboursRecursive positions Set.empty position
            Some (set, Set.difference positions set)
        )

    let basins =
        input.Heightmap
        |> Map.toSeq
        |> Seq.choose (fun (position, height) -> if height < 9 then Some position else None)
        |> Set.ofSeq
        |> groupConnected

    basins
    |> Seq.map Set.count
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce (*)

/////////////////////////

let testInput1 =  """
2199943210
3987894921
9856789892
8767896789
9899965678
"""

AoC.Day 9 [
    AoC.Part part1 [
        testInput1, 15
    ]
    AoC.Part part2 [
        testInput1, 1134
    ]
]
