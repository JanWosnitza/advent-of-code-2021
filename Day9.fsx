// https://adventofcode.com/2021/day/9
#load "Util.fsx"

open System

let day = Util.adventDay 9 """
2199943210
3987894921
9856789892
8767896789
9899965678
"""

let heightmap =
    day.RawInput
    |> Seq.mapi (fun x row ->
        row
        |> Seq.mapi (fun y c -> ((x, y), int c - int '0'))
        |> Seq.toList
    )
    |> Seq.collect id
    |> Map.ofSeq

let getNeighbours (x, y) =
    [
        x - 1, y; x + 1, y
        x, y - 1; x, y + 1
    ]

module Part1 =
    let risks =
        heightmap
        |> Map.toSeq
        |> Seq.filter (fun (position, height) ->
            getNeighbours position
            |> Seq.choose (fun position -> Map.tryFind position heightmap)
            |> Seq.forall (fun neighbourHeight -> height < neighbourHeight)
        )
        |> Seq.map (snd >> (+)1)
        |> Seq.toList

module Part2 =
    let rec findNeighboursRecursive (validPositions) (visited) (position) =
        if Set.contains position visited then
            visited
        elif not <| Set.contains position validPositions then
            visited
        else
            (Set.add position visited, getNeighbours position)
            ||> Seq.fold (findNeighboursRecursive validPositions)

    let basinsSizes =
        heightmap
        |> Map.toSeq
        |> Seq.choose (fun (position, height) -> if height < 9 then Some position else None)
        |> Set.ofSeq
        |> List.unfold (fun positions ->
            if positions.IsEmpty then None else
            let position = Set.minElement positions
            let set = findNeighboursRecursive positions Set.empty position
            Some (Set.count set, Set.difference positions set)
        )
        |> List.sortDescending

day.Answer(
    part1 = (Part1.risks |> Seq.sum),
    part2 = (Part2.basinsSizes |> List.sortDescending |> Seq.take 3 |> Seq.reduce (*))
)
