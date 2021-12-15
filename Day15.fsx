// https://adventofcode.com/2021/day/15
#load "Advent.fsx"
#r "nuget: FSharpx.Collections, 3.0.1"
open Advent
open FSharpx.Collections

[<Struct>]
type Risk =
    | Risk of int
    static member (+) (Risk a, Risk b) = Risk (a + b)

let parse = Input.toMultiline >> fun input ->
    {|
        Size = (input[0].Length, input.Length)
        Risks =
            input
            |> Seq.mapi (fun x row ->
                row
                |> Seq.mapi (fun y c -> ((x, y), Risk (int c - int '0')))
            )
            |> Seq.collect id
            |> Map.ofSeq
    |}

let tryAstar (getNeighbours:'Node -> list<'Node>) (getDistance:'Distance -> 'Node -> 'Distance * 'Distance) (source:'Node, sourceDistance:'Distance) (target:'Node) =
    let rec search (closedSet:Set<'Node>) (openSet:Heap<'Distance * 'Distance * 'Node * list<'Node>>) =
        match Heap.tryUncons openSet with
        | None -> None
        | Some ((_, distance, currentNode, path), openSet) ->
            if currentNode = target then
                Some (distance, currentNode :: path)
            elif closedSet |> Set.contains currentNode then
                search closedSet openSet
            else
                let openSet =
                    (openSet, getNeighbours currentNode)
                    ||> List.fold (fun openSet neighbourNode ->
                        let estimatedDistance, distance = getDistance distance neighbourNode
                        openSet
                        |> Heap.insert (estimatedDistance, distance, neighbourNode, currentNode :: path)
                    )

                let closedSet =
                    closedSet
                    |> Set.add currentNode

                search closedSet openSet

    let openSet =
        let estimatedDistance, distance = getDistance sourceDistance source
        Heap.empty false
        |> Heap.insert (estimatedDistance, distance, source, [])

    let closedSet =
        Set.empty
    
    search closedSet openSet

let getNeighbours (map) (x, y) =
    [
        x - 1, y
        x + 1, y
        x, y - 1
        x, y + 1
    ]
    |> List.filter (fun position -> map |> Map.containsKey position)

let estimateRisk (map) (targetX, targetY) risk (x, y) =
    let risk = risk + (map |> Map.find (x, y))
    Risk (abs (targetX - x) + abs (targetY - y)) + risk, risk

let part1 = parse >> fun input ->
    let sizeX, sizeY = input.Size

    let map = input.Risks

    let source = 0, 0
    let target = sizeX - 1, sizeY - 1

    let (Risk sourceRisk) = map |> Map.find source
    tryAstar (getNeighbours map) (estimateRisk map target) (source, Risk (-sourceRisk)) target
    |> Option.get
    |> fst

let part2 = parse >> fun input ->
    let sizeX, sizeY = input.Size

    let map =
        input.Risks
        |> Map.toSeq
        |> Seq.collect (fun ((x, y), (Risk risk)) ->
            seq {
                for ix = 0 to 4 do
                for iy = 0 to 4 do
                    yield (
                        ((sizeX * ix) + x, (sizeY * iy) + y),
                        Risk (((risk - 1 + ix + iy) % 9) + 1)
                    )
            }
        )
        |> Map.ofSeq

    let source = 0, 0
    let target = sizeX * 5 - 1, sizeY * 5 - 1
    let (Risk sourceRisk) = map |> Map.find source
    tryAstar (getNeighbours map) (estimateRisk map target) (source, Risk (-sourceRisk)) target
    |> Option.get
    |> fst

///////////////////////////////

let testInput1 = """
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
"""

AoC.Day 15 [
    AoC.Part part1 [
        testInput1, Risk 40
    ]
    AoC.Part part2 [
        testInput1, Risk 315
    ]
]
