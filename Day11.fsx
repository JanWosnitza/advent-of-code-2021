// https://adventofcode.com/2021/day/1
#load "Advent.fsx"
open Advent

type Octopus = {Energy : int}

type Position = {X : int; Y : int}

type State = Map<Position, Octopus>

let incEnergy (octopus) = {octopus with Energy = octopus.Energy + 1}
let zeroEnergy (octopus) = {octopus with Energy = 0}

let getNeighbours {X = x; Y = y} =
    [
        {X = x - 1; Y = y - 1}; {X = x + 0; Y = y - 1}; {X = x + 1; Y = y - 1}
        {X = x - 1; Y = y + 0};                         {X = x + 1; Y = y + 0}
        {X = x - 1; Y = y + 1}; {X = x + 0; Y = y + 1}; {X = x + 1; Y = y + 1}
    ]

let flashAt (position:Position) (state:State) : State =
    let state =
        state
        |> Map.change position (Option.map zeroEnergy)

    (state, position |> getNeighbours)
    ||> List.fold (fun state pos ->
        state
        |> Map.change pos (Option.map (fun ocotpus ->
            if ocotpus.Energy > 0
            then incEnergy ocotpus
            else ocotpus
        ))
    )

let tryDoFlashes (state:State) =
    let flashPositions =
        state
        |> Map.toSeq
        |> Seq.filter (fun (_, octopus) -> octopus.Energy > 9)
        |> Seq.map fst
        |> Seq.toList

    if flashPositions.Length = 0 then
        None
    else
        (flashPositions, state)
        ||> Seq.foldBack flashAt
        |> Some

let step (state:State) =        
    let state =
        state
        |> Map.map (fun _ -> incEnergy)

    state
    |> List.unfold (tryDoFlashes >> Option.map (fun state -> (state, state)))
    |> List.tryLast
    |> Option.defaultValue state

let steps (state:State) =
    state
    |> Seq.unfold (step >> (fun state -> Some (state, state)))

Day 11 {
Parse =
    fun input ->
    {|
        State =
            input
            |> Seq.mapi (fun x row ->
                row
                |> Seq.mapi (fun y c ->
                    let pos = {X = x; Y = y}
                    let octo = {Energy = int c - int '0'}
                    pos, octo
                )
            )
            |> Seq.collect id
            |> Map.ofSeq
    |}

Part1 =
    1656, fun input ->
    input.State
    |> steps
    |> Seq.take 100
    |> Seq.sumBy (fun state ->
        state
        |> Map.filter (fun _ {Energy=energy} -> energy = 0)
        |> Map.count
    )

Part2 =
    195, fun input ->
    input.State
    |> steps
    |> Seq.findIndex (Map.forall (fun _ {Energy=energy} -> energy = 0))
    |> (+) 1

TestInput = """
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
"""
}