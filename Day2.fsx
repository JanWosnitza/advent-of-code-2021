// https://adventofcode.com/2021/day/2
#load "Util.fsx"

let adventDay = Util.adventDay 2 """
forward 5
down 5
forward 8
up 3
down 8
forward 2
"""

let (|Command|_|) (command:string) (input:string) =
    if input.StartsWith(command) then
        input.Substring(command.Length + 1) |> int |> Some
    else
        None

type Command = Forward | Down | Up

let commands =
    adventDay.RawInput
    |> Seq.map (function
        | Command "forward" x -> Forward, x
        | Command "down" x -> Down, x
        | Command "up" x -> Up, x
        | input -> failwith $"Unexpected input {input}"
    )

module Part1 =
    type State =
        {Horizontal:int; Depth:int}
        static member Init = {Horizontal = 0; Depth = 0}

    let updateState (state:State) =
        function
        | Forward, x -> {state with Horizontal = state.Horizontal + x}
        | Down, x -> {state with Depth = state.Depth + x}
        | Up, x -> {state with Depth = state.Depth - x}

    let state =
        commands
        |> Seq.fold updateState State.Init

module Part2 =
    type State =
        {Horizontal:int; Depth:int; Aim:int}
        static member Init = {Horizontal = 0; Depth = 0; Aim = 0}

    let updateState (state:State) =
        function
        | Forward, x ->
            {state with
                Horizontal = state.Horizontal + x
                Depth = state.Depth + state.Aim * x
            }
        | Down, x -> {state with Aim = state.Aim + x}
        | Up, x -> {state with Aim = state.Aim - x}

    let state =
        commands
        |> Seq.fold updateState State.Init

adventDay.Answer(
    part1 = Part1.state.Horizontal * Part1.state.Depth,
    part2 = Part2.state.Horizontal * Part2.state.Depth
)
