// https://adventofcode.com/2021/day/2
#load "Advent.fsx"
open Advent

type Command = Forward | Down | Up

let parse = Input.toMultiline >> fun input ->
    let (|Command|_|) (command:string) (input:string) =
        if input.StartsWith(command) then
            input.Substring(command.Length + 1) |> int |> Some
        else
            None

    {|
        Commands =
            input
            |> Seq.map (function
                | Command "forward" x -> Forward, x
                | Command "down" x -> Down, x
                | Command "up" x -> Up, x
                | input -> failwith $"Unexpected input {input}"
            )
    |}

type State1 = {Horizontal:int; Depth:int}

module State1 =
    let update (command) (state:State1) =
        match command with
        | Forward, x -> {state with Horizontal = state.Horizontal + x}
        | Down, x -> {state with Depth = state.Depth + x}
        | Up, x -> {state with Depth = state.Depth - x}

let part1 = parse >> fun input ->
    let state =
        ({Horizontal = 0; Depth = 0}, input.Commands)
        ||> Seq.fold (fun state command -> State1.update command state)

    state.Horizontal * state.Depth

type State2 = {Horizontal:int; Depth:int; Aim:int}

module State2 =
    let update (command) (state:State2) =
        match command with
        | Forward, x ->
            {state with
                Horizontal = state.Horizontal + x
                Depth = state.Depth + state.Aim * x
            }
        | Down, x -> {state with Aim = state.Aim + x}
        | Up, x -> {state with Aim = state.Aim - x}

let part2 = parse >> fun input ->
    let state =
        ({Horizontal = 0; Depth = 0; Aim = 0}, input.Commands)
        ||> Seq.fold (fun state command -> State2.update command state)
    
    state.Horizontal * state.Depth

////////////////////////////
let input1 = """
forward 5
down 5
forward 8
up 3
down 8
forward 2
"""

AoC.Day 2 [
    AoC.Part part1 [
        input1, 150
    ]
    AoC.Part part2 [
        input1, 900
    ]
]