// https://adventofcode.com/2021/day/2
#load "Advent.fsx"
open Advent

type Command = Forward | Down | Up

type State1 = {Horizontal:int; Depth:int}

type State2 = {Horizontal:int; Depth:int; Aim:int}

Day 2 {
Parse =
    fun input ->
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

Part1 =
    150, fun input ->
    let state =
        ({Horizontal = 0; Depth = 0}, input.Commands)
        ||> Seq.fold (fun state ->
            function
            | Forward, x -> {state with Horizontal = state.Horizontal + x}
            | Down, x -> {state with Depth = state.Depth + x}
            | Up, x -> {state with Depth = state.Depth - x}
        )

    state.Horizontal * state.Depth

Part2 =
    900, fun input ->
    let state =
        ({Horizontal = 0; Depth = 0; Aim = 0}, input.Commands)
        ||> Seq.fold (fun state ->
            function
            | Forward, x ->
                {state with
                    Horizontal = state.Horizontal + x
                    Depth = state.Depth + state.Aim * x
                }
            | Down, x -> {state with Aim = state.Aim + x}
            | Up, x -> {state with Aim = state.Aim - x}
        )
    
    state.Horizontal * state.Depth
}

<| """
forward 5
down 5
forward 8
up 3
down 8
forward 2
"""
