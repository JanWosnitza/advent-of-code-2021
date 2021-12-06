// https://adventofcode.com/2021/day/2
#load "Util.fsx"

Util.useTestInput ()
let input = Util.getInput "https://adventofcode.com/2021/day/2/input" """
forward 5
down 5
forward 8
up 3
down 8
forward 2
"""

module Part1 =
    type State =
        {Horizontal:int; Depth:int}
        override this.ToString() = $"{this.Horizontal * this.Depth}"
        static member Init = {Horizontal = 0; Depth = 0}

    let (|Command|_|) (command:string) (input:string) =
        if input.StartsWith(command) then
            input.Substring(command.Length + 1) |> int |> Some
        else
            None

    let updateState (state:State) =
        function
        | Command "forward" x -> {state with Horizontal = state.Horizontal + x}
        | Command "down" x -> {state with Depth = state.Depth + x}
        | Command "up" x -> {state with Depth = state.Depth - x}
        | input -> failwith $"Unexpected input {input}"

    let state =
        input
        |> Seq.fold updateState State.Init
    
    state
    |> printfn "Part 1 = %O"    

module Part2 =
    type State =
        {Horizontal:int; Depth:int; Aim:int}
        override this.ToString() = $"{this.Horizontal * this.Depth}"
        static member Init = {Horizontal = 0; Depth = 0; Aim = 0}

    let (|Command|_|) (command:string) (input:string) =
        if input.StartsWith(command) then
            input.Substring(command.Length + 1) |> int |> Some
        else
            None

    let updateState (state:State) =
        function
        | Command "forward" x ->
            {state with
                Horizontal = state.Horizontal + x
                Depth = state.Depth + state.Aim * x
            }
        | Command "down" x -> {state with Aim = state.Aim + x}
        | Command "up" x -> {state with Aim = state.Aim - x}
        | input -> failwith $"Unexpected input {input}"

    let state =
        input
        |> Seq.fold updateState State.Init

    state
    |> printfn "Part 2 = %O"
