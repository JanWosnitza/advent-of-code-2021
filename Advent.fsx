open System
open System.Diagnostics
open System.IO

module Util =
    let stringTrim (x:string) = x.Trim()

    let stringSplit (splits:string seq) (x:string) =
        x.Trim().Split(Array.ofSeq splits, StringSplitOptions.None)

    let memoize () =
        let cache = System.Collections.Generic.Dictionary<_,_>()
        fun (f) (x) ->
        match cache.TryGetValue(x) with
        | true, count -> count
        | _ ->
            let ret = f x
            cache.Add(x, ret)
            ret

open Util

let private session =
    let lastArg = System.Environment.GetCommandLineArgs() |> Array.last
    let sessionPath = Path.Combine(__SOURCE_DIRECTORY__ , "sessions", lastArg)
    
    if File.Exists(sessionPath) then
        IO.File.ReadAllText(sessionPath).Trim()
        |> Some
    elif lastArg.Length = 96 && (not <| lastArg.EndsWith(".fsx", StringComparison.InvariantCultureIgnoreCase)) then
        Some lastArg
    else
        None

if Option.isNone session then
    printfn $"No Session => Test Mode"

type InputType = Test | Real

let private getInput (dayOfAdvent:int) (testInput:string) =
    let prep (inputType) (raw) =
        inputType,
        raw
        |> stringTrim
        |> stringSplit ["\n"; "\r\n"]

    match session with
    | None ->
        testInput
        |> prep Test
    | Some session ->
        use cl = new Net.Http.HttpClient()
        cl.DefaultRequestHeaders.Add("cookie", $"session={session}")
        cl.GetStringAsync($"https://adventofcode.com/2021/day/{dayOfAdvent}/input")
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> prep Real

type Solution<'a> =
    {
        Expected : 'a
        Solve : unit -> 'a
    }

type ISolutions<'p1, 'p2> =
    abstract Part1 : 'p1 * (unit -> 'p1)
    abstract Part2 : ('p2 * (unit -> 'p2)) option

type Solutions1<'p1> =
    {
        Part1 : 'p1 * (unit -> 'p1)
    }
    interface ISolutions<'p1, unit> with
        member this.Part1 = this.Part1
        member this.Part2 = None

type Solutions2<'p1, 'p2> =
    {
        Part1 : 'p1 * (unit -> 'p1)
        Part2 : 'p2 * (unit -> 'p2)
    }
    interface ISolutions<'p1, 'p2> with
        member this.Part1 = this.Part1
        member this.Part2 = Some this.Part2

let private measure (message1:string) (f) (message2:_->string) =
    printf "%s" message1
    let watch = Stopwatch.StartNew()
    let x = f ()
    watch.Stop()
    let message2 = message2 x    
    printfn $" {message2} {String(' ', 40 - message1.Length - message2.Length)} {watch.Elapsed}"

    x

let solution (dayOfAdvent:int) (testInput:string) (getSolutions:string[]->#ISolutions<_,_>) =
    printfn $"Day {dayOfAdvent} : https://adventofcode.com/2021/day/{dayOfAdvent}"

    let inputType, input =
        measure
            "  Fetching Data:"
            (fun () -> getInput dayOfAdvent testInput)
            (fun _ -> "OK")
    
    let solutions =
        measure
            "  Parsing Input:"
            (fun () -> getSolutions input)
            (fun _ -> "OK")

    let test (name) (expected, solve) =
        measure
            $"  {name}:"
            solve
            (fun result ->
                match inputType with
                | Test when result = expected -> $"SUCCESS"
                | Test -> $"FAILED with {result}"
                | Real -> $"{result}"
            )
        |> ignore

    solutions.Part1 |> test "Part 1"
    solutions.Part2 |> Option.iter (test "Part 2")
    // TODO Post solution to webserver and show result XDDDD
