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

type Session = {Name:string; Key:string}

let private dirSessions = Path.Combine(__SOURCE_DIRECTORY__ , "sessions")

let private session =
    let lastArg = System.Environment.GetCommandLineArgs() |> Array.last
    let sessionPath = Path.Combine(dirSessions, lastArg)
    
    if File.Exists(sessionPath) then
        let key = IO.File.ReadAllText(sessionPath).Trim()
        Some {Name = lastArg; Key = key}
    elif lastArg.Length = 96 && (not <| lastArg.EndsWith(".fsx", StringComparison.InvariantCultureIgnoreCase)) then
        Some {Name = lastArg; Key = lastArg}
    else
        printfn $"No Session => Test Mode"
        None

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
    | Some {Name=sessionName; Key=sessionKey} ->
        let rawInput =
            let dirCache = Path.Combine(dirSessions, $"{sessionName}_cache")
            Directory.CreateDirectory(dirCache) |> ignore

            let cachedPath = Path.Combine(dirCache, $"{dayOfAdvent}.txt")
            if File.Exists(cachedPath) then
                File.ReadAllText(cachedPath)
            else
                let response =
                    use cl = new Net.Http.HttpClient()
                    cl.DefaultRequestHeaders.Add("cookie", $"session={sessionKey}")
                    cl.GetStringAsync($"https://adventofcode.com/2021/day/{dayOfAdvent}/input")
                    |> Async.AwaitTask
                    |> Async.RunSynchronously
                File.WriteAllText(cachedPath, response)
                response

        rawInput
        |> prep Real

type NotImplemented = struct end
let NotImplemented = NotImplemented(), fun _ -> NotImplemented()

type AdventDay<'input, 'answer1, 'answer2> =
    {
        Parse : string[] -> 'input
        Part1 : 'answer1 * ('input -> 'answer1)
        Part2 : ('answer2 * ('input -> 'answer2))
        TestInput : string
    }

let private measure (message1:string) (f) (message2:_->string) =
    printf "%s" message1
    let watch = Stopwatch.StartNew()
    let x = f ()
    watch.Stop()
    let message2 = message2 x
    printfn $" {message2} {String(' ', max 1 (40 - message1.Length - message2.Length) )} {watch.Elapsed}"

    x

let Day (day:int) (data:AdventDay<_,_,_>) =
    printfn $"Day {day} - https://adventofcode.com/2021/day/{day}"

    let inputType, input = getInput day data.TestInput

    let input =
        measure
            "  Parsing:"
            (fun () -> data.Parse input)
            (fun _ -> "OK")

    let test (name) (expected:'answer, solve) =
        if typeof<'answer> = typeof<NotImplemented> then
            printfn $"  {name}: NO IMPLEMENTAION"
        else
            measure
                $"  {name}:"
                (fun () -> solve input)
                (fun result ->
                    match inputType with
                    | Test when result = expected -> $"SUCCESS"
                    | Test -> $"FAILED with {result}"
                    | Real -> $"{result}"
                )
            |> ignore

    data.Part1 |> test "Part 1"
    data.Part2 |> test "Part 2"
    // TODO Post solution to webserver and show result XDDDD
