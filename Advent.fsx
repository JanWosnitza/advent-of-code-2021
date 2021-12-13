open System
open System.Diagnostics
open System.IO

module Util =
    let memoize () =
        let cache = System.Collections.Generic.Dictionary<_,_>()
        fun (f) (x) ->
        match cache.TryGetValue(x) with
        | true, count -> count
        | _ ->
            let ret = f x
            cache.Add(x, ret)
            ret

module Input =
    let trim (x:string) = x.Trim()

    let split (splits:string seq) (x:string) =
        x.Trim().Split(Array.ofSeq splits, StringSplitOptions.None)

    let toMultiline (x:string) =
        x
        |> trim
        |> split ["\n"; "\r\n"]

module AoC =
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
            None

    let private tryGetInput (dayOfAdvent:int) =
        session |> Option.map (fun (session) ->
        let dirCache = Path.Combine(dirSessions, $"{session.Name}_cache")
        Directory.CreateDirectory(dirCache) |> ignore

        let cachedPath = Path.Combine(dirCache, $"{dayOfAdvent}.txt")
        if File.Exists(cachedPath) then
            File.ReadAllText(cachedPath)
        else
            let response =
                use cl = new Net.Http.HttpClient()
                cl.DefaultRequestHeaders.Add("cookie", $"session={session.Key}")
                cl.GetStringAsync($"https://adventofcode.com/2021/day/{dayOfAdvent}/input")
                |> Async.AwaitTask
                |> Async.RunSynchronously
            File.WriteAllText(cachedPath, response)
            response
        )

    let private measure f x =
        let watch = Stopwatch.StartNew()
        let x = f x
        watch.Stop()
        (watch.Elapsed, x)

    type PartExec =
        | Test
        | Run of string

    type Part = Part of (string -> int -> PartExec -> unit)

    let Part (f:string->'a) (inputs:(string * 'a) list) =
        let inputs =
            inputs
            |> List.map (fun (input, expected) -> (input.TrimStart(), expected))

        let formatTime (time:TimeSpan) =
            let time = time.TotalSeconds.ToString("0.0000")
            $"[{time}s]"

        Part <| fun context part exec ->
        match exec with
        | Test ->
            inputs
            |> List.iteri (fun i (input, expected) ->
                printf $"{context} Part{part} Test{i + 1}"
                let time, result = measure f input
                printfn $" {formatTime time}"
                let result = f input
                if result <> expected then
                    printfn $"FAILURE:"
                    printfn $"{result}"
                else
                    printfn $"SUCCESS"
            )
        | Run input ->
            printf $"{context} Part{part}"
            let time, result = measure f input
            printfn $" {formatTime time}"
            printfn $"{result}"

    let Day (day:int) (tests:Part list) =
        let context = $"Day{day}"
        let exec =
            match tryGetInput day with
            | None -> Test
            | Some input -> Run input
        tests
        |> List.iteri (fun i (Part f) -> f context (i + 1) exec)
