open System

let stringTrim (x:string) = x.Trim()

let stringSplit (splits:string seq) (x:string) =
    x.Trim().Split(Array.ofSeq splits, StringSplitOptions.None)

let private tryGetSession () =
    let lastArg = fsi.CommandLineArgs |> Array.last
    if lastArg.Length <> 96 || lastArg.EndsWith(".fsx", StringComparison.InvariantCultureIgnoreCase)
    then None
    else Some lastArg

type AdventDay =
    {
        Day : int
        RawInput : string[]
    }

    member this.Answer(part1, ?part2) =
        match part2 with
        | None -> printfn $"Day {this.Day}: Part1 = {part1}"
        | Some part2 -> printfn $"Day {this.Day}: Part1 = {part1} | Part2 = {part2}"

let adventDay (dayOfAdvent:int) (testInput:string) =
    let rawText =
        match tryGetSession () with
        | None ->
            testInput
        | Some session ->            
            try
                use cl = new Net.Http.HttpClient()
                cl.DefaultRequestHeaders.Add("cookie", $"session={session}")
                cl.GetStringAsync($"https://adventofcode.com/2021/day/{dayOfAdvent}/input")
                |> Async.AwaitTask
                |> Async.RunSynchronously
            finally
                printfn ""

    {
        Day = dayOfAdvent
        RawInput =
            rawText
            |> stringTrim
            |> stringSplit ["\n"; "\r\n"]
    }
