open System

let private sessionId = System.IO.File.ReadLines(".sessionid") |> Seq.head

let mutable private isUseTestInput = false
let useTestInput () = isUseTestInput <- true

let getInput (urlInput:string) (testInput:string) =
    let text =
        if isUseTestInput then
            testInput.Trim()
        else
            printfn "Downloading..."
            use cl = new System.Net.Http.HttpClient()
            cl.DefaultRequestHeaders.Add("cookie", $"session={sessionId}")
            cl.GetStringAsync(urlInput)
            |> Async.AwaitTask
            |> Async.RunSynchronously
    
    text.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.ofArray
