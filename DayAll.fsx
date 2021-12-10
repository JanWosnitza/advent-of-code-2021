#load "Advent.fsx"

let watch = System.Diagnostics.Stopwatch.StartNew()

printfn "Running all days"
printfn ""

#load "Day1.fsx"
#load "Day2.fsx"
#load "Day3.fsx"
#load "Day4.fsx"
#load "Day5.fsx"
#load "Day6.fsx"
#load "Day7.fsx"
#load "Day8.fsx"
#load "Day9.fsx"
#load "Day10.fsx"

watch.Stop()

printfn ""
//       |===========================================|
printfn $"Finished Running All Days                  {watch.Elapsed}"