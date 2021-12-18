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
#load "Day11.fsx"
#load "Day12.fsx"
#load "Day13.fsx"
#load "Day14.fsx"
#load "Day15.fsx"
#load "Day16.fsx"
#load "Day17.fsx"
#load "Day18.fsx"

watch.Stop()

printfn ""
//       |===========================================|
printfn $"Finished Running All Days                  {watch.Elapsed}"
