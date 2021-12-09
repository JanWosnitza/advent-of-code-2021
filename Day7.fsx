// https://adventofcode.com/2021/day/7
#load "Advent.fsx"
open Advent

solution 7 """
16,1,2,0,4,2,7,1,2,14
"""
<| fun input ->

let hpositions =
    input.[0]
    |> Util.stringSplit [","]
    |> Array.map int
    |> Array.sort

let medianPosition = hpositions.[(hpositions.Length + 1) / 2]

{
    Part1 = 37, fun () ->
        hpositions
        |> Array.sumBy ((-) medianPosition >> abs)
    
    Part2 = 168, fun () ->
        let averagePosition =
            hpositions
            |> Seq.averageBy float
            |> round
            |> int

        let getFuel (position) =
            hpositions
            |> Array.sumBy (fun p ->
                let n = abs (p - position)
                (n * n + n) / 2 // == n * (n + 1) / 2
            )

        // With fuel consumtption "n * n" average would be the best position but the "n" term skews it of a little.
        let direction = sign (medianPosition - averagePosition)
        Seq.initInfinite (fun x -> averagePosition + direction * x)
        |> Seq.map getFuel
        |> Seq.pairwise
        |> Seq.find (fun (a, b) -> a < b)
        |> fst
}
