// https://adventofcode.com/2021/day/7
#load "Util.fsx"

let day = Util.adventDay 7 """
16,1,2,0,4,2,7,1,2,14
"""

let hpositions =
    day.RawInput.[0]
    |> Util.stringSplit [","]
    |> Array.map int
    |> Array.sort

let meanPosition = hpositions.[(hpositions.Length + 1) / 2]

module Part1 =
    let fuel =
        hpositions
        |> Array.sumBy ((-) meanPosition >> abs)

module Part2 =
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

    let fuel =
        // With fuel consumtption "n * n" average would be the best position but the "n" term skews it of a little.
        let direction = sign (meanPosition - averagePosition)
        Seq.initInfinite (fun x -> averagePosition + direction * x)
        |> Seq.map getFuel
        |> Seq.pairwise
        |> Seq.find (fun (a, b) -> a < b)
        |> fst

day.Answer(
    part1 = Part1.fuel,
    part2 = Part2.fuel
)
