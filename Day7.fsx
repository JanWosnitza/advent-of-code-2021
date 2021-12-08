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

module Part1 =
    let bestPosition = hpositions.[(hpositions.Length + 1) / 2]

    let fuel =
        hpositions
        |> Array.sumBy ((-) bestPosition >> abs)

module Part2 =
    let averagePosition =
        hpositions
        |> Seq.averageBy float

    let fuel =
        let get (position) =
            let position = int position
            hpositions
            |> Array.sumBy (fun p ->
                let n = abs (p - position)
                (n * n + n) / 2 // == n * (n + 1) / 2
            )

        (
            averagePosition |> floor |> get,
            averagePosition |> ceil |> get
        ) ||> min

day.Answer(
    part1 = Part1.fuel,
    part2 = Part2.fuel
)
