// https://adventofcode.com/2021/day/12
#load "Advent.fsx"
open Advent

type Cave = Cave of string

let caveStart = Cave "start"
let caveEnd = Cave "end"

let parse = Input.toMultiline >> fun input ->
    {|
        Connectionts =
            input
            |> Array.map (fun connection ->
                let ab = connection |> Input.split ["-"]
                (Cave ab.[0], Cave ab.[1])
            )
    |}

let (|Small|Big|) (Cave name) =
    if System.Char.IsLower name.[0] then
        Small
    else
        Big

let findPaths (source) (target) (allow2ndVisit) (connections) =
    let rec find (visitedTwice) (path) =
        let current = List.head path
        if current = source then
            Seq.singleton path
        else
            connections
            |> Map.find current
            |> Seq.collect (fun next ->
                if next = target then Seq.empty else

                match next with
                | Small ->
                    match List.contains next path, visitedTwice with
                    | true, false ->
                        find true (next :: path)
                    | false, _ ->
                        find visitedTwice (next :: path)
                    | _ ->
                        Seq.empty
                | Big ->
                    find visitedTwice (next :: path)
            )

    find (not allow2ndVisit) [target]

let toMap (connections:(Cave * Cave)[]) =
    Seq.concat [
        connections
        connections |> Array.map (fun (a,b) -> (b,a))
    ]
    |> Seq.groupBy fst
    |> Seq.map (fun (a, abs) -> (a, abs |> Seq.map snd |> Seq.toArray))
    |> Map.ofSeq

let part1 = parse >> fun input ->
    input.Connectionts
    |> toMap
    |> findPaths caveStart caveEnd false
    |> Seq.length

let part2 = parse >> fun input ->
    input.Connectionts
    |> toMap
    |> findPaths caveStart caveEnd true
    |> Seq.length

//////////////////////////////
let testInput1 = """
start-A
start-b
A-c
A-b
b-d
A-end
b-end
"""

let testInput2 = """
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
"""

let testInput3 = """
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
"""

AoC.Day 12 [
    AoC.Part part1 [
        testInput1, 10
        testInput2, 19
        testInput3, 226
    ]
    AoC.Part part2 [
        testInput1, 36
        testInput2, 103
        testInput3, 3509
    ]
]
