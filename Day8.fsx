// https://adventofcode.com/2021/day/8
#load "Advent.fsx"
open Advent

type Segment = A | B | C | D | E | F | G

type Digit = Digit of Segment list

type Row =
    {
        Input : Digit list
        Output : Digit list
    }

let parse = Input.toMultiline >> fun input ->
    let convert (x:string) =
        x |> Seq.map (function
            | 'a' -> A
            | 'b' -> B
            | 'c' -> C
            | 'd' -> D
            | 'e' -> E
            | 'f' -> F
            | 'g' -> G
            | i -> failwith $"Invalid Input '{i}'"
        )
        |> Seq.sort
        |> Seq.toList
        |> Digit

    {|
        Rows =
            input
            |> Array.map (fun row ->
                match row |> Input.split [" | "] with
                | [|input; output|] ->
                    {
                        Input =
                            input
                            |> Input.split [" "]
                            |> Seq.map convert
                            |> Seq.toList
                        Output =
                            output
                            |> Input.split [" "]
                            |> Seq.map convert
                            |> Seq.toList
                    }
                | _ -> failwith $"Invalid row {row}"
            )
    |}

type Permuation = Permuation of Map<Segment, Segment>

let segmentMap =
    [
        0, [A;B;C;E;F;G]
        1, [C;F]
        2, [A;C;D;E;G]
        3, [A;C;D;F;G]
        4, [B;C;D;F]
        5, [A;B;D;F;G]
        6, [A;B;D;E;F;G]
        7, [A;C;F]
        8, [A;B;C;D;E;F;G]
        9, [A;B;C;D;F;G]
    ]
    |> Seq.map (fun (a, b) -> b, a)
    |> Map.ofSeq

let part1 = parse >> fun input ->
    input.Rows
    |> Seq.collect (fun row -> row.Output)
    |> Seq.filter (fun (Digit digit) ->
        match digit |> List.length with
        | 2 | 3 | 4 | 7 -> true
        | _ -> false
    )
    |> Seq.length

let part2 = parse >> fun input ->
    // this is brute force. given the amount of permutaions (5040) it's not a problem
    // BUT solving this with logical programming would be more interesting

    let permute list =
        let rec inserts e = function
            | [] -> [[e]]
            | x::xs as list -> (e::list)::(inserts e xs |> List.map (fun xs' -> x::xs'))

        List.fold (fun accum x -> List.collect (inserts x) accum) [[]] list

    let permutations =
        let all = [A;B;C;D;E;F;G]
        permute all
        |> List.map (List.zip all)
        |> List.map (Map.ofSeq >> Permuation)

    let tryConvert segments = segmentMap |> Map.tryFind segments

    let tryMap (Permuation perm) (Digit digit) =
        digit
        |> Seq.map (fun s -> Map.find s perm)
        |> Seq.sort
        |> Seq.toList
        |> tryConvert

    let findValidPermutation (input:Digit list) =
        permutations
        |> List.find (fun perm ->
            input
            |> List.forall (tryMap perm >> Option.isSome)
        )

    input.Rows
    |> Array.map (fun row ->
        let perm = findValidPermutation row.Input

        row.Output
        |> Seq.map (tryMap perm >> Option.get)
        |> Seq.fold (fun num digit -> num * 10 + digit) 0
    )
    |> Array.sum

////////////////
let testInput1 =  """
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
"""

AoC.Day 8 [
    AoC.Part part1 [
        testInput1, 26
    ]
    AoC.Part part2 [
        testInput1, 61229
    ]
]
