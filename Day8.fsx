// https://adventofcode.com/2021/day/8
#load "Advent.fsx"
open Advent

open System

type Segment = A | B | C | D | E | F | G

type Row =
    {
        Input : Segment list list
        Output : Segment list list
    }

type Permuation = Permuation of Collections.Generic.IDictionary<Segment, Segment>

solution 8 """
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
<| fun input ->

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

let rows =
    input
    |> Array.map (fun x ->
        let idx = x.IndexOf('|')
        {
            Input =
                x.Substring(0, idx)
                |> Util.stringSplit [" "]
                |> Seq.map convert
                |> Seq.toList
            Output =
                x.Substring(idx + 1)
                |> Util.stringSplit [" "]
                |> Seq.map convert
                |> Seq.toList
        }
    )

{
    Part1 = 26, fun () ->
        rows
        |> Seq.collect (fun row -> row.Output)
        |> Seq.filter (List.length >> function
            | 2 | 3 | 4 | 7 -> true
            | _ -> false
        )
        |> Seq.length

    Part2 = 61229, fun () ->
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
            |> List.map (dict >> Permuation)

        let tryMap (Permuation perm) (segemts) =
            segemts
            |> Seq.map (fun s -> perm.[s])
            |> Seq.sort
            |> Seq.toList
            |> function
                | [A;B;C;E;F;G] -> Some 0
                | [C;F] -> Some 1
                | [A;C;D;E;G] -> Some 2
                | [A;C;D;F;G]  -> Some 3
                | [B;C;D;F] -> Some 4
                | [A;B;D;F;G] -> Some 5
                | [A;B;D;E;F;G] -> Some 6
                | [A;C;F;] -> Some 7
                | [A;B;C;D;E;F;G] -> Some 8
                | [A;B;C;D;F;G] -> Some 9
                | _ -> None

        rows
        |> Array.map (fun row ->
            let perm =
                permutations
                |> List.find (fun perm ->
                    row.Input
                    |> List.forall (tryMap perm >> Option.isSome)
                )

            row.Output
            |> Seq.map (tryMap perm >> Option.get)
            |> Seq.fold (fun num digit -> num * 10 + digit) 0
        )
        |> Array.sum
}
