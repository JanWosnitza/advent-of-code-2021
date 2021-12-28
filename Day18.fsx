// https://adventofcode.com/2021/day/18
#load "Advent.fsx"
open Advent

type Fish =
    | Pair of Left:Fish * Right:Fish
    | Number of int

module Fish =
    let tryExplode (fish) =
        let rec addLeftTo (addLeft) (fish) =
            match fish with
            | Number x -> Number (x + addLeft)
            | Pair (leftFish, rightFish) -> Pair (leftFish, addLeftTo addLeft rightFish)

        let rec addRightTo (addRight) (fish) =
            match fish with
            | Number x -> Number (x + addRight)
            | Pair (leftFish, rightFish) -> Pair (addRightTo addRight leftFish, rightFish)

        let rec recurse (depth) (fish) : option<option<int> * Fish * option<int>> =
            match fish with
            | Number _ -> None

            | Pair (Number addLeft, Number addRight) when depth <= 0 ->
                Some (Some addLeft, Number 0, Some addRight)

            | Pair (fishLeft, fishRight) ->
                recurse (depth - 1) fishLeft
                |> Option.map (fun (addLeft, fishLeft, addRight) ->
                    match addRight with
                    | Some addRight ->
                        (addLeft, Pair (fishLeft, addRightTo addRight fishRight), None)
                    | None ->
                        (addLeft, Pair (fishLeft, fishRight), None)
                )
                |> Option.orElseWith (fun () ->
                    recurse (depth - 1) fishRight
                    |> Option.map (fun (addLeft, fishRight, addRight) ->
                        match addLeft with
                        | Some addLeft ->
                            (None, Pair (addLeftTo addLeft fishLeft, fishRight), addRight)
                        | None ->
                            (None, Pair (fishLeft, fishRight), addRight)
                    )
                )

        recurse 4 fish
        |> Option.map (fun (_, fish, _) -> fish)

    let rec trySplit (fish) =
        match fish with
        | Number mag ->
            if mag >= 10 then
                Some (Pair (
                    Number (mag / 2),
                    Number ((mag + 1) / 2)
                ))
            else
                None
        | Pair (fishLeft, fishRight) ->
            trySplit fishLeft
            |> Option.map (fun fishLeft -> Pair (fishLeft, fishRight))
            |> Option.orElseWith (fun () ->
                trySplit fishRight
                |> Option.map (fun fishRight -> Pair (fishLeft, fishRight))
            )

    let rec reduce (fish) =    
        match tryExplode fish with
        | Some fish -> reduce fish
        | None ->
            match trySplit fish with
            | Some fish -> reduce fish
            | None -> fish

    let combine (left) (right) =
        Pair (left, right)
        |> reduce

    let rec score =
        function
        | Number x -> bigint x
        | Pair (left, right) -> 3I * score left + 2I * score right

let parse = Input.toMultiline >> fun input ->
    let rec read =
        function
        | '[' :: xs ->
            let leftFish, xs = read xs
            match xs with
            | ',' :: xs ->
                let rightFish, xs = read xs
                match xs with
                | ']' :: xs ->
                    Pair (leftFish, rightFish), xs
                | c -> failwith $"Expected [ but found {c}"
            | c -> failwith $"Expected [ but found {c}"
        | c :: xs when c >= '0' && c <= '9' -> Number (int c - int '0'), xs
        | c -> failwith $"Expected [ but found {c}"

    {|
        AllFish =
            input
            |> Seq.map (List.ofSeq >> read >> fst)
            |> Seq.toArray
    |}

let part1 = parse >> fun input ->
    input.AllFish
    |> Seq.reduce Fish.combine
    |> Fish.score

let part2 = parse >> fun input ->
    let indices = [0 .. input.AllFish.Length - 1]
    Seq.allPairs indices indices
    |> Seq.filter (fun (i, j) -> i <> j)
    |> Seq.map (fun (i, j) ->
        Fish.combine input.AllFish[i] input.AllFish[j]
        |> Fish.score
    )
    |> Seq.max

/////////////////////////////////

let testInput1 = """
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
"""

AoC.Day 18 [
    AoC.Part part1 [
        testInput1, 4140I
    ]
    AoC.Part part2 [
        testInput1, 3993I
    ]
]
