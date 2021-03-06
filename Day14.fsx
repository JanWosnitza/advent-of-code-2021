// https://adventofcode.com/2021/day/14
#load "Advent.fsx"
open Advent

type Element = Element of char

let parse = Input.toMultiline >> fun input ->
    {|
        Template =
            input
            |> Seq.head
            |> Seq.map Element
            |> Seq.toList

        Insertions =
            input
            |> Seq.skip 2
            |> Seq.map (fun row ->
                match row |> Input.split [" -> "] with
                | [|pair;insert|]
                    when pair.Length = 2 && insert.Length = 1 ->
                    ((Element pair.[0], Element pair.[1]), Element insert.[0])
                | _ -> failwith $"Invalid input {row}"
            )
            |> Map.ofSeq
    |}

type ElementCount = Map<Element, bigint>

module ElementCount =
    let empty : ElementCount = Map.empty

    let add (element) (count) (counts:ElementCount) =
        let oldCount =
            counts
            |> Map.tryFind element
            |> Option.defaultValue 0I

        counts
        |> Map.add element (oldCount + count)

    let combine (counts1:ElementCount) (counts2:ElementCount) :ElementCount =
        (counts2, Map.toSeq counts1)
        ||> Seq.fold (fun map (element, count) -> map |> add element count)

let getCounts insertions =
    let toTree (steps, e1, e2) =
        if steps <= 0 then
            ElementCount.empty
            |> ElementCount.add e2 1I
            |> TreeLeaf
        else
            let insert = insertions |> Map.find (e1, e2)
            TreeBranch [
                (steps - 1, e1, insert)
                (steps - 1, insert, e2)
            ]

    let fold = Util.treeFold toTree ElementCount.combine ElementCount.empty

    fun steps template ->
    template
    |> Seq.pairwise
    |> Seq.map (fun (e1, e2) -> fold (steps, e1, e2))
    |> Seq.reduce ElementCount.combine
    |> ElementCount.add (Seq.head template) 1I

let getScore (counts:ElementCount) =
    let counts = counts |> Map.toSeq
    let (minElement, minCount) = counts |> Seq.minBy snd
    let (maxElement, maxCount) = counts |> Seq.maxBy snd
    maxCount - minCount

let part1 = parse >> fun input ->
    input.Template
    |> getCounts input.Insertions 10
    |> getScore

let part2 = parse >> fun input ->
    input.Template
    |> getCounts input.Insertions 40
    |> getScore

////////////////////////

let testInput1 = """
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
"""

AoC.Day 14 [
    AoC.Part part1 [
        testInput1, 1_588I
    ]
    AoC.Part part2 [
        testInput1, 2_188_189_693_529I
    ]
]
