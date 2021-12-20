// https://adventofcode.com/2f2t/day/19
#load "Advent.fsx"
open Advent

type [<Struct>] Position =
    {X:int; Y:int; Z:int}
    static member (+) (a:Position, b:Position) = {X = a.X + b.X; Y = a.Y + b.Y; Z = a.Z + b.Z}
    static member (-) (a:Position, b:Position) = {X = a.X - b.X; Y = a.Y - b.Y; Z = a.Z - b.Z}

let rotations =
    let rotX (pos:Position) = {X = pos.X; Y = -pos.Z; Z = pos.Y}
    let rots (rot) =
        [
            rot
            rotX >> rot
            rotX >> rotX >> rot
            rotX >> rotX >> rotX >> rot
        ]

    let rotY (pos:Position) = {X = -pos.Z; Y = pos.Y; Z = pos.X}
    let rotZ (pos:Position) = {X = -pos.Y; Y = pos.X; Z = pos.Z}
    [
        rots (id)
        rots (rotY)
        rots (rotY >> rotY)
        rots (rotY >> rotY >> rotY)
        rots (rotZ)
        rots (rotZ >> rotZ >> rotZ)
    ]
    |> List.collect id

type [<Struct>] Beacons =
    {
        Scanners : list<Position>
        Positions : Set<Position>
    }

module Beacons =
    let combine (a:Beacons) (b:Beacons) =
        {
            Scanners = a.Scanners @ b.Scanners
            Positions = Set.union a.Positions b.Positions
        }

    let map (mapper) (x:Beacons) =
        {
            Scanners = x.Scanners |> List.map mapper
            Positions = x.Positions |> Set.map mapper
        }

    type [<Struct>] FeatureVector = FeatureVector of (int)

    let getFeatureVectors (positions:Set<Position>) =
        let positions = positions |> Array.ofSeq

        let getAxisFeature (selector) (positions:seq<Position>) =
            positions
            |> Seq.map selector
            |> Seq.indexed
            |> Seq.sortBy snd
            |> Seq.pairwise
            |> Seq.collect (fun ((idxA, posA), (idxB, posB)) -> [
                idxA, posA - posB
                idxB, posB - posA
            ])
            |> Seq.groupBy fst
            |> Seq.map (fun (idx, distances) -> (idx, distances |> Seq.map snd |> Seq.minBy abs)
            )
            |> Map.ofSeq

        let fxs = positions |> getAxisFeature (fun p -> p.X)
        let fys = positions |> getAxisFeature (fun p -> p.Y)
        let fzs = positions |> getAxisFeature (fun p -> p.Z)
        
        seq {0 .. positions.Length - 1}
        |> Seq.map (fun i ->
            let fx = fxs |> Map.find i
            let fy = fys |> Map.find i
            let fz = fzs |> Map.find i
            (
                FeatureVector (fx |> max fy |> max fz),
                positions[i]
            )
        )
        |> Seq.groupBy fst
        |> Seq.map (fun (feature, idxs) -> (feature, idxs |> Seq.map snd |> Seq.toList))
        |> Map.ofSeq

    let private tryGetCombinable_Shift (minIntersections:int) (target:Beacons, positionsTarget) (source:Beacons, positionsSource) =
        Seq.allPairs positionsTarget positionsSource
        |> Seq.map (fun (posTarget, posSource) ->
            let pOff = posTarget - posSource
            let source =
                source
                |> map (fun p -> p + pOff)
            let intersections = Set.intersect target.Positions source.Positions |> Set.count
            intersections, source
        )
        |> Seq.filter (fun (i, _) -> i >= minIntersections)
        |> Seq.sortBy fst
        |> Seq.tryHead
        |> Option.map snd

    let rec tryMakeCombinable (minIntersections:int) (target:Beacons) (source:Beacons) =
        let fvsTarget = getFeatureVectors target.Positions

        rotations
        |> Seq.map (fun rotation -> source |> map rotation)
        |> Seq.choose (fun (source) ->
            getFeatureVectors source.Positions
            |> Map.toSeq
            |> Seq.choose (fun (fvSource, positionsSource) ->
                match fvsTarget |> Map.tryFind fvSource with
                | None -> None
                | Some positionsTarget -> tryGetCombinable_Shift minIntersections (target, positionsTarget) (source, positionsSource)
            )
            |> Seq.tryHead
        )
        |> Seq.tryHead

    let rec tryMakeCombinableBF (minIntersections:int) (target:Beacons) (source:Beacons) =
        rotations
        |> Seq.map (fun rotation -> source |> map rotation)
        |> Seq.choose (fun (source) ->
            tryGetCombinable_Shift minIntersections (target, target.Positions) (source, source.Positions)
        )
        |> Seq.tryHead

    let combines (minIntersections:int) (beacons:list<Beacons>) =
        let rec loop (target:Beacons) (sources:list<Beacons>) =
            if sources |> List.isEmpty then target else

            let makeCombinalbes (algorithm) =
                sources
                |> Seq.indexed
                |> Seq.choose (fun (idx, source) ->
                    algorithm minIntersections target source
                    |> Option.map (fun source -> (idx, source))
                )
                |> Seq.toList

            let combinables = makeCombinalbes tryMakeCombinable

            let combinables =
                if not <| combinables.IsEmpty then
                    combinables
                else
                    makeCombinalbes tryMakeCombinableBF

            let target =
                (target, combinables |> Seq.map snd)
                ||> Seq.fold combine

            combinables
            |> List.map fst
            |> List.rev
            |> List.fold (fun sources idx -> sources |> List.removeAt idx) sources
            |> loop target

        match beacons with
        | [] -> failwith ""
        | b :: bs -> loop b bs

let rec readBeacons (positions) (rows) =
    match rows with
    | [] -> positions |> List.rev, []
    | "" :: rows -> positions |> List.rev, rows
    | row :: rows ->
        let position =
            match row |> Input.split [","] with
            | [|x; y; z|] -> {X=int x; Y=int y; Z=int z}
            | x -> failwith $"Invalid position {row}"

        rows
        |> readBeacons (position :: positions)

let rec readScanners (scanners:list<Beacons>) (rows:list<string>) =
    match rows with
    | [] -> scanners |> List.rev
    | header :: rows
      when header = $"--- scanner {scanners.Length} ---" ->
        let positions, rows = rows |> readBeacons []
        let beacons = {
            Scanners = [{X=0; Y=0; Z=0}]
            Positions = Set.ofSeq positions
        }
        rows
        |> readScanners (beacons :: scanners)
    | header :: rows ->
        failwith $"Invalid scanner {header}"

let parse = Input.toMultiline >> fun input ->
    {|
        Scanners =
            input
            |> List.ofArray
            |> readScanners []
    |}

let part1 = parse >> fun input ->
    let beacons =
        input.Scanners
        |> Beacons.combines 12

    beacons.Positions.Count

let part2 = parse >> fun input ->
    let beacons =
        input.Scanners
        |> Beacons.combines 12

    Seq.allPairs beacons.Scanners beacons.Scanners
    |> Seq.map (fun (a, b) ->
        let x = a - b
        abs x.X + abs x.Y + abs x.Z
    )
    |> Seq.max

////////////////////////////

let testInput1 = """
--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14
"""

AoC.Day 19 [
    AoC.Part part1 [
        testInput1, 79
    ]
    AoC.Part part2 [
        testInput1, 3621
    ]
]
