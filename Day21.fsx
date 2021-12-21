// https://adventofcode.com/2f2t/day/21
#load "Advent.fsx"
open Advent

let parse = Input.toMultiline >> fun input ->
    let startingPosition (s:string) =
        s
        |> Input.split [":"]
        |> Array.item 1
        |> int

    {|
        Player1 = startingPosition input[0]
        Player2 = startingPosition input[1]
    |}

type PlayerName = Player1 | Player2
type Player<'Name> = {Name : 'Name; Score : int; Position : int}

module Player =
    let create (name) (startingPosition) = {Name = name; Score = 0; Position = startingPosition}

    let move (distance) (player) =
        {player with Position = ((player.Position + distance - 1) % 10) + 1}

    let updateScore (player) =
        {player with Score = player.Score + player.Position}

module Part1 =
    type Die = {Next:int; Rolls:int}

    module Die =
        let init = {Next=1; Rolls=0}
        let roll (die:Die) = die.Next, {Next = (die.Next % 100) + 1; Rolls = die.Rolls + 1}

    module Player =
        let turn (die) (player:Player<_>) : Die * Player<_> =
            let roll1, die = Die.roll die
            let roll2, die = Die.roll die
            let roll3, die = Die.roll die
            
            let player =
                player
                |> Player.move roll1
                |> Player.move roll2
                |> Player.move roll3
                |> Player.updateScore
            
            die, player

    let rec play (die) (current) (next) =
        let die, current = Player.turn die current
        if current.Score >= 1000 then
            {|
                Die = die
                Winner = current
                Loser = next
            |}
        else
            play die next current

    let answer = parse >> fun input ->
        let player1 = Player.create Player1 input.Player1
        let player2 = Player.create Player2 input.Player2
        let result = play Die.init player1 player2
        
        result.Loser.Score * result.Die.Rolls

module Part2 =
    let answer = parse >> fun input ->
        let player1 = Player.create Player1 input.Player1
        let player2 = Player.create Player2 input.Player2
        
        let toTree (winningScore) (current:Player<_>, next) =
            if next.Score >= winningScore then
                match next.Name with
                | Player1 -> TreeLeaf (1I, 0I)
                | Player2 -> TreeLeaf (0I, 1I)
            else
                TreeBranch [
                    for roll = 1 to 3 do
                    let current =
                        current
                        |> Player.move roll

                    for roll = 1 to 3 do
                    let current =
                        current
                        |> Player.move roll

                    for roll = 1 to 3 do
                    let current =
                        current
                        |> Player.move roll
                        |> Player.updateScore

                    yield (next, current)
                ]

        let addWins (left1, left2) (right1, right2) = (left1 + right1, left2 + right2)

        let wins1, wins2 =
            (player1, player2)
            |> Util.treeFold (toTree 21) addWins (0I, 0I)

        max wins1 wins2

/////////////////////////////////

let testInput1 = """
Player 1 starting position: 4
Player 2 starting position: 8
"""

AoC.Day 21 [
    AoC.Part Part1.answer [
        testInput1, 739785
    ]
    AoC.Part Part2.answer [
        testInput1, 444356092776315I
    ]
]
