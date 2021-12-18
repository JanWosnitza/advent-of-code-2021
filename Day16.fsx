// https://adventofcode.com/2f2t/day/16
#load "Advent.fsx"
open Advent

type Bits = list<bool>

let binaryToInteger (binary:Bits) =
    (0I, binary)
    ||> List.fold (fun value -> function true -> value * 2I + 1I | false -> value * 2I)

type OperatorType = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo 

type PacketContent =
    | Literal of bigint
    | Operator of OperatorType * list<Packet>

and Packet = {
    Version : int
    Content : PacketContent
}

let takeBits (count:int) (bits:Bits) =
    bits[..count - 1], bits[count ..]

let rec readLiteral (bits:Bits) =
    let rec read (num:bigint) (bitsRead:int) (bits:Bits) =
        let bitsRead = bitsRead + 5
        match takeBits 5 bits with
        | true :: numBits, bits ->
            let num = (num <<< 4) + binaryToInteger numBits
            read num bitsRead bits
        | false :: numBits, bits ->
            let num = (num <<< 4) + binaryToInteger numBits
            num, bits
        | _ -> failwith ""

    read 0I 0 bits

let rec readOperator (bits:Bits) =
    match bits with
    | false :: bits ->
        let bitsLength, bits = takeBits 15 bits
        let length = binaryToInteger bitsLength |> int
        let bitsPackets, bits = takeBits length bits
        let subPackets =
            bitsPackets
            |> List.unfold (function
                | [] -> None
                | bits ->
                    readPacket bits |> Some
            )
        subPackets, bits
    | true :: bits ->
        let bitsPacketCount, bits = takeBits 11 bits
        let packetCount = binaryToInteger bitsPacketCount |> int
        
        let rec read (packets:list<Packet>) (bits:Bits) =
            if packets.Length = packetCount then List.rev packets, bits else
            let packet, bits = readPacket bits
            read (packet :: packets) bits

        read [] bits

    | _ -> failwith "Unexpected bits end."

and readPacket (bits:Bits) : Packet * Bits =
    let bVersion, bits = takeBits 3 bits
    let bTypeID, bits = takeBits 3 bits
    let content, bits =
        let readOperator (operatorType) =
            let packets, bits = readOperator bits
            Operator (operatorType, packets), bits
        match binaryToInteger bTypeID |> int with
        | 0 -> readOperator Sum
        | 1 -> readOperator Product
        | 2 -> readOperator Minimum
        | 3 -> readOperator Maximum
        | 4 ->
            let num, bits = readLiteral bits
            Literal num, bits
        | 5 -> readOperator GreaterThan
        | 6 -> readOperator LessThan
        | 7 -> readOperator EqualTo
        | _ -> failwith "Unreachable"
    let packet = {
        Version = binaryToInteger bVersion |> int
        Content = content
    }
    packet, bits

let parse = Input.trim >> fun input ->
    {|
        Packet =
            input
            |> Seq.collect (function
                | '0' -> [false; false; false; false]
                | '1' -> [false; false; false; true]
                | '2' -> [false; false; true; false]
                | '3' -> [false; false; true; true]
                | '4' -> [false; true; false; false]
                | '5' -> [false; true; false; true]
                | '6' -> [false; true; true; false]
                | '7' -> [false; true; true; true]
                | '8' -> [true; false; false; false]
                | '9' -> [true; false; false; true]
                | 'A' -> [true; false; true; false]
                | 'B' -> [true; false; true; true]
                | 'C' -> [true; true; false; false]
                | 'D' -> [true; true; false; true]
                | 'E' -> [true; true; true; false]
                | 'F' -> [true; true; true; true]
                | c -> failwith $"Not a hexadezimal number '{c}'"
            )
            |> Seq.toList
            |> readPacket
            |> fst
    |}

let part1 = parse >> fun input ->
    let rec sumVersions (packet:Packet) =
        match packet.Content with
        | Literal num -> packet.Version
        | Operator (operatorType, subPackets) ->
            packet.Version + Seq.sumBy sumVersions subPackets

    input.Packet
    |> sumVersions

let part2 = parse >> fun input ->
    let rec calc (packet:Packet) =
        match packet.Content with
        | Literal num -> num            
        | Operator (Sum, subpackets) ->
            subpackets
            |> Seq.map calc
            |> Seq.reduce (+)
        | Operator (Product, subpackets) ->
            subpackets
            |> Seq.map calc
            |> Seq.reduce (*)
        | Operator (Maximum, subpackets) ->
            subpackets
            |> Seq.map calc
            |> Seq.reduce (max)
        | Operator (Minimum, subpackets) ->
            subpackets
            |> Seq.map calc
            |> Seq.reduce (min)
        | Operator (GreaterThan, [subpacket1; subpacket2]) ->
            if calc subpacket1 > calc subpacket2 then 1I else 0I
        | Operator (LessThan, [subpacket1; subpacket2]) ->
            if calc subpacket1 < calc subpacket2 then 1I else 0I
        | Operator (EqualTo, [subpacket1; subpacket2]) ->
            if calc subpacket1 = calc subpacket2 then 1I else 0I
        | _ -> failwith $"Invalid packet {packet}"

    input.Packet
    |> calc

//////////////////////////////

AoC.Day 16 [
    AoC.Part part1 [
        "D2FE28", 6
        "38006F45291200", 9
        "EE00D40C823060", 14
        "8A004A801A8002F478", 16
        "620080001611562C8802118E34", 12
        "C0015000016115A2E0802F182340", 23
        "A0016C880162017C3686B18A3D4780", 31
    ]
    AoC.Part part2 [
        "C200B40A82", 3I
        "04005AC33890", 54I
        "880086C3E88112", 7I
        "CE00C43D881120", 9I
        "D8005AC2A8F0", 1I
        "F600BC2D8F", 0I
        "9C005AC2F8F0", 0I
        "9C0141080250320F1802104A08", 1I
    ]
]
