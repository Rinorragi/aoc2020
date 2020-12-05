open System
open Aoc.AocHelper

let calculateSeatId (row : int) (column : int) =
    row * 8 + column

let treeCharToInt (tree:string) (startPoint:int) =
    if (tree.[0] = 'F' || tree.[0] = 'L') 
        then startPoint
    elif (tree.[0] = 'B' || tree.[0] = 'R') 
        then ((pown 2 (tree.Length - 1)) + startPoint)
    else 0

let rec travelBinaryTree (tree:string) (startPoint:int) = 
    if (tree.Length = 1)
    then treeCharToInt tree startPoint
    else travelBinaryTree tree.[1 .. ] (treeCharToInt tree startPoint)

type Seat = 
    {
        RowPresentation: string
        ColumnPresentation: string
        Row: int
        Column: int
        Id: int}

let toSeatType (value : string * string) =
    let aRow = travelBinaryTree (fst value) 0
    let aColumn = travelBinaryTree (snd value) 0
    {
        RowPresentation = fst value
        ColumnPresentation = snd value
        Row = aRow
        Column = aColumn
        Id = calculateSeatId aRow aColumn }

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 5 - Part 1"

    let seatInput = 
        readLines "./input/input_day5.txt"
    
    let maxRowCharacters = 7
    let maxSeatCharacters = 3
    // 7 characters for row (max 127)
    // 3 for seat (max 7)
    let maxRow = pown 2 maxRowCharacters
    let maxSeat = pown 2 maxSeatCharacters

    let seatIds = 
        seatInput 
        |> Seq.map (fun f -> 
            f.[ .. maxRowCharacters - 1], 
            f.[ maxRowCharacters .. maxRowCharacters + maxSeatCharacters - 1])

    let seatDecoded =
        seatIds 
        |> Seq.map toSeatType

    let answer = 
        seatDecoded 
        |> Seq.mapi (fun i v -> i, v.Id)
        |> Seq.maxBy snd

    printfn "Highest id is %d " (snd answer)
    0 // return an integer exit code