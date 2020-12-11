open System
open System.IO

let isSeatEmptyOrFloor (seatChar : char) =
    let validSurrounding = [|'.';'L'|]
    validSurrounding |> Array.contains seatChar

let isSeatValidForOccupation (input : string array) (row : int) (col : int) =
    let rowWidth = input.[0].Length
    let colLength = input.Length
    let checkNorthWest = row = 0 || col = 0 || isSeatEmptyOrFloor input.[row-1].[col-1]
    let checkNorth = row = 0 || isSeatEmptyOrFloor input.[row-1].[col]
    let checkNorthEast = row = 0 || col = rowWidth - 1 || isSeatEmptyOrFloor input.[row-1].[col+1]
    let checkWest =  col = 0 || isSeatEmptyOrFloor input.[row].[col-1]
    let checkEast = col = rowWidth - 1 || isSeatEmptyOrFloor input.[row].[col+1]
    let checkSouthhWest = row = colLength - 1 || col = 0 || isSeatEmptyOrFloor input.[row+1].[col-1]
    let checkSouth = row = colLength - 1 || isSeatEmptyOrFloor input.[row+1].[col]
    let checkSouthEast = row = colLength - 1 || col = rowWidth - 1 || isSeatEmptyOrFloor input.[row+1].[col+1]
    let enoughRoom = [|checkNorthWest; checkNorth; checkNorthEast; checkWest; checkEast; checkSouthhWest; checkSouth; checkSouthEast|]
    input.[row].[col] = 'L'
    && (enoughRoom |> Array.filter(id) |> Array.length = 8)

let isSeatTooCrowded (input : string array) (row : int) (col : int) =
    let rowWidth = input.[0].Length
    let colLength = input.Length
    let occupiedChar = '#'
    let checkNorthWest = row <> 0 && col <> 0 && input.[row-1].[col-1] = occupiedChar
    let checkNorth = row <> 0 && input.[row-1].[col] = occupiedChar
    let checkNorthEast = row <> 0 && col <> rowWidth - 1 && input.[row-1].[col+1] = occupiedChar
    let checkWest =  col <> 0 && input.[row].[col-1] = occupiedChar
    let checkEast = col <> rowWidth - 1 && input.[row].[col+1] = occupiedChar
    let checkSouthhWest = row <> colLength - 1 && col <> 0 && input.[row+1].[col-1] = occupiedChar
    let checkSouth = row <> colLength - 1 && input.[row+1].[col] = occupiedChar
    let checkSouthEast = row <> colLength - 1 && col <> rowWidth - 1 && input.[row+1].[col+1] = occupiedChar
    let crowded = 
        [|checkNorthWest; checkNorth; checkNorthEast; checkWest; checkEast; checkSouthhWest; checkSouth; checkSouthEast|]
    input.[row].[col] = occupiedChar
    && (crowded |> Array.filter (id) |> Array.length) >= 4

let getOccupationFromDirection (input : string array) (startRow : int) (startCol : int) (rowdirection : int) (coldirection : int) =
    let rowWidth = input.[0].Length
    let colLength = input.Length
    let rec travelToDirection (row : int) (col : int) =
        let newRow = row + rowdirection
        let newCol = col + coldirection
        if newRow = -1 || newRow = colLength || newCol = -1 || newCol = rowWidth
        then 
            true
        elif input.[newRow].[newCol] = 'L'
        then 
            true
        elif input.[newRow].[newCol] = '#'
        then 
            false
        else 
            travelToDirection newRow newCol
    travelToDirection startRow startCol

let isSeatValidForOccupationPart2 (input : string array) (row : int) (col : int) =
    let checkNorthWest = getOccupationFromDirection input row col -1 -1
    let checkNorth = getOccupationFromDirection input row col -1 0
    let checkNorthEast = getOccupationFromDirection input row col -1 1
    let checkWest =  getOccupationFromDirection input row col 0 -1
    let checkEast = getOccupationFromDirection input row col 0 1
    let checkSouthhWest = getOccupationFromDirection input row col 1 -1
    let checkSouth = getOccupationFromDirection input row col 1 0
    let checkSouthEast = getOccupationFromDirection input row col 1 1
    let enoughRoom = [|checkNorthWest; checkNorth; checkNorthEast; checkWest; checkEast; checkSouthhWest; checkSouth; checkSouthEast|]
    // printfn ("Enough room for %d %d %c %A ") row col input.[row].[col] enoughRoom
    input.[row].[col] = 'L'
    && (enoughRoom |> Array.filter(id) |> Array.length = 8)

let isSeatTooCrowdedPart2 (input : string array) (row : int) (col : int) =
    let checkNorthWest = getOccupationFromDirection input row col -1 -1
    let checkNorth = getOccupationFromDirection input row col -1 0
    let checkNorthEast = getOccupationFromDirection input row col -1 1
    let checkWest =  getOccupationFromDirection input row col 0 -1
    let checkEast = getOccupationFromDirection input row col 0 1
    let checkSouthhWest = getOccupationFromDirection input row col 1 -1
    let checkSouth = getOccupationFromDirection input row col 1 0
    let checkSouthEast = getOccupationFromDirection input row col 1 1
    let crowded = 
        [|checkNorthWest; checkNorth; checkNorthEast; checkWest; checkEast; checkSouthhWest; checkSouth; checkSouthEast|]

    input.[row].[col] = '#'
    && (crowded |> Array.filter (not) |> Array.length) >= 5

let printSituation (input : string array) =
    printfn "-----------"
    input 
        |> Array.map (fun f -> printfn "%s" f)

let occupieSeatsUntilNoChange (input : string array) validnessCheck crowdnessCheck =
    let rec occupieSeats (seatSituation : string array) =
        let mutable acc = 0
        let newSituation = 
            seatSituation
            |> Array.mapi (fun ri r -> 
                r |> Seq.mapi (fun ci c -> 
                    if (validnessCheck seatSituation ri ci)
                    then 
                        acc <- acc + 1
                        '#'
                    elif (crowdnessCheck seatSituation ri ci)
                    then
                        acc <- acc + 1
                        'L'
                    else 
                        c))
            |> Array.map (fun f -> f |> Array.ofSeq |> String )
        printSituation newSituation |> ignore
        if (acc = 0) 
        then seatSituation
        else occupieSeats newSituation
    printSituation input |> ignore
    let stabileSituation = occupieSeats input
    stabileSituation

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 11 - Part 1"
    let seatInput = 
        File.ReadAllLines("./input/input_day11.txt")

    let modifiedSituation = occupieSeatsUntilNoChange seatInput isSeatValidForOccupation isSeatTooCrowded
    let occupiedSeats =
        Array.sumBy (fun f -> Seq.length(f |> Seq.filter (fun x -> x = '#'))) modifiedSituation
    printfn "%d" occupiedSeats

    printfn "Advent of Code Day 11 - Part 2"
    let modifiedSituationPart2 = occupieSeatsUntilNoChange seatInput isSeatValidForOccupationPart2 isSeatTooCrowdedPart2
    let occupiedSeatsPart2 =
        Array.sumBy (fun f -> Seq.length(f |> Seq.filter (fun x -> x = '#'))) modifiedSituationPart2
    printfn "%d" occupiedSeatsPart2

    0 // return an integer exit code