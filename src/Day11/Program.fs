open System
open System.IO

let IsSeatEmptyOrFloor (seatChar : char) =
    let validSurrounding = [|'.';'L'|]
    validSurrounding |> Array.contains seatChar

let IsSeatValidForOccupation (input : string array) (row : int) (col : int) =
    let rowWidth = input.[0].Length
    let colLength = input.Length
    let checkNorthWest = row = 0 || col = 0 || IsSeatEmptyOrFloor input.[row-1].[col-1]
    let checkNorth = row = 0 || IsSeatEmptyOrFloor input.[row-1].[col]
    let checkNorthEast = row = 0 || col = rowWidth - 1 || IsSeatEmptyOrFloor input.[row-1].[col+1]
    let checkWest =  col = 0 || IsSeatEmptyOrFloor input.[row].[col-1]
    let checkEast = col = rowWidth - 1 || IsSeatEmptyOrFloor input.[row].[col+1]
    let checkSouthhWest = row = colLength - 1 || col = 0 || IsSeatEmptyOrFloor input.[row+1].[col-1]
    let checkSouth = row = colLength - 1 || IsSeatEmptyOrFloor input.[row+1].[col]
    let checkSouthEast = row = colLength - 1 || col = rowWidth - 1 || IsSeatEmptyOrFloor input.[row+1].[col+1]
    let enoughRoom = [|checkNorthWest; checkNorth; checkNorthEast; checkWest; checkEast; checkSouthhWest; checkSouth; checkSouthEast|]
    // printfn ("Enough room for %d %d %c %A ") row col input.[row].[col] enoughRoom
    input.[row].[col] = 'L'
    && (enoughRoom |> Array.filter(id) |> Array.length = 8)

let IsSeatTooCrowded (input : string array) (row : int) (col : int) =
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

let printSituation (input : string array) =
    printfn "-----------"
    input 
        |> Array.map (fun f -> printfn "%s" f)

let occupieSeatsUntilNoChange (input : string array) =
    let rec occupieSeats (seatSituation : string array) =
        let mutable acc = 0
        let newSituation = 
            seatSituation
            |> Array.mapi (fun ri r -> 
                r |> Seq.mapi (fun ci c -> 
                    if (IsSeatValidForOccupation seatSituation ri ci)
                    then 
                        acc <- acc + 1
                        '#'
                    elif (IsSeatTooCrowded seatSituation ri ci)
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

    let modifiedSituation = occupieSeatsUntilNoChange seatInput
    let occupiedSeats =
        Array.sumBy (fun f -> Seq.length(f |> Seq.filter (fun x -> x = '#'))) modifiedSituation
    printfn "%d" occupiedSeats

    

    0 // return an integer exit code