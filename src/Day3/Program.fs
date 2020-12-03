open System
open System.IO

let treeCount 
    (slope : string array) 
    (slopeLength : int) 
    (slopeWidth : int)
    (slopeTravel : int)
    (slopeSpeed : int) =
    let mutable treeCount = 0
    // Starging from 0, increment 1 or 2, until slope is ended
    for i in 0 .. slopeSpeed .. slopeLength - 1  do 
        // Because travel is per row, sometimes we have so much speed
        // that we skip rows so we need to adjust location to skipping rows
        // by dividing with slopeSpeed
        let loc = (i * slopeTravel / slopeSpeed) % slopeWidth
        if slope.[i].[loc] = '#' 
        then treeCount <- treeCount + 1
    treeCount

let slopeMultiplication (slopeResults : int list) =
    let slopeResLen = 
        slopeResults 
        |> List.length
    let rec loop (acc : bigint) (counter : int) =
        if counter >= 0 
        then
            printfn "Trying to multiple %A with %d" acc slopeResults.[counter]
            loop (acc * (bigint slopeResults.[counter])) (counter - 1)
        else
            acc
    loop (bigint 1) (slopeResLen - 1)

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 3 - Part 1"

    let slope = 
        File.ReadAllLines("./input/input_day3.txt")

    let slopeWidth = 
        slope 
        |> Array.head 
        |> String.length
    
    let slopeLength = 
        slope
        |> Array.length

    // 3 was hardgiven for part 1
    treeCount slope slopeLength slopeWidth 3 1
        |> printfn "Part 1 answer is %d" 

    printfn "Advent of Code Day 3 - Part 2"
    (*
        Right 1, down 1.
        Right 3, down 1. (This is the slope you already checked.)
        Right 5, down 1.
        Right 7, down 1.
        Right 1, down 2.*)
    let slopeRoutes = [(1,1);(3,1);(5,1);(7,1);(1,2)]
    let answers = 
        slopeRoutes
        |> List.map (fun (a, b) -> treeCount slope slopeLength slopeWidth a b)
        |> slopeMultiplication

    printfn "Part 2 answer is %A " answers


    0 // return an integer exit code