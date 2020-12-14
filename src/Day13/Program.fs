open System
open System.IO

type Bus = {
    Id : int
    Wait : int
}

let intervalForBusses (busSeries : Bus array) (initialIndex : bigint) (step : bigint) =
    let mutable index = initialIndex
    let mutable continueLooping = true
    let totalBuses = busSeries.Length
    let mutable firstMatch = bigint 0

    while continueLooping do 
        let trial = 
            busSeries
            |> Array.map (fun f -> 
                let remainder = (bigint.Remainder((index + (bigint f.Wait)), (f.Id |> bigint)))
                remainder = (bigint 0))
        if (trial |> Array.filter (id) |> Array.length) = totalBuses 
        then
            if firstMatch = bigint 0
            then 
                firstMatch <- index
                index <- index + step
            else 
                printfn "Interval %A found at %A for array %A" (firstMatch - index) index busSeries
                continueLooping <- false
        else 
            index <- index + step
    firstMatch, index

let matchSeriesOfBusses (busSeries : Bus array) (initialIndex : bigint) (step : bigint) =
    let mutable index = initialIndex
    let mutable continueLooping = true
    let totalBuses = busSeries.Length

    while continueLooping do 
        let fixedIndex = index
        let trial = 
            busSeries
            |> Array.map (fun f -> 
                let remainder = (bigint.Remainder((fixedIndex + (bigint f.Wait)), (f.Id |> bigint)))
                remainder = (bigint 0))
        if (trial |> Array.filter (id) |> Array.length) = totalBuses
        then
            printfn "Interval %A found at %A for array %A" (index - initialIndex) index busSeries
            continueLooping <- false
        else 
            printfn "Index at %A has values %A" index trial
            index <- index + step
    index

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 13 - Part 1"
    let busInput = 
        File.ReadAllLines("./input/input_day13.txt")
    let earliestDeparture = busInput.[0] |> System.Int32.Parse
    let busIds = 
        busInput.[1].Split(",", StringSplitOptions.RemoveEmptyEntries)
        |> Array.filter(fun f -> f <> "x")
        |> Array.map System.Int32.Parse
        |> Array.map (fun b -> { 
            Id = b
            Wait = b - (earliestDeparture % b)
        })
        |> Array.sortBy (fun f -> f.Wait)

    let answerPart1 = busIds.[0].Id * busIds.[0].Wait
    printfn "Bus Id %d has wait time of %d which gives answer of %d " busIds.[0].Id busIds.[0].Wait answerPart1

    printfn "Advent of Code Day 13 - Part 2"
    let busSeries =
        busInput.[1].Split(",", StringSplitOptions.RemoveEmptyEntries)
        |> Array.mapi (fun i f -> 
            match f with 
            | "x" -> None 
            | _ -> Some {
                Id = (f |> System.Int32.Parse)
                Wait = i
            }
        )
        |> Array.filter (fun f -> f.IsSome)
        |> Array.map (fun f -> f.Value)
        |> Array.sortByDescending (fun f -> f.Id)
    
    // 7,0
    // 13,1
    // 59,4
    // 31,6
    // 19,7
    // A: 1068781
    // Try to find interval for the two biggest ones
    let mutable busAmount = 2
    let mutable startIndex = bigint (busSeries.[0].Id - busSeries.[0].Wait)
    let mutable interval = bigint (busSeries.[0].Id)
    while busAmount < busSeries.Length do
        let busRoutePart = busSeries |> Array.take busAmount
        let intervalFound = intervalForBusses busRoutePart startIndex interval
        startIndex <- (fst intervalFound)
        interval <- (snd intervalFound) - (fst intervalFound)
        busAmount <- busAmount + 1
        printfn "Interval %A found between %A - %A" interval (fst intervalFound) (snd intervalFound)
    let earliesForAll = matchSeriesOfBusses busSeries startIndex interval
    printfn "Answer part 2: %A" earliesForAll
    0 // return an integer exit code