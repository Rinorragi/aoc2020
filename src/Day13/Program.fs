open System
open System.IO

type Bus = {
    Id : int
    Wait : int
}

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
    
    printfn "Bus series %A " busSeries
    
    let mutable index = bigint 0
    let mutable continueLooping = true
    let longestBusRoute = 
        busSeries
        |> Array.maxBy (fun f -> f.Id)
    let step = (bigint longestBusRoute.Id)

    while continueLooping do 
        printf "Attempt %A" index
        let fixedIndex = index - bigint longestBusRoute.Wait
        let trial = 
            busSeries
            |> Array.map (fun f -> 
                let remainder = (bigint.Remainder((fixedIndex + (bigint f.Wait)), (f.Id |> bigint)))
                printf " (%d, %A)" f.Id remainder
                remainder = (bigint 0))
        if (trial |> Array.filter (id) |> Array.length) = busSeries.Length 
        then
            printfn ""
            printfn "Answer at %A is %A" fixedIndex trial
            continueLooping <- false
        else
            index <- index + step
            printfn ""
    
    0 // return an integer exit code