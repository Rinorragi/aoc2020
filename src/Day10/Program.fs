open System
open System.IO

type JoltageAdapter = {
    Increment : int
    Value : int}


let constructJoltageAdapterChain (input : int array) =
    let joltageChain : JoltageAdapter list = []
    let rec appendJoltageChain  (input : int array)  (startValue: int) (joltageAdapterChain : JoltageAdapter list) =
        let adapterCandidates = 
            input 
            |> Array.filter (fun f -> f > startValue && f <= startValue + 3)
        
        if (Array.isEmpty adapterCandidates) 
        then 
            let builtInAdapter = {
                Increment = 3
                Value = startValue + 3}
            (builtInAdapter :: joltageAdapterChain)
        else
            let nextJoltageAdapterValue = adapterCandidates |> Array.min
            let nextJoltageAdapter = {
                Increment = nextJoltageAdapterValue - startValue
                Value = nextJoltageAdapterValue
            }
            appendJoltageChain input nextJoltageAdapterValue (nextJoltageAdapter :: joltageAdapterChain)

    appendJoltageChain input 0 joltageChain

let calculateJoltageAdapters (joltageAdapterChain : JoltageAdapter list) =
    let increments = 
        joltageAdapterChain
        |> List.countBy (fun f -> f.Increment)
    increments

let single f xs = System.Linq.Enumerable.Single(xs, System.Func<_,_>(f))

let calculatePart1Answer (differences : (int * int) list) =
    let ones = 
        differences 
        |> single (fun f -> (fst f) = 1)
    let threes = 
        differences 
        |> single (fun f -> (fst f) = 3)
    (snd ones) * (snd threes)

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 10 - Part 1"

    let joltInput = 
        File.ReadAllLines("./input/input_day10.txt")
        |> Array.map (fun f -> f |> System.Int32.Parse)

    let joltageAdapterChain = constructJoltageAdapterChain joltInput
    let joltageAdapterCounts = calculateJoltageAdapters joltageAdapterChain

    let answer1 = calculatePart1Answer joltageAdapterCounts

    printfn "With joltage adapters: %A the answer is : %d " joltageAdapterCounts answer1
    0 // return an integer exit code