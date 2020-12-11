open System
open System.IO

type JoltageAdapter = {
    Increment : int
    Value : int}

let getThrd (_, _, c, _) = c
let getSnd (_, b, _, _) = b
let getFrth (_, _, _, d) = d

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

let joltageAdaptersCouldBeDropped (joltageAdapterChain : JoltageAdapter list) =

    joltageAdapterChain 
    |> List.mapi (fun i f -> 
        let prevValue = if (i = 0) then 0 else joltageAdapterChain.[i - 1].Value
        let canBeDroppedAlone = 
            i < joltageAdapterChain.Length - 1 
            && joltageAdapterChain.[i+1].Value <= prevValue + 3
        let canbeDoubleDroppedWithNextOne =
            canBeDroppedAlone
            && i < joltageAdapterChain.Length - 2
            && joltageAdapterChain.[i+2].Value <= prevValue + 3 
        (f, i, (canBeDroppedAlone), canbeDoubleDroppedWithNextOne))

let factorial (n : bigint) = [(bigint 1)..n] |> List.reduce (*)

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 10 - Part 1"

    let joltInput = 
        File.ReadAllLines("./input/input_day10.txt")
        |> Array.map (fun f -> f |> System.Int32.Parse)

    let joltageAdapterChain = 
        constructJoltageAdapterChain joltInput
        |> List.rev
    let joltageAdapterCounts = calculateJoltageAdapters joltageAdapterChain

    let answer1 = calculatePart1Answer joltageAdapterCounts

    printfn "With joltage adapters: %A the answer is : %d " joltageAdapterCounts answer1
    printfn "Advent of Code Day 10 - Part 2"
    let couldBeDroppedAdapters = 
        joltageAdaptersCouldBeDropped joltageAdapterChain
        |> List.filter (getThrd)
        |> List.map (getSnd)

    let couldBeDoubleDroppedAdapters =
        joltageAdaptersCouldBeDropped joltageAdapterChain
        |> List.filter (getFrth)
        |> List.map (getSnd)

    let singleCount = couldBeDroppedAdapters |> Seq.length
    let doubleCount = couldBeDoubleDroppedAdapters |> Seq.length
    printfn "Single droppable %d double droppable %d" singleCount doubleCount 

    let singleCountPermutations = factorial (bigint singleCount)
    let doubleCountPermutations = factorial (bigint doubleCount)
    printfn "%d + %A + %A" 1 singleCountPermutations doubleCountPermutations
    // TODO: figure out how to calculate permutations without brute forcing since trying to iterate 15! with example values kills the CPU

    0 // return an integer exit code