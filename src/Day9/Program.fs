open System
open System.IO
open System.Numerics

let thrd (_, _, c) = c

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 9 - Part 1"

    let xmasInput = 
        File.ReadAllLines("./input/input_day9.txt")
        |> Array.map (fun f -> f |> BigInteger.Parse)
    
    printfn "%A" xmasInput 

    let preAmbleLength = 25
    let mutable counter = preAmbleLength
    let mutable continueLooping = true 
    while counter < xmasInput.Length - 1 && continueLooping do
        let validationGroup = 
            xmasInput.[(counter - preAmbleLength) .. counter] 
            |> Seq.ofArray 
        let validationGroupSums = 
            Seq.allPairs validationGroup validationGroup
            |> Seq.map (fun f -> fst f, snd f, (fst f + snd f))
        counter <- counter + 1
        let valueToFind = xmasInput.[counter]
        if not (Seq.exists (fun f -> thrd f = valueToFind) validationGroupSums)
        then 
            printfn "Answer 1: %A" valueToFind
            let rec findSumContiguation (reverseCounterStart : int) =
                let mutable reverseCounter = reverseCounterStart
                let mutable reverseSum = xmasInput.[reverseCounter]
                while reverseSum < valueToFind do
                    reverseCounter <- reverseCounter - 1
                    reverseSum <- reverseSum + xmasInput.[reverseCounter]
                if reverseSum = valueToFind
                then 
                    (reverseCounter, reverseCounterStart)
                else 
                    findSumContiguation (reverseCounterStart - 1)
            
            let answerIndexes = findSumContiguation (counter - 1)
            let answer2elements = 
                xmasInput.[fst answerIndexes .. snd answerIndexes] |> Array.max, 
                xmasInput.[fst answerIndexes .. snd answerIndexes] |> Array.min
            printfn "Answer 2: %A creates sum (%A)  with indexes %A" answer2elements (fst answer2elements + snd answer2elements) answerIndexes
            continueLooping <- false

        
    0 // return an integer exit code