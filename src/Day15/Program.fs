open System
open System.Collections.Generic

[<EntryPoint>]
let main argv =
    let initialNumbers = [15;5;1;4;7;0]
    // Set last number to last number spot
    let mutable lastNumber = initialNumbers.[initialNumbers.Length - 1]
    // Initialize so that the last number is still missing from the list
    let spokenNumbersDict = new Dictionary<int, int>()
    for i in [0 .. initialNumbers.Length - 2] do
        spokenNumbersDict.Add(initialNumbers.[i], i)
    // 0-based array so 2019 is the 2020th
    for i in [initialNumbers.Length .. 29999999] do
        let lastRoundNumber = lastNumber
        if spokenNumbersDict.ContainsKey lastRoundNumber
        then
            let lastIndex = spokenNumbersDict.[lastRoundNumber]
            lastNumber <- i - lastIndex - 1
        else 
            lastNumber <- 0
        spokenNumbersDict.[lastRoundNumber] <- i - 1
        if i = 2019 then printfn "Part 1 answer: %d" lastNumber
    printfn "Part 2 answer: %d" lastNumber
    0 // return an integer exit code