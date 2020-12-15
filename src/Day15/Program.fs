open System

[<EntryPoint>]
let main argv =
    let initialNumbers = [15;5;1;4;7;0]
    // Set last number to last number spot
    let mutable lastNumber = initialNumbers.[initialNumbers.Length - 1]
    // Initialize so that the last number is still missing from the list
    let mutable (spokenNumbers : int list) = initialNumbers.[0 .. initialNumbers.Length - 2]
    // 0-based array so 2019 is the 2020th
    for i in [initialNumbers.Length .. 29999999] do
        let lastRoundNumber = lastNumber
        if (spokenNumbers |> List.exists (fun f -> f = lastRoundNumber))
        then
            let lastIndex = (spokenNumbers |> List.findIndexBack (fun f -> f = lastRoundNumber))
            lastNumber <- spokenNumbers.Length - lastIndex
        else 
            lastNumber <- 0
        spokenNumbers <- [lastRoundNumber] |> List.append spokenNumbers
        if i = 2019 then printfn "Part 1 answer: %d" lastNumber
        if i % 100000 = 0 then printfn "%d %A" i ((System.DateTime.Now).ToLocalTime())
    printfn "Part 2 answer: %d" lastNumber
        

    0 // return an integer exit code