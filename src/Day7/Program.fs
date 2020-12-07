open System

type BagRule = 
    {
        BagColor: string
        BagAncestorStr: string
        SuccessorBags: (string * int) array } 

let toBagRuleType (aBag : string) (containedBags : string) =
    
    let bagArray = 
        match containedBags with
        | containedBags when containedBags.Contains("no other bags.") -> 
            let emptyArray : (string * int) array = [||]
            emptyArray
        | _ -> 
            // Seems to have only 0-9 so let's be stupid
            containedBags.Split(", ", StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun f -> f.Substring(2, f.Length - 2).Split(" bag").[0], (f.Substring(0,1) |> System.Int32.Parse))

    {
        BagColor = aBag
        BagAncestorStr = containedBags
        SuccessorBags = bagArray}

let rec bagSearch (bags : BagRule array) (color : string) (foundBagsAcc : BagRule array) =
    let ancestors = 
        bags 
        |> Array.filter (fun f -> ((f.BagAncestorStr).Contains color && (not (Array.exists (fun x -> f.BagColor = x.BagColor) foundBagsAcc))))

    printfn "%A ancestor candidates" ancestors

    let accumulatedFoundBags =
        foundBagsAcc
        |> Array.append ancestors

    let ancestorsUnited = 
        ancestors 
        |> Array.map (fun f -> bagSearch bags f.BagColor accumulatedFoundBags)

    ancestorsUnited 
        |> Array.concat
        |> Array.append accumulatedFoundBags
        |> Array.distinct

    

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 7 - Part 1"
    let aviationRegulationRuleInput = 
        System.IO.File.ReadAllText "./input/input_day7.txt"

    let nl = "\n" // input seems to be in unix format
    let aviationRegulationRules =
        aviationRegulationRuleInput
            .Replace("\r\n", "\n") // if there is windows line-endings harmonize them
            .Split(nl, StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun f -> f.Split(" bags contain "))
        |> Array.map (fun f -> toBagRuleType f.[0] f.[1])

    let suitableBags =
        bagSearch aviationRegulationRules "shiny gold" [||]
    printfn "%A %d" suitableBags suitableBags.Length
    0 // return an integer exit code