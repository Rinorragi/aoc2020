open System
open System.Linq;
open System.Numerics

type BagRule = 
    {
        Color: string
        AncestorStr: string
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
        Color = aBag
        AncestorStr = containedBags
        SuccessorBags = bagArray}

let rec bagAncestorSearch (bags : BagRule array) (color : string) (foundBagsAcc : BagRule array) =
    let ancestors = 
        bags 
        |> Array.filter (fun f -> ((f.AncestorStr).Contains color && (not (Array.exists (fun x -> f.Color = x.Color) foundBagsAcc))))

    let accumulatedFoundBags =
        foundBagsAcc
        |> Array.append ancestors

    let ancestorsUnited = 
        ancestors 
        |> Array.map (fun f -> bagAncestorSearch bags f.Color accumulatedFoundBags)

    ancestorsUnited 
        |> Array.concat
        |> Array.append accumulatedFoundBags
        |> Array.distinct

let bagSuccessorCalculation (bags : BagRule array) (startBag : string) =
    let theOneBag = 
        bags 
        |> Array.pick (fun f -> if f.Color = startBag then Some(f) else None)
    let rec bagSuccessorSearch (bags : BagRule array) (ancestorBag : BagRule) (foundBagsAcc : bigint) =
        let successors =
            ancestorBag.SuccessorBags
            |> Array.map (fun f -> bags |> Array.pick (fun x -> if x.Color = fst f then Some(x) else None))
        if successors.Length = 0
        then
            foundBagsAcc + bigint(1)
        else
            let toReturn = 
                ancestorBag.SuccessorBags
                    |> Array.map (
                        fun f -> bags |> Array.filter (fun z -> z.Color = fst f) |> Array.map (fun x -> 
                        printfn "Successor %s %d times" (fst f) (snd f)
                        (bigint(snd f) * (bagSuccessorSearch bags x foundBagsAcc))))
                    |> Array.concat
                    |> List.ofArray
            let sum = List.fold (+) (bigint(1)) toReturn
            sum
    bagSuccessorSearch bags theOneBag (bigint 0)

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
        bagAncestorSearch aviationRegulationRules "shiny gold" [||]
    printfn "Answer 1: %d" suitableBags.Length

    printfn "Advent of Code Day 7 - Part 2"
    let successorTree = 
        bagSuccessorCalculation aviationRegulationRules "shiny gold"
    
    printfn "Answer 2: %A " (successorTree - bigint(1)) 
    0 // return an integer exit code