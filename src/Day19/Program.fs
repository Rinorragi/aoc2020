open System

let nl = "\n"

type SatelliteRule = {
    Number : int
    IsCharacterRule : bool
    HasAlternatives : bool
    NextRules : int array
    AlternativeRules : int array
    Character : string
} with 
    static member InitRule (no : int) (isTerminating : bool) (hasAlternatives : bool) (nextRules : int array) (alternativeRules : int array) (str : string) =
        {
            Number = no
            IsCharacterRule = isTerminating
            HasAlternatives = hasAlternatives
            NextRules = nextRules
            AlternativeRules = alternativeRules
            Character = str }

let concatMaps (ruleMap1 : Map<int,SatelliteRule>) (ruleMap2 : Map<int,SatelliteRule>) = 
    Map.fold (fun acc key value -> Map.add key value acc) ruleMap1 ruleMap2

let rec mergeArrayOfMaps (ruleMapArr : Map<int,SatelliteRule> array) (ruleMapAcc : Map<int,SatelliteRule>) = 
    if ruleMapArr.Length = 0
    then
        ruleMapAcc
    else
        let newAcc = concatMaps (ruleMapArr |> Array.head) ruleMapAcc
        mergeArrayOfMaps (ruleMapArr |> Array.tail) newAcc

let constructRule (rule : (int * string)) =
    let isTerminating = (snd rule).Contains('"')
    let hasAlternatives = (snd rule).Contains('|')
    let idNro = fst rule
    if isTerminating 
    then
        let quoteIndex = (snd rule).IndexOf('"')
        let charStr = (string ((snd rule).[quoteIndex + 1]))
        SatelliteRule.InitRule idNro isTerminating hasAlternatives [||] [||] charStr
    else 
        if hasAlternatives
        then
            let optionalRoutes = (snd rule).Split(" | ")
            let nextRules = 
                optionalRoutes.[0].Split(" ")
                |> Array.map (fun s -> s |> System.Int32.Parse)
            let otherRules = 
                optionalRoutes.[1].Split(" ")
                |> Array.map (fun s -> s |> System.Int32.Parse)
            SatelliteRule.InitRule idNro isTerminating hasAlternatives nextRules otherRules ""

        else
            let nextRules = 
                (snd rule).Split(" ")
                |> Array.map (fun s -> s |> System.Int32.Parse)
            SatelliteRule.InitRule idNro isTerminating hasAlternatives nextRules [||] ""

let rec validateSatelliteMessageAgainstRule (str : string) (rule : SatelliteRule) (rules : Map<int,SatelliteRule>) (accInit : int) = 
    // Rule is always connecting rule at the beginning
    let mutable acc = accInit
    let mutable shouldContinue = true
    for r in rule.NextRules do
        let nextRule = rules.[r]
        if nextRule.IsCharacterRule
        then 
            printfn "Message: '%s' Rule: %d Acc: %d Char: %s IndexChar: %s" str nextRule.Number acc nextRule.Character (string str.[acc])
            printfn "%A" nextRule
            if (string str.[acc]) = rule.Character
            then acc <- acc + 1
            else shouldContinue <- false
    
    shouldContinue
        (*
        if rule.HasAlternatives
        then
            let nextRules =
                rule.NextRules 
                |> Array.map (fun r -> validateSatelliteMessageAgainstRule str rules.[r] rules acc)
                |> Array.forall id
            let otherRules =
                rule.AlternativeRules 
                |> Array.map (fun r -> validateSatelliteMessageAgainstRule str rules.[r] rules acc)
                |> Array.forall id
            nextRules || otherRules
        else
            rule.NextRules 
            |> Array.map (fun r -> validateSatelliteMessageAgainstRule str rules.[r] rules acc)
            |> Array.forall id**)
            

let validateSatelliteMessage (str : string) (startRule : SatelliteRule) (rules : Map<int,SatelliteRule>) =
    validateSatelliteMessageAgainstRule str startRule rules 0

    
[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 19"
    let satelliteInput = 
        (System.IO.File.ReadAllText "./input/input_day19_example.txt")
            .Replace("\r\n", nl) // if there is windows line-endings harmonize them
            .Split(nl+nl, StringSplitOptions.RemoveEmptyEntries)

    let rules = 
        satelliteInput.[0].Split(nl)
        |> Array.map (fun s -> 
            let row = s.Split(": ")
            let nro = (row.[0] |> Int32.Parse)
            let rule = constructRule (nro, row.[1])
            (nro, rule))
        |> Map.ofArray

    let startPoint = rules.[0]

    let messages = 
        satelliteInput.[1].Split(nl, StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> (s, validateSatelliteMessage s startPoint rules))
    
    printfn "NOT FINISHED - DOES NOT WORK YET"
    
    0 // return an integer exit code