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

let pickRule (ruleStr : string) (ruleStrArray : (int * string) array) = 
    let nextRules = 
        ruleStr.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> 
            let nrNro = s |> System.Int32.Parse
            let nextRule = 
                ruleStrArray 
                |> Array.pick (fun (aNro, aRule) -> 
                    if (aNro = nrNro)
                    then Some(aNro, aRule)
                    else None)
            nextRule
        )
    nextRules

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

let rec validateSatelliteMessage (str : string) (startRule : SatelliteRule) (rules : Map<int,SatelliteRule>) =
    true

    
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
    
    printfn "%A" rules
    
    0 // return an integer exit code