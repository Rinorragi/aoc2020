open System
let nl = "\n"

type Rule = {
    Name : string
    Range1 : (int * int)
    Range2 : (int * int) }

let toRule (param : string) =
    let values = 
        param.Split(": ").[1].Split(" or ") // should be e.g [1-3;5-7]
        |> Seq.map (fun f -> 
            let sValues =f.Split("-")
            (sValues.[0] |> System.Int32.Parse), (sValues.[1] |> System.Int32.Parse))
        |> Array.ofSeq
    {
        Name = param.Split(": ").[0]
        Range1 = values.[0]
        Range2 = values.[1] }

let isFieldValid (field : int) (rule : Rule) =
    (field >= fst rule.Range1 && field <= snd rule.Range1) 
    || (field >= fst rule.Range2 && field <= snd rule.Range2)

let invalidValues (ticketFields : int array) (rules : Rule array) =
    Array.sumBy (fun c -> 
            if (rules |> Array.exists (fun r -> isFieldValid c r))
            then 0
            else c) ticketFields

let splitTicket (param : string) = 
    let sValues = param.Split(",")
    sValues
        |> Array.map System.Int32.Parse
let getFrst (a, _, _) = a
let getSnd (_, b, _) = b
let getThrd (_, _, c) = c

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 16"
    let ticketInput = 
        (System.IO.File.ReadAllText "./input/input_day16.txt")
            .Replace("\r\n", nl) // if there is windows line-endings harmonize them
            .Split(nl+nl)

    let myTicket = 
        ticketInput.[1].Split(nl).[1] // skip the topic
        |> splitTicket
    let otherTickets = 
        ticketInput.[2].Split(nl).[1 .. ] // skip the topic
        |> Array.map splitTicket
    let rules = 
        ticketInput.[0].Split(nl)
        |> Array.map toRule
    let answerPart1 =
        otherTickets 
        |> Array.sumBy (fun f -> invalidValues f rules)
    printfn "Answer part 1: %d" answerPart1
    
    let validTickets = 
        otherTickets
        |> Array.filter (fun row -> 
            row 
                |> Array.map (fun field -> (rules |> Array.exists (fun rule -> isFieldValid field rule))) 
                |> Array.forall (id))

    let fieldAmount = myTicket.Length
    let answer2 = 
        rules
        |> Array.map (fun rule -> // Rules and to which fields they are matching
            let validField =
                [|0 .. fieldAmount - 1|]
                |> Array.map (fun col -> 
                    col, validTickets 
                    |> Array.map(fun r -> col, isFieldValid r.[col] rule)
                    |> Array.forall snd)
                |> Array.filter (snd)
                |> Array.map (fst)
            rule, validField)
        |> Array.sortBy (snd >> Array.length) // Sort the rule macthing arrays so that the least matching column is first
        |> Array.fold (fun (accState : (Rule * int * int list) list) (ruleMatch : Rule * int array) -> 
            let usedIndexes = 
                match accState.Length with 
                | 0 -> []
                | _ -> accState |> List.head |> getThrd
            let nextFreeIndex =
                snd ruleMatch
                |> Array.filter (fun rf -> not (usedIndexes |> List.contains rf))
                |> Array.head
            let rule = fst ruleMatch
            let updatedUsedIndexes = nextFreeIndex::usedIndexes
            let stateToAppend = rule, nextFreeIndex, updatedUsedIndexes
            stateToAppend::accState) []
        |> List.filter (fun rf -> (getFrst rf).Name.Contains("departure")) // Filter the departure columns 
        |> List.map (fun af -> // Rules does not matter any more, convert myTicket values to int64
            int64 myTicket.[getSnd af])
        |> List.reduce (fun acc value -> (acc * value)) // multiply all elements to get the answer
            
    printfn "Answer part 2: %d" answer2

    0 // return an integer exit code