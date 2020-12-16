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
    let fieldArray = [|0 .. fieldAmount - 1|]
    let mutable fieldsFound : int array = [||]
    // Rules and to which fields they are matching
    let answer2 = 
        rules
        |> Array.map (fun rule ->
            let validField =
                fieldArray
                |> Array.map (fun col -> 
                    col, validTickets 
                    |> Array.map(fun r -> col, isFieldValid r.[col] rule)
                    |> Array.forall snd)
                |> Array.filter (snd)
                |> Array.map (fst)
            rule, validField)
        |> Array.sortBy (fun f -> snd f |> Array.length)
        |> Array.map (fun f -> 
            let ruleToHandle = f
            let nextFreeIndex = 
                snd ruleToHandle 
                |> Array.filter (fun rf -> not (fieldsFound |> Array.contains rf))
                |> Array.head
            fieldsFound <- Array.append [|nextFreeIndex|] fieldsFound
            fst ruleToHandle, nextFreeIndex)
        |> Array.filter (fun rf -> (fst rf).Name.Contains("departure"))
        |> Array.map (fun af -> 
            int64 myTicket.[snd af])
        |> Array.reduce (fun acc value -> (acc * value))
            
    printfn "Answer part 2: %d" answer2

    0 // return an integer exit code