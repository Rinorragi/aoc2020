open System



let calculateCommonAswersForGroup (customsGroup : string array)=
    customsGroup 
    |> Array.map Set.ofSeq
    |> Set.intersectMany
    |> Set.count

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 6 - Part 1"
    let customsInput = 
        System.IO.File.ReadAllText "./input/input_day6.txt"
    
    let nl = "\n" // input seems to be in unix format
    let customGroups =
        customsInput.Split(nl + nl)

    let distinctCustomAnswers = 
        customGroups
        |> Array.map (
            fun customsGroup -> 
                customsGroup
                    .Replace(nl, "")
                    .Replace(" ", "")
                    .Trim()
                |> Seq.toList
                |> List.distinct)

    let answer =
        distinctCustomAnswers
        |> Array.map (fun f -> f.Length)
        |> Seq.sum
    
    printfn "Answer 1: %d " answer

    printfn "Advent of Code Day 6 - Part 2"
    // To be honest not sure if all trimming and replacing and mangling is necessary
    let commonCustomAnswers = 
        customGroups
        |> Array.map (
            fun customsGroup ->
                customsGroup.Split(nl)
                |> Array.map (
                    fun answerRow -> 
                        answerRow
                        |> Seq.where System.Char.IsLetter
                        |> Seq.toList
                        |> List.distinct
                        |> Array.ofList
                        |> System.String)
                |> calculateCommonAswersForGroup)

    let answer2 =
        commonCustomAnswers
        |> Seq.sum     

    printfn "Answer 2 array %A " commonCustomAnswers
    printfn "Answer 2: %d " answer2

    0 // return an integer exit code