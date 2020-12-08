open System
open System.IO
open System.Collections.Generic

type Instruction = {
    Command : string
    Value : int 
    Index : int}

let toInstruction (param : string) (index : int)= 
    let instuctionArray = param.Split(" ", StringSplitOptions.RemoveEmptyEntries)
    let value = instuctionArray.[1] |> System.Int32.Parse
    {
        Command = instuctionArray.[0]
        Value = value
        Index = index  }

let instructionExecution (instructions : Instruction array) =

    let executedList = new List<Instruction>()
    let mutable indexPointer = 0
    let mutable accumulator = 0

    while not (executedList.Exists(fun f -> f.Index = indexPointer)) && indexPointer < instructions.Length do
        let toExecute = instructions.[indexPointer]
        match toExecute.Command with
        | "jmp" -> indexPointer <- indexPointer + toExecute.Value
        | "acc" -> 
            accumulator <- accumulator + toExecute.Value
            indexPointer <- indexPointer + 1
        | "nop" -> indexPointer <- indexPointer + 1
        | _ -> ()
        executedList.Add(toExecute)
        
    (accumulator, executedList.FindLast(fun x -> true).Index)


[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 8 - Part 1"

    let instructionInput = 
        File.ReadAllLines("./input/input_day8.txt")

    let (instruction : Instruction array) = 
        Array.init instructionInput.Length (fun i -> toInstruction instructionInput.[i] i)
    let answer = 
        instruction
        |> instructionExecution
    printfn "Answer 1: %d with last index %d" (fst answer) (snd answer)

    printfn "Advent of Code Day 8 - Part 2"
    let mutable counter = 0
    while counter < instructionInput.Length - 1 do
        match instruction.[counter].Command with
        | "jmp" | "nop" -> 
            let answerCandidate = 
                instruction 
                    |> Array.map (fun f -> 
                        if f.Index = counter && f.Command = "jmp" 
                        then { 
                            Command = "nop"
                            Index = f.Index
                            Value = f.Value }
                        elif f.Index = counter && f.Command = "nop"
                        then {
                            Command = "jmp"
                            Index = f.Index
                            Value = f.Value }
                        else f)
                    |> instructionExecution
            if snd answerCandidate = instructionInput.Length - 1
            then 
                printfn "Answer 2: %d" (fst answerCandidate)
            ()
        | _ -> ()
        counter <- counter + 1

    0 // return an integer exit code