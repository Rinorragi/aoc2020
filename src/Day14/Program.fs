open System
open System.Collections.Generic

type Instruction = {
    Address : int64
    Value : int64
    Mask : string
}

let nl = "\n"

let toInstruction (instructionGroup : string) =
    let instructs = instructionGroup.Split(nl, StringSplitOptions.RemoveEmptyEntries)
    let mask = instructs.[0]
    let instructionTypes = 
        instructs.[1 ..] 
        |> Array.map (fun f -> 
            let parts = f.Split(" = ")
            {
                Mask = mask
                Address = parts.[0].[4 .. parts.[0].Length - 2] |>  System.Int64.Parse
                Value = parts.[1] |> System.Int64.Parse
            })
    instructionTypes

let rec intToBinary (i : int64) =
    if i = int64 0 || i = int64 1
    then 
        string i
    else
        let bit = string (i % int64 2)
        (intToBinary (i / int64 2)) + bit

let calculateInstruction (ins : Instruction) =
    let sValue = intToBinary ins.Value |> Seq.rev |> Seq.map (string) |> String.concat ""
    let reverseMask = ins.Mask |> Seq.rev |> Seq.map (string) |> String.concat ""
    let mutable bitValue = ""
    let mutable acc = 0
    while acc < reverseMask.Length do 
        if reverseMask.[acc] = 'X'
        then
            if acc < sValue.Length
            then
                bitValue <- bitValue + string sValue.[acc]
            else 
                bitValue <- bitValue + "0"
        else 
            bitValue <- bitValue + string reverseMask.[acc]
        acc <- acc + 1
    bitValue <- (bitValue |> Seq.rev |> Seq.map (string) |> String.concat "")
    let iBitValue = Convert.ToInt64(bitValue, 2)
    ins.Address, iBitValue

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 14 - Part 1"

    let dockingInput = 
        System.IO.File.ReadAllText "./input/input_day14.txt"
    let dockingIstructions =
        dockingInput
            .Replace("\r\n", nl) // if there is windows line-endings harmonize them
            .Split("mask = ", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (toInstruction)
        |> Array.reduce Array.append

    let answer =
        dockingIstructions 
        |> Array.map (calculateInstruction)
        |> Array.rev
        |> Array.distinctBy (fst)
        |> Array.sumBy (snd)

    printfn "Part 1 answer: %d" answer

    0 // return an integer exit code