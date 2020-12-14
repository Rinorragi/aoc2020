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

let caclculateMemoryAddress (ins : Instruction) = 
    let sAddress = intToBinary ins.Address |> Seq.rev |> Seq.map (string) |> String.concat ""
    let reverseMask = ins.Mask |> Seq.rev |> Seq.map (string) |> String.concat ""
    let numberOfX = reverseMask |> Seq.filter (fun f -> f = 'X') |> Seq.length
    let numberOfPermutations = pown 2 numberOfX
    let mutable bitValue = ""
    let mutable acc = 0
    // 0 unchanged (use the value of address)
    // 1 overrided by 1 (always 1)
    // X floating (get all possible values meaning 0,1)
    while acc < reverseMask.Length do 
        if reverseMask.[acc] = '0'
        then
            if acc < sAddress.Length
            then
                bitValue <- bitValue + string sAddress.[acc]
            else 
                bitValue <- bitValue + "0"
        else 
            bitValue <- bitValue + string reverseMask.[acc]
        acc <- acc + 1
    // Reverse address back to have X masked address
    bitValue <- (bitValue |> Seq.rev |> Seq.map (string) |> String.concat "")
    let addressArray = Array.create numberOfPermutations (int64 0, int64 0)
    for i in 0 .. addressArray.Length - 1 do
        let addressBitFloater = (intToBinary (int64 i)).PadLeft(numberOfX, '0')
        let mutable counter = numberOfX
        let uniqueAddress = 
            bitValue
            |> Seq.mapi (fun i f -> 
                match f with 
                | 'X' -> 
                    counter <- counter - 1
                    if counter > addressBitFloater.Length - 1
                    then 
                        '0'
                    else
                        addressBitFloater.[counter]
                | _ -> f
            )
            |> Seq.map (string) 
            |> String.concat ""
        Array.set addressArray i (Convert.ToInt64(uniqueAddress, 2), ins.Value)
    addressArray

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
        |> Array.rev // reverse so distinct leaves last values intact
        |> Array.distinctBy (fst)
        |> Array.sumBy (snd)

    printfn "Part 1 answer: %d" answer

    printfn "Advent of Code Day 14 - Part 2"
    let answer2 =
        dockingIstructions 
        |> Array.map (caclculateMemoryAddress)
        |> Array.rev // reverse so distinct leaves last values intact
        |> Array.reduce Array.append
        |> Array.distinctBy (fst)
        |> Array.sumBy (snd)

    printfn "Part 2 answer: %d" answer2
    0 // return an integer exit code