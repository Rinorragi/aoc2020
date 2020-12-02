// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Aoc.AocHelper

type AocPasswordPart1 =
    {   PolicyMin: int
        PolicyMax: int
        PolicyChar: char
        Password: string
        PolicyCharCount : int}

type AocPasswordPart2 = 
    {
        PolicyPos1: int
        PolicyPos2: int
        PolicyChar: char
        Password: string
        PolicyPos1Match: bool
        PolicyPos2Match: bool
    }

let count aChar aStr =
    aStr
    |> Seq.filter (fun x -> x = aChar)
    |> Seq.length

let toAocPasswordPart1 (str:string) =
    let values = str.Split[|' ';'-'|]
    { 
        PolicyMin = values.[0] |> System.Int32.Parse; 
        PolicyMax = values.[1] |> System.Int32.Parse; 
        PolicyChar = values.[2].Chars(0)
        Password = values.[3] 
        PolicyCharCount = values.[3] |> count (values.[2].Chars(0)) }

let toAocPasswordPart2 (str:string) =
    let values = str.Split[|' ';'-'|]
    { 
        // adjust also from one-based to zerobased indexes
        PolicyPos1 = values.[0] |> System.Int32.Parse |> (fun f -> f-1) 
        PolicyPos2 = values.[1] |> System.Int32.Parse |> (fun f -> f-1)
        PolicyChar = values.[2].Chars(0)
        Password = values.[3] 
        PolicyPos1Match = values.[3].Chars((values.[0] |> System.Int32.Parse |> (fun f -> f-1))) = values.[2].Chars(0)  
        PolicyPos2Match = values.[3].Chars((values.[1] |> System.Int32.Parse |> (fun f -> f-1))) = values.[2].Chars(0)}

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 2 - Part 1"

    let pwdInput = 
        readLines "./input/input_day2.txt"

    let pwdsWithPolicyPart1 = 
        pwdInput
        |> Seq.map toAocPasswordPart1
        |> Seq.toList 

    let pwdsCount = pwdsWithPolicyPart1 |> Seq.length

    let correctPwdsCount =        
        pwdsWithPolicyPart1 
        |> Seq.where (fun a -> 
            a.PolicyCharCount >= a.PolicyMin 
            && a.PolicyCharCount <= a.PolicyMax)
        |> Seq.length

    printfn "Pwds %d from which %d are right " pwdsCount correctPwdsCount 

    printfn "Advent of Code Day 2 - Part 2"

    let pwdsWithPolicyPart2 = 
        pwdInput
        |> Seq.map toAocPasswordPart2
        |> Seq.toList 

    printfn "Pwd part 2 %A " pwdsWithPolicyPart2

    let correctPwdsCountPart2 =
        pwdsWithPolicyPart2
        |> Seq.where (fun a -> 
            a.PolicyPos1Match <> a.PolicyPos2Match) // xor for exactly one
        |> Seq.length

    printfn "Pwds %d from which %d are right " pwdsCount correctPwdsCountPart2 

    0
         


    