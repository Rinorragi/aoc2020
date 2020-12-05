open System
open Aoc.AocHelper
open System.Text.RegularExpressions

let validateEntry (value : bool * string) (pattern : string) =
    fst value && Regex.IsMatch(snd value, pattern)

let validateDigitsBetween (value : bool * string) (min : int) (max : int) =
    let intValue = snd value
    fst value && (intValue |> int |> (fun f -> f >= min && f <= max))

let validateHeight (value : bool * string) = 
    let hgtString = snd value
    fst value 
        && (
        if hgtString.EndsWith "in" then hgtString.[ .. 1] |> int |> (fun f -> f >= 59 && f <= 76)
        elif hgtString.EndsWith "cm" then hgtString.[ .. 2] |> int |> (fun f -> f >= 150 && f <= 193)
        else false)

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 4 - Part 1"

    let passInput = 
        System.IO.File.ReadAllText "./input/input_day4.txt"
    
    let nl = "\n" // input seems to be in unix format
    let passPorts =
        passInput.Split(nl + nl)
        |> Array.map (
            fun passPort -> passPort.Replace(nl, " ").Split ' ')
    
    let passPortDicts =
        passPorts
        |> Array.map (fun (f : string array) -> 
            printfn "%A " f
            dict [ for p in f -> (p.Split ':').[0], (p.Split ':').[1] ])

    let validPassPorts =
        passPortDicts
        |> Array.choose (fun f -> 
            if f.ContainsKey "byr" 
                && f.ContainsKey "iyr"
                && f.ContainsKey "eyr"
                && f.ContainsKey "hgt"
                && f.ContainsKey "hcl"
                && f.ContainsKey "ecl"
                && f.ContainsKey "pid"
            then Some f
            else None)
    printfn "Passports %d valid %d " passPorts.Length validPassPorts.Length
    
    printfn "Advent of Code Day 4 - Part 2"

    let byrRegex = @"^\d{4}$" // 1920-2002 
    let iyrRegex = @"^\d{4}$" // 2010-2020
    let eyrRegex = @"^\d{4}$" // 2020-2030
    let hclRegex = @"^#[0-9a-f]{6}$"
    let eclRegex = @"^(amb|blu|brn|gry|grn|hzl|oth)$"
    let pidRegex = @"^\d{9}$"
    let hgtRegex = @"^(\d{3}cm|\d{2}in)$" // between 150 193 or between 59 76

    let validPassPortsPart2 = 
        validPassPorts // all the keys should be here because former validation
        |> Array.choose (fun f ->
            if validateEntry (f.TryGetValue "byr") byrRegex
                && validateDigitsBetween (f.TryGetValue "byr") 1920 2002
                && validateEntry (f.TryGetValue "iyr") iyrRegex
                && validateDigitsBetween (f.TryGetValue "iyr") 2010 2020
                && validateEntry (f.TryGetValue "eyr") eyrRegex
                && validateDigitsBetween (f.TryGetValue "eyr") 2020 2030
                && validateEntry (f.TryGetValue "hcl") hclRegex
                && validateEntry (f.TryGetValue "ecl") eclRegex
                && validateEntry (f.TryGetValue "pid") pidRegex
                && validateEntry (f.TryGetValue "hgt") hgtRegex
                && validateHeight (f.TryGetValue "hgt")
            then Some f
            else None)

    
    printfn "Passports %d valid %d " passPorts.Length validPassPortsPart2.Length
    
    0 // return an integer exit code