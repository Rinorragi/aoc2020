open System
open System.Text.RegularExpressions

let  (|RegexMatcher|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [for g in m.Groups -> g.Value ])
    else None

let (|Int64Matcher|_|) (str : string) =
    match System.Int64.TryParse str with
    | (true, int64) -> Some(int64)
    | _ -> None

let rec calculateNext (mathArray : string array) (lastOperator : string) (acc : Int64) = 
    if mathArray.Length = 0
    then
        acc
    else
        let mathTail = mathArray |> Array.tail    
        let mathHead = mathArray.[0]
        printfn "Head: '%s' Operator: '%s' Value: '%d' Arraytail: %A" mathHead lastOperator acc mathTail
        match mathHead with
        | Int64Matcher int64Head -> 
            match lastOperator with 
                | RegexMatcher @"(\+)" [ op ] ->
                    calculateNext mathTail lastOperator (acc + int64Head) 
                | RegexMatcher @"(\*)" [ op ] ->
                    calculateNext mathTail lastOperator (acc * int64Head)
                | "" -> 
                    calculateNext mathTail lastOperator int64Head
                | _ -> failwith ("Unknown operator: " + lastOperator)
        | RegexMatcher @"(\+)" [ op ] ->
            calculateNext mathTail "+" acc
        | RegexMatcher @"(\*)" [ op ] ->
            calculateNext mathTail "*" acc
        | _ -> failwith "Not yet"

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 18"
    let mathInput = System.IO.File.ReadAllLines "./input/input_day18_example.txt"
    let mathArrays = 
        mathInput
        |> Array.map (fun s -> s.Split(" "))

    let test = calculateNext mathArrays.[0] "" (int64 0)

    printfn "%d" test
    0 // return an integer exit code