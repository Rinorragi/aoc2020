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
        //printfn "Head: '%s' Operator: '%s' Value: '%d' Arraytail: %A" mathHead lastOperator acc mathTail
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
        | _ -> failwith ("Unsupported string:" + mathHead)

let rec findClosingBracket (mathStr : string) (index : int) (acc : int) = 
    match mathStr.[0] with
    | ')' -> 
        if (acc > 0) 
        then findClosingBracket (mathStr.[1 ..]) (index + 1) (acc - 1)
        else index + 1
    | '(' -> findClosingBracket (mathStr.[1 ..]) (index + 1) (acc + 1)
    | _ -> findClosingBracket (mathStr.[1 ..]) (index + 1) acc

let rec solveString (mathStr : string) =
    printfn "ss: %s" mathStr
    System.Threading.Thread.Sleep(100)
    if not (mathStr.Contains "(") 
    then
        calculateNext (mathStr.Split(" ",StringSplitOptions.RemoveEmptyEntries)) "" (int64 0)
    else
        let firstIndex = mathStr.IndexOf("(")
        let sub = mathStr.[firstIndex + 1 ..]
        let lastIndex = findClosingBracket sub firstIndex 0
        let subValue = solveString (mathStr.[firstIndex + 1 .. lastIndex - 1])
        let lastStr = 
            if (lastIndex + 1 >= mathStr.Length) 
            then ""
            else mathStr.[lastIndex + 1 .. ]
        let prettifiedMathStr = (
            mathStr.[ .. firstIndex - 1] 
            + subValue.ToString("X").ToLowerInvariant() 
            + lastStr)
        printfn "pf: %s,%s,%s" mathStr.[ .. firstIndex - 1] (subValue.ToString("X").ToLowerInvariant()) lastStr 
        solveString prettifiedMathStr 

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 18"
    let mathInput = System.IO.File.ReadAllLines "./input/input_day18_example.txt"
    let mathArrays = 
        mathInput
        |> Array.map solveString

    printfn "%A" mathArrays
    0 // return an integer exit code