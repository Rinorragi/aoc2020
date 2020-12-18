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

let rec findContinuousInteger (mathStr : string) (index : int) =
    if mathStr.Length = 0
    then 
        index + 1
    else 
        match (string mathStr.[0]) with
        | Int64Matcher _ -> findContinuousInteger (mathStr.[1 ..]) (index + 1)
        | _ -> index

let rec findReverseContinuousInteger (mathStr : string) (index : int) =
    if index = 0
    then
        index
    else
        match (string mathStr.[mathStr.Length - 1]) with
        | Int64Matcher _ -> findContinuousInteger (mathStr.[.. mathStr.Length - 2]) (index - 1)
        | _ -> index

let rec solveString (mathStr : string) (plusBeforeMultiply : bool) (printSteps : bool) =
    if printSteps
    then 
        System.Threading.Thread.Sleep 100
        printfn "SS: '%s'" mathStr
    if not (mathStr.Contains "(") 
    then
        if not (plusBeforeMultiply && mathStr.Contains "+" && mathStr.Contains "*")
        then
            calculateNext (mathStr.Split(" ",StringSplitOptions.RemoveEmptyEntries)) "" (int64 0)
        else
            let plusIndex = mathStr.IndexOf("+")
            let firstString = mathStr.[.. plusIndex - 2]
            let lastString = mathStr.[plusIndex + 2 ..]
            let startIndex = findReverseContinuousInteger firstString (plusIndex - 3)
            let endIndex = findReverseContinuousInteger lastString (plusIndex + 2)
            let subString = mathStr.[startIndex .. endIndex]
            if printSteps
            then 
                printfn "PTS-sub: '%s' %d %d" subString startIndex endIndex
            let subValue = solveString subString plusBeforeMultiply printSteps
            
            let firstStr =
                if startIndex = 0
                then ""
                else mathStr.[.. startIndex]
            let lastStr = 
                if (endIndex + 1 >= mathStr.Length) 
                then ""
                else mathStr.[endIndex + 1 .. ]
            if printSteps
            then     
                printfn "PTS: %s '+' %s || '%s''%d''%s'" firstString lastString firstStr subValue lastStr
            let prettifiedMathStr = (
                firstStr
                + subValue.ToString("G").ToLowerInvariant() 
                + lastStr)
            solveString prettifiedMathStr plusBeforeMultiply printSteps
    else
        let firstIndex = mathStr.IndexOf("(")
        let sub = mathStr.[firstIndex + 1 ..]
        let lastIndex = findClosingBracket sub firstIndex 0
        let subValue = solveString (mathStr.[firstIndex + 1 .. lastIndex - 1]) plusBeforeMultiply printSteps
        let lastStr = 
            if (lastIndex + 1 >= mathStr.Length) 
            then ""
            else mathStr.[lastIndex + 1 .. ]
        let prettifiedMathStr = (
            mathStr.[ .. firstIndex - 1] 
            + subValue.ToString("G").ToLowerInvariant() 
            + lastStr)
        solveString prettifiedMathStr plusBeforeMultiply printSteps

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 18"
    let mathInput = System.IO.File.ReadAllLines "./input/input_day18_example.txt"
    (*
    let answer1 = 
        mathInput
        |> Array.sumBy (fun s -> solveString s false)
    printfn "Part 1 answer: %d" answer1
    *)
    let answer2 = 
        mathInput
        //|> Array.skip 5
        |> Array.sumBy (fun s -> 
            let a = solveString s true false
            printfn "Answer for %s: %d" s a
            a)
        //|> Array.sumBy (fun s -> solveString s true)
    printfn "Part 2 answer: %d" answer2
    0 // return an integer exit code