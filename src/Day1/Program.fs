open System
open System.IO

// Advent of Code Day 1
// https://adventofcode.com/2020/day/1
// Find the two entries that sum to 2020; what do you get if you multiply them together?

// Read line by line and yield as you go
let readLines(filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let rec cartList nll =
    let f0 n nll =
        match nll with
        | [] -> [[n]]
        | _ -> List.map (fun nl->n::nl) nll
    match nll with
    | [] -> []
    | h::t -> List.collect (fun n->f0 n (cartList t)) h

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 1 - Part 1"
    let elementToMultipleCandidates : int list = 
        readLines "./input/input_day1.txt" 
        |> Seq.map System.Int32.Parse
        |> List.ofSeq

    let cartesian = cartList [elementToMultipleCandidates;elementToMultipleCandidates]

    let myFilter = fun n -> List.sum n = 2020
    let matchingTuples = cartesian |> List.filter myFilter

    let candidate = matchingTuples |> List.head
    let firstItem = candidate |> List.head
    let lastItem = candidate |> List.last

    printf "%d " (firstItem*lastItem)

    printfn "Advent of Code Day 1 - Part 2"
    let allPairs =
        List.allPairs elementToMultipleCandidates elementToMultipleCandidates
        |> List.groupBy id
        |> List.choose (fun ((a,b),values) -> if a= b && List.length values = 1 then None else Some (a,b))

    let answer = 
        elementToMultipleCandidates |> Seq.pick (fun c ->
            allPairs |> Seq.tryPick (fun (a,b) -> if a + b + c = 2020 then Some (a*b*c) else None))
    
    0 // return an integer exit code