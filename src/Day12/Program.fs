open System
open System.IO

type Instruction = {
    Command : char
    Value : int
}

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 12 - Part 1"
    let navigationInput = 
        File.ReadAllLines("./input/input_day12.txt")
        |> Array.map (fun x -> {
            Command = x.[0]
            Value = (x.[1 ..] |> System.Int32.Parse)
        })

    let mutable verticalPos = 0
    let mutable horizontalPos = 0
    let mutable direction = 90 // east is starting direction

    for instruct in navigationInput do
        match instruct.Command with  
        | 'N' -> verticalPos <- verticalPos + instruct.Value
        | 'S' -> verticalPos <- verticalPos - instruct.Value 
        | 'E' -> horizontalPos <- horizontalPos + instruct.Value
        | 'W' -> horizontalPos <- horizontalPos - instruct.Value
        | 'L' -> direction <- direction - instruct.Value
        | 'R' -> direction <- direction + instruct.Value
        | 'F' -> 
            let easifiedValue = direction % 360
            match easifiedValue with
            | 0 -> verticalPos <- verticalPos + instruct.Value // Same as north
            | -180 | 180 -> verticalPos <- verticalPos - instruct.Value // Same as south
            | -270 | 90 -> horizontalPos <- horizontalPos + instruct.Value // Same as east
            | -90 | 270 -> horizontalPos <- horizontalPos - instruct.Value // Same as west
            | _ -> printfn "Incomplete match with %d " easifiedValue
        | _ -> printfn "Incomplete match with %A " instruct

    let answer = (abs verticalPos) + (abs horizontalPos)
    printfn "Part 1: Manhattan distance from (vertical,horizontal) 0,0 to %d %d is %d" verticalPos horizontalPos answer

    verticalPos <- 0
    horizontalPos <- 0
    let mutable waypointVerticalPos = 1
    let mutable waypointHorizontalPos = 10
    for instruct in navigationInput do
        printf "%c%d: " instruct.Command instruct.Value
        match instruct.Command with  
        | 'N' -> waypointVerticalPos <- waypointVerticalPos + instruct.Value
        | 'S' -> waypointVerticalPos <- waypointVerticalPos - instruct.Value 
        | 'E' -> waypointHorizontalPos <- waypointHorizontalPos + instruct.Value
        | 'W' -> waypointHorizontalPos <- waypointHorizontalPos - instruct.Value
        | 'L' -> 
            let howManyTimes = instruct.Value / 90
            for i in 1 .. howManyTimes do 
                let wpCurrentVertical = waypointVerticalPos
                let wpCurrentHorizontal = waypointHorizontalPos
                waypointHorizontalPos <- -1 * wpCurrentVertical // North becomes west or south becomes east, both means -1
                waypointVerticalPos <- wpCurrentHorizontal // East becomes north or west becomes south
        | 'R' -> 
            let howManyTimes = instruct.Value / 90
            for i in 1 .. howManyTimes do 
                let wpCurrentVertical = waypointVerticalPos
                let wpCurrentHorizontal = waypointHorizontalPos
                waypointHorizontalPos <- wpCurrentVertical // North becomes east or south becomes west
                waypointVerticalPos <- -1 * wpCurrentHorizontal // East becomes south or west becomes north
        | 'F' -> 
            let oldVerticalPos = verticalPos
            let oldHorizontalPos = horizontalPos
            verticalPos <- verticalPos + (instruct.Value * waypointVerticalPos)
            horizontalPos <- horizontalPos + (instruct.Value * waypointHorizontalPos)
            printf "From (%d,%d) towards (%d,%d) %d times to position (%d,%d)" oldVerticalPos oldHorizontalPos waypointVerticalPos waypointHorizontalPos instruct.Value verticalPos horizontalPos
        | _ -> printf "Incomplete match with %A " instruct
        printfn " waypoint at (%d,%d)" waypointVerticalPos waypointHorizontalPos
    let answer = (abs verticalPos) + (abs horizontalPos)
    printfn "Part 2: Manhattan distance from (vertical,horizontal) 0,0 to %d %d is %d" verticalPos horizontalPos answer

    0 // return an integer exit code