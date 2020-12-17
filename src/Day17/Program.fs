open System

type Cube = {
    X : int
    Y : int
    Z : int
    W : int
    Activated : bool
}
let initCube (x : int) (y : int) =
    {
        X = x
        Y = y
        Z = 0
        W = 0
        Activated = true }

let createCube4d (x : int) (y : int) (z : int) (w : int) =
    {
        X = x
        Y = y
        Z = z
        W = w
        Activated = true }

let createCube3d (x : int) (y : int) (z : int) =     
    {
        X = x
        Y = y
        Z = z
        W = 0
        Activated = true }

let countActiveNeighbors (x : int) (y : int) (z : int) (activeCubes : Cube list) =
    // 26 neighbors from three dimensional array
    activeCubes
    |> List.choose (fun ac -> 
        if  not (x = ac.X && y = ac.Y && z = ac.Z) // skip the given one
            && ([x - 1 .. x + 1] |> List.contains ac.X)
            && ([y - 1 .. y + 1] |> List.contains ac.Y)
            && ([z - 1 .. z + 1] |> List.contains ac.Z)
        then
            Some ac
        else None)
    |> List.length


let flipCubes (activeCubes : Cube list) =
    // Calculate the boundaries of cube dimension that could have activations
    let minX = (activeCubes |> List.minBy (fun c -> c.X)).X - 1
    let maxX = (activeCubes |> List.maxBy (fun c -> c.X)).X + 1
    let minY = (activeCubes |> List.minBy (fun c -> c.Y)).Y - 1
    let maxY = (activeCubes |> List.maxBy (fun c -> c.Y)).Y + 1
    let minZ = (activeCubes |> List.minBy (fun c -> c.Z)).Z - 1
    let maxZ = (activeCubes |> List.maxBy (fun c -> c.Z)).Z + 1

    // Loop through dimensions for each potentially activated cube
    let dimensionList =
         List.allPairs [minX .. maxX] [minY .. maxY]
        |> List.allPairs [minZ .. maxZ]
        |> List.map (fun (z,(x,y)) -> x,y,z)
    
    dimensionList 
        |> List.choose (fun (x,y,z) -> 
            let optionableCube = (activeCubes |> List.tryPick (fun ac -> 
                if ac.X = x && ac.Y = y && ac.Z = z && ac.Activated 
                then Some ac
                else None))
            let activeNeighbors = countActiveNeighbors x y z activeCubes
            match optionableCube with
            | Some ac -> 
                if activeNeighbors >= 2 && activeNeighbors <= 3
                then // remain active, otherwise disable 
                    Some (createCube3d x y z)
                else None
            | None -> 
                if activeNeighbors = 3
                then // activate
                    Some (createCube3d x y z)
                else None)

let rec flipCubesBy (activeCubes : Cube list) (acc : int) =
    printfn "Attempt acc %d length %d" acc activeCubes.Length
    if acc = 0
    then
        activeCubes
    else 
        flipCubesBy (flipCubes activeCubes) (acc - 1)

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 17"
    let cubeInput = System.IO.File.ReadAllLines "./input/input_day17.txt"
    let activeCubes =
        cubeInput 
        |> Array.rev // flip y to rise from bot to top
        |> Array.mapi (fun iy y -> 
            y 
                |> Seq.mapi (fun ix x -> 
                    match x with
                    | '#' -> Some(initCube ix iy)
                    | _ -> None)
                |> Seq.choose id
                |> Array.ofSeq)
        |> Array.collect id
        |> List.ofArray
    
    let answerPart1Array = flipCubesBy activeCubes 6

    printfn "Part 1 answer: %d" answerPart1Array.Length
 
    0 // return an integer exit code