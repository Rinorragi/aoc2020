open System

type Cube = {
    X : int
    Y : int
    Z : int
    W : int
    Activated : bool
} with 
    static member InitCube (x : int) (y : int) =
        {
            X = x
            Y = y
            Z = 0
            W = 0
            Activated = true }
    static member CreateCube4d (x : int) (y : int) (z : int) (w : int) =
        {
            X = x
            Y = y
            Z = z
            W = w
            Activated = true }

    static member CreateCube3d (x : int) (y : int) (z : int) =     
        {
            X = x
            Y = y
            Z = z
            W = 0
            Activated = true }

let countActiveNeighbors (activeCubes : Cube list) (dimensions : int) (x : int) (y : int) (z : int) (w : int)  =
    // 26 neighbors from three dimensional array
    activeCubes
    |> List.choose (fun ac -> 
        if  not (x = ac.X && y = ac.Y && z = ac.Z && w = ac.W) // skip the given one
            && ([x - 1 .. x + 1] |> List.contains ac.X)
            && ([y - 1 .. y + 1] |> List.contains ac.Y)
            && ([z - 1 .. z + 1] |> List.contains ac.Z)
            && (dimensions = 3 
                || (dimensions = 4 && ([w - 1 .. w + 1] |> List.contains ac.W)))
        then
            Some ac
        else None)
    |> List.length


let constructDimensionList (activeCubes : Cube list) (dimensions : int) =
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

    match dimensions with
    | 3 -> 
        dimensionList
            |> List.map (fun (x,y,z) -> x,y,z,0)
    | 4 -> 
        let minW = (activeCubes |> List.minBy (fun c -> c.W)).W - 1
        let maxW = (activeCubes |> List.maxBy (fun c -> c.W)).W + 1
        dimensionList
            |> List.allPairs [minW .. maxW]
            |> List.map (fun (w,(x,y,z)) -> x,y,z,w)
    | _ -> failwith "Unsupported amount of dimensions for constructDimensionList"


let flipCube3d (activeCubes : Cube list) (x : int) (y : int) (z : int)  = 
    let optionableCube = (activeCubes |> List.tryPick (fun ac -> 
        if ac.X = x && ac.Y = y && ac.Z = z && ac.Activated 
        then Some ac
        else None))
    let activeNeighbors = countActiveNeighbors activeCubes 3 x y z 0 
    match optionableCube with
    | Some ac -> 
        if activeNeighbors >= 2 && activeNeighbors <= 3
        then // remain active, otherwise disable 
            Some (Cube.CreateCube3d x y z)
        else None
    | None -> 
        if activeNeighbors = 3
        then // activate
            Some (Cube.CreateCube3d x y z)
        else None

let flipCube4d (activeCubes : Cube list) (x : int) (y : int) (z : int) (w : int)  = 
    let optionableCube = (activeCubes |> List.tryPick (fun ac -> 
        if ac.X = x && ac.Y = y && ac.Z = z && ac.W = w && ac.Activated 
        then Some ac
        else None))
    let activeNeighbors = countActiveNeighbors activeCubes 4 x y z w
    match optionableCube with
    | Some ac -> 
        if activeNeighbors >= 2 && activeNeighbors <= 3
        then // remain active, otherwise disable 
            Some (Cube.CreateCube4d x y z w)
        else None
    | None -> 
        if activeNeighbors = 3
        then // activate
            Some (Cube.CreateCube4d x y z w)
        else None

let flipCubes (activeCubes : Cube list) (dimensions : int)=
    let dimensionList = constructDimensionList activeCubes dimensions
    dimensionList 
        |> List.choose (fun (x,y,z,w) -> 
            match dimensions with 
            | 3 -> flipCube3d activeCubes x y z
            | 4 -> flipCube4d activeCubes x y z w
            | _ -> failwith "Unsupported amount of dimensions for flipCubes" )

let rec flipCubesBy (activeCubes : Cube list) (dimensions : int) (acc : int) =
    printfn "Attempt acc %d length %d" acc activeCubes.Length
    if acc = 0
    then
        activeCubes
    else 
        flipCubesBy (flipCubes activeCubes dimensions) dimensions (acc - 1)

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
                    | '#' -> Some(Cube.InitCube ix iy)
                    | _ -> None)
                |> Seq.choose id
                |> Array.ofSeq)
        |> Array.collect id
        |> List.ofArray
    
    let answerPart1Array = flipCubesBy activeCubes 3 6
    printfn "Part 1 answer: %d" answerPart1Array.Length

    let answerPart2Array = flipCubesBy activeCubes 4 6
    printfn "Part 2 answer: %d" answerPart2Array.Length
 
    0 // return an integer exit code