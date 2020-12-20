open System

let nl = "\n"

type JigSawPiece = {
    Id : int
    Left : string
    Right : string
    Top : string
    Bottom : string
}

let extractArrayIndex (strArr : string array) (index : int) =
    strArr
    |> Array.map (fun s -> s.[index])
    |> System.String

let toJigsawPiece (str : string) = 
    let stringArray = str.Split(nl)
    let id = stringArray.[0].Split("Tile ").[1].[0..3] |> System.Int32.Parse
    let top = stringArray.[1]
    let bottom = stringArray |> Array.last
    let arrTail = stringArray |> Array.tail
    let left = extractArrayIndex arrTail 0
    let right = extractArrayIndex  arrTail 9
    {
        Id = id
        Top = top
        Left = left
        Right = right
        Bottom = bottom
    }

let reverseString (str : string) =
    str|>  Seq.rev |> (Seq.map string >> String.concat "")

let constructPieceVariationArray (piece : JigSawPiece) =
    Set.ofList [
        piece.Left; 
        piece.Right; 
        piece.Top; 
        piece.Bottom; 
        reverseString piece.Left;
        reverseString piece.Right;
        reverseString piece.Top;
        reverseString piece.Bottom;]

let matchTwoPieces (piece : JigSawPiece) (piece2 : JigSawPiece) =
    let pieceArr = constructPieceVariationArray piece
    let piece2Arr = constructPieceVariationArray piece2
    let interSection = Set.intersect pieceArr piece2Arr
    not interSection.IsEmpty
    

let matchPieceToOthers (piece : JigSawPiece) (allPieces : JigSawPiece array) =
    let otherPieceSuitable = 
        allPieces
        |> Array.filter (fun p -> p <> piece)
        |> Array.map (fun p -> p.Id, matchTwoPieces piece p)
        |> Array.choose (fun ib -> if snd ib then Some(fst ib) else None)
    otherPieceSuitable


[<EntryPoint>]
let main argv =
    printfn "Advent of Code Day 20"
    let jigsawInput = 
        (System.IO.File.ReadAllText "./input/input_day20.txt")
            .Replace("\r\n", nl) // if there is windows line-endings harmonize them
            .Split(nl+nl, StringSplitOptions.RemoveEmptyEntries)
        |> Array.map toJigsawPiece
    
    let matchArray = 
        jigsawInput
        |> Array.map (fun p -> p, (matchPieceToOthers p jigsawInput))
        |> Array.sortBy (fun p -> (snd p).Length)

    let cornerPieces = 
        matchArray
        |> Array.choose (fun p -> if (snd p).Length = 2 then Some(fst p) else None)
    let answer1 = (int64 cornerPieces.[0].Id) * (int64 cornerPieces.[1].Id) * (int64 cornerPieces.[2].Id) * (int64 cornerPieces.[3].Id)

    printfn "Part 1 answer: %d" answer1
    0 // return an integer exit code