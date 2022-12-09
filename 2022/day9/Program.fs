open System.IO
let lines = File.ReadAllLines("input.txt")

type Move = 
    {rowAdj: int; colAdj: int; magnitude: int; direction: string}
    member this.decrementMagnitude() =
        let {rowAdj = rowAdj; colAdj = colAdj; magnitude = magnitude; direction = direction} = this

        match magnitude with
        | 0 -> failwith "Move cannot be decremented any further"
        | _ -> {rowAdj = rowAdj; colAdj = colAdj; magnitude = magnitude - 1; direction = direction}

type Position = 
    {row: int; col: int}
    member this.apply (move: Move) =
        let {rowAdj = rowAdj; colAdj = colAdj; magnitude = magnitude; direction = direction} = move
        let {row = row;  col = col} = this
        {row = row + rowAdj; col = col + colAdj}
    member this.subtract (other: Position) =
        let {row = row;  col = col} = this
        let {row = otherRow; col = otherCol} = other
        (row - otherRow, col - otherCol)
    static member create (row, col) = {row = row; col = col}

type Rope =
    { Parts : Position list}
    static member create(number) = 
        let parts = List.init number (fun _ -> Position.create(0,0))
        {Parts = parts}

let parseCommand (s:string) =
    let move = s[0]
    let magnitude = int s[2..]
    match move with
    | 'L' -> {rowAdj = 0; colAdj = -1; magnitude = magnitude; direction = "left"}
    | 'U' -> {rowAdj = 1; colAdj = 0; magnitude = magnitude; direction = "up"}
    | 'R' -> {rowAdj = 0; colAdj = 1; magnitude = magnitude; direction = "right"}
    | 'D' -> {rowAdj = -1; colAdj = 0; magnitude = magnitude; direction = "down"}
    | _ -> failwith "Invalid command"

printfn "parsing..."

let commands = lines |> List.ofArray |> List.map parseCommand

// the series of moves for the head will mean adjustmenst to the tail
// let's first figure out the sequence of coordinates that are zero based that the head will visit

let rope = Rope.create(10)

let calculateKnotPosition leadingKnot followingKnot =
    let {row = leadingRow;  col = leadingCol} = leadingKnot
    let {row = followingRow; col = followingCol} = followingKnot

    let (rowDiff, colDiff) = leadingKnot.subtract followingKnot

    match (rowDiff,colDiff) with
    | (0, 0) -> followingKnot
    | (0,-1) -> followingKnot
    | (0,-2) -> Position.create(leadingRow, followingCol - 1)
    | (0, 1) -> followingKnot
    | (0, 2) -> Position.create(leadingRow, followingCol + 1)
    | (-1, 0) -> followingKnot
    | (-2, 0) -> Position.create(followingRow - 1, leadingCol)
    | (1, 0) -> followingKnot
    | (2, 0) -> Position.create(followingRow + 1, leadingCol)

    // separated diagonally but not far enough
    | (-1,-1) -> followingKnot
    | (1, -1) -> followingKnot
    | (-1, 1) -> followingKnot
    | (1, 1) -> followingKnot

    // diagonals
    | (2, -1) -> Position.create(followingRow + 1, leadingCol)
    | (2,  1) -> Position.create(followingRow + 1, leadingCol)
    | (1, -2) -> Position.create(leadingRow, followingCol - 1)
    | (1,  2) -> Position.create(leadingRow, followingCol + 1)
    | (-1, -2) -> Position.create(leadingRow, followingCol - 1)
    | (-1,  2) -> Position.create(leadingRow, followingCol + 1)
    | (-2, -1) -> Position.create(followingRow - 1, leadingCol)
    | (-2,  1) -> Position.create(followingRow - 1, leadingCol)
    | (2, -2) -> Position.create(followingRow + 1, followingCol - 1)
    | (2,  2) -> Position.create(followingRow + 1, followingCol + 1)
    | (-2, -2) -> Position.create(followingRow - 1, followingCol - 1)
    | (-2,  2) -> Position.create(followingRow - 1, followingCol + 1)

    | _ -> failwith $"Invalid tail position: {rowDiff}, {colDiff}"


let rec stepExecution (acc: Rope list) move =
    let {magnitude = magnitude} = move

    if magnitude = 0 then
        acc
    else
        let rope = acc |> List.last

        let {Parts = parts } = rope

        let folder (acc: Position list) (part: Position) =
            match acc with
            | [] -> 
                let newPart = part.apply move
                acc @ [newPart]
            | _ ->
                let leadingKnot = acc |> List.last
                let newPart = calculateKnotPosition leadingKnot part
                acc @ [newPart]

        let newParts = parts |> List.fold folder []

        let newMove = move.decrementMagnitude()

        let newRope = {Parts = newParts}

        newMove |> stepExecution (acc @ [newRope])


printfn "moving..."

let sequence =
    commands
    |> List.fold stepExecution [rope]

// count of unique positions
printfn "count: %d" (sequence |> List.length)
printfn "unique: %d" (sequence |> List.map (fun r -> r.Parts |> List.last) |> List.distinct |> List.length)