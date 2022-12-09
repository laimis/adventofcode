open System.IO
let lines = File.ReadAllLines("input.txt")

type Move = int*int*int*string
type Position = int*int
type Rope = { Head: Position; Tail: Position; }

let parseCommand (s:string) =
    let move = s[0]
    let magnitude = int s[2..]
    match move with
    | 'L' -> Move (0, -1, magnitude, "left")
    | 'U' -> Move (1, 0, magnitude, "up")
    | 'R' -> Move (0, 1, magnitude, "right")
    | 'D' -> Move (-1, 0, magnitude, "down")
    | _ -> failwith "Invalid command"

printfn "parsing..."

let commands = lines |> List.ofArray |> List.map parseCommand

// the series of moves for the head will mean adjustmenst to the tail
// let's first figure out the sequence of coordinates that are zero based that the head will visit

let rope = {Head = Position(0, 0); Tail = Position(0, 0)}

let calculateTailPosition head tail =
    let (headRow, headCol) = head
    let (tailRow, tailCol) = tail

    let rowDiff = headRow - tailRow
    let colDiff = headCol - tailCol

    match (rowDiff,colDiff) with
    | (0, 0) -> tail
    | (0,-1) -> tail
    | (0,-2) -> (headRow, tailCol - 1)
    | (0, 1) -> tail
    | (0, 2) -> (headRow, tailCol + 1)
    | (-1, 0) -> tail
    | (-2, 0) -> (tailRow - 1, headCol)
    | (1, 0) -> tail
    | (2, 0) -> (tailRow + 1, headCol)

    // separated diagonally but not far enough
    | (-1,-1) -> (tailRow, tailCol)
    | (1, -1) -> (tailRow, tailCol)
    | (-1, 1) -> (tailRow, tailCol)
    | (1, 1) -> (tailRow, tailCol)

    // diagonals
    | (2, -1) -> (tailRow + 1, headCol)
    | (2,  1) -> (tailRow + 1, headCol)
    | (1, -2) -> (headRow, tailCol - 1)
    | (1,  2) -> (headRow, tailCol + 1)
    | (-1, -2) -> (headRow, tailCol - 1)
    | (-1,  2) -> (headRow, tailCol + 1)
    | (-2, -1) -> (tailRow - 1, headCol)
    | (-2,  1) -> (tailRow - 1, headCol)

    | _ -> failwith $"Invalid tail position: {rowDiff}, {colDiff}"

let getTail rope =
    let {Head = _; Tail = tail} = rope
    tail

let getHead rope =
    let {Head = head; Tail = _} = rope
    head

let rec stepExecution (acc: Rope list) move =
    let (rowAdj, colAdj, magnitude, desc) = move

    if magnitude = 0 then
        acc
    else
        let (headRow, headCol) = acc |> List.last |> getHead
        let tail = acc |> List.last |> getTail
        
        let newHead = Position(headRow + rowAdj, headCol + colAdj)
        let newTail = calculateTailPosition newHead tail

        printfn "   head: %A; tail: %A" newHead newTail

        let newMove = (rowAdj, colAdj, magnitude - 1, desc)

        newMove |> stepExecution (acc @ [{Head = newHead; Tail = newTail}])


printfn "moving..."

let sequence =
    commands
    |> List.fold stepExecution [rope]

// count of unique positions
printfn "count: %d" (sequence |> List.length)
printfn "unique: %d" (sequence |> List.map getTail |> List.distinct |> List.length)