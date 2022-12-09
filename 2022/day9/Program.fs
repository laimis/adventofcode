open System.IO
let lines = File.ReadAllLines("input.txt")

type Move = int*int*int*string
type Position = int*int
type Rope = {
    Head: Position;
    One: Position;
    Two: Position;
    Three: Position;
    Four: Position;
    Five: Position;
    Six: Position;
    Seven: Position;
    Eight: Position;
    Tail: Position;
}

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

let rope = {
    Head = Position(0, 0);
    One = Position(0, 0);
    Two = Position(0, 0);
    Three = Position(0, 0);
    Four = Position(0, 0);
    Five = Position(0, 0);
    Six = Position(0, 0);
    Seven = Position(0, 0);
    Eight = Position(0, 0);
    Tail = Position(0, 0)
}

let calculateKnotPosition leadingKnot followingKnot =
    let (leadingRow, leadingCol) = leadingKnot
    let (followingRow, followingCol) = followingKnot

    let rowDiff = leadingRow - followingRow
    let colDiff = leadingCol - followingCol

    match (rowDiff,colDiff) with
    | (0, 0) -> followingKnot
    | (0,-1) -> followingKnot
    | (0,-2) -> (leadingRow, followingCol - 1)
    | (0, 1) -> followingKnot
    | (0, 2) -> (leadingRow, followingCol + 1)
    | (-1, 0) -> followingKnot
    | (-2, 0) -> (followingRow - 1, leadingCol)
    | (1, 0) -> followingKnot
    | (2, 0) -> (followingRow + 1, leadingCol)

    // separated diagonally but not far enough
    | (-1,-1) -> (followingRow, followingCol)
    | (1, -1) -> (followingRow, followingCol)
    | (-1, 1) -> (followingRow, followingCol)
    | (1, 1) -> (followingRow, followingCol)

    // diagonals
    | (2, -1) -> (followingRow + 1, leadingCol)
    | (2,  1) -> (followingRow + 1, leadingCol)
    | (1, -2) -> (leadingRow, followingCol - 1)
    | (1,  2) -> (leadingRow, followingCol + 1)
    | (-1, -2) -> (leadingRow, followingCol - 1)
    | (-1,  2) -> (leadingRow, followingCol + 1)
    | (-2, -1) -> (followingRow - 1, leadingCol)
    | (-2,  1) -> (followingRow - 1, leadingCol)
    | (2, -2) -> (followingRow + 1, followingCol - 1)
    | (2,  2) -> (followingRow + 1, followingCol + 1)
    | (-2, -2) -> (followingRow - 1, followingCol - 1)
    | (-2,  2) -> (followingRow - 1, followingCol + 1)

    | _ -> failwith $"Invalid tail position: {rowDiff}, {colDiff}"


let rec stepExecution (acc: Rope list) move =
    let (rowAdj, colAdj, magnitude, desc) = move

    if magnitude = 0 then
        acc
    else
        let lastRope = acc |> List.last

        let (headRow, headCol) = lastRope.Head
        let newHead = Position(headRow + rowAdj, headCol + colAdj)
        
        let one = lastRope.One
        let newOne = calculateKnotPosition newHead one

        let two = lastRope.Two
        let newTwo = calculateKnotPosition newOne two

        let three = lastRope.Three
        let newThree = calculateKnotPosition newTwo three

        let four = lastRope.Four
        let newFour = calculateKnotPosition newThree four

        let five = lastRope.Five
        let newFive = calculateKnotPosition newFour five

        let six = lastRope.Six
        let newSix = calculateKnotPosition newFive six

        let seven = lastRope.Seven
        let newSeven = calculateKnotPosition newSix seven

        let eight = lastRope.Eight
        let newEight = calculateKnotPosition newSeven eight

        let tail = lastRope.Tail
        let newTail = calculateKnotPosition newEight tail

        let newMove = (rowAdj, colAdj, magnitude - 1, desc)

        let newRope = {Head = newHead; One = newOne; Two = newTwo; Three = newThree; Four = newFour; Five = newFive; Six = newSix; Seven = newSeven; Eight = newEight; Tail = newTail}

        newMove |> stepExecution (acc @ [newRope])


printfn "moving..."

let sequence =
    commands
    |> List.fold stepExecution [rope]

// count of unique positions
printfn "count: %d" (sequence |> List.length)
printfn "unique: %d" (sequence |> List.map (fun r -> r.Tail) |> List.distinct |> List.length)