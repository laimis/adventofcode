open System.IO
let lines = File.ReadAllLines("input.txt")

type Move = int*int*int*string

type Position = 
    {row: int; col: int}
    member this.apply (move: Move) =
        let (rowAdj, colAdj, _, _) = move
        let {row = row;  col = col} = this
        {row = row + rowAdj; col = col + colAdj}
    member this.subtract (other: Position) =
        let {row = row;  col = col} = this
        let {row = otherRow; col = otherCol} = other
        (row - otherRow, col - otherCol)
    static member create (row, col) = {row = row; col = col}

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
    Head = Position.create(0, 0);
    One = Position.create(0, 0);
    Two = Position.create(0, 0);
    Three = Position.create(0, 0);
    Four = Position.create(0, 0);
    Five = Position.create(0, 0);
    Six = Position.create(0, 0);
    Seven = Position.create(0, 0);
    Eight = Position.create(0, 0);
    Tail = Position.create(0, 0);
}

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
    let (rowAdj, colAdj, magnitude, desc) = move

    if magnitude = 0 then
        acc
    else
        let rope = acc |> List.last

        let newHead = rope.Head.apply move
        
        let one = rope.One
        let newOne = calculateKnotPosition newHead one

        let two = rope.Two
        let newTwo = calculateKnotPosition newOne two

        let three = rope.Three
        let newThree = calculateKnotPosition newTwo three

        let four = rope.Four
        let newFour = calculateKnotPosition newThree four

        let five = rope.Five
        let newFive = calculateKnotPosition newFour five

        let six = rope.Six
        let newSix = calculateKnotPosition newFive six

        let seven = rope.Seven
        let newSeven = calculateKnotPosition newSix seven

        let eight = rope.Eight
        let newEight = calculateKnotPosition newSeven eight

        let tail = rope.Tail
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