open System.IO

let lines = File.ReadAllLines "input.txt"


let mapLetterToScore letter =
    match letter with
    | "A" -> 1 // rock
    | "B" -> 2 // paper
    | "C" -> 3 // scizzor
    | "X" -> 1 // rock
    | "Y" -> 2 // paper
    | "Z" -> 3 // scizzor
    | _ -> raise (new System.Exception("Invalid input"))

let mapMyChoiceBasedOnOpponentChoiceAndDesiredOutcome oppChoice desiredOutcome =
    match (oppChoice, desiredOutcome) with
    | ("A", "X") -> "Z"
    | ("A", "Y") -> "X"
    | ("A", "Z") -> "Y"
    | ("B", "X") -> "X"
    | ("B", "Y") -> "Y"
    | ("B", "Z") -> "Z"
    | ("C", "X") -> "Y"
    | ("C", "Y") -> "Z"
    | ("C", "Z") -> "X"
    | _ -> raise (new System.Exception("Invalid input"))

let result =
    lines
    |> Array.map (fun line -> 
        let arr = line.Split(' ')
        (arr.[0], arr.[1])
    )
    |> Array.map (fun (oppChoice, desiredOutcome) -> 
        (oppChoice, desiredOutcome |> mapMyChoiceBasedOnOpponentChoiceAndDesiredOutcome oppChoice))
    |> Array.map (fun (oppChoice, myChoice) ->
        let oppScore = oppChoice |> mapLetterToScore
        let myScore = myChoice |> mapLetterToScore

        (oppScore, myScore)
    )
    |> Array.map (fun (oppScore, myScore) ->
        let outcome =
            match (oppScore, myScore) with
            | (1, 1) | (2, 2) | (3, 3) -> 3
            | (1, 2) | (2, 3) | (3, 1) -> 6
            | (1, 3) | (2, 1) | (3, 2) -> 0
            | _ -> raise (new System.Exception("Invalid score"))

        outcome + myScore
    )
    |> Array.sum

System.Console.WriteLine(result)
 