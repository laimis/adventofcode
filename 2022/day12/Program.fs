open System.IO
let input = File.ReadAllLines "input.txt"

let args = System.Environment.GetCommandLineArgs()

let verbose = 
    let paramOfInterest =
        args
        |> Seq.truncate 2
        |> Seq.toList
        
    match paramOfInterest with
    | [] -> false
    | [_; arg] when arg = "verbose" -> true
    | _ -> false

let shortestPath = FirstAttempt.findShortestPath verbose input
printfn $"Shortest path: {shortestPath.Length}"