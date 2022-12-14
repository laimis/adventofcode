open System.IO
let input = File.ReadAllLines "input.txt"

let shortestPath = FirstAttempt.findShortestPath true input
printfn $"Shortest path: {shortestPath.Length}"

// printfn "Parsing tree"
// let tree = SecondAttempt.createTree input

// printfn "Parsed tree"

// let shortestPath2 = SecondAttempt.findPath tree 'E'

// printfn $"Shortest path 2: {shortestPath2.Length}"