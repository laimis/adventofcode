open System.IO
open Ropes.Types

let lines = File.ReadAllLines("input.txt")

let commands =
    lines
    |> List.ofArray
    |> List.map Move.parse

System.Console.WriteLine("Enter the number of knots: ")
let input = System.Console.ReadLine()
let knots = int input

let rope = knots |> Rope.create

let ropes =
    commands
    |> rope.generateRopeSequence

let tail rope =
    rope.Parts |> List.last

printfn "count: %d" (ropes |> List.length)
printfn "unique: %d" (ropes |> List.map tail |> List.distinct |> List.length)