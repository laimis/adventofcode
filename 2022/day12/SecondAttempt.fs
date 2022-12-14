module SecondAttempt
open System.Collections.Generic

type Point = 
    {
        Row: int
        Column: int
    }

type Node =
    {
        Point: Point
        Value: int
        Character: char
        Children: Node list
    }

type Map = 
    {
        Rows: int
        Columns: int
        Grid: char array array
    }

    member this.Value point = this.Grid.[point.Row].[point.Column]
    member this.ValueInt point =
        let charMatch =
            match this.Value point with
            | 'S' -> 'a' 
            | 'E' -> 'z'
            | _ -> this.Value point

        int charMatch
    
let parse (input:string array) =
    let rows = input.Length
    let columns = input.[0].Length

    let grid = Array.init rows (fun r -> Array.init columns (fun c -> input.[r].[c]))

    { Rows = rows; Columns = columns; Grid = grid }

let findPoint map (value:char) =
        let startingRow =
            map.Grid
            |> Array.findIndex(fun row ->
                row
                |> Array.exists(fun c -> c = value))

        let startingColumn =
            map.Grid[startingRow]
            |> Array.findIndex(fun c -> c = value)

        { Row = startingRow; Column = startingColumn }

let left point = { point with Column = point.Column - 1 }
let right point = { point with Column = point.Column + 1 }
let up point = { point with Row = point.Row - 1 }
let down point = { point with Row = point.Row + 1 }

let validMove map source destination =
    let boundaryPass =
        destination.Row >= 0 && destination.Column >= 0
        && destination.Row < map.Rows && destination.Column < map.Columns

    if not boundaryPass then
        false
    else
        let sourceValue = map.ValueInt source
        let destinationValue = map.ValueInt destination
        let diff = destinationValue - sourceValue

        diff <= 1

let rec createNode (map:Map) point (visited:HashSet<Point>) =
    let value = map.ValueInt point
    let character = map.Value point
    visited.Add point |> ignore
    let childPoints = 
        [ left point; right point; up point; down point ]
        |> List.filter(fun p -> validMove map point p)
        |> List.filter(fun p -> not (visited.Contains p))

    // printfn $"Point: {point.Row}, {point.Column}"
    // printfn $"Value: {value}"
    // printfn $"Character: {character}"
    // printfn $"Child Points: {childPoints.Length}"
    // printfn $""
    // System.Console.ReadKey() |> ignore
    let newVisited = new HashSet<Point>(visited)
    newVisited.Add point |> ignore
    let children = childPoints |> List.map(fun p -> createNode map p newVisited)
    { Point = point; Value = value; Character = character; Children = children }

let createTree (input:string array) =

    let map = parse input
    let start = findPoint map 'S'
    
    createNode map start (HashSet<Point>())

let findPath (start:Node) (charOfInterest:char) =
    
    let candidatePaths = List<Point list>()

    let rec findPath' (node:Node) (path:Point list) =
        let currentChar = node.Character
        if currentChar = charOfInterest then
            candidatePaths.Add path
        else
            for node in node.Children do
                findPath' node (path @ [node.Point])

    findPath' start []

    let shortestPath =
        candidatePaths
        |> List.ofSeq
        |> List.sortBy(fun path -> path.Length)
        |> List.head

    shortestPath