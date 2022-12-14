open System.IO
open System.Collections.Generic
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

type Point = { Row:int; Column:int; }

let left point = { point with Column = point.Column - 1 }
let right point = { point with Column = point.Column + 1 }
let up point = { point with Row = point.Row - 1 }
let down point = { point with Row = point.Row + 1 }

type Map = 
    {
        Rows: int
        Columns: int
        Grid: char array array
    }
    member this.Value point = this.Grid[point.Row][point.Column]
    member this.ValueInt point =
        let charMatch =
            match this.Value point with
            | 'S' -> 'a' 
            | 'E' -> 'z'
            | _ -> this.Value point

        int charMatch

let parseMap (input: string array) =
    let rows = input.Length
    let columns = input[0].Length

    let grid = Array.init rows (fun r -> Array.init columns (fun c -> input[r][c]))

    { Rows = rows; Columns = columns; Grid = grid }

let getColorForCharacter character =
    // match letters from a to z and pick a color for each, a should be blue and z should be red, and others in between based on intensity
    let color =
        match character with
        | 'a' -> System.ConsoleColor.White
        | 'b' -> System.ConsoleColor.DarkBlue
        | 'c' -> System.ConsoleColor.DarkCyan
        | 'd' -> System.ConsoleColor.DarkGreen
        | 'e' -> System.ConsoleColor.DarkMagenta
        | 'f' -> System.ConsoleColor.DarkRed
        | 'g' -> System.ConsoleColor.DarkYellow
        | 'h' -> System.ConsoleColor.Gray
        | 'i' -> System.ConsoleColor.Green
        | 'j' -> System.ConsoleColor.Magenta
        | 'k' -> System.ConsoleColor.Red
        | 'l' -> System.ConsoleColor.Yellow
        | 'm' -> System.ConsoleColor.Cyan
        | 'n' -> System.ConsoleColor.DarkGray
        | 'o' -> System.ConsoleColor.DarkBlue
        | 'p' -> System.ConsoleColor.DarkCyan
        | 'q' -> System.ConsoleColor.DarkGreen
        | 'r' -> System.ConsoleColor.DarkMagenta
        | 's' -> System.ConsoleColor.DarkRed
        | 't' -> System.ConsoleColor.DarkYellow
        | 'u' -> System.ConsoleColor.Gray
        | 'v' -> System.ConsoleColor.Green
        | 'w' -> System.ConsoleColor.Magenta
        | 'x' -> System.ConsoleColor.Red
        | 'y' -> System.ConsoleColor.Yellow
        | 'z' -> System.ConsoleColor.Cyan
        | _ -> System.ConsoleColor.White

    color
        
        
let drawToConsole map (path:Point list) (priorityQueue:PriorityQueue<Point list, int> option) (shortestPath:Point list option) =
    if System.Console.IsOutputRedirected then
        ()
    else
        System.Console.Clear()
        System.Console.SetCursorPosition(0,0)
        
        let drawGrid() =
            map.Grid
            |> Array.iteri (fun rowi value -> 
                value
                |> Array.iteri (fun coli gc -> 
                    let color = getColorForCharacter gc
                    System.Console.ForegroundColor <- color
                    printf $"{gc}"
                )
                printfn ""
            )

        let drawPath() =
            System.Console.ForegroundColor <- System.ConsoleColor.Red
            path
            |> List.iteri (fun i p ->
                System.Console.SetCursorPosition(p.Column, p.Row)
                System.Console.Write($"*")
            )
            System.Console.SetCursorPosition(0, map.Rows + 1)
            System.Console.ResetColor()

        let drawShortestPathStatus() =
            System.Console.WriteLine($"Path length {path.Length}")
            let info =
                match shortestPath with
                | Some p -> $"Current shortest path of length {p.Length}"
                | None -> "No shortest path has been found yet"

            System.Console.WriteLine($"{info}")

            match priorityQueue with
            | Some pq -> System.Console.WriteLine($"Priority queue length {pq.Count}")
            | None -> ()

        // drawGrid()
        drawPath()
        drawShortestPathStatus()


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

let manhattanDistance (p1:Point) (p2:Point) =
    abs (p1.Row - p2.Row) + abs (p1.Column - p2.Column)

let boundaryCheck map nextPoint currentPoint =
    let isValid =
        nextPoint.Row >= 0 && nextPoint.Row < map.Rows &&
        nextPoint.Column >= 0 && nextPoint.Column < map.Columns
    
    if not isValid then
        false
    else
        let diff = (map.ValueInt nextPoint) - (map.ValueInt currentPoint)
        diff >= -1


let map = parseMap input

let mutable (shortestPath:Point list option) = None
let mutable counter =  0

let endPoint = findPoint map 'E'
let shortestDistances = new Dictionary<Point, int>()

let findPathNotRecursive startingPath =

    let priorityQueue = new PriorityQueue<Point list, int>()
    priorityQueue.Enqueue(startingPath, startingPath.Length)

    while priorityQueue.Count > 0 do

        counter <- counter + 1

        let currentPath = priorityQueue.Dequeue()
        let currentPoint = currentPath[currentPath.Length - 1]
        let currentValue = map.ValueInt currentPoint

        if verbose then
            printfn $"Examining {currentPoint.Row}x{currentPoint.Column} with value {currentValue}"

        if verbose || counter % 1000 = 0 then
            drawToConsole map currentPath (Some priorityQueue) shortestPath
            if verbose then
                System.Console.ReadLine() |> ignore

        shortestDistances[currentPoint] <- currentPath.Length

        if currentPoint = endPoint then
            printfn $"Found path to end at {currentPoint.Row}, {currentPoint.Column} with length of {currentPath.Length}"
            shortestPath <- Some currentPath
            if verbose then
                System.Console.ReadLine() |> ignore

        let qualifiedNodes = 
            [left currentPoint; right currentPoint; up currentPoint; down currentPoint]
            |> List.filter(fun p -> (p |> boundaryCheck map currentPoint) && (List.contains p currentPath |> not))

        if verbose then
            System.Console.WriteLine($"Found {qualifiedNodes.Length} qualified nodes")
            qualifiedNodes
            |> List.iter(fun p -> System.Console.WriteLine($"  {p.Row}x{p.Column} {map.Value p} {manhattanDistance p endPoint}"))
            System.Console.ReadLine() |> ignore

        for nextPoint in qualifiedNodes do
            
            let newPath = currentPath @ [nextPoint]
            
            let shortestDistance = shortestDistances.GetValueOrDefault(nextPoint, System.Int32.MaxValue)
            if shortestDistance > newPath.Length then
                if shortestPath.IsNone || newPath.Length < shortestPath.Value.Length then
                    priorityQueue.Enqueue(newPath, (manhattanDistance endPoint nextPoint) )
                else
                    printfn $"Skipping path {newPath.Length} because it's longer than the current shortest path {shortestPath.Value.Length}"
                    if verbose then
                        System.Console.ReadLine() |> ignore
            else
                if verbose then
                    printfn $"Skipping path because it's longer than the current shortest path {shortestDistance}"
                    System.Console.ReadLine() |> ignore

let rec findPath (path:Point list) =

    counter <- counter + 1

    if verbose || counter % 1000 = 0 then
        drawToConsole map path None shortestPath
        // if verbose then
        //     System.Console.ReadLine() |> ignore

    // if counter % 1000 = 0 then
    //     System.IO.File.WriteAllText("counter.txt", counter.ToString())

    let currentPoint = path[path.Length - 1]
    
    shortestDistances[currentPoint] <- path.Length

    if map.Value currentPoint = 'a' then
        if shortestPath.IsNone || path.Length < shortestPath.Value.Length then
            printfn $"Found path to end at {currentPoint.Row}, {currentPoint.Column} with length of {path.Length}"
            shortestPath <- Some path
            if verbose then
                System.Console.ReadLine() |> ignore

    let qualified_nodes = 
        [left currentPoint; right currentPoint; up currentPoint; down currentPoint]
        |> List.filter(fun p -> (boundaryCheck map p currentPoint) && (List.contains p path |> not))
        // |> List.sortBy(fun p -> manhattanDistance p endPoint)

    if verbose then
        System.Console.WriteLine($"Found {qualified_nodes.Length} qualified nodes")
        qualified_nodes
        |> List.iter(fun p -> System.Console.WriteLine($"  {p.Row}x{p.Column} {map.Value p} {manhattanDistance p endPoint}"))
        System.Console.ReadLine() |> ignore

    for nextPoint in qualified_nodes do
            
        let newPath = path @ [nextPoint]
        
        let shortestDistance = shortestDistances.GetValueOrDefault(nextPoint, System.Int32.MaxValue)
        if shortestDistance > newPath.Length then
            findPath newPath
        else
            if verbose then
                printfn $"Skipping path because it's longer than the current shortest path {shortestDistance}"
                System.Console.ReadLine() |> ignore

// let startingPoint = findPoint map 'S'
let startingPoint = findPoint map 'E'
printfn $"Starting at {startingPoint.Row}, {startingPoint.Column}"
let startingPath = [startingPoint]

drawToConsole map startingPath None shortestPath
System.Console.ReadLine() |> ignore


findPath startingPath

// findPathNotRecursive startingPath

match shortestPath with
| None -> printfn $"No path found"
| Some s ->
    drawToConsole map s None shortestPath
    printfn $"Path length: {s.Length - 1}"
