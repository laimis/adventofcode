open System.IO
open System.Collections.Generic
let input = File.ReadAllLines "input.txt"


type Point = { Row:int; Column:int; }

let left point = ({ point with Column = point.Column - 1 }, '<')
let right point = ({ point with Column = point.Column + 1 }, '>')
let up point = ({ point with Row = point.Row - 1 }, '^')
let down point = ({ point with Row = point.Row + 1 }, 'v')

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


type Path(points: Point list) =
    member this.Points = points
    member this.Length = points.Length
    member this.Append point = Path(points @ [point])
    member this.Last = points |> List.last
    member this.Contains (point:Point) = points |> List.contains point


let mutable (shortestPath:Path option) = None
let mutable (shortestGraphicalPath:char array option) = None
let mutable counter =  0
// let visisted = HashSet<Point>()

let drawToConsole map (path:Path) (newGraphicalPath:char array) =
    System.Console.Clear()
    System.Console.SetCursorPosition(0,0)
    map.Grid
    |> Array.iteri (fun rowi value -> 
        value
        |> Array.iteri (fun coli gc -> 
            printf $"{gc}"
        )
        printfn ""
    )

    System.Console.ForegroundColor <- System.ConsoleColor.Red
    path.Points
    |> List.iteri (fun i p ->
        System.Console.SetCursorPosition(p.Column, p.Row)
        System.Console.Write($"{newGraphicalPath.[i]}")
    )

    System.Console.SetCursorPosition(0, map.Rows + 1)

    System.Console.ResetColor()
    // let shortestPathLength = shortestPath |> Option.map(fun p -> p.Length) |> Option.defaultValue -1
    // System.Console.WriteLine($"Shortest path so far: {shortestPathLength}")


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

// let queue = Queue<Point>()
// let visited = HashSet<Point>()

let manhattanDistance (p1:Point) (p2:Point) =
    abs (p1.Row - p2.Row) + abs (p1.Column - p2.Column)

let boundaryCheck map p =
    let isValid =
        p.Row >= 0 && p.Row < map.Rows &&
        p.Column >= 0 && p.Column < map.Columns
    
    isValid

let rec findPath (verbose:bool) (map:Map) (path:Path) (graphicalPath:char array)  (endPoint:Point) =
    
    // visited.Add startingPoint |> ignore
    // queue.Enqueue startingPoint
    counter <- counter + 1

    if verbose || counter % 500000 = 0 then
        drawToConsole map path graphicalPath
        if verbose then
            System.Console.ReadLine() |> ignore

    let current_vertex = path.Last
    let currentValue = map.ValueInt current_vertex

    if map.Value current_vertex = 'E' then
        printfn $"Found path to end at {current_vertex.Row}, {current_vertex.Column} with length of {path.Length}"
        shortestPath <- Some path
        shortestGraphicalPath <- Some graphicalPath
        if verbose then
            System.Console.ReadLine() |> ignore

    let qualified_nodes = 
        [left current_vertex; right current_vertex; up current_vertex; down current_vertex]
        |> List.filter(fun (p, _) -> p |> boundaryCheck map)
        |> List.filter(fun (p, _) -> map.ValueInt p - currentValue >= -1)
        |> List.filter(fun (p, _) -> not (path.Contains p))
        |> List.sortBy(fun (p, _) -> manhattanDistance p endPoint)


    if verbose then
        System.Console.WriteLine($"Found {qualified_nodes.Length} qualified nodes")
        qualified_nodes
        |> List.iter(fun (p, _) -> System.Console.WriteLine($"  {p.Row}x{p.Column} {map.Value p} {manhattanDistance p endPoint}"))
        System.Console.ReadLine() |> ignore

    for (next_vertex, movement_character) in qualified_nodes do
            
        // System.Console.WriteLine($"trying out {next_vertex.Row}x{next_vertex.Column} [{map.Value next_vertex}]")
        // System.Console.ReadKey() |> ignore
        let newPath = path.Append next_vertex
        let newGraphicalPath = [|movement_character|] |> Array.append graphicalPath

        // only worth trying out new path if it's shorter than the current shortest path
        if shortestPath.IsNone || newPath.Length < shortestPath.Value.Length then
            // if not (visisted.Contains next_vertex) then
                findPath verbose map newPath newGraphicalPath endPoint
            // else
            //     printfn $"Skipping path {newPath} because it's already been visited"
            //     if verbose then
            //         System.Console.ReadLine() |> ignore
        else
            printfn $"Skipping path {newPath.Length} because it's longer than the current shortest path {shortestPath.Value.Length}"
            if verbose then
                System.Console.ReadLine() |> ignore

    // visisted.Add(current_vertex) |> ignore

let map = parseMap input

let startingPoint = findPoint map 'S'

printfn $"Starting at {startingPoint.Row}, {startingPoint.Column}"
let endPoint = findPoint map 'E'

let startingPath = Path([startingPoint])

findPath false map startingPath [|'S'|] endPoint

drawToConsole map (shortestPath.Value) (shortestGraphicalPath.Value)
printfn $"Path length: {shortestPath.Value.Length}"