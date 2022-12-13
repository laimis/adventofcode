open System.IO
open System.Collections.Generic
let input = File.ReadAllLines "input.txt"


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


type Path(points: Point list) =
    member this.Points = points
    member this.Length = points.Length
    member this.Append point = Path(points @ [point])
    member this.Last = points |> List.last
    member this.Contains (point:Point) = points |> List.contains point

let drawToConsole map (path:Path) =
    System.Console.Clear()
    System.Console.SetCursorPosition(0,0)
    map.Grid
    |> Array.iteri (fun rowi value -> 
        value
        |> Array.iteri (fun coli gc -> 
            let testPoint = { Row = rowi; Column = coli; }
            if path.Points |> List.contains testPoint then
                printf $"*"
            else
                printf $"{gc}"
        )
        printfn ""
    )


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

let mutable (shortestPath:Path option) = None

let rec findPath (verbose:bool) (map:Map) (path:Path) (endPoint:Point) =
    
    // visited.Add startingPoint |> ignore
    // queue.Enqueue startingPoint

    if verbose then
        drawToConsole map path
        System.Console.ReadLine() |> ignore

    let current_vertex = path.Last

    if map.Value current_vertex = 'E' then
        printfn $"Found path to end at {current_vertex.Row}, {current_vertex.Column} with length of {path.Length}"
        shortestPath <- Some path
        if verbose then
            System.Console.ReadLine() |> ignore

    let qualified_nodes = 
        [left current_vertex; right current_vertex; up current_vertex; down current_vertex]
        |> List.filter(fun p -> p |> boundaryCheck map)
        |> List.filter(fun p -> map.ValueInt p - map.ValueInt current_vertex <= 1)
        |> List.filter(fun p -> not (path.Contains p))
        |> List.sortBy(fun p -> manhattanDistance p endPoint)

    for next_vertex in qualified_nodes do
            
        let nextValue = map.ValueInt next_vertex
        let currentValue = map.ValueInt current_vertex

        if (nextValue - currentValue <= 1) then
            // System.Console.WriteLine($"trying out {next_vertex.Row}x{next_vertex.Column} [{map.Value next_vertex}]")
            // System.Console.ReadKey() |> ignore
            let newPath = path.Append next_vertex

            // only worth trying out new path if it's shorter than the current shortest path
            if shortestPath.IsNone || newPath.Length < shortestPath.Value.Length then
                findPath verbose map newPath endPoint
            else
                printfn $"Skipping path {newPath.Length} because it's longer than the current shortest path {shortestPath.Value.Length}"
                if verbose then
                    System.Console.ReadLine() |> ignore

let map = parseMap input

let startingPoint = findPoint map 'S'

printfn $"Starting at {startingPoint.Row}, {startingPoint.Column}"
let endPoint = findPoint map 'E'

let startingPath = Path([startingPoint])

findPath true map startingPath endPoint

drawToConsole map (shortestPath.Value)
printfn $"Path length: {shortestPath.Value.Length}"