module FirstAttempt
open System.Collections.Generic

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

    let getColorForCharacter character =
        // match letters from a to z and pick a color for each, a should be blue and z should be red, and others in between based on intensity
        let color =
            match character with
            | 'a' -> System.ConsoleColor.Blue
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
            
            
    let drawToConsole map (path:Path) (newGraphicalPath:char array) =
        if System.Console.IsOutputRedirected then
            ()
        else
            System.Console.Clear()
            System.Console.SetCursorPosition(0,0)
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

            System.Console.ForegroundColor <- System.ConsoleColor.Red
            path.Points
            |> List.iteri (fun i p ->
                System.Console.SetCursorPosition(p.Column, p.Row)
                System.Console.Write($"{newGraphicalPath.[i]}")
            )

            System.Console.SetCursorPosition(0, map.Rows + 1)
            System.Console.ResetColor()


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

    let boundaryCheck map p =
        let isValid =
            p.Row >= 0 && p.Row < map.Rows &&
            p.Column >= 0 && p.Column < map.Columns
        
        isValid

    let rec findPath (verbose:bool) (map:Map) (path:Path) (graphicalPath:char array) (shortestDistances:Dictionary<Point, int>) (endPoint:Point) =
        
        counter <- counter + 1

        if verbose || counter % 1000 = 0 then
            drawToConsole map path graphicalPath
            // if verbose then
            //     System.Console.ReadLine() |> ignore

        let current_vertex = path.Last
        let currentValue = map.ValueInt current_vertex

        shortestDistances[current_vertex] <- path.Length

        if map.Value current_vertex = 'E' then
            printfn $"Found path to end at {current_vertex.Row}, {current_vertex.Column} with length of {path.Length}"
            shortestPath <- Some path
            shortestGraphicalPath <- Some graphicalPath
            if verbose then
                System.Console.ReadLine() |> ignore

        let qualified_nodes = 
            [left current_vertex; right current_vertex; up current_vertex; down current_vertex]
            |> List.filter(fun (p, _) -> p |> boundaryCheck map)
            |> List.filter(fun (p, _) -> map.ValueInt p - currentValue <= 1)
            |> List.filter(fun (p, _) -> not (path.Contains p))
            |> List.sortBy(fun (p, _) -> manhattanDistance p endPoint)


        if verbose then
            System.Console.WriteLine($"Found {qualified_nodes.Length} qualified nodes")
            qualified_nodes
            |> List.iter(fun (p, _) -> System.Console.WriteLine($"  {p.Row}x{p.Column} {map.Value p} {manhattanDistance p endPoint}"))
            System.Console.ReadLine() |> ignore

        for (next_vertex, movement_character) in qualified_nodes do
                
            let newPath = path.Append next_vertex
            let newGraphicalPath = [|movement_character|] |> Array.append graphicalPath

            let shortestDistance = shortestDistances.GetValueOrDefault(next_vertex, System.Int32.MaxValue)
            if shortestDistance > newPath.Length then
                // only worth trying out new path if it's shorter than the current shortest path
                if shortestPath.IsNone || newPath.Length < shortestPath.Value.Length then
                    findPath verbose map newPath newGraphicalPath shortestDistances endPoint
                else
                    printfn $"Skipping path {newPath.Length} because it's longer than the current shortest path {shortestPath.Value.Length}"
                    if verbose then
                        System.Console.ReadLine() |> ignore
            else
                if verbose then
                    printfn $"Skipping path because it's longer than the current shortest path {shortestDistance}"
                    System.Console.ReadLine() |> ignore

    let findShortestPath verbose (input:string array) =
        let map = parseMap input

        let startingPoint = findPoint map 'S'

        printfn $"Starting at {startingPoint.Row}, {startingPoint.Column}"

        let endPoint = findPoint map 'E'

        let startingPath = Path([startingPoint])

        let shortestDistances = new Dictionary<Point, int>()

        drawToConsole map startingPath [|'S'|]
        System.Console.ReadLine() |> ignore

        try
            findPath verbose map startingPath [|'S'|] shortestDistances endPoint
        with
            | :? System.Exception as e ->
                printfn $"Exception at {counter} iterations"
                printfn $"Shortest path found so far: {shortestPath.Value.Length}"
                printfn $"Exception: {e.Message}"

        drawToConsole map (shortestPath.Value) (shortestGraphicalPath.Value)
        printfn $"Path length: {shortestPath.Value.Length}"

        shortestPath.Value