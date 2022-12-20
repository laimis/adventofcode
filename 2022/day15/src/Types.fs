namespace Day15

module Types =

    type Point = {x: int; y: int}

    type Board = {
        sensorBeaconPairs : (Point * Point) array
        coveragePoints : Set<Point>
        stringForm : string
        height: int
        width: int
        minX: int
    }

    let distanceFrom (point1:Point) (point2:Point) =
        abs (point1.x - point2.x) + abs (point1.y - point2.y)

    let bfs verbose start distance =
        let queue = new System.Collections.Generic.Queue<Point>()

        queue.Enqueue(start)
        
        let collected = new System.Collections.Generic.HashSet<Point>()

        while queue.Count > 0 do

            let point = queue.Dequeue()

            if verbose then
                System.Console.WriteLine($"Visiting {point}")

            collected.Add(point) |> ignore

            [
                {x = point.x + 1;   y = point.y }
                {x = point.x - 1;   y = point.y }
                {x = point.x;       y = point.y + 1 }
                {x = point.x;       y = point.y - 1 }
            ]
            |> List.filter  (fun p -> collected.Contains(p) |> not)
            |> List.filter  (fun p -> (start |> distanceFrom p) <= distance)
            |> List.iter    (fun p -> queue.Enqueue(p))

        collected
            |> Set.ofSeq

    let generateCoveragePoints source distance =
        bfs false source distance

    let generateCoveragePointsVerbose source distance =
        bfs true source distance

    let getRowAsString row board =
        let str = board.stringForm

        let position = board.width * row

        board.stringForm.Substring(position, board.width)

    let getUnavailableBeaconSpots row board =

        let str = board |> getRowAsString row

        str
        |> Seq.map( fun c-> 
            match c = '#' with
            | true -> 1
            | false -> 0
        )
        |> Seq.sum