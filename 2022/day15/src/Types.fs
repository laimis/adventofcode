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

    type SensorBeaconPair = {
        sensor : Point
        beacon : Point
        distance : int
    }

    let parseX (input:string) =
        let start = input.IndexOf("x=") + 2
        let ending = input.IndexOf(",")

        input.Substring(start, ending - start)

    let parseY (input:string) =
        let start = input.IndexOf("y=") + 2
        let ending = input.Length

        input.Substring(start, ending - start)

    let parse (input:string) =

        let parts = input.Split(':')
        
        let sensor = {
            x = int (parseX parts[0])
            y = int (parseY parts[0])
        }

        let beacon = {
            x = int (parseX parts[1])
            y = int (parseY parts[1])
        }

        (sensor, beacon)

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

    let findHeight (points:seq<Point>) =
        (points |> Seq.maxBy (fun p -> p.y)).y + 1

    let findWidth (points:seq<Point>) =
        (
            (points |> Seq.minBy (fun p -> p.x)).x,
            (points |> Seq.maxBy (fun p -> p.x)).x
        )

    let generateSensorBeaconPairs (verbose:bool) (input:string) =
        
        input.Split(System.Environment.NewLine)
        |> Array.map parse
        |> Array.map (fun (sensor, beacon) ->
            let distance = sensor |> distanceFrom beacon
            {
                sensor = sensor;
                beacon = beacon;
                distance = distance
            }
        )

    let calculateCoverageRangeForSensor sensorBeaconPair yOfInterest =
        let sensor = sensorBeaconPair.sensor

        let distance = sensor |> distanceFrom {x=sensor.x;y=yOfInterest}

        if distance > sensorBeaconPair.distance then
            None
        else
            let pointAdjustment = abs distance - sensorBeaconPair.distance
            // the points will be at the row of interest, -diff and +diff on x axis
            Some(
                {x=sensor.x - pointAdjustment; y=yOfInterest},
                {x=sensor.x + pointAdjustment; y=yOfInterest}
            )


    let generateStringBoard verbose (input:string) =

        if verbose then
            System.Console.WriteLine("generating board...")

        let sensorBeaconPairs =
            input.Split(System.Environment.NewLine)
            |> Array.map parse

        if verbose then
            System.Console.WriteLine("... parsed pairs")

        let sensors = sensorBeaconPairs |> Array.map fst |> Set.ofArray
        let beacons = sensorBeaconPairs |> Array.map snd |> Set.ofArray

        if verbose then
            System.Console.WriteLine($"... {sensors.Count} sensors")
            System.Console.WriteLine($"... {sensors.Count} beacons")

        let toCoveragePoints (sensor, beacon) =
            let distance = sensor |> distanceFrom beacon
            
            if verbose then
                System.Console.WriteLine($"... generating points for {sensor} with distance of {distance}")

            generateCoveragePoints sensor distance

        let coveragePoints =
            sensorBeaconPairs
            |> Array.map toCoveragePoints
            |> Array.fold( fun state set -> set |> Set.union state ) Set.empty<Point>
        
        let pointsForDimmensions = coveragePoints
        
        let height = pointsForDimmensions |> findHeight
        let (minx, maxx) = pointsForDimmensions |> findWidth

        let width = maxx - minx + 1

        let count = width * height

        if verbose then
            System.Console.WriteLine($"Generating board width={width}, height={height}, points: {width * height}")

        let str =
            String.init
                count
                (fun index ->
                    
                    let row = index / width
                    let column = index - row * width

                    let candidatePoint = {x = minx + column; y = row}

                    let containsSensor = sensors |> Set.contains candidatePoint
                    let containsBeacon = beacons |> Set.contains candidatePoint

                    match (containsSensor,containsBeacon) with
                    | (true,false) -> "S"
                    | (false,true) -> "B"
                    | _ -> 
                        let containsCoveragePoint = coveragePoints.Contains(candidatePoint)
                        match containsCoveragePoint with
                        | true -> "#"
                        | false -> "."
                )

        let rows = 
            str
            |> Seq.chunkBySize width
            |> Seq.map (fun c -> System.String(c))

        let stringForm = String.concat "" rows
        {
            stringForm = stringForm;
            height = height;
            width = width;
            minX = minx;
            coveragePoints = coveragePoints;
            sensorBeaconPairs = sensorBeaconPairs
        }

    let getRowAsString row board =
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