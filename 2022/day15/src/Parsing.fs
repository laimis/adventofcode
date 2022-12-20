namespace Day15

module Parsing =

    open Day15.Types

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

    let findHeight (points:seq<Point>) =
        (points |> Seq.maxBy (fun p -> p.y)).y + 1

    let findWidth (points:seq<Point>) =
        (
            (points |> Seq.minBy (fun p -> p.x)).x,
            (points |> Seq.maxBy (fun p -> p.x)).x
        )

    let generateBoard verbose (input:string) =

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
        // let pointsForDimmensions = sensors |> Set.union beacons

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