open Day15.Types
open Day15.Presentation
    
let input = System.IO.File.ReadAllText("input.txt")

let verbose = true

let pairs = generateSensorBeaconPairs false input

let filterOnlyThoseThatCanCover verbose yOfInterest pairs =
    pairs
    |> Array.map (fun p -> 
        let result = calculateCoverageRangeForSensor verbose p yOfInterest
        match result with
        | None ->
            None
        | Some (s,e) ->
            Some (p, s, e)
    )
    |> Array.choose id

let calculateCoveredPoints verbose pairs yOfInterest =

    let set = new System.Collections.Generic.HashSet<int>()

    let matched = pairs |> filterOnlyThoseThatCanCover verbose yOfInterest
        
    if verbose then
        System.Console.WriteLine("Calculated matched")

    matched
        |> Array.iter (
            fun (_,s,e) ->
                for i in [(s.x)..(e.x)] do
                    set.Add(i) |> ignore
            )

    if verbose then
        System.Console.WriteLine("Added points")

    matched
        |> Array.iter (fun (p,_,_) -> 
            if p.sensor.y = yOfInterest then set.Remove(p.sensor.x) |> ignore
            if p.beacon.y = yOfInterest then set.Remove(p.beacon.x) |> ignore
        )

    if verbose then
        System.Console.WriteLine("Removed sensor and beacons")

    set.Count

let yOfInterest = 2000000
let count = calculateCoveredPoints true pairs yOfInterest
System.Console.WriteLine(count)
// System.Console.ReadLine() |> ignore

pairs
    |> filterOnlyThoseThatCanCover false 0
    |> Array.iter (fun (p, s, e) ->
        System.Console.WriteLine($"{s.x} - {e.x}")
        System.Console.ReadLine() |> ignore
    )

// for i in [0..4000000] do
//     let count = pairs |> filterOnlyThoseThatCanCover false i
//     System.Console.WriteLine($"{i}: {count}")

// part 2, the signal has to be coming from 4000000