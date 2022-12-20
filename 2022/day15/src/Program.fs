open Day15.Types
open Day15.Presentation
    
let input = System.IO.File.ReadAllText("input.txt")

let verbose = true

let pairs = generateSensorBeaconPairs false input

let set = new System.Collections.Generic.HashSet<int>()
let yOfInterest = 2000000
let matched = 
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

matched
    |> Array.iter (
        fun (_,s,e) ->
            for i in [(s.x)..(e.x)] do
                set.Add(i) |> ignore
        )

matched
    |> Array.iter (fun (p,_,_) -> 
        if p.sensor.y = yOfInterest then set.Remove(p.sensor.x) |> ignore
        if p.beacon.y = yOfInterest then set.Remove(p.beacon.x) |> ignore
    )

System.Console.WriteLine(set.Count)