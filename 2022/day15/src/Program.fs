open Day15.Types
    
let input = System.IO.File.ReadAllText("input.txt")

let verbose = true

generateSensorBeaconPairs false input
|> Array.iter(fun p ->
    System.Console.WriteLine($"{p.sensor.x}x{p.sensor.y}: {p.distance}")
)

let rowOfInteret = 2000000
