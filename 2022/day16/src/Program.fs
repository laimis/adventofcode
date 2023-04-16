module Day16 =

    type Valve = {
        name: string;
        rate: int;
        next: string list;
        isOpen: bool;
    }

    type Action = 
        | Move of string
        | Open of string

    // "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
    let parseValve (line:string) =
        try
            let regex =
                new System.Text.RegularExpressions.Regex(
                    "Valve ([A-Z]{2}) has flow rate=(\d+); tunnel[s]? lead[s]? to valve[s]? (.+)$"
                )

            let m = regex.Match(line)
            
            {
                rate = int (m.Groups[2].Value);
                next = m.Groups[3].Value.Split(", ") |> List.ofSeq;
                name = m.Groups[1].Value;
                isOpen = false
            }
        with
            | :? System.FormatException -> 
                failwith $"Failed to parse {line}"

    let parseValves (input:string) =
        input.Split(System.Environment.NewLine)
        |> Array.map parseValve
        |> Array.map (fun e -> (e.name, e))
        |> Map.ofArray

let lines = @"Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"


open Day16

let outputIfVerbose verbose (message:string) =
    if verbose then
        System.Console.WriteLine(message)

let outputIfVerboseWithReadLine verbose (message:string) =
    if verbose then
        System.Console.WriteLine(message)
        System.Console.ReadLine() |> ignore

let findAllPathsDfs verbose (tree:Map<string,Valve>) start ending =
    
    let visit (currentNode:string) =
        $"checking if {currentNode} is {ending}" |> outputIfVerbose verbose
        currentNode = ending

    let rec allPathsRec currentNode currentPath foundPaths =
        
        $"Exploring {currentPath}..." |> outputIfVerbose verbose

        if currentNode |> visit then
            " destination found!" |> outputIfVerboseWithReadLine verbose

            foundPaths @ [currentPath]
        else
            if verbose then
                " destination not found, moving further" |> outputIfVerboseWithReadLine verbose

            let valve = tree[currentNode]
            let next = 
                valve.next
                |> List.filter ( fun n -> currentPath |> List.contains n |> not)

            match next with
            | [] -> foundPaths
            | _ -> 
                next 
                |> List.map(fun p -> 
                    let nextPath = currentPath @ [p]
                    allPathsRec p nextPath foundPaths
                )
                |> List.concat

    allPathsRec start [start] []

let findAllPathsBfs verbose (tree:Map<string,Valve>) start ending =
    
    let visit (currentNode:string) =

        $"checking if {currentNode} is {ending}" |> outputIfVerbose verbose
        
        currentNode = ending


    let queue = new System.Collections.Generic.Queue<string list>()
    queue.Enqueue([start])

    let rec allPathsRec foundPaths =
        if queue.Count = 0 then
            "Reached the end of queue" |> outputIfVerboseWithReadLine verbose
            foundPaths
        else
            let currentPath = queue.Dequeue()
            let currentNode = currentPath |> List.last

            $"Exploring {currentPath}..." |> outputIfVerbose verbose

            if currentNode |> visit then
                " destination found!" |> outputIfVerboseWithReadLine verbose

                allPathsRec foundPaths @ [currentPath]
            else
                " destination not found, moving further" |> outputIfVerboseWithReadLine verbose

                let valve = tree[currentNode]
                valve.next
                    |> List.filter ( fun n -> currentPath |> List.contains n |> not)
                    |> List.iter( fun n -> 
                        let newPath = currentPath @ [n]
                        queue.Enqueue(newPath)
                    )

                allPathsRec foundPaths

    allPathsRec []

let findShortestPath verbose (tree:Map<string,Valve>) start ending =
    
    findAllPathsBfs verbose tree start ending
    |> List.sortBy (fun p -> p.Length)
    |> List.head

let numberOfSteps verbose (tree:Map<string,Valve>) start ending =
    
    (findShortestPath verbose tree start ending |> List.length) - 1
    |> Some

let valvesWithPressure (valves:seq<Valve>) =
    valves
    |> Seq.filter (fun v -> v.rate > 0)
    |> Seq.sortByDescending (fun v -> v.rate)
    |> List.ofSeq

let pathPressureReleasePotential (valves:Map<string,Valve>) path =
    path
    |> List.map (fun name -> 
        let v = valves[name]
        match v.isOpen with
        | true -> 0
        | false -> v.rate)
    |> List.sum

let summarize valves start ending =
    
    let availablePaths =
        findAllPathsBfs false valves start ending

    let valve = valves[start]

    let shortest = 
        availablePaths
        |> List.sortBy (fun l -> l.Length)
        |> List.head
        |> List.length

    System.Console.WriteLine($"{valve.name} {valve.rate} - {shortest} - {availablePaths.Length}")
    System.Console.ReadLine() |> ignore

    availablePaths
        |> List.map(fun p -> pathPressureReleasePotential valves p)
        |> List.iteri(fun i p -> System.Console.WriteLine($"    {availablePaths[i].Length}: {p}"))

let valves = parseValves lines

let verbose = false

let sampleValvesWithPressure = valvesWithPressure valves.Values
System.Console.WriteLine("Valves with pressure:")
sampleValvesWithPressure
|> List.iter (fun v -> System.Console.WriteLine($"{v.name}: {v.rate}"))

System.Console.WriteLine("Now from input")

let inputTxt = System.IO.File.ReadAllText("input.txt")
let inputValves = parseValves inputTxt
let inputValvesWithPressure = valvesWithPressure inputValves.Values
System.Console.WriteLine("Valves with pressure:")
inputValvesWithPressure
    |> List.iter (fun v -> summarize inputValves (v.name) "AA")