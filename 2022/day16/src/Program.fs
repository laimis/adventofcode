module Day16 =

    type Valve = {
        name: string;
        rate: int;
        next: string list
    }

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
                name = m.Groups[1].Value
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


let findShortestPath verbose (tree:Map<string,Valve>) start ending =
    
    let visit (currentNode:string) =

        if verbose then
            System.Console.WriteLine($"checking if {currentNode} is {ending}")
        
        currentNode = ending


    let queue = new System.Collections.Generic.Queue<string list>()

    queue.Enqueue([start])

    let visited = new System.Collections.Generic.HashSet<string>()
    visited.Add(start) |> ignore

    let mutable shortestPath = []

    while queue.Count > 0 do
        let currentPath = queue.Dequeue()
        let currentNode = currentPath |> List.last
        if currentNode |> visit then
            shortestPath <- currentPath
            queue.Clear()
        else

            visited.Add(currentNode) |> ignore

            let valve = tree[currentNode]

            valve.next
            |> List.filter ( fun n -> visited.Contains(n) |> not)
            |> List.iter (fun n ->
                let newPath = [n] |> List.append currentPath
                queue.Enqueue(newPath)
            )

    shortestPath

let findAllPathsDfs verbose (tree:Map<string,Valve>) start ending =
    
    let visit (currentNode:string) =

        if verbose then
            System.Console.WriteLine($"checking if {currentNode} is {ending}")
        
        currentNode = ending

    let rec allPathsRec currentNode currentPath foundPaths =
        if verbose then
            System.Console.Write($"Exploring {currentPath}...")

        if currentNode |> visit then
            if verbose then
                System.Console.WriteLine(" destination found!")
                System.Console.ReadLine() |> ignore
                
            foundPaths @ [currentPath]
        else
            if verbose then
                System.Console.WriteLine(" destination not found, moving further")
                System.Console.ReadLine() |> ignore

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

        if verbose then
            System.Console.WriteLine($"checking if {currentNode} is {ending}")
        
        currentNode = ending


    let queue = new System.Collections.Generic.Queue<string list>()
    queue.Enqueue([start])

    let rec allPathsRec foundPaths =
        if queue.Count = 0 then
            if verbose then
                System.Console.WriteLine("Reached the end of queue")
                System.Console.ReadLine() |> ignore
            foundPaths
        else
            let currentPath = queue.Dequeue()
            let currentNode = currentPath |> List.last

            if verbose then
                System.Console.Write($"Exploring {currentPath}...")

            if currentNode |> visit then
                System.Console.WriteLine(" destination found!")
                System.Console.ReadLine() |> ignore
                allPathsRec foundPaths @ [currentPath]
            else
                System.Console.WriteLine(" destination not found, moving further")
                System.Console.ReadLine() |> ignore

                let valve = tree[currentNode]
                valve.next
                    |> List.filter ( fun n -> currentPath |> List.contains n |> not)
                    |> List.iter( fun n -> 
                        let newPath = currentPath @ [n]
                        queue.Enqueue(newPath)
                    )

                allPathsRec foundPaths

    allPathsRec []
                    
let numberOfSteps verbose (tree:Map<string,Valve>) start ending =
    
    let rec numberOfStepsRec currentNode visited distance =
        if verbose then
            System.Console.Write($"search for {ending}, at {currentNode} with distance {distance}")

        if currentNode = ending then
            if verbose then
                System.Console.WriteLine(", found!")
                System.Console.ReadLine() |> ignore
            Some distance
        else
            if verbose then
                    System.Console.WriteLine(", not found...")
                    System.Console.ReadLine() |> ignore
            let valve = tree[currentNode]
            let candidateDistances =
                valve.next
                |> List.filter (fun n -> visited |> Set.contains n |> not)
                |> List.map (fun n -> numberOfStepsRec n (visited |> Set.add currentNode) (distance + 1))
                |> List.choose id

            match candidateDistances with
            | [] -> None
            | _ -> Some (candidateDistances |> List.min)

    let distance = numberOfStepsRec start Set.empty<string> 0
    if verbose then
        System.Console.WriteLine($"distance from {start} to {ending} is {distance}")
    distance

let valves = parseValves lines

let verbose = true

// for from in valves.Keys do
//     for toNode in valves.Keys do
//         let distance = numberOfSteps false valves from toNode
//         System.Console.WriteLine($"Distance from {from} to {toNode} is {distance}")

// let shortestPath = findShortestPath true valves "AA" "CC"
// System.Console.WriteLine($"Shortest path from AA to CC is {shortestPath}")

// let bfsPaths = findAllPathsBfs true valves "AA" "CC"
// System.Console.WriteLine($"BFS Paths from AA to CC: {bfsPaths.Length}")
// bfsPaths |> List.iter System.Console.WriteLine

// let dfsPaths = findAllPathsDfs true valves "AA" "CC"
// System.Console.WriteLine($"DFS Paths from AA to CC: {dfsPaths.Length}")
// dfsPaths |> List.iter System.Console.WriteLine

// let problematic = findAllPathsDfs false valves "AA" "DD"

findAllPathsDfs false valves "AA" "DD"