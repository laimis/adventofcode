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

let bfs verbose (tree:Map<string,Valve>) start =
    
    let visit (currentNode:string) =
        if verbose then
            System.Console.WriteLine(currentNode)

    let queue = new System.Collections.Generic.Queue<string>()

    queue.Enqueue(start)

    let visited = new System.Collections.Generic.HashSet<string>()
    visited.Add(start) |> ignore

    while queue.Count > 0 do
        let currentNode = queue.Dequeue()

        visit currentNode

        visited.Add(currentNode) |> ignore

        let valve = tree[currentNode]

        valve.next
        |> List.filter ( fun n -> visited.Contains(n) |> not)
        |> List.iter (queue.Enqueue)

let numberOfSteps verbose (tree:Map<string,Valve>) start ending =
    
    let maxDistance = 1000
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

bfs verbose valves "AA"

for from in valves.Keys do
    for toNode in valves.Keys do
        let distance = numberOfSteps false valves from toNode
        System.Console.WriteLine($"Distance from {from} to {toNode} is {distance}")


