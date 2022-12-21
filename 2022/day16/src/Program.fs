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
            System.Console.WriteLine(regex.IsMatch(line))
            
            for g in m.Groups do
                System.Console.WriteLine(g.Value)

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

let valves = Day16.parseValves lines