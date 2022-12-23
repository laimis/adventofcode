module Tests

open Xunit

let sampleValveLine = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"

let sampleInput = @"Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"

[<Fact>]
let ``parse input works`` () =
    let valve = Program.Day16.parseValve sampleValveLine

    let nextMatch = ["DD";"II";"BB"] = valve.next

    Assert.Equal("AA", valve.name)
    Assert.Equal(0, valve.rate)
    Assert.True(nextMatch)

[<Fact>]
let ``parse with single exit valve``() =
    let valve = Program.Day16.parseValve "Valve HH has flow rate=22; tunnel leads to valve GG"

    Assert.Equal("HH", valve.name)
    Assert.Equal(22, valve.rate)
    Assert.True((["GG"] = valve.next))

let generateValves() =
    Program.Day16.parseValves sampleInput

[<Fact>]
let ``parse valves works``() =

    let valves = generateValves()

    Assert.Equal(10, valves.Count)

[<Fact>]
let ``number of steps works``() =
    let valves = generateValves()

    let aaToCC = Program.numberOfSteps false valves "AA" "CC"

    Assert.Equal(2, aaToCC.Value)

    let iiTojj = Program.numberOfSteps false valves "II" "JJ"

    Assert.Equal(1, iiTojj.Value)

    let jjTohh = Program.numberOfSteps false valves "JJ" "HH"

    Assert.Equal(7, jjTohh.Value)

[<Fact>]
let ``find shortest path works`` () =

    let valves = generateValves()

    let shortestPath = Program.findShortestPath false valves "AA" "DD"

    Assert.True(["AA";"DD"] = shortestPath)

[<Fact>]
let ``find all paths dfs works`` () =

    let valves = generateValves()

    let allPaths = Program.findAllPathsDfs false valves "AA" "DD"

    Assert.Equal(2, allPaths.Length)

    Assert.True(["AA"; "DD"] = allPaths[0])
    Assert.True(["AA";"BB"; "CC"; "DD"] = allPaths[1])