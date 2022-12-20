module Tests

open Day15.Parsing
open Day15.Types
open Xunit

let singleSensorBeaconPair = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"

let sampleInput = @"Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"
 
[<Fact>]
let ``parse X works``() = 
    let input = "Sensor at x=2, y=18"
    Assert.Equal("2", input |> parseX)

[<Fact>]
let ``parse Y works``() =
    let input = "Sensor at x=2, y=18"
    Assert.Equal("18", input |> parseY)

[<Theory>]
[<InlineData("Sensor at x=2, y=18: closest beacon is at x=-2, y=15", 2, 18, -2, 15)>]
[<InlineData("Sensor at x=13, y=2: closest beacon is at x=15, y=3", 13, 2, 15, 3)>]
let ``Parsing input works`` (input:string) sx sy bx by  =
    let (sensor, beacon) = parse input

    Assert.Equal({x=sx; y=sy}, sensor)
    Assert.Equal({x=bx; y=by}, beacon)

[<Fact>]
let ``Manhattan distance between two points correct``() =
    let (sensor, beacon) = parse singleSensorBeaconPair

    let distance = sensor |> distanceFrom beacon

    Assert.Equal(7, distance)

[<Theory>]
[<InlineData(1, 5)>]
[<InlineData(2, 13)>]
[<InlineData(3, 25)>]
[<InlineData(4, 41)>]
let ``Point coverage given sensor and closest beacon`` distance expectedPoints =

    let (sensor, _) = parse singleSensorBeaconPair

    let points = generateCoveragePoints sensor distance

    Assert.Equal(expectedPoints, points.Count)

[<Fact>]
let ``board parsing works``() =

    let board = generateBoard false sampleInput

    Assert.Equal(37, board.width)
    Assert.Equal(27, board.height)
    Assert.StartsWith(
        "##########S########################...###########################S",
        board.stringForm
    )

[<Fact>]
let ``coverage point count works`` () =

    let board = generateBoard false sampleInput

    let row = board |> getRowAsString 10

    Assert.Equal(
        "......####B######################....",
        row
    )

[<Fact>]
let ``non beacon point count works`` () =

    let board = generateBoard false sampleInput

    let count = board |> getUnavailableBeaconSpots 10

    Assert.Equal(26, count)
