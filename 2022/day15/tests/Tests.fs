module Tests

open Day15
open Xunit

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
