module Tests

open Xunit
open AdventOfCode.Day14

let _sampleLines = @"498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"

[<Fact>]
let ``Point parsing works`` () =
    let point = parsePoint "498,13"

    Assert.True(498 = point.x)
    Assert.True(13 = point.y)

[<Fact>]
let ``Multiple points are parsed correctly`` () =
    let input = "498,13 -> 498,17 -> 491,17 -> 491,21 -> 507,21 -> 507,17 -> 502,17 -> 502,13"

    let points =
        input.Split(" -> ")
        |> Array.map parsePoint

    Assert.Equal(8, points.Length)

    let firstPoint = points[0]
    Assert.Equal(498, firstPoint.x)
    Assert.Equal(13, firstPoint.y)

    let lastPoint = points[points.Length - 1]
    Assert.Equal(502, lastPoint.x)
    Assert.Equal(13, lastPoint.y)

[<Fact>]
let ``Line parsing works`` () =
    let input = @"503,4 -> 502,4 -> 502,9 -> 494,9"

    let lines = parseLines input

    let firstLine = lines[0]

    Assert.Equal(503, firstLine.start.x)
    Assert.Equal(4, firstLine.start.y)
    Assert.Equal(502, firstLine.finish.x)
    Assert.Equal(4, firstLine.finish.y)

[<Fact>]
let ``Line to points works`` () =
    let input = @"503,4 -> 502,4 -> 502,9 -> 494,9"

    let firstLine = parseLines input |> Array.head

    let points = extractPoints firstLine

    Assert.Equal(2, points.Length)
    Assert.Equal({x=502; y=4}, points[0])
    Assert.Equal({x=503; y=4}, points[1])

    let secondLine = parseLines input |> Array.skip 1 |> Array.head

    let secondPoints = extractPoints secondLine

    Assert.Equal(6, secondPoints.Length)

    Assert.Equal({x=502; y=4}, secondPoints[0])
    Assert.Equal({x=502; y=5}, secondPoints[1])
    Assert.Equal({x=502; y=6}, secondPoints[2])
    Assert.Equal({x=502; y=7}, secondPoints[3])
    Assert.Equal({x=502; y=8}, secondPoints[4])
    Assert.Equal({x=502; y=9}, secondPoints[5])

[<Fact>]
let ``Raw input to points sequence works`` () =
    let points = parseToDistinctPoints _sampleLines
    Assert.Equal(20, points.Length)

[<Fact>]
let ``Height discovery works`` () =

    let height = _sampleLines |> parseToDistinctPoints |> findHeight

    Assert.Equal(10, height)

[<Fact>]
let ``Width range discovery works`` () =

    let (minX, maxX) = _sampleLines |>  parseToDistinctPoints |> findWidth

    Assert.Equal(494, minX)
    Assert.Equal(503, maxX)

[<Fact>]
let ``Render works`` () =

    let board = _sampleLines |> parseToDistinctPoints |> generateBoard

    let expected = @"............................................#...##....#...#...###...#.........#.........#.#########."

    Assert.Equal(expected, board.stringForm)
    Assert.Equal(10, board.width)
    Assert.Equal(10, board.height)

[<Fact>]
let ``Drop sand, renders sand as o`` () =

    let board = _sampleLines |> parseToDistinctPoints |> generateBoard

    Assert.Equal(board.height, 10)
