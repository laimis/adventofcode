module Tests

open System
open Xunit

[<Fact>]
let ``My test`` () =
    let point : Day14.Day14.Point = {x=1; y=2}
    Assert.True(point.x = 1)
