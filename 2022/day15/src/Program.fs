open Day15.Types
open Day15.Parsing
open Day15.Presentation
    
let input = System.IO.File.ReadAllText("input.txt")

let verbose = true

let board = generateBoard verbose input

System.Console.WriteLine($"Board generated")
System.Console.WriteLine($"width: {board.width}, height: {board.height}")
System.Console.WriteLine(board.stringForm)

System.Console.WriteLine(board |> renderNicely)
// let unavailableSpots = board |> getUnavailableBeaconSpots 10
// System.Console.WriteLine(unavailableSpots)