namespace Day15

module Presentation =
    open Types
    
    let renderNicely board =
        board.stringForm
            |> Seq.chunkBySize board.width
            |> Seq.map (fun c -> System.String(c))
            |> Seq.iter (fun s -> System.Console.WriteLine(s))