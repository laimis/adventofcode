namespace AdventOfCode

module Day14 =
    open System.Collections.Generic

    type Point = {
        x : int
        y : int
    }

    type Line = {
        start: Point
        finish: Point
    }

    type Board = {
        stringForm: string
        width: int
        height: int
        minX: int
        pebbles: Set<Point>
    }

    let parsePoint (input:string) =
        let parts = input.Split(',')
        match parts with
        | [|x; y|] -> {x = int x; y = int y}
        | _ -> failwith $"unexpected point format {input}"

    let parseLines (input:string) =
        input.Split(" -> ")
        |> Array.pairwise
        |> Array.map( fun (s,e) -> {start=parsePoint s; finish=parsePoint e})

    let extractPoints (line:Line) =
        
        let startX = line.start.x
        let startY = line.start.y
        let finishX = line.finish.x
        let finishY = line.finish.y

        match (startX, startY, finishX, finishY) with
        | (sx,_,fx,_) when sx = fx -> 
           let min = System.Math.Min(startY, finishY)
           let max = System.Math.Max(startY, finishY)
           
           [|min..max|]
           |> Array.map( fun py -> {x = startX; y = py})

        | (_, sy, _, fy) when sy = fy ->
            let min = System.Math.Min(startX, finishX)
            let max = System.Math.Max(startX, finishX)
            
            [|min..max|]
            |> Array.map( fun px -> {x = px; y = startY})

        | _ -> failwith "unexpected line configuration"

    let parseToDistinctPoints (input:string) =
        input.Split(System.Environment.NewLine)
        |> Array.collect parseLines
        |> Array.collect extractPoints
        |> Array.distinct
        |> List.ofArray

    let findHeight (points:Point list) =
        (points |> List.maxBy (fun p -> p.y)).y + 1

    let findWidth (points:Point list) =
        (
            (points |> List.minBy (fun p -> p.x)).x,
            (points |> List.maxBy (fun p -> p.x)).x
        )

    let generateBoard verbose (points:Point list) =
        let height = points |> findHeight
        let (minx, maxx) = points |> findWidth

        let width = maxx - minx + 1

        if verbose then
            System.Console.WriteLine($"Generating board width={width}, height={height}")

        let str =
            String.init
                (width * (height-1) + height)
                (fun index ->
                    
                    let row = index / width
                    let column = index - row * width

                    let candidatePoint = {x = minx + column; y = row}

                    let contains = points |> List.contains candidatePoint

                    if verbose then
                        System.Console.WriteLine($"Mapped {index} to x={column},y={row}")

                    match contains with
                    | true -> "#"
                    | false -> "."
                )

        let rows = 
            str
            |> Seq.chunkBySize width
            |> Seq.map (fun c -> System.String(c))

        let stringForm = String.concat "" rows
        {
            stringForm = stringForm;
            height = height;
            width = width;
            minX = minx;
            pebbles = Set.empty<Point>
        }

    let convertToStringPosition board point =
        point.y * board.width + (point.x - board.minX)

    let renderNicely board =
        board.stringForm
            |> Seq.chunkBySize board.width
            |> Seq.map (fun c -> System.String(c))
            |> Seq.iter (fun s -> System.Console.WriteLine(s))

    let insideBoardBounds board point =
        point.x >= board.minX &&
        point.x < (board.minX + board.width) &&
        point.y < board.height

    let boardPointEmpty board point =
        
        if board.pebbles |> Set.contains point then
            false
        else
            if point |> insideBoardBounds board then
                let pointPosition = convertToStringPosition board point
                let c = board.stringForm[pointPosition]
                c = '.'
            else
                true

    let dropSand isInsideWorldCheck (board:Board) =
        
        let rec dropSandRecursive currentBoard point =
            
            let down = {point with y = point.y + 1}
            let left = {down with x = point.x - 1}
            let right = {down with x = point.x + 1}

            let validMoves =
                [down; left; right]
                |> List.map (fun p -> 
                    let valid = p|> boardPointEmpty currentBoard
                    match valid with
                    | true -> Some p
                    | false -> None
                )
                |> List.choose id

            match validMoves with
            | [] ->
                {currentBoard with pebbles = currentBoard.pebbles |> Set.add point} 
            | head :: _ ->
                if head |> isInsideWorldCheck currentBoard |> not then
                    currentBoard
                else
                    dropSandRecursive currentBoard head

        let start = {x = 500; y = 0}
        dropSandRecursive board start

    let debugBoard board =
        System.Console.Clear()
        System.Console.SetCursorPosition(0, 0)
        renderNicely board

        board.pebbles
        |> Seq.iter (fun p -> 
            // conver to coordinates
            let x = p.x - board.minX
            let y = p.y

            System.Console.SetCursorPosition(x,y)
            System.Console.Write('o')
        )

    let rec dropSandUntilOverflow isInsideWorldCheck board =

        let afterDrop = board |> dropSand isInsideWorldCheck
        match afterDrop.pebbles.Count = board.pebbles.Count with
        | true -> board
        | false -> afterDrop |> dropSandUntilOverflow isInsideWorldCheck

    let points =
        System.IO.File.ReadAllText("input.txt")
        |> parseToDistinctPoints

    System.Console.WriteLine($"Points {points.Length}")
    
    let board = points |> generateBoard false

    System.Console.WriteLine($"Board: height {board.height}, width {board.width}, minx {board.minX}")

    System.Console.WriteLine(board.stringForm)
    let newBoard = board |> dropSandUntilOverflow insideBoardBounds

    System.Console.WriteLine($"{newBoard.pebbles.Count}")