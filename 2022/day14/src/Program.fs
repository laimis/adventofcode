namespace AdventOfCode

module Day14 =

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

    let findHeight (points:Point array) =
        (points |> Array.maxBy (fun p -> p.y)).y + 1

    let findWidth (points:Point array) =
        (
            (points |> Array.minBy (fun p -> p.x)).x,
            (points |> Array.maxBy (fun p -> p.x)).x
        )

    let generateBoard (points:Point array) =
        let height = points |> findHeight
        let (minx, maxx) = points |> findWidth

        let width = maxx - minx + 1

        let str =
            String.init
                (width * (height-1) + height)
                (fun index ->
                    
                    let row = index / (height)
                    let column = index - row * width

                    let candidatePoint = {x = minx + column; y = row}

                    let contains = points |> Array.contains candidatePoint

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
            minX = minx
        }

    let convertToStringPosition board point =
        point.y * board.width + (point.x - board.minX)

    let renderNicely board =
        board.stringForm
            |> Seq.chunkBySize board.width
            |> Seq.map (fun c -> System.String(c))
            |> Seq.iter (fun s -> System.Console.WriteLine(s))

    let points =
        System.IO.File.ReadAllText("input.txt")
        |> parseToDistinctPoints

    let dropSand (board:Board) =
        let createNewBoardWithSandPebble board position =
            let arr = board.stringForm.ToCharArray()
            arr[position] <- 'o'
            let newForm = System.String(arr)
            { board with stringForm = newForm }

        let rec dropSandRecursive currentBoard point =
            
            let newPoint = {point with y = point.y + 1}
            let newPointPosition = convertToStringPosition currentBoard newPoint
            let c = currentBoard.stringForm[newPointPosition]
            match c with 
            | '.' -> dropSandRecursive (createNewBoardWithSandPebble currentBoard newPointPosition) newPoint
            | _ -> currentBoard

        let start = {x = 500; y = 0}
        dropSandRecursive board start
        
        // let positionInString = convertToStringPosition board start
        // arr[positionInString] <- 'o'
        // let newForm = System.String(arr)
        // { board with stringForm = newForm }

    let (minx, maxx) = points |> findWidth
    let width = maxx - minx + 1
    let board = points |> generateBoard

    System.Console.WriteLine(board.stringForm)
    
    board |> dropSand |> renderNicely