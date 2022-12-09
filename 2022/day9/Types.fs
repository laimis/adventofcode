namespace Ropes

module Types =
    type Move = 
        {rowAdj: int; colAdj: int; magnitude: int; direction: string}
        member this.decrementMagnitude() =
            let {rowAdj = rowAdj; colAdj = colAdj; magnitude = magnitude; direction = direction} = this

            match magnitude with
            | 0 -> failwith "Move cannot be decremented any further"
            | _ -> {rowAdj = rowAdj; colAdj = colAdj; magnitude = magnitude - 1; direction = direction}

        static member parse (input:string) =    
            let move = input[0]
            let magnitude = int input[2..]
            match move with
            | 'L' -> {rowAdj = 0; colAdj = -1; magnitude = magnitude; direction = "left"}
            | 'U' -> {rowAdj = 1; colAdj = 0; magnitude = magnitude; direction = "up"}
            | 'R' -> {rowAdj = 0; colAdj = 1; magnitude = magnitude; direction = "right"}
            | 'D' -> {rowAdj = -1; colAdj = 0; magnitude = magnitude; direction = "down"}
            | _ -> failwith "Invalid command"

    type Position = 
        {row: int; col: int}
        member this.apply (move: Move) =
            let {rowAdj = rowAdj; colAdj = colAdj; magnitude = magnitude; direction = direction} = move
            let {row = row;  col = col} = this
            {row = row + rowAdj; col = col + colAdj}
        member this.subtract (other: Position) =
            let {row = row;  col = col} = this
            let {row = otherRow; col = otherCol} = other
            (row - otherRow, col - otherCol)

        member leadingKnot.calculateKnotPosition followingKnot =
            let {row = leadingRow;  col = leadingCol} = leadingKnot
            let {row = followingRow; col = followingCol} = followingKnot

            let (rowDiff, colDiff) = leadingKnot.subtract followingKnot

            match (rowDiff,colDiff) with
            | (0, 0) -> followingKnot
            | (0,-1) -> followingKnot
            | (0,-2) -> Position.create(leadingRow, followingCol - 1)
            | (0, 1) -> followingKnot
            | (0, 2) -> Position.create(leadingRow, followingCol + 1)
            | (-1, 0) -> followingKnot
            | (-2, 0) -> Position.create(followingRow - 1, leadingCol)
            | (1, 0) -> followingKnot
            | (2, 0) -> Position.create(followingRow + 1, leadingCol)

            // separated diagonally but not far enough
            | (-1,-1) -> followingKnot
            | (1, -1) -> followingKnot
            | (-1, 1) -> followingKnot
            | (1, 1) -> followingKnot

            // diagonals
            | (2, -1) -> Position.create(followingRow + 1, leadingCol)
            | (2,  1) -> Position.create(followingRow + 1, leadingCol)
            | (1, -2) -> Position.create(leadingRow, followingCol - 1)
            | (1,  2) -> Position.create(leadingRow, followingCol + 1)
            | (-1, -2) -> Position.create(leadingRow, followingCol - 1)
            | (-1,  2) -> Position.create(leadingRow, followingCol + 1)
            | (-2, -1) -> Position.create(followingRow - 1, leadingCol)
            | (-2,  1) -> Position.create(followingRow - 1, leadingCol)
            | (2, -2) -> Position.create(followingRow + 1, followingCol - 1)
            | (2,  2) -> Position.create(followingRow + 1, followingCol + 1)
            | (-2, -2) -> Position.create(followingRow - 1, followingCol - 1)
            | (-2,  2) -> Position.create(followingRow - 1, followingCol + 1)

            | _ -> failwith $"Invalid tail position: {rowDiff}, {colDiff}"
        static member create (row, col) = {row = row; col = col}

    type Rope =
        { Parts : Position list}

        member this.move moves =
            let rec stepExecution (acc: Rope list) move =
                let {magnitude = magnitude} = move

                if magnitude = 0 then
                    acc
                else
                    let rope = acc |> List.last

                    let {Parts = parts } = rope

                    let folder (acc: Position list) (part: Position) =
                        match acc with
                        | [] -> 
                            let newPart = part.apply move
                            acc @ [newPart]
                        | _ ->
                            let leadingKnot = acc |> List.last
                            let followingKnot = leadingKnot.calculateKnotPosition part
                            acc @ [followingKnot]

                    let newParts = parts |> List.fold folder []

                    let newMove = move.decrementMagnitude()

                    let newRope = {Parts = newParts}

                    newMove |> stepExecution (acc @ [newRope])
            
            moves
            |> List.fold stepExecution [this]

            
        static member create number = 
            let parts = List.init number (fun _ -> Position.create(0,0))
            {Parts = parts}