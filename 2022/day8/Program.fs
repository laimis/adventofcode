open System.IO
let input = File.ReadAllLines("input.txt")

let rows = input.Length
let columns = input[0].Length

let grid =
    input 
    |> Array.map (fun line -> 
        line.ToCharArray()
        |> Array.map (fun c -> int c - 48)
    )

let rec checkIfHidden (grid:int[][]) (row:int) (col:int) (moveFunc) (value:int) =
    let (newRow, newCol) = moveFunc(row, col)
    if newCol < 0 || newRow < 0 then
        false
    elif newRow >= rows || newCol >= columns then
        false
    else
        let toCompare = grid[newRow][newCol]
        if toCompare >= value then
            true
        else
            checkIfHidden grid newRow newCol moveFunc value

let checkIfVisible (grid:int[][]) (row:int) (col:int) =

    if row = 0 then
        true
    elif col = 0 then
        true
    else
        let value = grid[row][col]

        let isHiddenToLeft = checkIfHidden grid row col (fun (row, col) -> (row, col-1)) value
        let isHiddenToRight = checkIfHidden grid row col (fun (row, col) -> (row, col+1)) value
        let isHiddenUp = checkIfHidden grid row col (fun (row, col) -> (row-1, col)) value
        let isHiddenDown = checkIfHidden grid row col (fun (row, col) -> (row+1, col)) value

        not isHiddenToLeft || not isHiddenToRight || not isHiddenUp || not isHiddenDown

let iterateOverEachColumn rowIndex columnIndex value =
    let boolean = checkIfVisible grid rowIndex columnIndex
    match boolean with
    | true -> 1
    | false -> 0

let answer =
    grid
    |> Array.mapi (fun rowIndex row ->
        row
        |> Array.mapi (fun columnIndex value ->
            iterateOverEachColumn rowIndex columnIndex value
        )
    )

let sum = answer |> Array.sumBy (fun x -> x |> Array.sumBy (fun y -> y))

printfn "sum: %d" sum