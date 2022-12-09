open System.IO
let input = File.ReadAllLines("input.txt")

let rows = input.Length
let columns = input[0].Length

// create a 2D array of ints
let grid = Array2D.create rows columns 0

printfn "rows: %d, columns: %d" rows columns

// // initialize grid
input 
|> Array.iteri (fun row line -> 
    line.ToCharArray()
    |> Array.iteri (fun col ch ->
        let num = int (ch.ToString())
        Array2D.set grid row col num
    )
)

let rec checkIfHidden (grid:int[,]) (row:int) (col:int) (moveFunc) (value:int) =
    let (newRow, newCol) = moveFunc(row, col)
    if newCol < 0 || newRow < 0 then
        false
    elif newRow >= rows || newCol >= columns then
        false
    else
        let toCompare = grid[newRow, newCol]
        if toCompare >= value then
            true
        else
            checkIfHidden grid newRow newCol moveFunc value

let checkIfVisible (grid:int[,]) (row:int) (col:int) =

    if row = 0 then
        true
    elif col = 0 then
        true
    else
        let value = grid.[row, col]

        let isHiddenToLeft = checkIfHidden grid row col (fun (row, col) -> (row, col-1)) value
        let isHiddenToRight = checkIfHidden grid row col (fun (row, col) -> (row, col+1)) value
        let isHiddenUp = checkIfHidden grid row col (fun (row, col) -> (row-1, col)) value
        let isHiddenDown = checkIfHidden grid row col (fun (row, col) -> (row+1, col)) value

        not isHiddenToLeft || not isHiddenToRight || not isHiddenUp || not isHiddenDown

let mutable sum = 0

grid
    |> Array2D.iteri (fun row col value ->
        let boolean = checkIfVisible grid row col
        match boolean with
        | true -> sum <- sum + 1
        | false -> ()

        printfn "row: %d, col: %d, value: %d, boolean: %b" row col value boolean
    )

printfn "sum: %d" sum