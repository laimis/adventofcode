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

let mapToVisibilityValue rowIndex columnIndex =
    let boolean = checkIfVisible grid rowIndex columnIndex
    match boolean with
    | true -> 1
    | false -> 0

let rec calculateScenicScore (grid:int[][]) (row:int) (col:int) (moveFunc) value score =
    
    let (newRow, newCol) = moveFunc(row, col)
    if newCol <= 0 || newRow <= 0 then
        score
    elif newRow >= rows - 1 || newCol >= columns - 1 then
        score
    else
        let toCompare = grid[newRow][newCol]
        if toCompare >= value then
            score
        else
            calculateScenicScore grid newRow newCol moveFunc value (score + 1)

// grid 1 2 moveright 5 1
//   newR: 1, newC: 3
//   toCompare: 1
//      grid 1 3 moveright 5 2
//         newR: 1, newC: 4
let mapToScenicScore rowIndex columnIndex =
    let value = grid[rowIndex][columnIndex]

    let upScore = calculateScenicScore grid rowIndex columnIndex (fun (row, col) -> (row-1, col)) value 1
    let leftScore = calculateScenicScore grid rowIndex columnIndex (fun (row, col) -> (row, col-1)) value 1
    let downScore = calculateScenicScore grid rowIndex columnIndex (fun (row, col) -> (row+1, col)) value 1
    let rightScore = calculateScenicScore grid rowIndex columnIndex (fun (row, col) -> (row, col+1)) value 1
    let finalScore = upScore * leftScore * downScore * rightScore
    // printfn "(%d x %d) [%d]: up: %d, left: %d, down: %d, right: %d ==> %d" rowIndex columnIndex value upScore leftScore downScore rightScore finalScore
    finalScore

let transformArrayWithFunc func arr =
    arr
    |> Array.mapi (fun rowIndex row ->
        row
        |> Array.mapi (fun columnIndex value ->
            func rowIndex columnIndex
        )
    )

let folder (acc:int[]) (row:int[]) =
    Array.concat [acc; row]

let flatten arr =
    arr |> Array.fold folder (array.Empty<int>())

let sum = grid |> transformArrayWithFunc mapToVisibilityValue |> flatten |> Array.sum
printfn "Number of visible trees: %d" sum

let bestSpot = grid |> transformArrayWithFunc mapToScenicScore |> flatten |> Array.max
printfn "Best spot's scenic score: %d" bestSpot