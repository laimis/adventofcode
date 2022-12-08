open System.Collections
// input looks like this:

// [V]     [B]                     [F]
// [N] [Q] [W]                 [R] [B]
// [F] [D] [S]     [B]         [L] [P]
// [S] [J] [C]     [F] [C]     [D] [G]
// [M] [M] [H] [L] [P] [N]     [P] [V]
// [P] [L] [D] [C] [T] [Q] [R] [S] [J]
// [H] [R] [Q] [S] [V] [R] [V] [Z] [S]
// [J] [S] [N] [R] [M] [T] [G] [C] [D]
//  1   2   3   4   5   6   7   8   9 

// move 1 from 8 to 4
// move 1 from 7 to 8
// move 1 from 6 to 3
// move 2 from 6 to 5
// move 8 from 5 to 1
// move 5 from 3 to 8
// move 1 from 7 to 8

// write code that reads this input and creates a stack for each column and then
// processes move directions

let text = System.IO.File.ReadAllLines("input.txt")

let rec findNumberOfStacks (lines:string array) index =
    if lines[index].Contains("[") then
        findNumberOfStacks lines (index + 1)
    else
        let numberOfStacks = lines[index].Trim().Split([|' '|]) |> Array.last
        (int numberOfStacks, index)

let createStacks numberOfStacks =
    Array.init numberOfStacks (fun i -> new Stack())

let (numberOfStacks, stackRowIndex) = (findNumberOfStacks text 0)

printfn "Number of stacks: %d, and row index is %d" numberOfStacks stackRowIndex

let stacks = createStacks numberOfStacks

// initialize stacks with the starting configuration
for row = (stackRowIndex - 1) downto 0 do
    let line = text[row]

    printfn "Processing line: %s" line

    line.ToCharArray()
        |> Array.chunkBySize 4
        |> Array.map (fun chunk -> new string(chunk))
        |> Array.map (fun s -> s.Trim().Replace("[", "").Replace("]", ""))
        |> Array.iteri (fun i s ->
            printfn "%d: %s" i s
            match s with
            | "" -> ()
            | _ -> stacks[i].Push(s)
        )

let moveByBlocks quantity from dest (stacks:Stack array) =
    let movingStack = new Stack()
    for i in 1 .. quantity do
        let disk = stacks[from].Pop()
        printfn "   Moving %s from %d to %d" (disk.ToString()) from dest
        movingStack.Push(disk)

    while movingStack.Count > 0 do
        let disk = movingStack.Pop()
        stacks[dest].Push(disk)

let moveOneByOne quantity from dest (stacks:Stack array) =
    
    for i in 1 .. quantity do
        let disk = stacks[from].Pop()
        printfn "   Moving %s from %d to %d" (disk.ToString()) from dest
        stacks[dest].Push(disk)


// now process 'move' directions by going to the command lines (which is 2 rows past the stack row index)
text
    |> Array.skip (stackRowIndex + 2)
    |> Array.iter (fun line -> 
        printfn "Processing line: %s" line
        let parts = line.Split([|' '|])

        let quantity = int parts[1]
        let from = (int parts[3]) - 1
        let dest = (int parts[5]) - 1

        stacks |> moveByBlocks quantity from dest
    )

// pop one element from each stack to form the final answer
stacks 
    |> Array.iter (fun s -> printf "%s" (s.Pop().ToString()))