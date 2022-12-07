open System.IO
printfn "Hello from F#"

// https://adventofcode.com/2022/day/1

let lines = File.ReadAllText "input.txt"

let stringReader = new StringReader(lines)

let mutable sum = 0
let mutable max1 = 0
let mutable max2 = 0
let mutable max3 = 0

while true do
    let parsedNumber = 
        match stringReader.ReadLine() with
        | null -> -1
        | "" -> 0
        | line -> line |> int

    // System.Console.WriteLine($"Read in {parsedNumber}")

    match parsedNumber with
    | 0 -> 
        // printfn "elf calc completed: %d" sum
        if sum > max3 then
            max1 <- max2
            max2 <- max3
            max3 <- sum
        elif sum > max2 then
            max1 <- max2
            max2 <- sum
        elif sum > max1 then
            max1 <- sum
        sum <- 0
    | -1 -> 
        printfn "Max amounts are %d %d %d" max1 max2 max3
        printfn "Sum of that is %d" (max1 + max2 + max3)
        System.Environment.Exit 0
    | _ ->
        sum <- sum + parsedNumber