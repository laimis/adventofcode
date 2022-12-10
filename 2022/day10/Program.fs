open System.IO
let input = File.ReadAllLines "input.txt"

type Command =
    {
        ToAdd: int
        Cycles: int
        Name: string
    }

let parseIntoInstruction (line: string) =
    let parts = line.Split ' '
    let command = parts.[0]

    match command with
    | "noop" -> { ToAdd = 0; Cycles = 1; Name = "nop" }
    | txt when txt.StartsWith("addx")  -> 
        let numberToAdd = int parts.[1]
        { ToAdd = numberToAdd; Cycles = 2; Name = "addx" }
    | _ -> failwith "Unknown command"

let initialValue = 1

let registerValueGenerator (acc: int list) (command: Command) =
    let last =
        match acc with
        | [] -> initialValue
        | _ -> acc.[acc.Length - 1]

    match command.Name with
    | "nop" -> acc @ [last]
    | "addx" -> acc @ [last] @ [last + command.ToAdd]
    | _ -> failwith "Unknown command"

let registerSequence =
    input
    |> Array.map parseIntoInstruction
    |> Array.fold registerValueGenerator []

printfn "Register sequence length: %d" registerSequence.Length
// tricky thing here to not is that we look at the value of the register before the index cycle, as the value might have been different
// at the start of the cycle vs the end (at the end addx finishes executing, for example)
let takeOnlyEvery40thRegisterFrom20 (acc: int list, index) (_: int) =
    let toReturn =
        match index with
        | 19 -> 
            let valueToAdd = registerSequence[index - 1]
            printfn "Adding %d with value %d" (index + 1) valueToAdd
            (acc @ [valueToAdd], index + 1)
        | _ when (index - 19) % 40 = 0 -> 
            let valueToAdd = registerSequence[index - 1]
            printfn "Adding %d with value %d" (index + 1) valueToAdd
            (acc @ [valueToAdd], index + 1)
        | _ -> (acc, index + 1)
    
    toReturn

// registers to check is 20th register and then every 40 registers after that in the sequence
let sequenceToCheck =
    registerSequence
    |> List.fold takeOnlyEvery40thRegisterFrom20 ([],0)
    |> fst

printfn "Sequence to check length: %d" sequenceToCheck.Length


let sum = 
    sequenceToCheck 
    |> List.mapi (fun index value ->
        let result = (20 + index * 40) * value
        printfn "%d * %d = %d" (20 + index * 40) value result
        result
    )
    |> List.sum

printfn "Sum: %d" sum

// go through each register in the sequence and draw a . or a # depending on the value
// this is just to see the pattern in the sequence

let drawRegister (acc: string list) (value: int) =
    // let toAdd =
    //     match value with
    //     | 0 -> "."
    //     | 1 -> "#"
    //     | _ -> failwith "Unknown value"
    printfn "%d" value
    acc @ ["toAdd"]

let registerDrawing =
    registerSequence
    |> List.fold drawRegister []