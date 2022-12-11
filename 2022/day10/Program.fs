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
    |> Array.fold registerValueGenerator [1]

printfn "Register sequence length: %d" registerSequence.Length
// tricky thing here to not is that we look at the value of the register before the index cycle, as the value might have been different
// at the start of the cycle vs the end (at the end addx finishes executing, for example)
let takeOnlyEvery40thRegisterFrom20 (acc: int list, index) (register: int) =
    let toReturn =
        match index with
        | _ when index = 19 || (index - 19) % 40 = 0 -> 
            printfn "Adding %d with value %d" (index + 1) register
            (acc @ [register], index + 1)
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

// 13140
printfn "Sum: %d" sum

// go through each register in the sequence and draw a . or a # depending on the value
// this is just to see the pattern in the sequence

type RenderState =
    {
        Cycle: int
        Screen: char list
    }

let drawRegister (state: RenderState) (register: int) =
    let { Cycle = cycle; Screen = screen } = state

    let cycleNumberToUse = cycle % 40

    let positionBeingDrawn = cycleNumberToUse - 1

    let minBoundary = register - 1
    let maxBoundary = register + 1

    let characterToDraw = 
        match positionBeingDrawn with
        | _ when positionBeingDrawn >= minBoundary && positionBeingDrawn <= maxBoundary -> '#'
        | _ -> '.'

    let newScreen = screen @ [characterToDraw]
    let newScreenStr = new System.String(newScreen |> List.toArray)

    // printfn $"Cycle: {cycleNumberToUse}, Register: {register}, Boundary: [{minBoundary},{maxBoundary}], Character: {characterToDraw}, screen: {newScreenStr}"
    // System.Console.ReadKey() |> ignore

    { state with Screen = newScreen; Cycle = state.Cycle + 1; }

let render =
    registerSequence
    |> List.fold drawRegister { Cycle = 1; Screen = [] }

render.Screen
|> List.iteri (fun index character ->
    if index % 40 = 0 then printfn ""
    printf "%c" character
)