open System.IO
let lines = File.ReadAllText("input.txt")

type Monkey =
    {
        items: int64 array
        operation: int64 -> int64
        testInt: int64
        recipientIndexIfTrue: int
        recipientIndexIfFalse: int
    }

let reader = new System.IO.StringReader(lines)

let multipleWithConstant constant number =
    number * constant

let addWithConstant constant number =
    number + constant

let multiplyByItself number =
    number * number

let addToItself number =
    number + number

let numberOfRounds = 10000

let parseOperation (line:string) =
    match line with
    | txt when txt.Contains "old *" ->
        let parts = txt.Split(" ")
        match parts.[2] with
        | "old" -> multiplyByItself
        | _ ->
            let constant = int64 parts[2]
            multipleWithConstant constant
    | txt when txt.Contains "old +" ->
        let parts = txt.Split(" ")
        match parts.[2] with
        | "old" -> addToItself
        | _ ->
            let constant = int64 parts[2]
            addWithConstant constant
    | _ -> failwith "Unknown operation"

let rec readMonkey (reader: StringReader) (monkeys: Monkey array) =
    let line = reader.ReadLine()
    if line = null then
        monkeys
    else
        let itemsLine = reader.ReadLine()
        let cleanedItemsLine = itemsLine.Replace("  Starting items: ", "")
        let items =
            cleanedItemsLine.Split(", ")
            |> Array.map int64

        let operationLine = reader.ReadLine()
        let cleanedOperationLine = operationLine.Replace("  Operation: new = ", "")
        printfn "%s" cleanedOperationLine
        let operation = parseOperation cleanedOperationLine
            
        let testLine = reader.ReadLine()
        let cleanedTestLine = testLine.Replace("  Test: divisible by ", "")
        let testInt = int cleanedTestLine

        let recipientLineIfTrue = reader.ReadLine()
        let cleanedRecipientLineIfTrue = recipientLineIfTrue.Replace("    If true: throw to monkey ", "")
        let recipientIndexIfTrue = int cleanedRecipientLineIfTrue

        let recipientLineIfFalse = reader.ReadLine()
        let cleanedRecipientLineIfFalse = recipientLineIfFalse.Replace("    If false: throw to monkey ", "")
        let recipientIndexIfFalse = int cleanedRecipientLineIfFalse
        
        let monkey =
            {
                items = items
                operation = operation
                testInt = testInt
                recipientIndexIfTrue = recipientIndexIfTrue
                recipientIndexIfFalse = recipientIndexIfFalse
            }
        
        let newList = [|monkey|] |> Array.append monkeys

        let nextLine = reader.ReadLine()
        if nextLine = null then
            newList
        else
            readMonkey reader newList

let monkeys = readMonkey reader (Array.empty<Monkey>)

type Round =
    {
        monkeys: Monkey array
        roundNumber: int
        inspectedItemsCounter : int64 array
        thrownItems : int64 array array
    }

let runRound relief round =

    round.monkeys
    |> Array.iteri (fun currentMonkeyIndex monkey -> 

        // printfn "Monkey %d" currentMonkeyIndex

        let items = monkey.items
        let operation = monkey.operation
        let testInt = monkey.testInt
        let recipientIndexIfTrue = monkey.recipientIndexIfTrue
        let recipientIndexIfFalse = monkey.recipientIndexIfFalse

        let itemsToInspect = round.thrownItems[currentMonkeyIndex] |> Array.append items
        for item in itemsToInspect do

            let newLevel = operation item
            let inspectedItem = relief newLevel

            let testResult = inspectedItem % testInt = 0L

            let recipient =
                match testResult with
                | true -> recipientIndexIfTrue
                | false -> recipientIndexIfFalse

            round.inspectedItemsCounter[currentMonkeyIndex] <- round.inspectedItemsCounter[currentMonkeyIndex] + 1L

            let newThrownItems =
                [|inspectedItem|]
                |> Array.append round.thrownItems[recipient]

            round.thrownItems[recipient] <- newThrownItems
            
            // printfn $"   Monkey inspected {item}, turned into {inspectedItem} and sent it to monkey {recipient}."

        round.thrownItems[currentMonkeyIndex] <- [||]

        // create new monkey instance with empty items
        let newMonkey = { monkey with items = [||] }

        round.monkeys.[currentMonkeyIndex] <- newMonkey

        // System.Console.ReadLine() |> ignore
    )

    { round with roundNumber = round.roundNumber + 1 }

let rec runRounds relief (round: Round) =
    let roundNumber = round.roundNumber
    
    if roundNumber > numberOfRounds then
        round
    else
        printfn "Round %d" round.roundNumber
        let newRound = round |> runRound relief

        // if roundNumber = 1 || roundNumber = 20 || roundNumber % 1000 = 0 then
        //     newRound.inspectedItemsCounter |> Array.iter (fun x -> printfn "%d" x)
        //     System.Console.ReadKey() |> ignore
        // else
        //     ()

        runRounds relief newRound


let round = {
    monkeys = monkeys;
    roundNumber = 1;
    inspectedItemsCounter = (Array.zeroCreate monkeys.Length);
    thrownItems = (Array.zeroCreate monkeys.Length) |> Array.map (fun _ -> [||])
}

let divideByThree input =
    input / 3

let testProduct = 
    monkeys
    |> Array.map (fun x -> x.testInt)
    |> Array.fold (*) 1L

let secondPartRelief input =
    input % testProduct

let result = runRounds secondPartRelief round

printfn "Finished rounds"

let monkeyBusiness =
    result.inspectedItemsCounter
    |> Array.sortDescending
    |> Array.take 2
    |> Array.fold (*) 1L

printfn "Monkey business: %d" monkeyBusiness