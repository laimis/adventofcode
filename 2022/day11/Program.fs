open System.IO
let lines = File.ReadAllText("input.txt")

type Monkey =
    {
        items: int array
        operation: int -> int
        testInt: int
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

let parseOperation (line:string) =
    match line with
    | txt when txt.Contains "old *" ->
        let parts = txt.Split(" ")
        match parts.[2] with
        | "old" -> multiplyByItself
        | _ ->
            let constant = int parts[2]
            multipleWithConstant constant
    | txt when txt.Contains "old +" ->
        let parts = txt.Split(" ")
        match parts.[2] with
        | "old" -> addToItself
        | _ ->
            let constant = int parts[2]
            addWithConstant constant
    | _ -> failwith "Unknown operation"

let rec readMonkey (reader: System.IO.StringReader) list =
    let line = reader.ReadLine()
    if line = null then
        list
    else
        let itemsLine = reader.ReadLine()
        let cleanedItemsLine = itemsLine.Replace("  Starting items: ", "")
        let items =
            cleanedItemsLine.Split(", ")
            |> Array.map int

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
        
        let newList = list @ [monkey]

        let nextLine = reader.ReadLine()
        if nextLine = null then
            newList
        else
            readMonkey reader newList

let monkeys = readMonkey reader []

printfn "%A" monkeys
