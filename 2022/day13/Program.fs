open System.IO
let input = File.ReadAllLines("input.txt")


type PacketData =
    | ListOfPacketData of PacketData ResizeArray
    | Int of int

let isNumber (input:char) =
    System.Char.IsNumber(input)

let outputLineWithIndent message level =
    for _ in [0..level] do
        printf " "
    printfn $"{message}"

let parse (toParse:string) =
    
    // outputLineWithIndent toParse 0
    // System.Console.ReadLine() |> ignore
    let rec parseInternal (data:PacketData) input level =
        match input with
        | "" -> data
        
        | txt when txt.StartsWith("[") ->
            match data with
            | ListOfPacketData list -> 
                let newData = (ListOfPacketData (ResizeArray<PacketData>()))
                list.Add(newData)
                parseInternal newData (input.Substring(1)) (level+1)
            | _ -> failwith "Unexpected data type"
        
        | txt when txt.StartsWith(",") ->
            parseInternal data (txt.Substring(1)) (level + 1)
        
        | txt when txt[0] |> isNumber ->
            let rec readNumber index (numberString:string) (finalString:string) =
                let c = numberString[index]
                if isNumber c then
                    readNumber (index + 1) numberString (finalString + c.ToString())
                else
                    finalString
            
            let numberString = readNumber 0 txt ""

            let number = int numberString

            match data with
            | ListOfPacketData list ->
                let number = Int(number)
                list.Add(number)
                parseInternal data (input.Substring(numberString.Length)) (level + 1)
            | _ -> failwith "Unexpected data type for number parsing path"

        | txt when txt.StartsWith("]") ->
            parseInternal data (txt.Substring(1)) (level + 1)

        | _ -> failwith $"Unexpected string sequence {input}"

    let resultingList = (ListOfPacketData (ResizeArray<PacketData>()))

    parseInternal resultingList toParse 0 |> ignore

    resultingList


let convertToString (data:PacketData) =
    
    let rec convertToStringInternal (currentData:PacketData) (str:System.Text.StringBuilder) =
        match currentData with
        | ListOfPacketData list ->
            if list.Count = 0 then
                str
            else
                let subBuilder = new System.Text.StringBuilder()
                subBuilder.Append("[") |> ignore
                list
                    |> Seq.iteri (fun subI subValue ->
                        match subValue with
                        | ListOfPacketData subList ->
                            let subData = ListOfPacketData subList
                            convertToStringInternal subData subBuilder |> ignore
                        | Int number -> 
                            subBuilder.Append(number.ToString()) |> ignore
                            if subI < list.Count - 1 then
                                subBuilder.Append(",") |> ignore
                    )
                subBuilder.Append("]") |> ignore
                str.Append(subBuilder.ToString()) |> ignore
                str
        | Int number ->
            str.Append(number.ToString())


    let builder = new System.Text.StringBuilder()
    convertToStringInternal data builder |> ignore
    builder.ToString()

let verifyPackets (left:PacketData) (right:PacketData) =

    let outputAndAskToEnter message level =
        outputLineWithIndent message level
        System.Console.ReadLine() |> ignore

    let rec verifyPacketsInternal left right level =
        let leftStr = convertToString left
        let rightStr = convertToString right

        outputAndAskToEnter $"Comparing {leftStr} vs {rightStr}" level
        
        match (left, right) with
        | (ListOfPacketData leftList, ListOfPacketData rightList) ->
            
            outputAndAskToEnter "chose list vs list" level

            let zip = Seq.zip leftList rightList
            let listComparison =
                zip |> Seq.forall (fun (l, r) -> verifyPacketsInternal l r (level+1))

            // list checked out, now make sure the left one was the same or shorter length than right
            let listComparisonResult =
                match listComparison with
                | true -> leftList.Count <= rightList.Count
                | false -> false

            outputAndAskToEnter $"result is {listComparisonResult}" level
            
            listComparisonResult

        | (ListOfPacketData leftList, Int right) ->
            let rightList = ResizeArray<PacketData>()
            rightList.Add(Int(right))
            verifyPacketsInternal (ListOfPacketData leftList) (ListOfPacketData rightList) (level+1)

        | (Int left, ListOfPacketData rightList) ->
            let leftList = ResizeArray<PacketData>()
            leftList.Add(Int(left))
            verifyPacketsInternal (ListOfPacketData leftList) (ListOfPacketData rightList) (level + 1)

        | (Int left, Int right) ->
            left <= right

    let verifyResult = verifyPacketsInternal left right 0
    printfn $"result: {verifyResult}"
    System.Console.ReadLine() |> ignore
    verifyResult

System.Console.Clear()

let sum =
    input
    |> Seq.chunkBySize 3
    |> Seq.map (fun arr -> (arr[0], arr[1]))
    |> Seq.map (fun (l,r) -> 
        let leftParsed = parse l
        let rightParsed = parse r
        ( leftParsed, rightParsed )
    )
    |> Seq.map (fun (left, right) ->
        verifyPackets left right
    )
    |> Seq.indexed
    |> Seq.map (fun (index, passed) -> 
        match passed with
        | true -> index + 1
        | false -> 0
    )
    |> Seq.sum

printfn "Sum: %i" sum
