open System.IO
let input = File.ReadAllLines("input.txt")


type PacketData =
    | PacketDataList of ResizeArray<PacketData>
    | Int of int

let isNumber (input:char) =
    System.Char.IsNumber(input)

let verbose = false
let outputLineWithIndent message level =
    if verbose then
        for _ in [0..level] do
            printf " "
        
        printfn $"{message}"

let outputAndAskToEnter message level =
    outputLineWithIndent message level
    if verbose then
        System.Console.ReadLine() |> ignore

let parse (toParse:string) =
    
    let findExpressionEnd (input:string) =
        
        let fold state (c:char) =
    
            let (found:bool,bracketCounter:int,pos:int) = state
            match found with
            | true -> (found, bracketCounter,pos)
            | false ->
                let newPos = pos + 1
                match c with
                | '[' -> (false, bracketCounter + 1, newPos)
                | ']' ->
                    match bracketCounter - 1 with
                    | 0 -> (true, 0, newPos)
                    | _ -> (false, bracketCounter - 1, newPos)    
                | _ -> (false, bracketCounter, newPos)

        let initialState = (false, 0, 0)

        let (found, _, position) = input.ToCharArray() |> Array.fold fold initialState

        if found |> not then
            failwith "unable to find string"
        position

    let rec parseInternal (input:string) (currentList:PacketData) : PacketData =

        match input with
        
        | "" -> currentList

        | txt when txt.StartsWith("[") ->
            let ending = findExpressionEnd txt
            let expression = txt.Substring(1, ending - 2)

            match currentList with
            | Int _ -> failwith "unexpected int type for [ start"
            | PacketDataList list -> 

                let newList = (PacketDataList (new ResizeArray<PacketData>()))
                list.Add(newList)
                parseInternal expression newList |> ignore
                parseInternal (input.Substring(ending)) currentList

        | txt when txt[0] |> isNumber ->
            let rec readNumber index (numberString:string) (finalString:string) =
                if index = numberString.Length then
                    finalString
                else
                    let c = numberString[index]
                    if isNumber c then
                        readNumber (index + 1) numberString (finalString + c.ToString())
                    else
                        finalString
            
            let numberString = readNumber 0 txt ""

            let number = int numberString

            match currentList with
            | PacketDataList list ->
                let number = Int(number)
                list.Add(number)
                let newInput = input.Substring(numberString.Length)
                parseInternal newInput currentList
            | _ -> failwith "Unexpected data type for number parsing path"

        | txt when txt[0] = ',' ->
            parseInternal (txt.Substring(1)) currentList

        | _ -> failwith $"Unexpected string sequence {input}"

    let list = (PacketDataList (new ResizeArray<PacketData>()))
    parseInternal toParse list |> ignore
    list


let convertToString (data:PacketData) =
    
    let rec printRec item str =
        match item with
        | Int n -> $"{str}{n}"
        | PacketDataList list -> 
            let exprList =
                list
                |> Seq.map (fun item ->
                    match item with
                    | Int n -> printRec (Int n) ""
                    | PacketDataList l -> printRec (PacketDataList l) ""
                )
                |> List.ofSeq
                
            let expr =
                match exprList with
                | [] -> ""
                | _ -> exprList |> List.reduce (fun s1 s2 -> $"{s1},{s2}")
            
            $"{str}[{expr}]"

    printRec data ""

let comparer (left:PacketData) (right:PacketData) : int =

    let rec verifyPacketsInternal left right level : int =
        let leftStr = convertToString left
        let rightStr = convertToString right

        outputAndAskToEnter $"Comparing {leftStr} vs {rightStr}" level
        
        match (left, right) with
        | (PacketDataList leftList, PacketDataList rightList) ->

            if leftList.Count = 0 && rightList.Count = 0 then
                0
            elif leftList.Count = 0 && rightList.Count > 0 then
                -1
            elif leftList.Count > 0 && rightList.Count = 0 then
                1
            else
                let leftFirst = leftList[0]
                let rightFirst = rightList[0]

                match verifyPacketsInternal leftFirst rightFirst (level + 1) with
                | 0 -> 
                    let leftRemainingItems = leftList |> Seq.skip 1
                    let remainderL = (PacketDataList (ResizeArray<PacketData>(leftRemainingItems)))

                    let rightRemainingItems = rightList |> Seq.skip 1
                    let remainderR = (PacketDataList (ResizeArray<PacketData>(rightRemainingItems)))

                    verifyPacketsInternal remainderL remainderR (level + 1)
                | cmp -> cmp

        | (PacketDataList leftList, Int right) ->
            let rightList = ResizeArray<PacketData>()
            rightList.Add(Int(right))
            verifyPacketsInternal (PacketDataList leftList) (PacketDataList rightList) (level+1)

        | (Int left, PacketDataList rightList) ->
            let leftList = ResizeArray<PacketData>()
            leftList.Add(Int(left))
            verifyPacketsInternal (PacketDataList leftList) (PacketDataList rightList) (level + 1)

        | (Int left, Int right) ->
            compare left right

    let verifyResult = verifyPacketsInternal left right 0
    outputAndAskToEnter $"result: {verifyResult}" 0
    verifyResult

if System.Console.IsOutputRedirected |> not then
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
        let result = comparer left right
        printfn $"{convertToString left} vs {convertToString right}: {result}"
        result
    )
    |> Seq.indexed
    |> Seq.map (fun (index, verifyResult) -> 
        if verifyResult < 0
            then index + 1
        else
            0
    )
    |> Seq.sum

printfn $"Sum: {sum}"

// part two, did not understand the exact requirements, but it seems like it's about sorting the packets, then finding index of the market pakets


let packets =
    input
    |> Seq.chunkBySize 3
    |> Seq.map (fun arr -> (arr[0], arr[1]))
    |> Seq.collect (fun (l,r) ->
        [
            parse l
            parse r        
        ]
    )
    |> Seq.append [(parse "[[2]]"); parse "[[6]]"]
    |> Seq.sortWith comparer
    |> Seq.map convertToString
    |> Seq.indexed
    |> Seq.map (fun (index,string) ->
        match string with
        | "[[[2]]]" -> index + 1
        | "[[[6]]]" -> index + 1
        | _ -> 0
    )
    |> Seq.filter (fun n -> n = 0 |> not)
    |> Seq.fold (*) 1

printfn $"Product {packets}"
