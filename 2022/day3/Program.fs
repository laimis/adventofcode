open System.IO

let lines = File.ReadAllLines "input.txt"

let splitIntoCompartments (line:string) =
    (line.Substring(0, line.Length / 2), line.Substring(line.Length / 2))

let findCommonItems (line1:string) (line2:string) =
    line1.ToCharArray() |> Array.filter (fun c -> line2.Contains c)

// a through z have priorities 1 through 26
// and A through Z have priorities 27 through 52
let mapPriority c =
    let intValue = (int c)

    if intValue >= 97 && intValue <= 122 then
        intValue - 96
    else
        intValue - 38


let partOne =
    lines
    |> Array.map splitIntoCompartments
    |> Array.map (fun (a, b) -> findCommonItems a b)
    |> Array.map (fun a -> a |> Array.distinct)
    |> Array.map (fun a -> a.[0])
    |> Array.map mapPriority
    |> Array.sum
    
printfn "%d" partOne

let chunks = (lines |> Array.length) / 3

let partTwo =
    lines
    |> Array.splitInto chunks
    |> Array.map (fun (arr) -> 
        arr[0].ToCharArray() |> Array.filter (fun c-> arr[1].ToCharArray() |> Array.contains c && arr[2].ToCharArray() |> Array.contains c) |> Array.head
    )
    |> Array.map mapPriority
    |> Array.sum

printfn "%d" partTwo