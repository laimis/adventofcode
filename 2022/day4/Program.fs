// read lines from input.txt

type Range = {min:int; max:int}

// input is a-b
// where a is min and b is max
let toRange (input:string) =
    let parts = input.Split('-')
    {min =int parts.[0]; max = int parts.[1]}

let contains (a:Range) (b:Range) =
    a.min >= b.min && a.max <= b.max

let overlaps (a:Range) (b:Range) =
    b.min >= a.min && b.min <= a.max || b.max >= a.min && b.max <= a.max

let fullyContainedCount =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (fun line -> line.Split(','))
    |> Array.map (fun line -> toRange line.[0], toRange line.[1])
    |> Array.filter (fun (a,b) -> contains a b || contains b a)
    |> Array.length

printfn "%d" fullyContainedCount

let overlappingCount =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (fun line -> line.Split(','))
    |> Array.map (fun line -> toRange line.[0], toRange line.[1])
    |> Array.filter (fun (a,b) -> overlaps a b || overlaps b a)
    |> Array.length

printf "%d" overlappingCount