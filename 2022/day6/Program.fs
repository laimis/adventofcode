open System.IO
let input = File.ReadAllText "input.txt"

let folder (row: int, found:bool) (chars: char array) =

    if (found) then
        (row, found)
    else
        let allUnique = chars |> Array.distinct |> Array.length = chars.Length

        if allUnique then
            (row, true)
        else
            (row + 1, false)

let findCharacterOfInterest (input:string) windowSize =
    let (windowIndex, found) =  
        input.ToCharArray()
        |> Array.windowed windowSize
        |> Array.fold folder (0, false)

    windowIndex + windowSize

let startOfPacketWindowSize = 4
let startOfPacket = findCharacterOfInterest input startOfPacketWindowSize
printfn "Start of packet: %d" startOfPacket

let startOfMessageWindowSize = 14
let startOfMessage = findCharacterOfInterest input startOfMessageWindowSize
printfn "Start of message: %d" startOfMessage

// 0 - 4
// 1 - 5
// 2 - 6
// 3 - 7
// 4 - 8
// 5 - 9
// 6 - 10
// 7 - 11