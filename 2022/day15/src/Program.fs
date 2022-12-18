module Day15

    type Point = {x: int; y: int}

    let parseX (input:string) =
        let start = input.IndexOf("x=") + 2
        let ending = input.IndexOf(",")

        input.Substring(start, ending - start)

    let parseY (input:string) =
        let start = input.IndexOf("y=") + 2
        let ending = input.Length

        input.Substring(start, ending - start)

    let parse (input:string) =

        let parts = input.Split(':')
        
        let sensor = {
            x = int (parseX parts[0])
            y = int (parseY parts[0])
        }

        let beacon = {
            x = int (parseX parts[1])
            y = int (parseY parts[1])
        }

        (sensor, beacon)

    let distanceFrom point1 point2 =
        abs (point1.x - point2.x) + abs (point1.y - point2.y)    

    System.Console.WriteLine("Hello")