open System.Linq
type Point = { X : int; Y : int}
             static member (+) (a,b) = { X = a.X + b.X; Y = a.Y + b.Y }

type Move = Up of int | Down of int | Right of int | Left of int
            static member 
                Parse (s:string) = 
                    let v = s.[1..] |> int
                    match s.[0] with
                    | 'U' -> Up(v)
                    | 'D' -> Down(v)
                    | 'R' -> Right(v)
                    | _ -> Left(v)

let Develop =
    function
    | Up(x) -> Seq.init x (fun _ -> { X = 0; Y = -1 })
    | Down(x) -> Seq.init x (fun _ -> { X = 0; Y = 1 })
    | Right(x) -> Seq.init x (fun _ -> { X = 1; Y = 0 })
    | Left(x) -> Seq.init x (fun _ -> { X = -1; Y = 0 })

let trace moves = moves |> Seq.map Move.Parse |> Seq.collect Develop |> Seq.scan (+) { X = 0; Y = 0}
let manhattan p = abs p.X + abs p.Y
let closestIntersection intersections = intersections|> Array.map manhattan |> Array.min
let fastestIntersection intersections w1 w2 = 
    intersections 
        |> Array.map (fun p -> 
                        let steps1 = w1 |> Seq.findIndex (fun p' -> p = p' )
                        let steps2 = w2 |> Seq.findIndex (fun p' -> p = p' )
                        steps1 + steps2)
        |> Array.min

let input = "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83"
let wires = input.Split(System.Environment.NewLine.ToCharArray())
let wire1 = wires.[0].Split([|','|]) |> trace
let wire2 = wires.[1].Split([|','|]) |> trace

let intersections = Enumerable.Intersect(wire1,wire2) |> Seq.skip 1 |> Seq.toArray
let shortest = closestIntersection intersections
let fastest = fastestIntersection intersections wire1 wire2
