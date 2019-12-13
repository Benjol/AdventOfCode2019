
open System
open System.Text.RegularExpressions

type Vector = { X : int; Y : int; Z : int }
    with static member Zero = { X = 0; Y = 0; Z = 0 }
         static member OfArray (arr:int[]) = { X = arr.[0]; Y = arr.[1]; Z = arr.[2] }
         static member (+) (v1, v2) = { X = v1.X + v2.X; Y = v1.Y + v2.Y; Z = v1.Z + v2.Z }
         static member (%) (v1, v2) = { X = sign (v2.X - v1.X); Y = sign (v2.Y - v1.Y); Z = sign (v2.Z - v1.Z) }
         member this.Energy = (abs this.X) + (abs this.Y) + (abs this.Z)
         override this.ToString() = sprintf "<x=%A, y=%A, z=%A>" this.X this.Y this.Z

type Planet = { Velocity : Vector; Position : Vector }
    with static member Parse s =
                        let xyz = Regex.Matches(s, "-?\d+")
                                    |> Seq.cast<Match> |> Seq.map (fun m -> int m.Value)
                                    |> Array.ofSeq
                        { Velocity = Vector.Zero; Position = Vector.OfArray xyz }
         static member ApplyVelocity v = { v with Position = v.Position + v.Velocity }
         static member ApplyGravity this other = { this with Velocity = this.Velocity + (this.Position % other.Position) }
         static member Energy this = this.Velocity.Energy * this.Position.Energy
         override this.ToString() = sprintf "pos=%s, vel=%s" (string this.Position) (string this.Velocity)

let input = System.IO.File.ReadAllText(@"C:\Temp\Day12.txt")
let planets = input.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries) |> Array.map Planet.Parse

let except this arr = arr |> Array.filter (fun t -> t <> this)

let timeStep planets =
    planets |> Array.map (fun planet -> planets |> except planet |> Array.fold Planet.ApplyGravity planet)
            |> Array.map Planet.ApplyVelocity

let rec timeSteps times planets =
    match times with
    | 0 -> planets
    | _ -> timeSteps (times - 1) (timeStep planets)

let part1 = planets |> timeSteps 1000 |> Array.map Planet.Energy |> Array.sum

//part2 (I had to look on reddit, but annoying because I was so close)
let iterate (p1,p2,p3,p4,v1,v2,v3,v4) =
    let (d12,d13,d14,d23,d24,d34) = sign (p2 - p1), sign (p3 - p1), sign(p4 - p1), sign(p3 - p2), sign(p4 - p2), sign(p4 - p3)
    let (v1,v2,v3,v4) = v1 + d12 + d13 + d14, v2 - d12 + d23 + d24, v3 - d13 - d23 + d34, v4 - d14 - d24 - d34
    (p1+v1,p2+v2,p3+v3,p4+v4,v1,v2,v3,v4)

let axisRepeat startState = Seq.initInfinite id
                               |> Seq.scan (fun (_, state) i -> i + 1, iterate state) (0, startState)
                               |> Seq.scan (fun (set, _) (i,state) ->
                                                match Set.contains state set with
                                                | true -> set, Some i
                                                | false -> Set.add state set, None ) (Set.empty, None)
                               |> Seq.pick snd

let xRepeat = axisRepeat (planets.[0].Position.X,planets.[1].Position.X,planets.[2].Position.X,planets.[3].Position.X,0,0,0,0)
let yRepeat = axisRepeat (planets.[0].Position.Y,planets.[1].Position.Y,planets.[2].Position.Y,planets.[3].Position.Y,0,0,0,0)
let zRepeat = axisRepeat (planets.[0].Position.Z,planets.[1].Position.Z,planets.[2].Position.Z,planets.[3].Position.Z,0,0,0,0)

let lcm arr =
    let inline (%) a b = if a % b = 0 then a / b else a
    let rec inner arr acc n =
        if Array.forall (fun x -> x = 1) arr then
            acc |> List.map int64 |> List.reduce (*)
        else
            let result = arr |> Array.map (fun a -> a % n)
            if result = arr then
                //nothing changed
                inner result acc (n + 1)
            else // add n to acc and try again
                inner result (n::acc) n
    inner arr [] 2

let part2 = lcm [|xRepeat;yRepeat;zRepeat|]
