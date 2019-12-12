let input = System.IO.File.ReadAllText(@"C:\Temp\Day12.txt")

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

let planets = input.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries) |> Array.map Planet.Parse

let except this arr = arr |> Array.filter (fun t -> t <> this)

let timeStep planets =
    planets |> Array.map (fun planet -> planets |> except planet |> Array.fold Planet.ApplyGravity planet)
            |> Array.map Planet.ApplyVelocity

let rec timeSteps times planets =
    match times with
    | 0 -> planets
    | _ -> timeSteps (times - 1) (timeStep planets)

//planets |> timeSteps 1000 |> Array.map string |> Array.iter (printfn "%s")
planets |> timeSteps 1000 |> Array.map Planet.Energy |> Array.sum
