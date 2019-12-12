let input = System.IO.File.ReadAllText(@"C:\Temp\ReallyTemp\Day10.txt")
open System
let asteroids = input.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries )
                    |> Array.mapi (fun y row -> row.ToCharArray() |> Array.mapi (fun x c -> (x,y,c)))
                    |> Array.collect id
                    |> Array.choose (fun (x,y,c) -> if c = '#' then Some(x,y) else None)

let polar (x1:int,y1:int) (x2:int,y2:int) =
    let dx, dy = float (x2 - x1), float (y2 - y1)
    let r = sqrt (dx * dx + dy * dy)
    let t = Math.PI + (atan2 -dx dy) //angle 0 vertical and only positive (for sortBy, part2)
    r,t

let except this arr = arr |> Array.filter (fun t -> t <> this)
let lookup = asteroids |> Array.map (fun asteroid -> asteroids |> except asteroid |> Array.map (fun p -> p, polar asteroid p))
let grouped = lookup |> Array.map (fun coords -> coords |> Array.groupBy (snd >> snd)) //group by angle
let greatest = grouped |> Array.maxBy (fun x -> Array.length x) //choose one with most distinct angles
let part1 = greatest |> Array.length //number of distinct angles = number of directly visible asteroids

let peel seqofseqs =
    Seq.initInfinite id
        |> Seq.collect (fun n -> seqofseqs |> Seq.where (fun list -> Seq.length list > n) |> Seq.map (Seq.item n))

let part2 = greatest |> Array.sortBy fst 
                     |> Array.map (fun (g,l) -> l |> Array.sortBy (snd >> fst) |> Array.map (fun ((x,y),_) -> x * 100 + y))
                     |> peel 
                     |> Seq.item 199 //asking for too many will hang, peel is infinite
