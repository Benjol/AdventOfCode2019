let input = @"......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####"


let asteroids = input.Split(System.Environment.NewLine.ToCharArray())
                    |> Array.mapi (fun y row -> row.ToCharArray() |> Array.mapi (fun x c -> (x,y,c)))
                    |> Array.collect id
                    |> Array.choose (fun (x,y,c) -> if c = '#' then Some(x,y) else None)

let polar ((x1:int,y1:int),(x2:int,y2:int)) = 
    let dx, dy = float (x2 - x1), float (y2 - y1)
    let r = sqrt (dx * dx + dy * dy)
    let t = atan2 dy dx
    r,t
    
let xvepairwise arr this = arr |> Array.filter (fun t -> t <> this) |> Array.map (fun t -> this, t)
let coordinatepairs = asteroids |> Array.map (fun asteroid -> asteroid, xvepairwise asteroids asteroid |> Array.map polar)
let part1 = coordinatepairs |> Array.map (fun (asteroid, coords) -> coords |> Array.groupBy snd |> Array.length) |> Array.max
