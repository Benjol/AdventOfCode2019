let input = "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L"

let pairs = input.Split(System.Environment.NewLine.ToCharArray()) |> Array.map (fun x -> x.Split([|')'|])) |> Array.map (fun a -> a.[0],a.[1])

let (planets,satellites) = Array.unzip pairs
let com = planets |> Array.find (fun planet -> not <| Array.contains planet satellites)

let rec countorbits depth planet:int  =
    //get satellites
    let satellites = pairs |> Array.where (fun (p,s) -> p = planet) |> Array.map snd
    //for each recursion, increment depth
    let sum = satellites |> Array.sumBy (fun s -> countorbits (depth + 1) s)
    printfn "countorbits %A -> depth:%A %A satellites: (total: %A)" planet depth satellites (sum + 1)
    //return own depth + sum of child depths
    sum + depth

countorbits 0 com
