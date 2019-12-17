open System.Collections.Generic
type Chemical = { Quantity: int; Name: string }
    with static member Parse (s:string) =
                    let arr = s.Trim().Split(' ')
                    { Quantity = int arr.[0]; Name = arr.[1] }

type Reactions = IDictionary<string,int*Chemical[]>

let ParseReactions (s:string) =
                    s.Split(System.Environment.NewLine.ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map (fun line ->
                                              let arr = line.Split([|"=>"|], System.StringSplitOptions.RemoveEmptyEntries)
                                              let inputs = arr.[0].Split(',') |> Array.map Chemical.Parse
                                              let output = arr.[1] |> Chemical.Parse
                                              (output.Name, (output.Quantity, inputs)))
                    |> dict

let tryFind key (dict:Dictionary<_,_>) =
    match dict.ContainsKey(key) with
    | true -> Some(dict.[key])
    | false -> None

let checkRemainder chemical remainders = 
    match tryFind chemical.Name remainders with
    | Some(qty) ->
        let remainder = qty - chemical.Quantity
        if remainder < 0 then
            remainders.Remove(chemical.Name) |> ignore
            abs remainder
        else
            remainders.[chemical.Name] <- remainder
            0
    | None -> chemical.Quantity

let oreNeeded (reactions:Reactions) quantity remainders =
    let rec collectDependencies (requiredOutputs:Chemical[]) =
        requiredOutputs |> Array.map (fun chemical ->
            let needed = checkRemainder chemical remainders
            match chemical.Name with
            | "ORE" -> needed
            | _ -> let minProduced, chemicalsNeeded = reactions.[chemical.Name]
                   let multiples, rem = match needed / minProduced, needed % minProduced with
                                        | (x,0) -> x, 0
                                        | (x,y) -> x + 1, minProduced - y
                   match tryFind chemical.Name remainders with
                   | Some(q) -> remainders.[chemical.Name] <- q + rem
                   | None -> remainders.Add(chemical.Name, rem)
                   collectDependencies (chemicalsNeeded |> Array.map (fun c -> { Quantity = multiples * c.Quantity; Name = c.Name }))
            ) |> Array.sum

    collectDependencies [|{ Quantity = quantity; Name = "FUEL" }|]

let input = @"157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
let reactions = ParseReactions input
let ore_needed_per_fuel reactions = oreNeeded reactions 1 (new Dictionary<string,int>())
