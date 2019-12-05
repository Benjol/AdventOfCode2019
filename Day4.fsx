// rules 1 & 2 true, by definition
let regx = System.Text.RegularExpressions.Regex("(\d)\1{1,}")
let rule3 s = regx.IsMatch(s)
let rule4 (s:string) = s.ToCharArray() |> Array.fold (fun (ok,prev) c -> ok && c >= prev, c) (true, '0') |> fst
[|168630 .. 718098|] |> Array.map string |> Array.where (fun s -> rule3 s && rule4 s) |> Array.length
