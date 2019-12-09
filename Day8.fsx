let height, width = 6, 25
let size = height * width
let input = System.IO.File.ReadAllText(@"C:\Temp\Day8.txt")
let layercount = input.Length / size

let charcount search (layer:string) = layer.ToCharArray() |> Seq.where (fun c -> c = search) |> Seq.length

let mostzeros = Seq.init layercount (fun index -> input.Substring(index * size, size))
                    |> Seq.minBy (fun layer -> charcount '0' layer)

let ones, twos = charcount '1' mostzeros, charcount '2' mostzeros
let answer = ones * twos
