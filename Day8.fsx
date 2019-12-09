let getLayers (input:string) height width = 
    let size = height * width
    let layercount = input.Length / size
    Seq.init layercount (fun index -> input.Substring(index * size, size))

let answerA layers= 
    let charcount search (layer:string) = layer.ToCharArray() |> Seq.where (fun c -> c = search) |> Seq.length
    let mostzeros = layers |> Seq.minBy (fun layer -> charcount '0' layer)
    let ones, twos = charcount '1' mostzeros, charcount '2' mostzeros
    ones * twos

let answerB layers = 
    let squashpixels (top, bottom) = match top with '2' -> bottom | _ -> top
    let squashlayers top bottom = Seq.zip top bottom |> Seq.map squashpixels
    layers |> Seq.cast |> Seq.reduce squashlayers 

let printlayer layer width = 
    layer |> Seq.map (function '0' -> '⬛' | _ -> '⬜') 
          |> Seq.chunkBySize width 
          |> Seq.iter (Array.ofSeq >> System.String >> printfn "%s")

let height, width = 6, 25
let input = System.IO.File.ReadAllText(@"C:\Temp\ReallyTemp\Day8.txt")
let layers = getLayers input height width
let checksum = answerA layers
let layer = answerB layers
printlayer layer width
