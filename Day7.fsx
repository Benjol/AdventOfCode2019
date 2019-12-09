type ParameterMode = PositionMode | ImmediateMode

type Parameter = 
    | Value of int
    | Address of int

type Instruction =
    | Initial
    | Add of Parameter * Parameter * Parameter
    | Multiply of Parameter * Parameter * Parameter
    | Input of Parameter
    | Output of Parameter
    | JumpIfTrue of Parameter * Parameter
    | JumpIfFalse of Parameter * Parameter
    | LessThan of Parameter * Parameter * Parameter
    | Equals of Parameter * Parameter * Parameter
    | Stop

let parseParamMode (opCode:int) parameterPosition =
    let level = pown 10 (parameterPosition + 1)
    match (opCode / level) % 10 with
    | 1 -> ImmediateMode
    | _ -> PositionMode

//parse one instruction from array, return next position
let parseInstruction (arr:int[]) (i:int):(Instruction * int) = 
    let opcode = arr.[i]
    let getParameterAt pos = 
        match parseParamMode opcode pos with
        | PositionMode -> Address(int arr.[i + pos])
        | ImmediateMode ->  Value(int arr.[i + pos])
    match opcode % 100 with
    | 1 -> Add ( getParameterAt 1, getParameterAt 2, getParameterAt 3), i + 4
    | 2 -> Multiply ( getParameterAt 1, getParameterAt 2, getParameterAt 3), i + 4
    | 3 -> Input ( getParameterAt 1), i + 2
    | 4 -> Output ( getParameterAt 1), i + 2
    | 5 -> JumpIfTrue ( getParameterAt 1, getParameterAt 2), i + 3
    | 6 -> JumpIfFalse ( getParameterAt 1, getParameterAt 2), i + 3
    | 7 -> LessThan ( getParameterAt 1, getParameterAt 2, getParameterAt 3), i + 4
    | 8 -> Equals ( getParameterAt 1, getParameterAt 2, getParameterAt 3), i + 4
    | _ -> Stop, 0

let getParameter (arr:int[]) parameter =
    //printfn "Read from %A" parameter
    match parameter with
    | Value(v) -> v
    | Address(i) -> arr.[i]

let setParameter (arr:int[]) parameter value =
    //printfn "Write %A to %A" value parameter
    match parameter with
    | Value(v) -> failwith "Can't write to a Parameter in immediate mode"
    | Address(i) -> arr.[i] <- value
 
type InputOutput = { Input : unit -> int; Output : int -> unit }

let run arr io = 
    let arr = Array.copy arr //protect source
    let mutable (instruction, index) = parseInstruction arr 0
    //printfn "%A" (instruction, index)
    while instruction <> Stop do
        match instruction with
        | Add(p1, p2, p3) -> setParameter arr p3 ((getParameter arr p1) + (getParameter arr p2))
        | Multiply(p1, p2, p3) -> setParameter arr p3 ((getParameter arr p1) * (getParameter arr p2))
        | Input(p1) -> io.Input() |> setParameter arr p1
        | Output(p1) -> getParameter arr p1  |> io.Output
        | JumpIfTrue(p1, p2) -> if getParameter arr p1 <> 0 then index <- getParameter arr p2
        | JumpIfFalse(p1, p2) -> if getParameter arr p1 = 0 then index <- getParameter arr p2
        | LessThan(p1, p2, p3) -> if getParameter arr p1 < getParameter arr p2 then
                                     setParameter arr p3 1
                                  else
                                     setParameter arr p3 0
        | Equals(p1, p2, p3) ->  if getParameter arr p1 = getParameter arr p2 then
                                     setParameter arr p3 1
                                  else
                                     setParameter arr p3 0
        | _ -> ()

        let tuple = parseInstruction arr index
        instruction <- fst tuple
        index <- snd tuple
        //printfn "%A" tuple

let amplifier program phase input = 
    let mutable output = 0
    let inputs = [|phase;input|]
    let mutable index = -1 //yuck!
    let io = { Input = (fun () -> index <- index + 1; inputs.[index]); Output = (fun x -> output <- x) }
    run program io
    output

let multipleAmplifiers program phases =
    let mutable input = 0
    for phase in phases do 
        input <- amplifier program phase input
    input

let getMax program phaseList =
    let rec insertions x = function
        | []             -> [[x]]
        | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

    let rec permutations = function
        | []      -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))
    
    phaseList |> permutations |> Seq.map (multipleAmplifiers program) |> Seq.max

let program = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0".Split([|','|]) |> Array.map int
getMax program [10;1;2;3;4]
