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
    printfn "Read from %A" parameter
    match parameter with
    | Value(v) -> v
    | Address(i) -> arr.[i]

let setParameter (arr:int[]) parameter value =
    printfn "Write %A to %A" value parameter
    match parameter with
    | Value(v) -> failwith "Can't write to a Parameter in immediate mode"
    | Address(i) -> arr.[i] <- value
 
let run arr = 
    let mutable (instruction, index) = parseInstruction arr 0
    printfn "%A" (instruction, index)
    while instruction <> Stop do
        match instruction with
        | Add(p1, p2, p3) -> setParameter arr p3 ((getParameter arr p1) + (getParameter arr p2))
        | Multiply(p1, p2, p3) -> setParameter arr p3 ((getParameter arr p1) * (getParameter arr p2))
        | Input(p1) -> printf "Enter value then hit Enter:"
                       let input = System.Console.ReadLine()
                       setParameter arr p1 (int input)
        | Output(p1) -> System.Console.WriteLine(getParameter arr p1)
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
        printfn "%A" tuple
