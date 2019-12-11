type ParameterMode = PositionMode | ImmediateMode | RelativeMode

type Parameter =
    | Value of int64
    | Address of int64
    | Relative of int64

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
    | OffsetBase of Parameter
    | Stop

let parseParamMode (opCode:int64) parameterPosition =
    let level = pown 10L (parameterPosition + 1)
    match (opCode / level) % 10L with
    | 0L -> PositionMode
    | 1L -> ImmediateMode
    | _ -> RelativeMode

type Computer (program:int64[]) =
    let memory = program |> Array.mapi (fun i v -> int64 i, v) |> dict |> System.Collections.Generic.Dictionary
    let mutable relativeBase = 0L

    let getMemory address =
        //printfn "getMemory %A" address
        if not <| memory.ContainsKey(address) then memory.Add(address, 0L)
        memory.[address]

    let setMemory address value =
        //printfn "setMemory %A = %A" address value
        if not <| memory.ContainsKey(address) then
            memory.Add(address, value)
        else
            memory.[address] <- value

    member this.getParameter parameter =
        //printfn "Read from %A" parameter
        match parameter with
        | Value(v) -> v
        | Address(i) -> getMemory i
        | Relative(i) -> getMemory (relativeBase + i)

    member this.setParameter parameter value =
        //printfn "Write %A to %A" value parameter
        match parameter with
        | Value(v) -> failwith "Can't write to a Parameter in immediate mode"
        | Address(i) -> setMemory i value
        | Relative(i) -> setMemory (relativeBase + i) value

    member this.adjustBase offset =
        //printfn "Adjust base to %A" (relativeBase + offset)
        relativeBase <- relativeBase + offset

    //parse one instruction from array, return next position
    member this.getInstruction (i:int64):(Instruction * int64) =
        let opcode = memory.[i]
        //printfn "getInstruction at %A, opcode = %A" i opcode
        let getParameterAt pos =
            match parseParamMode opcode pos with
            | PositionMode -> Address(memory.[i + (int64)pos])
            | ImmediateMode -> Value(memory.[i + (int64)pos])
            | RelativeMode -> Relative(memory.[i + (int64)pos])
        match (int opcode) % 100 with
        | 1 -> Add (getParameterAt 1, getParameterAt 2, getParameterAt 3), i + 4L
        | 2 -> Multiply (getParameterAt 1, getParameterAt 2, getParameterAt 3), i + 4L
        | 3 -> Input (getParameterAt 1), i + 2L
        | 4 -> Output (getParameterAt 1), i + 2L
        | 5 -> JumpIfTrue (getParameterAt 1, getParameterAt 2), i + 3L
        | 6 -> JumpIfFalse (getParameterAt 1, getParameterAt 2), i + 3L
        | 7 -> LessThan (getParameterAt 1, getParameterAt 2, getParameterAt 3), i + 4L
        | 8 -> Equals (getParameterAt 1, getParameterAt 2, getParameterAt 3), i + 4L
        | 9 -> OffsetBase (getParameterAt 1), i + 2L
        | _ -> Stop, 0L

type InputOutput = 
    abstract member Input : unit -> int64    
    abstract member Output : int64 -> unit

let run arr (io:InputOutput) =
    let computer = Computer(arr)
    let mutable (instruction, index) = computer.getInstruction 0L
    while instruction <> Stop do
        match instruction with
        | Add(p1, p2, p3) -> computer.setParameter p3 ((computer.getParameter p1) + (computer.getParameter p2))
        | Multiply(p1, p2, p3) -> computer.setParameter p3 ((computer.getParameter p1) * (computer.getParameter p2))
        | Input(p1) -> io.Input() |> computer.setParameter p1
        | Output(p1) -> computer.getParameter p1  |> io.Output
        | JumpIfTrue(p1, p2) -> if computer.getParameter p1 <> 0L then index <- computer.getParameter p2
        | JumpIfFalse(p1, p2) -> if computer.getParameter p1 = 0L then index <- computer.getParameter p2
        | LessThan(p1, p2, p3) -> if computer.getParameter p1 < computer.getParameter p2 then
                                     computer.setParameter p3 1L
                                  else
                                     computer.setParameter p3 0L
        | Equals(p1, p2, p3) ->  if computer.getParameter p1 = computer.getParameter p2 then
                                     computer.setParameter p3 1L
                                  else
                                     computer.setParameter p3 0L
        | OffsetBase(p1) -> computer.getParameter p1 |> computer.adjustBase
        | _ -> ()

        let tuple = computer.getInstruction index
        instruction <- fst tuple
        index <- snd tuple
        
type Paint = Black | White
    with
        static member ToLong = function Black -> 0L | _ -> 1L
        static member FromLong = function 0L -> Black | _ -> White
type Orientation = Up | Right | Down | Left
type Mode = Paint | Move
type Turn = Clockwise | Anticlockwise
    with static member FromLong = function 0L -> Anticlockwise | _ -> Clockwise

open System.Collections.Generic
type Robot(initial:Paint) =
    let mutable position = (0,0)
    let mutable orientation = Up
    let mutable mode = Paint
    let panels = [position, initial] |> dict |> Dictionary
    member this.PanelCount = panels.Count
    member this.CheckCreatePos () = if not <| panels.ContainsKey(position) then panels.Add(position, Black)
    member this.GetColour () = this.CheckCreatePos(); panels.[position] |> Paint.ToLong
    member this.SetColour colour = this.CheckCreatePos(); panels.[position] <- Paint.FromLong colour
    member this.TurnAndMove turn =
        match Turn.FromLong turn with
        | Clockwise -> orientation <- match orientation with Up -> Right | Right -> Down | Down -> Left | Left -> Up
        | Anticlockwise -> orientation <- match orientation with Up -> Left | Left -> Down | Down -> Right | Right -> Up
        let (x,y) = position
        position <- match orientation with Up -> (x,y-1) | Right -> (x+1,y) | Down -> (x,y+1) | Left -> (x-1,y)
    member this.SaveToImage path =
        let key (kvp:KeyValuePair<_,_>) = kvp.Key
        let (mxx,mxy,mnx,mny) = panels |> Seq.map key |> Seq.fold (fun (mxx,mxy,mnx,mny) (x,y) -> (max mxx x, max mxy y, min mnx x, min mny y)) (0,0,0,0)
        let (width,height) = 1 + mxx - mnx, 1 + mxy - mny
        let bmp = new System.Drawing.Bitmap(width, height)
        panels |> Seq.where (fun kvp -> kvp.Value = White) |> Seq.map key 
               |> Seq.iter (fun (x,y) -> bmp.SetPixel(x - mnx, y - mny, System.Drawing.Color.White))
        bmp.Save path

    interface InputOutput with
        member this.Input () = 
            this.GetColour ()
        member this.Output x = 
            match mode with 
            | Paint -> this.SetColour x
                       mode <- Move
            | Move -> this.TurnAndMove x
                      mode <- Paint
    
let input = System.IO.File.ReadAllText(@"C:\Temp\Day11.txt").Split([|','|]) |> Array.map int64
let part1 = Robot(Black)
run input part1
printfn "%A" (part1.PanelCount)

let part2 = Robot(White)
run input part2
part2.SaveToImage(@"C:\Temp\Day11.bmp")
