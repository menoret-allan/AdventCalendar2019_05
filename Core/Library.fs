﻿namespace Core

module Computer =
    type Mode =
        | Position
        | Immediate
    
    type Instruction =
        | Add of (int * int * int)
        | Mult  of (int * int * int)
        | Input of (int * int)
        | Output of int
        | Jump of int
        | NoAction of int
        | LessThan of (int * int)
        | Equal of (int * int)
        | End

    let translate (instructions: string) =
        instructions.Split [|','|]
        |> Seq.map(int)
        |> Seq.toArray
    
    let translateBack result =
        match result with
        | None -> "Unknown instruction found"
        | Some(instructions, outputs) -> "Instructions: " + (instructions |> Seq.map(string) |> String.concat ",") + "\nOutputs: " + (outputs |> Seq.map(string) |> String.concat ",")

    let getValue (instructions: int []) pos =
        instructions.[pos]

    let setValue (instructions: int []) value pos =
        instructions.[pos] <- value
        instructions

    let getModeX list place =
        if list |> List.length < place then Position
        else if list.[place - 1] = 0 then Position
        else Immediate

    let getMode2 getVal pos list place =
        match getModeX list place with
            | Position -> getVal (pos + place)
            | Immediate -> pos + place

    let jumpIf cond value position =
        match (value <> 0) = cond with
        | true -> Jump(position)
        | false -> NoAction(3)

    let lessThan left right position =
        if left < right then LessThan(1, position)
        else LessThan(0, position)

    let equalOp left right position =
        if left = right then Equal(1, position)
        else Equal(0, position)

    let findInstruction inputValue instructions pos =
        let getVal = getValue instructions
        let getPosition = getMode2 getVal pos
        let intList = instructions.[pos] |> string |> Seq.rev |> Seq.map string |> Seq.map int |> Seq.toList
        match intList with
        | 9::9::_ -> Some(End)
        | 1::0::rest | 1::rest -> Some(Add(getPosition rest 1 |> getVal, getPosition rest 2 |> getVal, getVal (pos + 3)))
        | 2::0::rest | 2::rest -> Some(Mult(getPosition rest 1 |> getVal, getPosition rest 2 |> getVal, getVal (pos + 3)))
        | 3::0::rest | 3::rest -> Some(Input(inputValue, getPosition rest 1))
        | 4::0::rest | 4::rest -> Some(Output(getPosition rest 1 |> getVal))
        | 5::0::rest | 5::rest -> Some(jumpIf true (getPosition rest 1 |> getVal) (getPosition rest 2 |> getVal))
        | 6::0::rest | 6::rest -> Some(jumpIf false (getPosition rest 1 |> getVal) (getPosition rest 2 |> getVal))
        | 7::0::rest | 7::rest -> Some(lessThan (getPosition rest 1 |> getVal) (getPosition rest 2 |> getVal) (getVal (pos + 3)))
        | 8::0::rest | 8::rest -> Some(equalOp (getPosition rest 1 |> getVal) (getPosition rest 2 |> getVal) (getVal (pos + 3)))
        | _ -> None

    let update instructions pos value =
        Array.set instructions pos value
        instructions

    let interprete inputValue (instructions: int []) =
        let rec loop (instructions: int []) (output: seq<int>) (pos: int) =
            let insertInInstruction = setValue instructions
            match findInstruction inputValue instructions pos with
            | Some End -> Some (instructions, output)
            | Some(Output(x)) -> loop instructions (Seq.append output (Seq.singleton x)) (pos+2)
            | Some(Input(value, position)) -> loop (insertInInstruction value position) output (pos+2)
            | Some(Add(left, right, position)) -> loop (insertInInstruction (left + right) position) output (pos+4)
            | Some(Mult(left, right, position)) -> loop (insertInInstruction (left * right) position) output (pos+4)
            | Some(Jump(position)) -> loop instructions output position
            | Some(NoAction(increment)) -> loop instructions output (pos+increment)
            | Some(LessThan(value, position)) -> loop (insertInInstruction value position) output (pos+4)
            | Some(Equal(value, position)) -> loop (insertInInstruction value position) output (pos+4)
            | None -> None
        loop instructions Seq.empty 0
    
    let compute inputValue (instructions: string) =
        instructions |> translate |> interprete inputValue |> translateBack
