namespace Core

module Computer =
    type Mode =
        | Position
        | Immediate
    
    type Instruction =
        | Add of (int * int * int)
        | Mult  of (int * int * int)
        | Input of (int * int)
        | Output of int
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

    let findInstruction instructions pos =
        let getVal = getValue instructions
        let getPosition = getMode2 getVal pos
        let intList = instructions.[pos] |> string |> Seq.rev |> Seq.map string |> Seq.map int |> Seq.toList
        match intList with
        | 9::9::_ -> Some(End)
        | 1::0::rest | 1::rest -> Some(Add(getPosition rest 1 |> getVal, getPosition rest 2 |> getVal, getVal (pos + 3)))
        | 2::0::rest | 2::rest -> Some(Mult(getPosition rest 1 |> getVal, getPosition rest 2 |> getVal, getVal (pos + 3)))
        | 3::0::rest | 3::rest -> Some(Input(1, getPosition rest 1))
        | 4::0::rest | 4::rest -> Some(Output(getPosition rest 1 |> getVal))
        | _ -> None

    let update instructions pos value =
        Array.set instructions pos value
        instructions

    let interprete (instructions: int []) =
        let rec loop (instructions: int []) (output: seq<int>) (pos: int) =
            let insertInInstruction = setValue instructions
            match findInstruction instructions pos with
            | Some End -> Some (instructions, output)
            | Some(Output(x)) -> loop instructions (Seq.append output (Seq.singleton x)) (pos+2)
            | Some(Input(value, position)) -> loop (insertInInstruction value position) output (pos+2)
            | Some(Add(left, right, position)) -> loop (insertInInstruction (left + right) position) output (pos+4)
            | Some(Mult(left, right, position)) -> loop (insertInInstruction (left * right) position) output (pos+4)
            | None -> None
        loop instructions Seq.empty 0
    
    let compute (input: string) =
        input |> translate |> interprete |> translateBack































    //let applyNewValueForInstruction instructions (noun, verb) =
    //    let result = Array.copy instructions
    //    Array.set result 1 noun
    //    Array.set result 2 verb
    //    result

    //let findSpecificResult genInstruction (pair: (int * int)) =
    //       match interprete (genInstruction pair) |> Array.item 0 with
    //       | 19690720 -> Some pair
    //       | _ -> None

    //let findSet (input: string) =
    //    let genInstruction = input |> translate |> applyNewValueForInstruction |> findSpecificResult
    //    let pairs = seq {for noun in 0..100 do yield! seq {for verb in 0..100 do (noun , verb) } }
    //    pairs |> Seq.choose(genInstruction) |> Seq.exactlyOne 
