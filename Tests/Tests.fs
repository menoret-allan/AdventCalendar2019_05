module Tests

open FsUnit
open Xunit
open Core.Computer

[<Theory>]
[<InlineData("1,0,0,0,99", "Instructions: 2,0,0,0,99")>]
[<InlineData("2,3,0,3,99", "Instructions: 2,3,0,6,99")>]
[<InlineData("2,4,4,5,99,0", "Instructions: 2,4,4,5,99,9801")>]
[<InlineData("1,1,1,4,99,5,6,0,99", "Instructions: 30,1,1,4,2,5,6,0,99")>]
[<InlineData("1002,4,3,4,33", "Instructions: 1002,4,3,4,99")>]
[<InlineData("1101,100,-1,4,0", "Instructions: 1101,100,-1,4,99")>]
[<InlineData("1004,2,99", "Instructions: 1004,2,99\nOutputs: 99")>]
let ``str calculator`` (instructions:string, expectation:string) =
    instructions |> compute |> should startWith expectation

[<Fact>]
let ``read output simple instruction`` () = 
    findInstruction [|4;4;2;3;69|] 0 |> should equal (Some (Output(69)))

[<Fact>]
let ``read output with more digit simple instruction`` () = 
    findInstruction [|1004;4;2;3;42|] 0 |> should equal (Some (Output(42)))

[<Fact>]
let ``read output with more digit simple instruction with 1 mode`` () = 
    findInstruction [|1104;42|] 0 |> should equal (Some (Output(42)))

[<Fact>]
let ``Check the first input instruction`` () =
    findInstruction [|3;225|] 0 |> should equal (Some (Input(1,225)))

[<Fact>]
let ``Check the second input instruction`` () =
    findInstruction [|3;7;1;7;6;6;1100;1|] 2 |> should equal (Some (Add(1, 1100, 6)))

[<Fact>]
let ``Check the third input instruction`` () =
    findInstruction [|3;12;1;12;6;6;1101;1;238;12;104;0;1|] 6 |> should equal (Some (Add(1, 238, 12)))

[<Fact>]
let ``Check the fourth input instruction`` () =
    findInstruction [|3;11;1;11;6;6;1101;1;238;225;104;0|] 10 |> should equal (Some (Output(0)))

[<Theory>]
[<InlineData("", 1)>]
[<InlineData("", 2)>]
[<InlineData("", 3)>]
[<InlineData("0", 1)>]
[<InlineData("10", 2)>]
[<InlineData("110", 3)>]
let ``Get mode X return Position`` (list: string) (pos: int) = 
    let input = list |> Seq.map (string) |> Seq.map (int) |> Seq.toList 
    let result = getModeX input pos
    result |> should equal Position

[<Theory>]
[<InlineData("1", 1)>]
[<InlineData("01", 2)>]
[<InlineData("001", 3)>]
let ``Get mode X return Immediate`` (list: string) (pos: int) = 
    let input = list |> Seq.map (string) |> Seq.map (int) |> Seq.toList 
    let result = getModeX input pos
    result |> should equal Immediate






