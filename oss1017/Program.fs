open System
open Expecto

open ski_combinator
open Parser
open TokeniserStub



// TESTBENCH

let testCases = [

    "ListSize", 
    FuncApp( BuiltInFunc Size, SeqExp ( Literal (IntLit 1), SeqExp ( Literal (IntLit 2),  SeqExp ( Literal (IntLit 3), Null )))), 
    Literal (IntLit 3);

    "List Size 2",
    FuncApp( BuiltInFunc Size, SeqExp ( Literal (IntLit 1), Null)),
    Literal (IntLit 1);

    "List Head",
    FuncApp( BuiltInFunc Head, SeqExp ( Literal (IntLit 1), Null)),
    Literal (IntLit 1);

    "List tail",
    FuncApp( BuiltInFunc Tail, SeqExp ( Literal (IntLit 1), SeqExp ( Literal (IntLit 2),  SeqExp ( Literal (IntLit 3), Null )))),
    SeqExp ( Literal (IntLit 2),  SeqExp ( Literal (IntLit 3), Null ));

]

let testEval (testName, input, expectedOutput) =
    testCase testName <| fun () ->
        Expect.equal (eval input) expectedOutput ""

[<Tests>]
let tests = testList "Evaluator test" <| List.map testEval testCases

[<EntryPoint>]
let main argv =
    
    runTestsInAssembly defaultConfig [||] |> ignore

    0