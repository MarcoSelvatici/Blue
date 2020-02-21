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

    "Add",
    FuncApp( FuncApp( BuiltInFunc Plus, Literal (IntLit 4)), Literal (IntLit 2)),
    Literal (IntLit 6);
    
    "Sub",
    FuncApp( FuncApp( BuiltInFunc Minus, Literal (IntLit 4)), Literal (IntLit 2)),
    Literal (IntLit 2);

    "Mult",
    FuncApp( FuncApp( BuiltInFunc Mult, Literal (IntLit 4)), Literal (IntLit 2)),
    Literal (IntLit 8);

    "Div",
    FuncApp( FuncApp( BuiltInFunc Div, Literal (IntLit 4)), Literal (IntLit 2)),
    Literal (IntLit 2);

    "greater (true)",
    FuncApp( FuncApp( BuiltInFunc Greater, Literal (IntLit 4)), Literal (IntLit 2)),
    Literal (BoolLit true);

    "greater (false)",
    FuncApp( FuncApp( BuiltInFunc Greater, Literal (IntLit 1)), Literal (IntLit 2)),
    Literal (BoolLit false);

    "greater equal (true)",
    FuncApp( FuncApp( BuiltInFunc GreaterEq, Literal (IntLit 4)), Literal (IntLit 4)),
    Literal (BoolLit true);

    "greater equal (false)",
    FuncApp( FuncApp( BuiltInFunc GreaterEq, Literal (IntLit 4)), Literal (IntLit 5)),
    Literal (BoolLit false);

    "less (false)",
    FuncApp( FuncApp( BuiltInFunc Less, Literal (IntLit 4)), Literal (IntLit 2)),
    Literal (BoolLit false);

    "less (true)",
    FuncApp( FuncApp( BuiltInFunc Less, Literal (IntLit 1)), Literal (IntLit 2)),
    Literal (BoolLit true);

    "less equal (flase)",
    FuncApp( FuncApp( BuiltInFunc LessEq, Literal (IntLit 4)), Literal (IntLit 2)),
    Literal (BoolLit false);

    "less equal (true)",
    FuncApp( FuncApp( BuiltInFunc LessEq, Literal (IntLit 4)), Literal (IntLit 4)),
    Literal (BoolLit true);

    "equal (false)",
    FuncApp( FuncApp( BuiltInFunc Equal, Literal (IntLit 4)), Literal (IntLit 2)),
    Literal (BoolLit false);

    "equal (true)",
    FuncApp( FuncApp( BuiltInFunc Equal, Literal (IntLit 2)), Literal (IntLit 2)),
    Literal (BoolLit true);

    "combination of several tests",
    FuncApp( FuncApp( BuiltInFunc Equal, FuncApp( BuiltInFunc Head, SeqExp ( Literal (IntLit 2),  SeqExp ( Literal (IntLit 3), Null )))), FuncApp( FuncApp( BuiltInFunc Mult, Literal (IntLit 1)), Literal (IntLit 2))),
    Literal (BoolLit true);

    "lambda testing",
    FuncApp (Lambda { LambdaParam = "x"; LambdaBody = FuncApp ( FuncApp( BuiltInFunc Plus , Identifier "x" ), Literal (IntLit 1)) } , Literal (IntLit 5)),
    Literal (IntLit 6);

    "lambda identity",
    Lambda { LambdaParam = "x" ; LambdaBody = Identifier "x" },
    Combinator I;

    "lambda passthrough",
    FuncApp (Lambda { LambdaParam = "x"; LambdaBody = Identifier "x" } , Literal (IntLit 5)),
    Literal (IntLit 5);

]

let testEval (testName, input, expectedOutput) =
    testCase testName <| fun () ->
        Expect.equal (input |> eval |> interpret) expectedOutput ""

[<Tests>]
let tests = testList "Evaluator test" <| List.map testEval testCases

[<EntryPoint>]
let main argv =
    
    runTestsInAssembly defaultConfig [||] |> ignore

    0