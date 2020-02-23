open System
open Expecto

open ski_combinator
open Parser
open TokeniserStub

let tmp = 
    FuncDefExp {
        FuncName = "f"; 
        FuncBody =  Lambda { 
            LambdaParam = "x" ; 
            LambdaBody = FuncApp( FuncApp( BuiltInFunc Plus, Identifier "x"), Literal (IntLit 2)) }; 
        Rest = FuncApp( Identifier "f", Literal (IntLit 5))}


// TESTBENCH

let rec buildList lst =
    match lst with
    | [] -> Null
    | [x] -> SeqExp (Literal (IntLit x), Null)
    | head::tail -> SeqExp ( Literal (IntLit head), buildList tail )



let listTests = [

    "ListSize short", 
    FuncApp( BuiltInFunc Size, [1..3] |> buildList), 
    Literal (IntLit 3);

    "ListSize long", 
    FuncApp( BuiltInFunc Size, [1..1000] |> buildList), 
    Literal (IntLit 1000);

    "ListSize 1", 
    FuncApp( BuiltInFunc Size, [1] |> buildList),
    Literal (IntLit 1);

    "ListSize null", 
    FuncApp( BuiltInFunc Size, [] |> buildList), 
    Literal (IntLit 0);

    "List Size string",
    FuncApp( BuiltInFunc Size, SeqExp ( Literal (StringLit "this is a list"), Null)),
    Literal (IntLit 1);

    "List Head",
    FuncApp( BuiltInFunc Head, [1..3] |> buildList),
    Literal (IntLit 1);

    "List tail",
    FuncApp( BuiltInFunc Tail, SeqExp ( Literal (IntLit 1), SeqExp ( Literal (IntLit 2),  SeqExp ( Literal (IntLit 3), Null )))),
    SeqExp ( Literal (IntLit 2),  SeqExp ( Literal (IntLit 3), Null ));

]

let arithmeticTests = [

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

]

let booleanTests = [
    
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

]

let lambdaTests = [

    "lambda testing",
    FuncApp (Lambda { 
        LambdaParam = "x";
        LambdaBody = FuncApp ( FuncApp( BuiltInFunc Plus , Identifier "x" ), Literal (IntLit 1)) } , Literal (IntLit 5)),
    Literal (IntLit 6);

    "lambda identity",
    Lambda { 
        LambdaParam = "x" ;
        LambdaBody = Identifier "x" },
    Combinator I;

    "lambda passthrough",
    FuncApp (Lambda {
        LambdaParam = "x";
        LambdaBody = Identifier "x" } , Literal (IntLit 5)),
    Literal (IntLit 5);

    "lambda x+y",
    FuncApp (FuncApp (Lambda { 
        LambdaParam = "x"; 
        LambdaBody = Lambda { 
            LambdaParam = "y"; 
            LambdaBody = FuncApp ( FuncApp( BuiltInFunc Plus , Identifier "x" ),  Identifier "y")  }},Literal (IntLit 12)),Literal (IntLit 10)),
    Literal (IntLit 22);

]

let funcDefTests = [

    "func def w/ lambdas",
    FuncDefExp {
        FuncName = "x";
        FuncBody = buildLambda "y" (buildLambda "z" (Literal (IntLit 2)));
        Rest = FuncApp( FuncApp( Identifier "x", Literal (IntLit 5)), Literal (BoolLit true)) ;},
    Literal (IntLit 2);

    "unused func def and ret ID",
    FuncDefExp {
        FuncName = "f";
        FuncBody = FuncDefExp {
            FuncName = "fun"; 
            FuncBody = Literal (IntLit 2);
            Rest = Identifier "p";}; 
        Rest = FuncDefExp {
            FuncName = "g";
            FuncBody = Literal (StringLit "aaa"); 
            Rest = Identifier "z";};},
    Identifier "z";

    "double identity lambda and ret int",
    FuncDefExp {
        FuncName = "f";
        FuncBody =  Lambda { 
            LambdaParam = "x" ; 
            LambdaBody = Identifier "x" }; 
        Rest = FuncApp( FuncApp (Identifier "f", Identifier "f"), Literal (IntLit 5));},
    Literal (IntLit 5);

    "func def w/ lambda and builtin",
    FuncDefExp {
        FuncName = "f";
        FuncBody =  Lambda { 
            LambdaParam = "x" ;
            LambdaBody = FuncApp( FuncApp( BuiltInFunc Plus, Identifier "x"), Literal (IntLit 2)) };
        Rest = FuncApp( Identifier "f", Literal (IntLit 5))},
    Literal (IntLit 7);

    "func test",
    FuncDefExp {
        FuncName = "main"; 
        FuncBody = FuncDefExp {
            FuncName = "f"; 
            FuncBody = FuncApp (FuncApp (BuiltInFunc Less, FuncApp (FuncApp (BuiltInFunc Plus,Literal (IntLit 2)), FuncApp (FuncApp (BuiltInFunc Minus, FuncApp (FuncApp (BuiltInFunc Mult,Literal (IntLit 3)), Literal (IntLit 4))),Literal (IntLit 5)))), Literal (IntLit 6)); 
            Rest = FuncDefExp {
                FuncName = "g"; 
                FuncBody =  Literal (BoolLit true);
                Rest = FuncDefExp {
                    FuncName = "h"; 
                    FuncBody = FuncApp (BuiltInFunc Not, Identifier "f"); 
                    Rest =  FuncApp (FuncApp (BuiltInFunc And, Identifier "g"), Identifier "h")} } }; 
        Rest = Identifier "main"},
    Literal ( BoolLit true);
]

let ifThenElseTests = [
    "simple ifThenElse true",
    IfExp ( Literal (BoolLit true),Literal (StringLit "condition evaluated to true") ,Literal (StringLit "condition evaluated to false")),
    Literal (StringLit "condition evaluated to true");

    "simple ifThenElse false",
    IfExp ( Literal (BoolLit false),Literal (StringLit "condition evaluated to true") ,Literal (StringLit "condition evaluated to false")),
    Literal (StringLit "condition evaluated to false");

    "needs eval ifThenElse false",
    IfExp (
        FuncApp (FuncApp (BuiltInFunc Less, FuncApp (FuncApp (BuiltInFunc Plus,Literal (IntLit 2)), FuncApp (FuncApp (BuiltInFunc Minus, FuncApp (FuncApp (BuiltInFunc Mult,Literal (IntLit 3)), Literal (IntLit 4))),Literal (IntLit 5)))), Literal (IntLit 6)),
        Literal (StringLit "condition evaluated to true"),
        Literal (StringLit "condition evaluated to false")
    ),
    Literal (StringLit "condition evaluated to false");

    "needs eval ifThenElse true",
    IfExp (
        FuncApp (FuncApp (BuiltInFunc Greater, FuncApp (FuncApp (BuiltInFunc Plus,Literal (IntLit 2)), FuncApp (FuncApp (BuiltInFunc Minus, FuncApp (FuncApp (BuiltInFunc Mult,Literal (IntLit 3)), Literal (IntLit 4))),Literal (IntLit 5)))), Literal (IntLit 6)),
        Literal (StringLit "condition evaluated to true"),
        Literal (StringLit "condition evaluated to false")
    ),
    Literal (StringLit "condition evaluated to true");
]

let generalTests = [

    "combination of several tests",
    FuncApp( FuncApp( BuiltInFunc Equal, FuncApp( BuiltInFunc Head, SeqExp ( Literal (IntLit 2),  SeqExp ( Literal (IntLit 3), Null )))), FuncApp( FuncApp( BuiltInFunc Mult, Literal (IntLit 1)), Literal (IntLit 2))),
    Literal (BoolLit true);

    "arithmetic bool funcapp tests 2+3*4-5<6",
    FuncApp (FuncApp (BuiltInFunc Less, FuncApp (FuncApp (BuiltInFunc Plus,Literal (IntLit 2)), FuncApp (FuncApp (BuiltInFunc Minus, FuncApp (FuncApp (BuiltInFunc Mult,Literal (IntLit 3)), Literal (IntLit 4))),Literal (IntLit 5)))), Literal (IntLit 6)),
    Literal (BoolLit false);

    //"func app w/ missing arg",
    //FuncDefExp {FuncName = "f"; FuncBody =  Lambda { LambdaParam = "x" ; LambdaBody = FuncApp( FuncApp( BuiltInFunc Plus, Identifier "x"), Literal (IntLit 2)) }; Rest = FuncApp( Identifier "f", Literal (IntLit 5));},
    
]


let testCases = [
    listTests;
    arithmeticTests;
    booleanTests;
    lambdaTests;
    funcDefTests;
    generalTests;
    ifThenElseTests;
]


let testEval (testName, input, expectedOutput) =
    testCase testName <| fun () ->
        Expect.equal (input |> combinatorRuntime) expectedOutput ""

let runTestLists  lst = 
    testList "Evaluator test" <| List.map testEval (List.fold (fun x y -> x @ y) [] lst)

[<Tests>]
let tests = runTestLists testCases

[<EntryPoint>]
let main argv =
    

    runTestsInAssembly defaultConfig [||] |> ignore
    //FuncDefExp {FuncName = "f"; FuncBody =  Lambda { LambdaParam = "x" ; LambdaBody = FuncApp( FuncApp( BuiltInFunc Plus, Identifier "x"), Literal (IntLit 2)) }; Rest = FuncApp( Identifier "f", Literal (IntLit 5));}
    //FuncApp (FuncApp(Lambda { LambdaParam = "x"; LambdaBody = Lambda { LambdaParam = "y"; LambdaBody = FuncApp ( FuncApp( BuiltInFunc Plus , Identifier "x" ),  Identifier "y")  }},Literal (IntLit 12)),Literal (IntLit 10))
    //|> combinatorRuntime
    //|> print
    0