open System
open Expecto

open ski_combinator
open Parser
open TokeniserStub

// TESTBENCH

let rec buildList lstType lst =
    match lst with
    | [] ->  SeqExp (Null, Null)
    | [x] -> SeqExp (Literal (lstType x), Null)
    | head::tail -> SeqExp ( Literal (lstType head), buildList lstType tail )

let stringTests = [

    "explode",
    FuncApp (BuiltInFunc Explode, Literal (StringLit "Hello, World.")),
    "Hello, World." |> Seq.toList |> List.map string |> buildList StringLit;

    "implode",
    FuncApp (BuiltInFunc Implode, "Hello, World." |> Seq.toList |> List.map string |> buildList StringLit),
    Literal (StringLit "Hello, World.");

    "implode explode",
    FuncApp ( BuiltInFunc Implode, FuncApp (BuiltInFunc Explode, Literal (StringLit "Hello, World."))),
    Literal (StringLit "Hello, World.");
    
]

let listTests = [

    "ListSize short", 
    FuncApp( BuiltInFunc Size, (IntLit, [1..3]) ||> buildList), 
    Literal (IntLit 3);

    "ListSize long", 
    FuncApp( BuiltInFunc Size, (IntLit, [1..6000]) ||> buildList), 
    Literal (IntLit 6000);

    "ListSize 1", 
    FuncApp( BuiltInFunc Size, (IntLit, [1]) ||> buildList),
    Literal (IntLit 1);

    "ListSize null", 
    FuncApp( BuiltInFunc Size, (IntLit, []) ||> buildList), 
    Literal (IntLit 0);

    "List Size string",
    FuncApp( BuiltInFunc Size, SeqExp ( Literal (StringLit "this is a list"), Null)),
    Literal (IntLit 1);

    "List Head",
    FuncApp( BuiltInFunc Head, (IntLit, [1..3]) ||> buildList),
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

    "simple function",
    FuncDefExp {FuncName = "f"; FuncBody = Literal (IntLit 2); Rest = Identifier "f";},
    Literal (IntLit 2);

    "simple 2 let",
    FuncDefExp {FuncName = "f"; FuncBody = Literal (IntLit 2); Rest = FuncDefExp {FuncName = "g"; FuncBody = Literal (StringLit "aaa"); Rest = Identifier "z";};},
    Identifier "z";

    "3 let",
    FuncDefExp {FuncName = "f"; FuncBody = FuncDefExp {FuncName = "fun"; FuncBody = Literal (IntLit 2); Rest = Identifier "p";}; Rest = FuncDefExp {FuncName = "g"; FuncBody = Literal (StringLit "aaa"); Rest = Identifier "z";};},
    Identifier "z";


    "function with arguments",
    FuncDefExp {
        FuncName = "f";
        FuncBody = Lambda { 
            LambdaParam = "a" ;
            LambdaBody = Lambda { 
                LambdaParam = "b" ;
                LambdaBody = FuncApp( FuncApp( BuiltInFunc Minus, Identifier "a"), Identifier "b") };
            };
        Rest = FuncApp (FuncApp (Identifier "f", Literal (IntLit 12)), Literal (IntLit 2))
    },
    Literal (IntLit 10);

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

let combinatorTests = [

    "identity",
    Lambda { 
        LambdaParam = "x" ;
        LambdaBody = Identifier "x" 
    },
    Combinator I

    "switch args (combinator only)",
    Lambda { 
        LambdaParam = "x" ;
        LambdaBody = Lambda { 
            LambdaParam = "y" ;
            LambdaBody = FuncApp (Identifier "y", Identifier "x") 
        } 
    },
    // S (K (S I)) (S (K K) I)
    FuncApp (
        FuncApp (
            Combinator S, 
            FuncApp ( 
                Combinator K,
                FuncApp (
                    Combinator S,
                    Combinator I 
                ) 
            ) 
        ),
        FuncApp (
            FuncApp (
                Combinator S,
                FuncApp (
                    Combinator K,
                    Combinator K
                )
            ), 
            Combinator I 
        ) 
    );

    "switch args (with arguments)",
    FuncApp ( 
        FuncApp (
            Lambda { 
                LambdaParam = "x" ;
                LambdaBody = Lambda { 
                    LambdaParam = "y" ;
                    LambdaBody = FuncApp (Identifier "y", Identifier "x") 
                } 
            }, 
            Literal (StringLit "World")
        ),
        Literal (StringLit "Hello ")
    ),
    // S (K (S I)) (S (K K) I)
    FuncApp ( Literal (StringLit "Hello "),  Literal (StringLit "World") );


]

let recursionTests = [

    "factorial 5",
    FuncDefExp {
        FuncName = "fact"; 
        FuncBody = Lambda { 
            LambdaParam = "n";
            LambdaBody = IfExp (FuncApp ( FuncApp (BuiltInFunc LessEq, Identifier "n"), Literal (IntLit 1) ),  Literal (IntLit 1), FuncApp ( FuncApp (BuiltInFunc Mult, Identifier "n"),  FuncApp ( Identifier "fact" , FuncApp ( FuncApp (BuiltInFunc Minus, Identifier "n"), Literal (IntLit 1) ) ) ) )
            };
        Rest = FuncApp (Identifier "fact", Literal (IntLit 5))
    },
    Literal (IntLit 120)


]

let basicPassThroughTests = [
 
    "int",
    Literal (IntLit 1),
    Literal (IntLit 1);

    "string",
    Literal (StringLit "Hello"),
    Literal (StringLit "Hello");

    "bool",
    Literal (BoolLit true),
    Literal (BoolLit true);

    "pair",
    SeqExp (Literal (IntLit 1), Literal (IntLit 1)),
    SeqExp (Literal (IntLit 1), Literal (IntLit 1));

    "list",
    SeqExp (Literal (IntLit 1), SeqExp (Literal (IntLit 1), Null)),
    SeqExp (Literal (IntLit 1), SeqExp (Literal (IntLit 1), Null));

]

let generalTests = [



    "combination of several tests",
    FuncApp( FuncApp( BuiltInFunc Equal, FuncApp( BuiltInFunc Head, SeqExp ( Literal (IntLit 2),  SeqExp ( Literal (IntLit 3), Null )))), FuncApp( FuncApp( BuiltInFunc Mult, Literal (IntLit 1)), Literal (IntLit 2))),
    Literal (BoolLit true);

    "arithmetic bool funcapp tests 2+3*4-5<6",
    FuncApp (FuncApp (BuiltInFunc Less, FuncApp (FuncApp (BuiltInFunc Plus,Literal (IntLit 2)), FuncApp (FuncApp (BuiltInFunc Minus, FuncApp (FuncApp (BuiltInFunc Mult,Literal (IntLit 3)), Literal (IntLit 4))),Literal (IntLit 5)))), Literal (IntLit 6)),
    Literal (BoolLit false);

    "func app w/ missing arg",
    FuncDefExp {FuncName = "f"; FuncBody =  Lambda { LambdaParam = "x" ; LambdaBody = FuncApp( FuncApp( BuiltInFunc Plus, Identifier "x"), Literal (IntLit 2)) }; Rest = FuncApp( Identifier "f", Literal (IntLit 5));},
    Literal (IntLit 7);
    
]


let testCases = [
    listTests;
    arithmeticTests;
    booleanTests;
    lambdaTests;
    funcDefTests;
    generalTests;
    ifThenElseTests;
    combinatorTests;
    recursionTests;
    basicPassThroughTests;
    stringTests;
]

/// Run an Expecto test given a 3-tuple containing the test name, the input to the runtime and the expected output
let testEval (testName, input, expectedOutput) =
    testCase testName <| fun () ->
        Expect.equal (input |> combinatorRuntime) expectedOutput ""
 

[<Tests>]
/// Evaluates all test cases in the list of lists: testCases
let tests = 
    testList "Evaluator test" <| List.map testEval (List.fold (fun x y -> x @ y) [] testCases)


/// Start Expecto testing
let testAll() = 
    runTestsInAssembly defaultConfig [||] |> ignore


/// Run a single input using the combinator runtime
let singleEval = 
    combinatorRuntime >> print


[<EntryPoint>]
let main argv =

    let TC1 = 
        FuncDefExp {
            FuncName = "f";
            FuncBody =  Lambda { 
                LambdaParam = "x" ;
                LambdaBody = FuncApp( FuncApp( BuiltInFunc Plus, Identifier "x"), Literal (IntLit 2)) 
            }; 
            Rest = FuncApp( Identifier "f", Literal (IntLit 5));
        }

    let TC2 =
        FuncApp (
            Lambda { 
                LambdaParam = "x";
                LambdaBody = 
                    FuncDefExp {
                        FuncName = "y";
                        FuncBody = Literal (IntLit 1);
                        Rest = 
                            FuncApp (
                                FuncApp (
                                    BuiltInFunc Mult,
                                    Identifier "x"
                                ),
                                Identifier "y"
                            );
                    }
            },
            Identifier "z"
        )

    let TC3 =
        FuncDefExp {
            FuncName = "x";
            FuncBody = buildLambda "y" (FuncApp (FuncApp (BuiltInFunc Plus, Identifier "x"), Identifier "y")); 
            Rest = FuncDefExp {
                FuncName = "z";
                FuncBody = buildLambda "a" ( buildLambda "b" (FuncApp (FuncApp (BuiltInFunc And, FuncApp (FuncApp (BuiltInFunc Less, Identifier "a"), Identifier "b")), Identifier "z")) );
                Rest = FuncApp(Identifier "x", FuncApp(FuncApp (Identifier "z", Literal (IntLit 1)), Literal (IntLit 2)))}}


    //// RUN ALL EXPECTO TESTS ////
    //testAll()

    //// USE THE COMBINATOR RUNTIME TO EVALUATE A PROGRAM ////
    singleEval TC1

    0