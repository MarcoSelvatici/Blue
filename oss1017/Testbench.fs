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
    Ok (FuncApp (BuiltInFunc Explode, Literal (StringLit "Hello, World."))),
    "Hello, World." |> Seq.toList |> List.map string |> buildList StringLit |> Ok;

    "Explode error",
    Ok (FuncApp (BuiltInFunc Explode, Null)),
    Error "SKI runtime error: Built-in function evaluation Error: \ncannot explode argument of type which is not string";

    "implode",
    Ok (FuncApp (BuiltInFunc Implode, "Hello, World." |> Seq.toList |> List.map string |> buildList StringLit)),
    Ok (Literal (StringLit "Hello, World."));

    "implode Error",
    Ok (FuncApp (BuiltInFunc Implode, Null)),
    Error "SKI runtime error: Built-in function evaluation Error: \nCannot implode argument of type which is not string list";

    "implode explode",
    Ok (FuncApp ( BuiltInFunc Implode, FuncApp (BuiltInFunc Explode, Literal (StringLit "Hello, World.")))),
    Ok (Literal (StringLit "Hello, World."));
    
]

let listTests = [

    "ListSize short", 
    Ok (FuncApp( BuiltInFunc Size, (IntLit, [1..3]) ||> buildList)), 
    Ok (Literal (IntLit 3));

    "ListSize long", 
    Ok (FuncApp( BuiltInFunc Size, (IntLit, [1..6000]) ||> buildList)), 
    Ok (Literal (IntLit 6000));

    "ListSize 1", 
    Ok (FuncApp( BuiltInFunc Size, (IntLit, [1]) ||> buildList)),
    Ok (Literal (IntLit 1));

    "ListSize null", 
    Ok (FuncApp( BuiltInFunc Size, (IntLit, []) ||> buildList)), 
    Ok (Literal (IntLit 0));

    "ListSize Error", 
    Ok (FuncApp( BuiltInFunc Size, Literal (StringLit "I'm a string!"))), 
    Error "SKI runtime error: Built-in function evaluation Error: \nError getting size of list: Invalid input: Literal (StringLit \"I'm a string!\")";

    "List Size string",
    Ok (FuncApp( BuiltInFunc Size, SeqExp ( Literal (StringLit "this is a list"), Null))),
    Ok (Literal (IntLit 1));

    "List Head",
    Ok (FuncApp( BuiltInFunc Head, (IntLit, [1..3]) ||> buildList)),
    Ok (Literal (IntLit 1));

    "List Head error",
    Ok (FuncApp( BuiltInFunc Head, Literal (IntLit 10))),
    Error "SKI runtime error: Built-in function evaluation Error: \nError getting head of list/sequence";

    "List tail",
    Ok (FuncApp( BuiltInFunc Tail, SeqExp ( Literal (IntLit 1), SeqExp ( Literal (IntLit 2),  SeqExp ( Literal (IntLit 3), Null ))))),
    Ok (SeqExp ( Literal (IntLit 2),  SeqExp ( Literal (IntLit 3), Null )));

    "List Tail error",
    Ok (FuncApp( BuiltInFunc Tail, Literal (IntLit 10))),
    Error "SKI runtime error: Built-in function evaluation Error: \nError getting tail of list/sequence";

]

let arithmeticTests = [

    "Add",
    Ok (FuncApp( FuncApp( BuiltInFunc Plus, Literal (IntLit 4)), Literal (IntLit 2))),
    Ok (Literal (IntLit 6));
    
    "Sub",
    Ok (FuncApp( FuncApp( BuiltInFunc Minus, Literal (IntLit 4)), Literal (IntLit 2))),
    Ok (Literal (IntLit 2));

    "Mult",
    Ok (FuncApp( FuncApp( BuiltInFunc Mult, Literal (IntLit 4)), Literal (IntLit 2))),
    Ok (Literal (IntLit 8));

    "Div",
    Ok (FuncApp( FuncApp( BuiltInFunc Div, Literal (IntLit 4)), Literal (IntLit 2))),
    Ok (Literal (IntLit 2));

    "Div 0 Error",
    Ok (FuncApp( FuncApp( BuiltInFunc Div, Literal (IntLit 4)), Literal (IntLit 0))),
    Error "SKI runtime error: Built-in function evaluation Error: \nDivision by 0";

]

let booleanTests = [
    
    "greater (true)",
    Ok (FuncApp( FuncApp( BuiltInFunc Greater, Literal (IntLit 4)), Literal (IntLit 2))),
    Ok (Literal (BoolLit true));

    "greater (false)",
    Ok (FuncApp( FuncApp( BuiltInFunc Greater, Literal (IntLit 1)), Literal (IntLit 2))),
    Ok (Literal (BoolLit false));

    "greater equal (true)",
    Ok (FuncApp( FuncApp( BuiltInFunc GreaterEq, Literal (IntLit 4)), Literal (IntLit 4))),
    Ok (Literal (BoolLit true));

    "greater equal (false)",
    Ok (FuncApp( FuncApp( BuiltInFunc GreaterEq, Literal (IntLit 4)), Literal (IntLit 5))),
    Ok (Literal (BoolLit false));

    "less (false)",
    Ok (FuncApp( FuncApp( BuiltInFunc Less, Literal (IntLit 4)), Literal (IntLit 2))),
    Ok (Literal (BoolLit false));

    "less (true)",
    Ok (FuncApp( FuncApp( BuiltInFunc Less, Literal (IntLit 1)), Literal (IntLit 2))),
    Ok (Literal (BoolLit true));

    "less equal (flase)",
    Ok (FuncApp( FuncApp( BuiltInFunc LessEq, Literal (IntLit 4)), Literal (IntLit 2))),
    Ok (Literal (BoolLit false));

    "less equal (true)",
    Ok (FuncApp( FuncApp( BuiltInFunc LessEq, Literal (IntLit 4)), Literal (IntLit 4))),
    Ok (Literal (BoolLit true));

    "equal (false)",
    Ok (FuncApp( FuncApp( BuiltInFunc Equal, Literal (IntLit 4)), Literal (IntLit 2))),
    Ok (Literal (BoolLit false));

    "equal (true)",
    Ok (FuncApp( FuncApp( BuiltInFunc Equal, Literal (IntLit 2)), Literal (IntLit 2))),
    Ok (Literal (BoolLit true));

    "not (true)",
    Ok (FuncApp( BuiltInFunc Not, Literal (BoolLit true))),
    Ok (Literal (BoolLit false));

    "not (false)",
    Ok (FuncApp( BuiltInFunc Not, Literal (BoolLit false))),
    Ok (Literal (BoolLit true));

    "not (error)",
    Ok (FuncApp( BuiltInFunc Not, Literal (IntLit 1))),
    Error "SKI runtime error: Built-in function evaluation Error: \nError evaluating built-in function with 1 argument: opertor \'Not\'" ;

]

let lambdaTests = [

    "lambda testing",
    Ok (FuncApp (Lambda { 
        LambdaParam = "x";
        LambdaBody = FuncApp ( FuncApp( BuiltInFunc Plus , Identifier "x" ), Literal (IntLit 1)) } , Literal (IntLit 5))),
    Ok (Literal (IntLit 6));

    "lambda identity",
    Ok (Lambda { 
        LambdaParam = "x" ;
        LambdaBody = Identifier "x" }),
    Ok (Combinator I);

    "lambda passthrough",
    Ok (FuncApp (Lambda {
        LambdaParam = "x";
        LambdaBody = Identifier "x" } , Literal (IntLit 5))),
    Ok (Literal (IntLit 5));

    "lambda x+y",
    Ok (FuncApp (FuncApp (Lambda { 
        LambdaParam = "x"; 
        LambdaBody = Lambda { 
            LambdaParam = "y"; 
            LambdaBody = FuncApp ( FuncApp( BuiltInFunc Plus , Identifier "x" ),  Identifier "y")  }},Literal (IntLit 12)),Literal (IntLit 10))),
    Ok (Literal (IntLit 22));

]

let funcDefTests = [

    "func def w/ lambdas",
    Ok (FuncDefExp {
        FuncName = "x";
        FuncBody = buildLambda "y" (buildLambda "z" (Literal (IntLit 2)));
        Rest = FuncApp( FuncApp( Identifier "x", Literal (IntLit 5)), Literal (BoolLit true)) ;}),
    Ok (Literal (IntLit 2));

    "unused func def and ret ID",
    Ok (FuncDefExp {
        FuncName = "f";
        FuncBody = FuncDefExp {
            FuncName = "fun"; 
            FuncBody = Literal (IntLit 2);
            Rest = Identifier "p";}; 
        Rest = FuncDefExp {
            FuncName = "g";
            FuncBody = Literal (StringLit "aaa"); 
            Rest = Identifier "z";};}),
    Ok (Identifier "z");

    "double identity lambda and ret int",
    Ok (FuncDefExp {
        FuncName = "f";
        FuncBody =  Lambda { 
            LambdaParam = "x" ; 
            LambdaBody = Identifier "x" }; 
        Rest = FuncApp( FuncApp (Identifier "f", Identifier "f"), Literal (IntLit 5));}),
    Ok (Literal (IntLit 5));

    "func def w/ lambda and builtin",
    Ok (FuncDefExp {
        FuncName = "f";
        FuncBody =  Lambda { 
            LambdaParam = "x" ;
            LambdaBody = FuncApp( FuncApp( BuiltInFunc Plus, Identifier "x"), Literal (IntLit 2)) };
        Rest = FuncApp( Identifier "f", Literal (IntLit 5))}),
    Ok (Literal (IntLit 7));

    "func test",
    Ok (FuncDefExp {
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
        Rest = Identifier "main"}),
    Ok (Literal ( BoolLit true));

    "simple function",
    Ok (FuncDefExp {FuncName = "f"; FuncBody = Literal (IntLit 2); Rest = Identifier "f";}),
    Ok (Literal (IntLit 2));

    "simple 2 let",
    Ok (FuncDefExp {FuncName = "f"; FuncBody = Literal (IntLit 2); Rest = FuncDefExp {FuncName = "g"; FuncBody = Literal (StringLit "aaa"); Rest = Identifier "z";};}),
    Ok (Identifier "z");

    "3 let",
    Ok (FuncDefExp {FuncName = "f"; FuncBody = FuncDefExp {FuncName = "fun"; FuncBody = Literal (IntLit 2); Rest = Identifier "p";}; Rest = FuncDefExp {FuncName = "g"; FuncBody = Literal (StringLit "aaa"); Rest = Identifier "z";};}),
    Ok (Identifier "z");


    "function with arguments",
    Ok (FuncDefExp {
        FuncName = "f";
        FuncBody = Lambda { 
            LambdaParam = "a" ;
            LambdaBody = Lambda { 
                LambdaParam = "b" ;
                LambdaBody = FuncApp( FuncApp( BuiltInFunc Minus, Identifier "a"), Identifier "b") };
            };
        Rest = FuncApp (FuncApp (Identifier "f", Literal (IntLit 12)), Literal (IntLit 2))
    }),
    Ok (Literal (IntLit 10));

    "double fn def with same ID",
    Ok(
        FuncDefExp {
            FuncName = "f";
            FuncBody = 
                FuncDefExp {
                    FuncName = "f";
                    FuncBody = Literal (IntLit 10); 
                    Rest = Identifier "f";
                }
            Rest = FuncApp( FuncApp( BuiltInFunc Minus, Identifier "f"), Literal (IntLit 2));
        }
    ),
    Ok (Literal (IntLit 8));

]

let ifThenElseTests = [
    "simple ifThenElse true",
    Ok (IfExp ( Literal (BoolLit true),Literal (StringLit "condition evaluated to true") ,Literal (StringLit "condition evaluated to false"))),
    Ok (Literal (StringLit "condition evaluated to true"));

    "simple ifThenElse false",
    Ok (IfExp ( Literal (BoolLit false),Literal (StringLit "condition evaluated to true") ,Literal (StringLit "condition evaluated to false"))),
    Ok (Literal (StringLit "condition evaluated to false"));

    "needs eval ifThenElse false",
    Ok (IfExp (
        FuncApp (FuncApp (BuiltInFunc Less, FuncApp (FuncApp (BuiltInFunc Plus,Literal (IntLit 2)), FuncApp (FuncApp (BuiltInFunc Minus, FuncApp (FuncApp (BuiltInFunc Mult,Literal (IntLit 3)), Literal (IntLit 4))),Literal (IntLit 5)))), Literal (IntLit 6)),
        Literal (StringLit "condition evaluated to true"),
        Literal (StringLit "condition evaluated to false")
    )),
    Ok (Literal (StringLit "condition evaluated to false"));

    "needs eval ifThenElse true",
    Ok (IfExp (
        FuncApp (FuncApp (BuiltInFunc Greater, FuncApp (FuncApp (BuiltInFunc Plus,Literal (IntLit 2)), FuncApp (FuncApp (BuiltInFunc Minus, FuncApp (FuncApp (BuiltInFunc Mult,Literal (IntLit 3)), Literal (IntLit 4))),Literal (IntLit 5)))), Literal (IntLit 6)),
        Literal (StringLit "condition evaluated to true"),
        Literal (StringLit "condition evaluated to false")
    )),
    Ok (Literal (StringLit "condition evaluated to true"));

    "ifThenElse invalid cond",
    Ok (IfExp ( Literal (StringLit "hello"), Literal (BoolLit true), Literal (BoolLit false))),
    Error "SKI runtime error: Bracket Abstraction Error: \nUnexpected value in ifThenElse condition";
]

let combinatorTests = [

    "identity",
    Ok (Lambda { 
        LambdaParam = "x" ;
        LambdaBody = Identifier "x" 
    }),
    Ok (Combinator I);

    "switch args (combinator only)",
    Ok (Lambda { 
        LambdaParam = "x" ;
        LambdaBody = Lambda { 
            LambdaParam = "y" ;
            LambdaBody = FuncApp (Identifier "y", Identifier "x") 
        } 
    }),
    // S (K (S I)) (S (K K) I)
    Ok (FuncApp (
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
    ));

    "switch args (with arguments)",
    Ok (FuncApp ( 
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
    )),
    // S (K (S I)) (S (K K) I)
    Ok (FuncApp ( Literal (StringLit "Hello "),  Literal (StringLit "World") ));


]

let recursionTests = [

    "factorial 5",
    Ok (FuncDefExp {
        FuncName = "fact"; 
        FuncBody = Lambda { 
            LambdaParam = "n";
            LambdaBody = IfExp (
                FuncApp ( FuncApp (BuiltInFunc LessEq, Identifier "n"), Literal (IntLit 1) ),
                Literal (IntLit 1),
                FuncApp ( FuncApp (BuiltInFunc Mult, Identifier "n"),  FuncApp ( Identifier "fact" , FuncApp ( FuncApp (BuiltInFunc Minus, Identifier "n"), Literal (IntLit 1) ) ) ) )
            };
        Rest = FuncApp (Identifier "fact", Literal (IntLit 5))
    }),
    Ok (Literal (IntLit 120));


]

let basicPassThroughTests = [
 
    "int",
    Ok (Literal (IntLit 1)),
    Ok (Literal (IntLit 1));

    "string",
    Ok (Literal (StringLit "Hello")),
    Ok (Literal (StringLit "Hello"));

    "bool",
    Ok (Literal (BoolLit true)),
    Ok (Literal (BoolLit true));

    "pair",
    Ok (SeqExp (Literal (IntLit 1), Literal (IntLit 1))),
    Ok (SeqExp (Literal (IntLit 1), Literal (IntLit 1)));

    "list",
    Ok (SeqExp (Literal (IntLit 1), SeqExp (Literal (IntLit 1), Null))),
    Ok (SeqExp (Literal (IntLit 1), SeqExp (Literal (IntLit 1), Null)));

]

let generalTests = [

    "combination of several tests",
    Ok (FuncApp( FuncApp( BuiltInFunc Equal, FuncApp( BuiltInFunc Head, SeqExp ( Literal (IntLit 2),  SeqExp ( Literal (IntLit 3), Null )))), FuncApp( FuncApp( BuiltInFunc Mult, Literal (IntLit 1)), Literal (IntLit 2)))),
    Ok (Literal (BoolLit true));

    "arithmetic bool funcapp tests 2+3*4-5<6",
    Ok (FuncApp (FuncApp (BuiltInFunc Less, FuncApp (FuncApp (BuiltInFunc Plus,Literal (IntLit 2)), FuncApp (FuncApp (BuiltInFunc Minus, FuncApp (FuncApp (BuiltInFunc Mult,Literal (IntLit 3)), Literal (IntLit 4))),Literal (IntLit 5)))), Literal (IntLit 6))),
    Ok (Literal (BoolLit false));

    "func app w/ missing arg",
    Ok (FuncDefExp {FuncName = "f"; FuncBody =  Lambda { LambdaParam = "x" ; LambdaBody = FuncApp( FuncApp( BuiltInFunc Plus, Identifier "x"), Literal (IntLit 2)) }; Rest = FuncApp( Identifier "f", Literal (IntLit 5));}),
    Ok (Literal (IntLit 7));
    
]

let generalErrorTests = [

    "Invalid input to runtime",
    Error "Type Error",
    Error "Invalid Ast supplied as input to SKI Runtime: \nType Error";

    "invalid exp in input: RoundExp",
    Ok (RoundExp Null),
    Error "SKI runtime error: Bracket Abstraction Error: \nRoundExp should not be returned by parser";

    "invalid exp in input: FuncAppList",
    Ok (FuncAppList [Null]),
    Error "SKI runtime error: Bracket Abstraction Error: \nFuncAppList should not be returned by parser";

    "invalid exp in input: IdentifierList",
    Ok (IdentifierList [""]),
    Error "SKI runtime error: Bracket Abstraction Error: \nIdentifierList should not be returned by parser";

    "Lambda input error",
    Ok (Lambda { LambdaParam = "x";LambdaBody = SeqExp (Identifier "x",Null)}),
    Error <| sprintf "SKI runtime error: Bracket Abstraction Error: \nUnable to bracket abstract lambda: %A" {LambdaParam = "x";LambdaBody = Identifier "z"};

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
    generalErrorTests;
]

/// Run an Expecto test given a 3-tuple containing the test name, the input to the runtime and the expected output
let testEval (testName, input, expectedOutput) =
    testCase testName <| fun () ->
        Expect.equal (input |> combinatorRuntime) (expectedOutput) ""
 

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
                Rest = FuncApp(Identifier "x", FuncApp(FuncApp (Identifier "z", Literal (IntLit 1)), Literal (IntLit 2)))
            }
        }


    let TC4 = 
        FuncDefExp {
            FuncName = "f";
            FuncBody = 
                FuncDefExp {
                    FuncName = "f";
                    FuncBody = Literal (IntLit 10); 
                    Rest = Identifier "f";
                }
            Rest = FuncApp( FuncApp( BuiltInFunc Minus, Identifier "f"), Literal (IntLit 2));
        }

    //// RUN ALL EXPECTO TESTS ////
    testAll()

    //// USE THE COMBINATOR RUNTIME TO EVALUATE A PROGRAM ////
    //singleEval (Ok TC4)

    0