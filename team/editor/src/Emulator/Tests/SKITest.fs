// Author: oss1017 (Oliver Stiff)
// Testbench for the combinator runtime

module TestSKIRuntime

open SharedTypes
open SKIRuntime



///Takes an F# list as input and returns a list made of nested pairs in our language (SeqExp)
let rec buildList lstType lst =
    match lst with
    | [] -> SeqExp(Null,Null)
    | [x] -> SeqExp (Literal (lstType x), SeqExp(Null,Null))
    | head::tail -> SeqExp ( Literal (lstType head), buildList lstType tail )


/// Tests for built-in functions related to strings
let stringTests = [

    "explode",
    FuncApp (BuiltInFunc Explode, Literal (StringLit "Hello, World.")),
    "Hello, World." |> Seq.toList |> List.map string |> buildList StringLit |> Ok;

    "Explode error",
    FuncApp (BuiltInFunc Explode, Null),
    buildErrorSKI "Built-in function evaluation Error: \ncannot explode argument of type which is not string";

    "implode",
    FuncApp (BuiltInFunc Implode, "Hello, World." |> Seq.toList |> List.map string |> buildList StringLit),
    Ok (Literal (StringLit "Hello, World."));

    "implode Error",
    FuncApp (BuiltInFunc Implode, Null),
    buildErrorSKI "Built-in function evaluation Error: \nCannot implode argument of type which is not string list";

    "implode explode",
    FuncApp ( BuiltInFunc Implode, FuncApp (BuiltInFunc Explode, Literal (StringLit "Hello, World."))),
    Ok (Literal (StringLit "Hello, World."));
    
]


/// Tests for built-in functions related to lists 
let listTests = [

    "ListSize short", 
    FuncApp( BuiltInFunc Size, (IntLit, [1..3]) ||> buildList), 
    Ok (Literal (IntLit 3));

    "ListSize long", 
    FuncApp( BuiltInFunc Size, (IntLit, [1..1000]) ||> buildList), 
    Ok (Literal (IntLit 1000));

    "ListSize 1", 
    FuncApp( BuiltInFunc Size, (IntLit, [1]) ||> buildList),
    Ok (Literal (IntLit 1));

    "ListSize null", 
    FuncApp( BuiltInFunc Size, (IntLit, []) ||> buildList), 
    Ok (Literal (IntLit 0));

    "ListSize Error", 
    FuncApp( BuiltInFunc Size, Literal (StringLit "I'm a string!")), 
    buildErrorSKI "Built-in function evaluation Error: \nError getting size of list: Invalid input: Literal (StringLit \"I'm a string!\")";

    "List Size string",
    FuncApp( BuiltInFunc Size, SeqExp ( Literal (StringLit "this is a list"), SeqExp(Null,Null))),
    Ok (Literal (IntLit 1));

    "List Head",
    FuncApp( BuiltInFunc Head, (IntLit, [1..3]) ||> buildList),
    Ok (Literal (IntLit 1));

    "List Head error",
    FuncApp( BuiltInFunc Head, Literal (IntLit 10)),
    buildErrorSKI "Built-in function evaluation Error: \nError getting head of list/sequence";

    "List tail",
    FuncApp( BuiltInFunc Tail, SeqExp ( Literal (IntLit 1), SeqExp ( Literal (IntLit 2),  SeqExp ( Literal (IntLit 3), SeqExp(Null,Null) )))),
    Ok (SeqExp ( Literal (IntLit 2),  SeqExp ( Literal (IntLit 3), SeqExp(Null,Null) )));

    "List Tail error",
    FuncApp( BuiltInFunc Tail, Literal (IntLit 10)),
    buildErrorSKI "Built-in function evaluation Error: \nError getting tail of list/sequence";

]


/// Tests for built-in functions related to arithmetic operations
let arithmeticTests = [

    "Add",
    FuncApp( FuncApp( BuiltInFunc Plus, Literal (IntLit 4)), Literal (IntLit 2)),
    Ok (Literal (IntLit 6));
    
    "Sub",
    FuncApp( FuncApp( BuiltInFunc Minus, Literal (IntLit 4)), Literal (IntLit 2)),
    Ok (Literal (IntLit 2));

    "Mult",
    FuncApp( FuncApp( BuiltInFunc Mult, Literal (IntLit 4)), Literal (IntLit 2)),
    Ok (Literal (IntLit 8));

    "Div",
    FuncApp( FuncApp( BuiltInFunc Div, Literal (IntLit 4)), Literal (IntLit 2)),
    Ok (Literal (IntLit 2));

    "Div 0 Error",
    FuncApp( FuncApp( BuiltInFunc Div, Literal (IntLit 4)), Literal (IntLit 0)),
    buildErrorSKI "Built-in function evaluation Error: \nDivision by 0";

]

/// Tests for built-in function 'test'
let testTests = [

    "test int", 
    FuncApp( BuiltInFunc Test, Literal (IntLit 1)),
    Ok (Literal (BoolLit true));

    "test bool", 
    FuncApp( BuiltInFunc Test, Literal (BoolLit false)),
    Ok (Literal (BoolLit true));

    "test string",
    FuncApp( BuiltInFunc Test, Literal (StringLit "hello")),
    Ok (Literal (BoolLit true));

    "test list 1",
    FuncApp( BuiltInFunc Test, (IntLit, [1]) ||> buildList),
    Ok (Literal (BoolLit true));

    "test list 2",
    FuncApp( BuiltInFunc Test, (StringLit, []) ||> buildList),
    Ok (Literal (BoolLit true));

    "test def",
    FuncApp( BuiltInFunc Test, FuncDefExp {FuncName = "f"; FuncBody = Literal (IntLit 2); Rest = Identifier "f";}),
    Ok (Literal (BoolLit true));

    "test eval",
    FuncApp( BuiltInFunc Test, FuncApp( FuncApp( BuiltInFunc Minus, Literal (IntLit 4)), Literal (IntLit 2))),
    Ok (Literal (BoolLit true));

    "test lambda",
    FuncApp( BuiltInFunc Test, LambdaExp { LambdaParam = "x" ; LambdaBody = Identifier "x" }),
    Ok (Literal (BoolLit false));

]

/// Tests for built-in functions related to logical operators
let booleanTests = [
    
    "greater (true)",
    FuncApp( FuncApp( BuiltInFunc Greater, Literal (IntLit 4)), Literal (IntLit 2)),
    Ok (Literal (BoolLit true));

    "greater (false)",
    FuncApp( FuncApp( BuiltInFunc Greater, Literal (IntLit 1)), Literal (IntLit 2)),
    Ok (Literal (BoolLit false));

    "greater equal (true)",
    FuncApp( FuncApp( BuiltInFunc GreaterEq, Literal (IntLit 4)), Literal (IntLit 4)),
    Ok (Literal (BoolLit true));

    "greater equal (false)",
    FuncApp( FuncApp( BuiltInFunc GreaterEq, Literal (IntLit 4)), Literal (IntLit 5)),
    Ok (Literal (BoolLit false));

    "less (false)",
    FuncApp( FuncApp( BuiltInFunc Less, Literal (IntLit 4)), Literal (IntLit 2)),
    Ok (Literal (BoolLit false));

    "less (true)",
    FuncApp( FuncApp( BuiltInFunc Less, Literal (IntLit 1)), Literal (IntLit 2)),
    Ok (Literal (BoolLit true));

    "less equal (flase)",
    FuncApp( FuncApp( BuiltInFunc LessEq, Literal (IntLit 4)), Literal (IntLit 2)),
    Ok (Literal (BoolLit false));

    "less equal (true)",
    FuncApp( FuncApp( BuiltInFunc LessEq, Literal (IntLit 4)), Literal (IntLit 4)),
    Ok (Literal (BoolLit true));

    "equal (false)",
    FuncApp( FuncApp( BuiltInFunc Equal, Literal (IntLit 4)), Literal (IntLit 2)),
    Ok (Literal (BoolLit false));

    "equal (true)",
    FuncApp( FuncApp( BuiltInFunc Equal, Literal (IntLit 2)), Literal (IntLit 2)),
    Ok (Literal (BoolLit true));

    "not (true)",
    FuncApp( BuiltInFunc Not, Literal (BoolLit true)),
    Ok (Literal (BoolLit false));

    "not (false)",
    FuncApp( BuiltInFunc Not, Literal (BoolLit false)),
    Ok (Literal (BoolLit true));

    "not (error)",
    FuncApp( BuiltInFunc Not, Literal (IntLit 1)),
    buildErrorSKI "Built-in function evaluation Error: \nError evaluating built-in function with 1 argument: operator 'Not' with argument 'Ok (Literal (IntLit 1))'" ;

]


/// Tests for lambdas
let lambdaTests = [

    "lambda testing",
    FuncApp (LambdaExp { 
    LambdaParam = "x";
    LambdaBody = FuncApp ( FuncApp( BuiltInFunc Plus , Identifier "x" ), Literal (IntLit 1)) } , Literal (IntLit 5)),
    Ok (Literal (IntLit 6));

    "lambda identity",
    LambdaExp { 
        LambdaParam = "x" ;
        LambdaBody = Identifier "x" },
    Ok (Combinator I);

    "lambda passthrough",
    FuncApp (LambdaExp {
        LambdaParam = "x";
        LambdaBody = Identifier "x" } , Literal (IntLit 5)),
    Ok (Literal (IntLit 5));

    "lambda x+y",
    FuncApp (FuncApp (LambdaExp { 
        LambdaParam = "x"; 
        LambdaBody = LambdaExp { 
            LambdaParam = "y"; 
            LambdaBody = FuncApp ( FuncApp( BuiltInFunc Plus , Identifier "x" ),  Identifier "y")  }},Literal (IntLit 12)),Literal (IntLit 10)),
    Ok (Literal (IntLit 22));

]


/// Tests for function definitions / simple programs
let funcDefTests = [

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
    Ok (Identifier "z");

    "double identity lambda and ret int",
    FuncDefExp {
        FuncName = "f";
        FuncBody =  LambdaExp { 
            LambdaParam = "x" ; 
            LambdaBody = Identifier "x" }; 
        Rest = FuncApp( FuncApp (Identifier "f", Identifier "f"), Literal (IntLit 5));},
    Ok (Literal (IntLit 5));

    "func def w/ lambda and builtin",
    FuncDefExp {
        FuncName = "f";
        FuncBody =  LambdaExp { 
            LambdaParam = "x" ;
            LambdaBody = FuncApp( FuncApp( BuiltInFunc Plus, Identifier "x"), Literal (IntLit 2)) };
        Rest = FuncApp( Identifier "f", Literal (IntLit 5))},
    Ok (Literal (IntLit 7));

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
    Ok (Literal ( BoolLit true));

    "simple function",
    FuncDefExp {FuncName = "f"; FuncBody = Literal (IntLit 2); Rest = Identifier "f";},
    Ok (Literal (IntLit 2));

    "simple 2 let",
    FuncDefExp {FuncName = "f"; FuncBody = Literal (IntLit 2); Rest = FuncDefExp {FuncName = "g"; FuncBody = Literal (StringLit "aaa"); Rest = Identifier "z";};},
    Ok (Identifier "z");

    "3 let",
    FuncDefExp {FuncName = "f"; FuncBody = FuncDefExp {FuncName = "fun"; FuncBody = Literal (IntLit 2); Rest = Identifier "p";}; Rest = FuncDefExp {FuncName = "g"; FuncBody = Literal (StringLit "aaa"); Rest = Identifier "z";};},
    Ok (Identifier "z");


    "function with arguments",
    FuncDefExp {
        FuncName = "f";
        FuncBody = LambdaExp { 
            LambdaParam = "a" ;
            LambdaBody = LambdaExp { 
                LambdaParam = "b" ;
                LambdaBody = FuncApp( FuncApp( BuiltInFunc Minus, Identifier "a"), Identifier "b") };
            };
        Rest = FuncApp (FuncApp (Identifier "f", Literal (IntLit 12)), Literal (IntLit 2))
    },
    Ok (Literal (IntLit 10));

    "double fn def with same ID",
    FuncDefExp {
        FuncName = "f";
        FuncBody = 
            FuncDefExp {
                FuncName = "f";
                FuncBody = Literal (IntLit 10); 
                Rest = Identifier "f";
            }
        Rest = FuncApp( FuncApp( BuiltInFunc Minus, Identifier "f"), Literal (IntLit 2));
    },
    Ok (Literal (IntLit 8));

]


/// Tests for conditional statements
let ifThenElseTests = [
    "simple ifThenElse true",
    IfExp ( Literal (BoolLit true),Literal (StringLit "condition evaluated to true") ,Literal (StringLit "condition evaluated to false")),
    Ok (Literal (StringLit "condition evaluated to true"));

    "simple ifThenElse false",
    IfExp ( Literal (BoolLit false),Literal (StringLit "condition evaluated to true") ,Literal (StringLit "condition evaluated to false")),
    Ok (Literal (StringLit "condition evaluated to false"));

    "needs eval ifThenElse false",
    IfExp (
                FuncApp (FuncApp (BuiltInFunc Less, FuncApp (FuncApp (BuiltInFunc Plus,Literal (IntLit 2)), FuncApp (FuncApp (BuiltInFunc Minus, FuncApp (FuncApp (BuiltInFunc Mult,Literal (IntLit 3)), Literal (IntLit 4))),Literal (IntLit 5)))), Literal (IntLit 6)),
                Literal (StringLit "condition evaluated to true"),
                Literal (StringLit "condition evaluated to false")
    ),
    Ok (Literal (StringLit "condition evaluated to false"));

    "needs eval ifThenElse true",
    IfExp (
                FuncApp (FuncApp (BuiltInFunc Greater, FuncApp (FuncApp (BuiltInFunc Plus,Literal (IntLit 2)), FuncApp (FuncApp (BuiltInFunc Minus, FuncApp (FuncApp (BuiltInFunc Mult,Literal (IntLit 3)), Literal (IntLit 4))),Literal (IntLit 5)))), Literal (IntLit 6)),
                Literal (StringLit "condition evaluated to true"),
                Literal (StringLit "condition evaluated to false")
    ),
    Ok (Literal (StringLit "condition evaluated to true"));

    "ifThenElse invalid cond",
    IfExp ( Literal (StringLit "hello"), Literal (BoolLit true), Literal (BoolLit false)),
    buildErrorSKI "Bracket Abstraction Error: \nUnexpected value in ifThenElse condition";
]


/// Tests demonstrating combinator reduction functionality
let combinatorTests = [

    "identity",
    LambdaExp { 
        LambdaParam = "x" ;
        LambdaBody = Identifier "x" 
    },
    Ok (Combinator I);

    "switch args (combinator only)",
    LambdaExp { 
        LambdaParam = "x" ;
        LambdaBody = LambdaExp { 
            LambdaParam = "y" ;
            LambdaBody = FuncApp (Identifier "y", Identifier "x") 
        } 
    },
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
    FuncApp ( 
            FuncApp (
                LambdaExp { 
                    LambdaParam = "x" ;
                    LambdaBody = LambdaExp { 
                        LambdaParam = "y" ;
                        LambdaBody = FuncApp (Identifier "y", Identifier "x") 
                    } 
                }, 
                Literal (StringLit "World")
            ),
            Literal (StringLit "Hello ")
    ),
    // S (K (S I)) (S (K K) I)
    Ok (FuncApp ( Literal (StringLit "Hello "),  Literal (StringLit "World") ));

]


// Recursive function testing

/// F# definition of factorial
let rec fact n =
    if n <= 1
    then 1
    else n * fact (n-1)


/// Output of Ast for factorial function
let runtimeFact n =
    FuncDefExp {
        FuncName = "fact"; 
        FuncBody = LambdaExp { 
            LambdaParam = "n";
            LambdaBody = IfExp (
                                FuncApp ( FuncApp (BuiltInFunc LessEq, Identifier "n"), Literal (IntLit 1) ),
                                Literal (IntLit 1),
                                FuncApp ( FuncApp (BuiltInFunc Mult, Identifier "n"),  FuncApp ( Identifier "fact" , FuncApp ( FuncApp (BuiltInFunc Minus, Identifier "n"), Literal (IntLit 1) ) ) ) )
            };
        Rest = FuncApp (Identifier "fact", Literal (IntLit n))
    }


/// F# definition of fib
let rec fib x =
    if x = 0 || x = 1
    then x
    else fib(x-1) + fib(x-2)


/// Output of Ast for fib function
let runtimeFib x =
    FuncDefExp {
        FuncName = "fib"; 
        FuncBody = LambdaExp { 
            LambdaParam = "x";
            LambdaBody = IfExp (
                                FuncApp ( FuncApp (BuiltInFunc Equal, Identifier "x"), Literal (IntLit 1) ),
                                    Literal (IntLit 1),
                                    IfExp (
                                        FuncApp ( FuncApp (BuiltInFunc Equal, Identifier "x"), Literal (IntLit 0) ),
                                        Literal (IntLit 0),
                                        FuncApp ( 
                                            FuncApp (
                                                BuiltInFunc Plus, 
                                                FuncApp ( Identifier "fib" , FuncApp ( FuncApp (BuiltInFunc Minus, Identifier "x"), Literal (IntLit 1) ))
                                            ),
                                            FuncApp ( Identifier "fib" , FuncApp ( FuncApp (BuiltInFunc Minus, Identifier "x"), Literal (IntLit 2) )) 
                                        ) 
                                    ) 
                                )
            };
        Rest = FuncApp (Identifier "fib", Literal (IntLit x))
    }


/// Tests to demonstrate recursion functionality (no mutual recursion)
let recursionTests = [

    "factorial 0",
    runtimeFact 0,
    0 |> fact |> IntLit |> Literal |> Ok;

    "factorial 1",
    runtimeFact 1,
    1 |> fact |> IntLit |> Literal |> Ok;

    "factorial 5",
    runtimeFact 5,
    5 |> fact |> IntLit |> Literal |> Ok;

    "factorial 10",
    runtimeFact 10,
    10 |> fact |> IntLit |> Literal |> Ok;

    "factorial 12",
    runtimeFact 12,
    12 |> fact |> IntLit |> Literal |> Ok;

    "fib 0",
    runtimeFib 0,
    0 |> fib |> IntLit |> Literal |> Ok;

    "fib 1",
    runtimeFib 1,
    1 |> fib |> IntLit |> Literal |> Ok;

    "fib 5",
    runtimeFib 5,
    5 |> fib |> IntLit |> Literal |> Ok;

    "fib 20",
    runtimeFib 20,
    20 |> fib |> IntLit |> Literal |> Ok;

]


/// See if simple expressions pass through runtime un-altered
let basicPassThroughTests = [
 
    "int",
    Literal (IntLit 1),
    Ok (Literal (IntLit 1));

    "string",
    Literal (StringLit "Hello"),
    Ok (Literal (StringLit "Hello"));

    "bool",
    Literal (BoolLit true),
    Ok (Literal (BoolLit true));

    "pair",
    SeqExp (Literal (IntLit 1), Literal (IntLit 1)),
    Ok (SeqExp (Literal (IntLit 1), Literal (IntLit 1)));

    "list",
    SeqExp (Literal (IntLit 1), SeqExp (Literal (IntLit 1), SeqExp (Null,Null))),
    Ok (SeqExp (Literal (IntLit 1), SeqExp (Literal (IntLit 1), SeqExp (Null,Null))));

]


///  Testing of several features working simultaneously  
let generalTests = [

    "combination of several tests",
    FuncApp( FuncApp( BuiltInFunc Equal, FuncApp( BuiltInFunc Head, SeqExp ( Literal (IntLit 2),  SeqExp ( Literal (IntLit 3), Null )))), FuncApp( FuncApp( BuiltInFunc Mult, Literal (IntLit 1)), Literal (IntLit 2))),
    Ok (Literal (BoolLit true));

    "arithmetic bool funcapp tests 2+3*4-5<6",
    FuncApp (FuncApp (BuiltInFunc Less, FuncApp (FuncApp (BuiltInFunc Plus,Literal (IntLit 2)), FuncApp (FuncApp (BuiltInFunc Minus, FuncApp (FuncApp (BuiltInFunc Mult,Literal (IntLit 3)), Literal (IntLit 4))),Literal (IntLit 5)))), Literal (IntLit 6)),
    Ok (Literal (BoolLit false));

    "func def/ app",
    FuncDefExp {FuncName = "f"; FuncBody =  LambdaExp { LambdaParam = "x" ; LambdaBody = FuncApp( FuncApp( BuiltInFunc Plus, Identifier "x"), Literal (IntLit 2)) }; Rest = FuncApp( Identifier "f", Literal (IntLit 5));},
    Ok (Literal (IntLit 7));
    
]


/// Test error reporting functionality
let generalErrorTests = [

    "invalid exp in input: FuncAppList",
    FuncAppList [Null],
    buildErrorSKI "Bracket Abstraction Error: \nFuncAppList should not be returned by parser";

    "invalid exp in input: IdentifierList",
    IdentifierList [""],
    buildErrorSKI "Bracket Abstraction Error: \nIdentifierList should not be returned by parser";

]


/// List of all test categories to be run
let runtimeTests = [
    testTests;
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


/// Run a single input using the combinator runtime
let singleEval = 
    combinatorRuntime >> print

/// Create list with all SKI runtime tests
let testCasesSKIRuntime = 
   List.fold (fun x y -> x @ y) [] runtimeTests