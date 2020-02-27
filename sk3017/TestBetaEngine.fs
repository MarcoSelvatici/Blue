module TestBetaEngine

open TokeniserParserStub
open BetaEngine
open Expecto

type TestInfo = (string * string * Ast * Result<Ast,string>) list

// helper functions to shorten and decluter test definitions
let trueL = Literal (BoolLit true)
let falseL = Literal (BoolLit false)
let intL n = Literal (IntLit n)
let stringL s = Literal (StringLit s)
let lam n body = 
    Lambda { LambdaParam = n; LambdaBody = body}
let def name body rest = 
    FuncDefExp {FuncName=name; FuncBody=body; Rest=rest}
let rec buildList list =
    match list with
    | [] -> Null
    | ele::rest -> SeqExp(ele, buildList rest)

let unaryBuiltin builtin arg = 
    FuncApp (BuiltInFunc builtin, arg)
let binaryBuiltin builtin lhs rhs =
    FuncApp (FuncApp (BuiltInFunc builtin, lhs), rhs)

// some programs as AST's for testing with diffrent parameters
let simpleRecAST b =
    FuncDefExp {
        FuncName="f"
        FuncBody= lam "b"
            (IfExp (
                Identifier "b",
                Null,
                FuncApp (Identifier "f", trueL)
             ))
        Rest = FuncApp (Identifier "f",b)
    }
let factorialAST r = 
    FuncDefExp { 
        FuncName="factorial";
        FuncBody= lam "n" 
            (IfExp ( 
                binaryBuiltin Equal (Identifier "n") (intL 0),
                intL 1,
                ( binaryBuiltin Mult 
                 (Identifier "n")
                 ( FuncApp (Identifier "factorial", binaryBuiltin Minus (Identifier "n") (intL 1) ) )   
                )
            ) )
        Rest = r
    }
let addRec m n = 
    let isZero k = binaryBuiltin Equal k (intL 0)
    let decrement k = binaryBuiltin Minus k (intL 1)
    let increment k = binaryBuiltin Plus k (intL 1)
    let mI = Identifier "m"
    let nI = Identifier "n"
    let call a b = FuncApp (FuncApp (Identifier "multRec",a),b)
    let f = def "multRec" (lam "m"  (lam "n" (IfExp  ( isZero mI , nI , call (decrement mI) (increment  nI) ) ) ) )
    f <| call (intL m) (intL n)
    

/// tests with the same input and output ASTs
let testId : TestInfo= 
    [ 
    "Literal int", "6", intL 6;
    "Literal bool", "true", trueL;
    "Literal string", "\"abc\"" , stringL "abc";
    "Null", "Null", Null;
    "BuiltInFunc >",">", BuiltInFunc Greater;
    "BuiltInFunc <","<", BuiltInFunc Less;
    "BuiltInFunc >=",">=", BuiltInFunc GreaterEq;
    "BuiltInFunc <=","<=", BuiltInFunc LessEq;
    "BuiltInFunc =","=", BuiltInFunc Greater;
    "Sequence (Null,Null)","(Null,Null)",SeqExp (Null,Null);
    "Lambda \\a.a", "\\a.a", Lambda { LambdaParam = "a"; LambdaBody = Identifier "a";}
    "Nested lambda", "(\\a.(\\b.b)a)", 
    Lambda { LambdaParam = "a"; LambdaBody = FuncApp (Lambda { LambdaParam = "b"; LambdaBody = Identifier "b";}, Identifier "a" );}
    ] 
    |> List.map (fun (n,d,i) -> (n,sprintf "%s -> %s" d d,i,Ok i))

/// tests that should return reduced (Ok AST)
let testOk : TestInfo= 
    [
    // internal    
    "buildList empty","buildList [] -> Null", buildList [], Null;
    "buildList","buildList [Null; Null] -> [Null; Null]", buildList [Null; Null], SeqExp (Null, SeqExp (Null, Null));  
    
    "Round expression (-10)", "(-10) -> -10", RoundExp (intL -10), (intL -10);
    "Nested Round expression (((Null)))", "(((Null)))", Null |> RoundExp |> RoundExp |> RoundExp, Null;
    "if true", "if true then \"abc\" else Null", 
    IfExp (trueL, stringL "abc", Null),  stringL "abc";
    "if false", "if false then \"abc\" else Null", 
    IfExp (falseL, stringL "abc", Null), Null;
    "Function Definition", "let c = Null in c -> Null",
    FuncDefExp {FuncName="c"; FuncBody=Null; Rest=Identifier "c"}, Null;
    "Nested Function Definition", "let c = Null in let d = c in d -> Null",
    def "c" Null ( def "d" (Identifier "c") (Identifier "d")), Null;
    "if (identifier)", "(\\x.if x then -14 else 3) -14",
    FuncApp ( lam "x" ( IfExp (Identifier "x", intL -14, intL 3)) ,trueL) , intL -14;
    
    "Function application of lambda", "(\\a.a) null -> null",
    FuncApp ( Lambda { LambdaParam = "a"; LambdaBody = Identifier "a";}, Null), Null;
    "Nested lambda application", "(\\m n.m) 10 20",
    FuncApp (FuncApp ( lam "m" (lam "n" (Identifier "m") ), intL 10), intL 20), intL 10;
    "Lambda variable name closure", "(\\n.(\\n.n)) false",
    FuncApp( lam "n" (lam "n" (Identifier "n")), falseL ), lam "n" (Identifier "n");
    "Let Lambda name closure", "let k = Null in (\\k.k)",
    def "k" Null (lam "k" (Identifier "k")),lam "k" (Identifier "k" );
    "Lambda Let name closure", "(\\k.let k = true in k) 11",
    FuncApp (lam "k" (def "k" (trueL) (Identifier "k")), intL 11), trueL;
    
    // arithmetic
    ">", "5 > 1  -> true" , binaryBuiltin Greater   (intL 5) (intL 1), trueL;
    "<", "3 < 0 -> false" , binaryBuiltin Less      (intL 3) (intL 0), falseL;
    ">=","2 >= 4 -> false", binaryBuiltin GreaterEq (intL 2) (intL 4), falseL;
    "<=","3 <= 3 -> true" , binaryBuiltin LessEq    (intL 3) (intL 3), trueL;
    "= true" ,"9 = 9 -> true" , binaryBuiltin Equal (intL 9) (intL 9), trueL;
    "= false","6 = 7 -> false", binaryBuiltin Equal (intL 6) (intL 7), falseL;
    
    "and false","T and F -> F", binaryBuiltin And trueL falseL, falseL;
    "and true", "T and T -> T", binaryBuiltin And trueL trueL , trueL;
    "or true",  "T or F  -> T", binaryBuiltin Or  trueL falseL, trueL;
    "or false", "F or F  -> F", binaryBuiltin Or falseL falseL, falseL;

    "not T", "not T -> F", unaryBuiltin Not trueL, falseL;
    "not F", "not F -> R", unaryBuiltin Not falseL, trueL;
    
    "+", "7+8 -> 15", binaryBuiltin Plus  (intL 7) (intL 8), (intL 15);
    "-", "5-3 ->  2", binaryBuiltin Minus (intL 5) (intL 3), (intL  2);
    "*", "3*7 -> 21", binaryBuiltin Mult  (intL 3) (intL 7), (intL 21);
    "/", "9/3 ->  3", binaryBuiltin Div   (intL 9) (intL 3), (intL  3);

    "chain builtin operations", "2 * 3 - 4 * 5 < 6 -> true",
    binaryBuiltin Less
        ( binaryBuiltin Minus
                (binaryBuiltin Mult (intL 2) (intL 3))
                (binaryBuiltin Mult (intL 4) (intL 5)) )
        (intL 6),
    trueL;
    "Functionn Defintion multiple use", "let c = 10 in c + c",
    def "c" (intL 10) (binaryBuiltin Plus ( Identifier "c") ( Identifier "c")), intL 20;
    "Function Definition and arithmetic", "let c = 10 in c * 6 + c / 2 + c -> 75",
    def "c" (intL 10)( binaryBuiltin Plus
                    ( binaryBuiltin Mult (Identifier "c") (intL 6) )
                    ( binaryBuiltin Plus 
                        (binaryBuiltin Div (Identifier "c") (intL 2))
                        (Identifier "c")
                    ) ),
    intL 75;
    
    // string
    "String Equality true", "StrEq \"word\" \"word\" -> true",
    binaryBuiltin StrEq (stringL "word") (stringL "word"), trueL;
    "String Equality false", "StrEq \"EIE\" \"EEE\" -> false",
    binaryBuiltin StrEq (stringL "EIE") (stringL "EEE"), falseL;
    "Explode", "Explode \"atlas\" -> [\"a\",\"t\",\"l\",\"a\",\"s\"]",
    unaryBuiltin Explode (stringL "atlas"),  buildList ( "atlas" |> Seq.toList |> List.map (string >> stringL) );

    // lists
    "Head", "Head [1 2 3] -> 1", unaryBuiltin Head (buildList [intL 1;intL 2;intL 3]), intL(1);
    "Head with identifiers", "Head [c c] -> c",
    (unaryBuiltin Head (buildList [Identifier "c"; Identifier "c"])), (Identifier "c");
    "Tail", "Tail [1 2 3] -> [2 3]", unaryBuiltin Tail (buildList [intL 1;intL 2;intL 3]), buildList [intL 2; intL 3];
    "Size 0", "Size Null -> 0", unaryBuiltin Size (buildList []), intL 0;
    "Size 1", "Size [ Null ] -> 1", unaryBuiltin Size (buildList [Null]), intL 1;
    "Size 99", "Size [ 0..98 ] -> 99", unaryBuiltin Size (buildList ([ 0..98 ]|> List.map (intL))), intL 99;
    "Append","Append 1 [2] -> [1 2]", binaryBuiltin Append (intL 1) (buildList [intL 2]), buildList [intL 1;intL 2];
    "Append empty", "Append 9 [] -> [9]", binaryBuiltin Append (intL 9) Null, buildList [intL 9];
    "Append diffrent types", "Append true [\"997\", 997, Null, [1] ] ->[true ,\"997\", 997, Null, [] ] ",
    binaryBuiltin Append trueL (buildList [stringL "997"; intL 997; Null; buildList [intL 1] ]), 
    buildList [trueL; stringL "997"; intL 997; Null; buildList [intL 1] ];
    "Append lazy", "Append [] [x] -> [[] x]",
    binaryBuiltin Append Null (buildList [Identifier "x"]),buildList [Null;Identifier "x"] ;

    // recursion
    "Simple recursion", "let f c = if c then Null else (f true) in f false -> Null",
    simpleRecAST <| falseL, Null;
    "Recursion - factorial (basecase)", "factorial 0 -> 1",
    factorialAST <| FuncApp (Identifier "factorial", intL 0), intL 1;
    "Recursion - factorial 5", "factorial 5 -> 120",
    factorialAST <| FuncApp (Identifier "factorial", intL 5), intL 120;
    "Recursion - factorial 10", "factorial 10 -> 39916800", 
    factorialAST <| FuncApp (Identifier "factorial", intL 11), intL 39916800;
    "Add recurively", "add 100 100 -> 200", addRec 100 100, intL 200;

    ] 
    |> List.map (fun (n,d,i,o) -> (n,d,i,Ok o))


/// test that should return (Error string)
let testErr : TestInfo= 
    [
    "Identifier", "foo", Identifier "foo", "Identifier \'foo\' is not defined";
    "Function Application List", "[]", FuncAppList [], "What? parser returned FuncAppList";
    "Identifier List", "[]", IdentifierList [],"What? parser returned IdentifierList";

    "> wrong type", "3 > Null", binaryBuiltin Greater (intL 3) Null, "Greater is unsuported for Literal (IntLit 3), Null";
    "= wrong type" ,"true = 1", binaryBuiltin Equal trueL (intL 1), "Equal is unsuported for Literal (BoolLit true), Literal (IntLit 1)";
    "String Equality wrong type" , "StrEq \"dog\" Null", binaryBuiltin StrEq (stringL "dog") Null, "StrEq is unsuported for Literal (StringLit \"dog\"), Null";

    "Head wrong type", "Head Null", unaryBuiltin Head Null, "Head is unsuported for Null";
    //"Lambda \\a.b", "\\a.b", Lambda { LambdaParam = "a"; LambdaBody = Identifier "b"},"Identifier \'b\' is not defined"; // Lazy
    ]
    |> List.map (fun (n,d,i,o) -> (n,d,i,Error o))

let makeTest f (name, description, input, output) =
    test name { Expect.equal (f input) output description}

[<Tests>]
let tests =
    testId 
    |> List.append testOk
    //|> List.append testErr 
    |> List.map (makeTest runAst) 
    |> testList "Hardwired" 
 

let allTests() =
    runTestsInAssembly defaultConfig [||] |> ignore

// TODO add test thatt chack that names defined in lambdas dont mix with that in definitions
// nested function application with lambdas
// TODO : ENABLE ERROR TEST