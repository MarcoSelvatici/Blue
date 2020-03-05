module TestBetaEngine

open TestLib
open SharedTypes
open BetaEngine
//open Expecto

type TestInfo = (string * Ast * Result<Ast,BetaEngineError>) list

// helper functions to shorten and decluter test definitions
let trueL = Literal (BoolLit true)
let falseL = Literal (BoolLit false)
let intL n = Literal (IntLit n)
let stringL s = Literal (StringLit s)
let lam n body = 
    LambdaExp { LambdaParam = n; LambdaBody = body}
let def name body rest = 
    FuncDefExp {FuncName=name; FuncBody=body; Rest=rest}
let rec buildList list =
    match list with
    | [] -> Null
    | ele::rest -> SeqExp(ele, buildList rest)

let F a1 a2 = FuncApp (a1,a2)
let F2 a1 a2 a3 = F (F a1 a2) a3
let FbuiltIn b = F (BuiltInFunc b)
let F2builtIn b = F2 (BuiltInFunc b)
let idn = Identifier

// some programs as AST's for testing with diffrent parameters
let simpleRecAST b =
    FuncDefExp {
        FuncName="f"
        FuncBody= lam "b"
            (IfExp (
                idn "b",
                Null,
                FuncApp (idn "f", trueL)
             ))
        Rest = FuncApp (idn "f",b)
    } 
let factorialAST r = 
    FuncDefExp { 
        FuncName="factorial";
        FuncBody= lam "n" 
            (IfExp ( 
                F2builtIn Equal (idn "n") (intL 0),
                intL 1,
                ( F2builtIn Mult 
                 (idn "n")
                 ( FuncApp (idn "factorial", F2builtIn Minus (idn "n") (intL 1) ) )   
                )
            ) )
        Rest = r
    }
let addRec m n = 
    let isZero k = F2builtIn Equal k (intL 0)
    let decrement k = F2builtIn Minus k (intL 1)
    let increment k = F2builtIn Plus k (intL 1)
    let mI = idn "m"
    let nI = idn "n"
    let call = F2 (idn "multRec")
    let f = def "multRec" (lam "m"  (lam "n" (IfExp  ( isZero mI , nI , call (decrement mI) (increment  nI) ) ) ) )
    f <| call (intL m) (intL n)
    
let concatDescription = sprintf "%s\n%s" 

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
    "LambdaExp \\a.a", "\\a.a", LambdaExp { LambdaParam = "a"; LambdaBody = idn "a";}
    "Nested LambdaExp", "(\\a.(\\b.b)a)", 
    LambdaExp { LambdaParam = "a"; LambdaBody = F (LambdaExp { LambdaParam = "b"; LambdaBody = idn "b";}) (idn "a" );}
    ] 
    |> List.map (fun (n,d,i) -> (concatDescription n (sprintf "%s -> %s" d d),i,Ok i))

/// tests that should return reduced (Ok AST)
let testOk : TestInfo= 
    [
    // internal    
    "buildList empty","buildList [] -> Null", buildList [], Null;
    "buildList","buildList [Null; Null] -> [Null; Null]", buildList [Null; Null], SeqExp (Null, SeqExp (Null, Null));  
    
    "if true", "if true then \"abc\" else Null", 
    IfExp (trueL, stringL "abc", Null),  stringL "abc";
    "if false", "if false then \"abc\" else Null", 
    IfExp (falseL, stringL "abc", Null), Null;
    "Function Definition", "let c = Null in c -> Null",
    FuncDefExp {FuncName="c"; FuncBody=Null; Rest=idn "c"}, Null;
    "Nested Function Definition", "let c = Null in let d = c in d -> Null",
    def "c" Null ( def "d" (idn "c") (idn "d")), Null;
    "if (idn)", "(\\x.if x then -14 else 3) -14",
    F ( lam "x" ( IfExp (idn "x", intL -14, intL 3))) trueL , intL -14;
    "Applied Function Definition", "(let double x = 2*x in double) 18",
    F (def "double" (lam "a" (F2builtIn Mult (intL 2) (idn "a"))) (idn "double"))  (intL 18), (intL 36);
        
    "Function application of LambdaExp", "(\\a.a) null -> null",
    F ( LambdaExp { LambdaParam = "a"; LambdaBody = idn "a";}) Null, Null;
    "Nested LambdaExp application", "(\\m n.m) 10 20",
    F2 (lam "m" (lam "n" (idn "m"))) (intL 10) (intL 20), intL 10;
    "Partial application", "(\\a b.a) falseL -> \\b. falseL",
    F (lam "a" (lam "b" (idn "a"))) (falseL), (lam "b" falseL);
    "Can F# do this?","let id=\\x.x in id id",
    def "id" (lam "x" (idn "x")) (F (idn "id") (idn "id")),
    lam "x" (idn "x");

    "LambdaExp variable name closure", "(\\n.(\\n.n)) false",
    F ( lam "n" (lam "n" (idn "n"))) falseL , lam "n" (idn "n");
    "Let LambdaExp name closure", "let k = Null in (\\k.k)",
    def "k" Null (lam "k" (idn "k")),lam "k" (idn "k" );
    "LambdaExp Let name closure", "(\\k.let k = true in k) 11",
    F (lam "k" (def "k" (trueL) (idn "k"))) (intL 11), trueL;
    
    // arithmetic
    ">", "5 > 1  -> true" , F2builtIn Greater   (intL 5) (intL 1), trueL;
    "<", "3 < 0 -> false" , F2builtIn Less      (intL 3) (intL 0), falseL;
    ">=","2 >= 4 -> false", F2builtIn GreaterEq (intL 2) (intL 4), falseL;
    "<=","3 <= 3 -> true" , F2builtIn LessEq    (intL 3) (intL 3), trueL;
    "= true" ,"9 = 9 -> true" , F2builtIn Equal (intL 9) (intL 9), trueL;
    "= false","6 = 7 -> false", F2builtIn Equal (intL 6) (intL 7), falseL;
    
    "and false","T and F -> F", F2builtIn And trueL falseL, falseL;
    "and true", "T and T -> T", F2builtIn And trueL trueL , trueL;
    "or true",  "T or F  -> T", F2builtIn Or  trueL falseL, trueL;
    "or false", "F or F  -> F", F2builtIn Or falseL falseL, falseL;

    "not T", "not T -> F", FbuiltIn Not trueL, falseL;
    "not F", "not F -> R", FbuiltIn Not falseL, trueL;
    
    "+", "7+8 -> 15", F2builtIn Plus  (intL 7) (intL 8), (intL 15);
    "-", "5-3 ->  2", F2builtIn Minus (intL 5) (intL 3), (intL  2);
    "*", "3*7 -> 21", F2builtIn Mult  (intL 3) (intL 7), (intL 21);
    "/", "9/3 ->  3", F2builtIn Div   (intL 9) (intL 3), (intL  3);
    "/2","5/2 ->  2", F2builtIn Div   (intL 5) (intL 2), (intL  2);

    "%", "let mod a b = a - (a/b)*b in mod 14 3 -> 2",
    def "mod" (lam "a" (lam "b" (F2builtIn Minus (idn "a") (F2builtIn Mult (F2builtIn Div (idn "a") (idn "b")) (idn "b"))))) 
     (F2 (idn "mod") (intL 14) (intL 3)), (intL 2);
    "%2", "let mod a b = a - (a/b)*b in mod 997 11 -> ",
    def "mod" (lam "a" (lam "b" (F2builtIn Minus (idn "a") (F2builtIn Mult (F2builtIn Div (idn "a") (idn "b")) (idn "b"))))) 
     (F2 (idn "mod") (intL 997) (intL 11)), (intL 7);

    "chain builtin operations", "2 * 3 - 4 * 5 < 6 -> true",
    F2builtIn Less
        ( F2builtIn Minus
                (F2builtIn Mult (intL 2) (intL 3))
                (F2builtIn Mult (intL 4) (intL 5)) )
        (intL 6),
    trueL;
    "Functionn Defintion multiple use", "let c = 10 in c + c",
    def "c" (intL 10) (F2builtIn Plus ( idn "c") ( idn "c")), intL 20;
    "Function Definition and arithmetic", "let c = 10 in c * 6 + c / 2 + c -> 75",
    def "c" (intL 10)( F2builtIn Plus
                    ( F2builtIn Mult (idn "c") (intL 6) )
                    ( F2builtIn Plus 
                        (F2builtIn Div (idn "c") (intL 2))
                        (idn "c")
                    ) ),
    intL 75;
    "Determinant", "a*d-b*c -> 3 * 6 - 5 * 4 = -2 ",
    F2 
      (F2 (lam "a" (lam "b" (lam "c" (lam "d" 
                               (F2builtIn Minus 
                                 (F2builtIn Mult (Identifier "a") (Identifier "d") ) 
                                 (F2builtIn Mult (Identifier "b") (Identifier "c"))
      ))))) 
        (intL 3) (intL 4 )) (intL 5) (intL 6), intL -2;
    
    // string
    "String Equality true", "StrEq \"word\" \"word\" -> true",
    F2builtIn StrEq (stringL "word") (stringL "word"), trueL;
    "String Equality false", "StrEq \"EIE\" \"EEE\" -> false",
    F2builtIn StrEq (stringL "EIE") (stringL "EEE"), falseL;
    "Explode", "Explode \"atlas\" -> [\"a\",\"t\",\"l\",\"a\",\"s\"]",
    FbuiltIn Explode (stringL "atlas"),  buildList ( "atlas" |> Seq.toList |> List.map (string >> stringL) );
    "Implode", "Implode [\"a\",\"t\",\"l\",\"a\",\"s\"] -> \"atlas\"",
    FbuiltIn Implode (buildList ( "atlas" |> Seq.toList |> List.map (string >> stringL) )), stringL "atlas";
    "Explode -> Append -> Implode", "Explode \"ongoozle\" |> Append \"g\" |> Implode",
    FbuiltIn Explode (stringL "ongoozle") |> F2builtIn Append (stringL "g") |> FbuiltIn Implode , stringL "gongoozle";

    // lists
    "Head", "Head [1 2 3] -> 1", FbuiltIn Head (buildList [intL 1;intL 2;intL 3]), intL(1);
    "Head with idns", "Head [c c] -> c",
    (FbuiltIn Head (buildList [idn "c"; idn "c"])), (idn "c");
    "Tail", "Tail [1 2 3] -> [2 3]", FbuiltIn Tail (buildList [intL 1;intL 2;intL 3]), buildList [intL 2; intL 3];
    "Size 0", "Size Null -> 0", FbuiltIn Size (buildList []), intL 0;
    "Size 1", "Size [ Null ] -> 1", FbuiltIn Size (buildList [Null]), intL 1;
    "Size 99", "Size [ 0..98 ] -> 99", FbuiltIn Size (buildList ([ 0..98 ]|> List.map (intL))), intL 99;
    "Append","Append 1 [2] -> [1 2]", F2builtIn Append (intL 1) (buildList [intL 2]), buildList [intL 1;intL 2];
    "Append empty", "Append 9 [] -> [9]", F2builtIn Append (intL 9) Null, buildList [intL 9];
    "Append diffrent types", "Append true [\"997\", 997, Null, [1] ] ->[true ,\"997\", 997, Null, [] ] ",
    F2builtIn Append trueL (buildList [stringL "997"; intL 997; Null; buildList [intL 1] ]), 
    buildList [trueL; stringL "997"; intL 997; Null; buildList [intL 1] ];
    "Append lazy", "Append [] [x] -> [[] x]",
    F2builtIn Append Null (buildList [idn "x"]),buildList [Null;idn "x"] ;

    // recursion
    "Simple recursion", "let f c = if c then Null else (f true) in f false -> Null",
    simpleRecAST <| falseL, Null;
    "Recursion - factorial (basecase)", "factorial 0 -> 1",
    factorialAST <| F (idn "factorial") (intL 0), intL 1;
    "Recursion - factorial 5", "factorial 5 -> 120",
    factorialAST <| F (idn "factorial") (intL 5), intL 120;
    "Recursion - factorial 10", "factorial 10 -> 39916800", 
    factorialAST <| F (idn "factorial") (intL 11), intL 39916800;
    "Add recurively", "add 100 100 -> 200", addRec 100 100, intL 200;
    ] 
    |> List.map (fun (n,d,i,o) -> (concatDescription n d,i,Ok o))


/// test that should return (Error string)
let testErr : TestInfo= 
    let id = int64 -1 |> Some
    [
    "idn", "foo", idn "foo", "Identifier \'foo\' is not defined", Identifier "foo";
    "Function Application List", "[]", FuncAppList [], "What? FuncAppList in BetaEngine", FuncAppList [];
    "Identifier List", "[]", IdentifierList [],"What? IdentifierList in BetaEngine", IdentifierList [];

    "> wrong type", "3 > Null", F2builtIn Greater (intL 3) Null, 
    "Greater is unsuported for Literal (IntLit 3), Null", 
    F2builtIn Greater (intL 3) Null;
   
    "= wrong type" ,"true = 1", F2builtIn Equal trueL (intL 1), 
    "Equal is unsuported for Literal (BoolLit true), Literal (IntLit 1)", 
    F2builtIn Equal trueL (intL 1);

    "String Equality wrong type" , "StrEq \"dog\" Null", F2builtIn StrEq (stringL "dog") Null, 
    "StrEq is unsuported for Literal (StringLit \"dog\"), Null", 
    F2builtIn StrEq (stringL "dog") Null;

    "Head wrong type", "Head Null", FbuiltIn Head Null, "Head is unsuported for Null", FbuiltIn Head Null;
    ]
    |> List.map (fun (n,d,i,m,o) -> concatDescription n d,i,Error {msg=m; trace=[]; ast=o} ) 

let testErrTrace : TestInfo =
    [
        
    ]

let upcastError =
    function
    | Error e -> Error <| BetaEngineError e
    | Ok x -> Ok x

let testCasesBetaEngine =
    [testId; testOk; testErr]
    |> List.reduce List.append
    |> List.map (fun (d,i,o) -> (d,i,upcastError o))