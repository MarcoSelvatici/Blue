open System
open TokeniserParserStub
open BetaEngine
open Expecto

type TestInfo = (string * string * Ast * Result<Ast,string>) list


// TODO refactor some tests
let intL n = Literal (IntLit n)
let boolL b = Literal (BoolLit b)
let stringL s = Literal (StringLit s)
let lam n body = Lambda { LambdaParam = n; LambdaBody = body}

// some programs as AST's for testing with diffrent parameters
let simpleRecAST b =
    FuncDefExp {
        FuncName="f"
        FuncBody= lam "b"
            (IfExp (
                Identifier "b",
                Null,
                FuncApp (Identifier "f", boolL true)
             ))
        Rest = FuncApp (Identifier "f",b)
    }

let factorialAST r = 
    FuncDefExp { 
        FuncName="factorial";
        FuncBody= lam "n" 
            (IfExp ( 
                FuncApp (FuncApp (BuiltInFunc Equal, Identifier "n"), intL 0),        
                intL 1,
                FuncApp(
                    FuncApp (
                        BuiltInFunc Mult,
                        FuncApp ( Identifier "factorial", FuncApp (FuncApp (BuiltInFunc Minus, Identifier "n"), intL 1)) // f(n-1)
                    ),
                    Identifier "n"
                )
                
            ))
        Rest = r
    }


/// tests with the same input and output asts
let testId : TestInfo= 
    [ 
    "Literal int", "6", intL 6;
    "Literal bool", "true", boolL true;
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

/// tests that should return reduced (Ok ast)
let testOk : TestInfo= 
    [
    "Round expression (-10)", "(-10) -> -10", RoundExp (intL -10), (intL -10);
    "Nested Round expression (((Null)))", "(((Null)))", Null |> RoundExp |> RoundExp |> RoundExp, Null;
    "if true", "if true then \"abc\" else Null", 
    IfExp (boolL true, stringL "abc", Null),  stringL "abc";
    "if false", "if false then \"abc\" else Null", 
    IfExp (boolL false, stringL "abc", Null), Null;
    "Function Definition", "let c = Null in c -> Null",
    FuncDefExp {FuncName="c"; FuncBody=Null; Rest=Identifier "c"}, Null;
    "Nested Function Definition", "let c = Null in let d = c in d -> Null",
    FuncDefExp {FuncName="c"; FuncBody=Null; Rest=(FuncDefExp {FuncName="d"; FuncBody=Identifier "c"; Rest=Identifier "d"})}, Null;
    
    "Function application of lambda", "(\\a.a) null -> null",
    FuncApp ( Lambda { LambdaParam = "a"; LambdaBody = Identifier "a";}, Null), Null;
    "Nested lambda internal reduction", "(\\a.(\\b.b)Null) -> (\\a.Null)", 
    Lambda { LambdaParam = "a"; LambdaBody = FuncApp (Lambda { LambdaParam = "b"; LambdaBody = Identifier "b";}, Null );},
    Lambda { LambdaParam = "a"; LambdaBody = Null;};

    "+", "7+8 -> 15", FuncApp (FuncApp (BuiltInFunc Plus, intL 7),intL 8), intL 15;
    "-", "5-3 ->  2", FuncApp (FuncApp (BuiltInFunc Minus, intL 5),intL 3), intL 2;

    "Simple recursion", "let f c = if c then Null else (f true) in f false -> Null",
    simpleRecAST <| boolL false, Null;
    "Recursion - factorial (basecase)", "factorial 0 -> 1",
    factorialAST <| FuncApp (Identifier "factorial", intL 0), intL 1;
    "Recursion - factorial 5", "factorial 5 -> 120",
    factorialAST <| FuncApp (Identifier "factorial", intL 5), intL 120;
    "Recursion - factorial 10", "factorial 10 -> 120", 
    factorialAST <| FuncApp (Identifier "factorial", intL 11), intL 39916800;
    
    ] 
    |> List.map (fun (n,d,i,o) -> (n,d,i,Ok o))


/// test that should return (Error string)
let testErr : TestInfo= 
    [
    "Identifier", "foo", Identifier "foo", "Identifier \'foo\' is not defined";
    "Function Application List", "[]", FuncAppList [], "What? parser returned FuncAppList";
    "Identifier List", "[]", IdentifierList [],"What? parser returned IdentifierList";
    "Lambda \\a.b", "\\a.b", Lambda { LambdaParam = "a"; LambdaBody = Identifier "b"},"Identifier \'b\' is not defined";
    ]
    |> List.map (fun (n,d,i,o) -> (n,d,i,Error o))

let makeTest f (name, description, input, output) =
    test name { Expect.equal (f input) output description}

[<Tests>]
let tests =
    testId 
    |> List.append testOk
    |> List.append testErr 
    |> List.map (makeTest runAst) 
    |> testList "Hardwired" 
 

let allTests() =
    runTestsInAssembly defaultConfig [||] |> ignore

[<EntryPoint>]
let main argv =
    allTests()
    Console.ReadKey() |> ignore
    0 
