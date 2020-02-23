open System
open TokeniserParserStub
open BetaEngine
open Expecto

type TestInfo = (string * string * Ast * Result<Ast,string>) list

/// tests with the same input and output asts
let testId : TestInfo= 
    [ 
    "Literal int", "6", Literal ( IntLit 6 );
    "Literal bool", "true", Literal (BoolLit true);
    "Literal string", "\"abc\"" , Literal (StringLit "abc");
    "Null", "Null", Null;
    "BuiltInFunc >",">", BuiltInFunc Greater;
    "BuiltInFunc <","<", BuiltInFunc Less;
    "BuiltInFunc >=",">=", BuiltInFunc GreaterEq;
    "BuiltInFunc <=","<=", BuiltInFunc LessEq;
    "BuiltInFunc =","=", BuiltInFunc Greater;
    "Sequence (Null,Null)","(Null,Null)",SeqExp (Null,Null);
    ] 
    |> List.map (fun (n,d,i) -> (n,sprintf "%s -> %s" d d,i,Ok i))

/// tests that should return reduced (Ok ast)
let testOk : TestInfo= 
    [
    "Round expression (-10)", "(-10) -> -10", RoundExp (Literal (IntLit -10)), Literal (IntLit -10);
    "Nested Round expression (((Null)))", "(((Null)))", Null |> RoundExp |> RoundExp |> RoundExp, Null;
    "if true", "if true then \"abc\" else Null", 
    IfExp ((Literal (BoolLit true)), Literal (StringLit "abc"), Null), Literal (StringLit "abc"); 
    "if false", "if false then \"abc\" else Null", 
    IfExp ((Literal (BoolLit false)), Literal (StringLit "abc"), Null), Null;
    "Function Definition", "let c = Null in c -> Null",
    FuncDefExp {FuncName="c"; FuncBody=Null; Rest=Identifier "c"}, Null;
    "Nested Function Definition", "let c = Null in let d = c in d -> Null",
    FuncDefExp {FuncName="c"; FuncBody=Null; Rest=(FuncDefExp {FuncName="d"; FuncBody=Identifier "c"; Rest=Identifier "d"})}, Null;
    ] 
    |> List.map (fun (n,d,i,o) -> (n,d,i,Ok o))


/// test that should return (Error string)
let testErr : TestInfo= 
    [
    "Identifier", "foo", Identifier "foo", "Identifier \'foo\' is not defined";
    "Function Application List", "[]", FuncAppList [], "What? parser returned FuncAppList";
    "Identifier List", "[]", IdentifierList [],"What? parser returned IdentifierList";      
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
