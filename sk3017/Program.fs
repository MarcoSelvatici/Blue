open System
open TokeniserParserStub
open BetaEngine
open Expecto

// TODO: change name
// test that should return AST
let testOk = 
    [ 
    "Literal 6", "6 -> 6", Literal ( IntLit 6 ), Literal ( IntLit 6 );
    "Literal true", "true -> true", Literal (BoolLit true), Literal (BoolLit true);
    "Literal \"abc\"", "\"abc\" -> \"abc\"" , Literal (StringLit "abc"), Literal (StringLit "abc");
    "Null", "Null -> Null", Null, Null;
    // builtin
    // sequence
    // roundexp
    // ifexp
    // FuncDefExp
    ] 
    |> List.map (fun (a,b,c,d) -> (a,b,c,Ok d))


// test that should return error
let testErr = 
    [
    "Identifier", "foo", Identifier "foo", "Identifier \'foo\' is not defined";
    "Function Application List", "[]", FuncAppList [], "What? parser returned FuncAppList";
    "Identifier List", "[]", IdentifierList [],"What? parser returned IdentifierList";      
    ]
    |> List.map (fun (a,b,c,d) -> (a,b,c,Error d))

let makeTest f (name, description, input, output) =
    test name { Expect.equal (f input) output description}

[<Tests>]
let tests =
    List.append testOk testErr |>
    List.map (makeTest runAst) |>
    testList "Hardwired" 
 

let allTests() =
    runTestsInAssembly defaultConfig [||] |> ignore

[<EntryPoint>]
let main argv =
    allTests()
    Console.ReadKey() |> ignore
    0 
