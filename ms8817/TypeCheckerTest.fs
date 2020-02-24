open Expecto
open TokeniserStub
open Parser
open TypeChecker

let testCases = [
    "Simple Int", Literal (IntLit 1),
        Ok (Base Int);
    "Simple Identifer", Identifier "x",
        Error "Identifier x is not bound";
    "Simple if", IfExp (Literal (BoolLit true), Literal (IntLit 1), Literal (IntLit 2)),
        Ok (Base Int);
    "Simple if mismatch", IfExp (Literal (BoolLit true), Literal (StringLit "a"), Literal (IntLit 2)),
        Error "Types Base String and Base Int are not compatable";
    "Simple if non-bool", IfExp (Literal (IntLit 1), Literal (StringLit "a"), Literal (StringLit "b")),
        Error "Types Base Int and Base Bool are not compatable";
    "Nested if", IfExp (Literal (BoolLit false), IfExp (Literal (BoolLit true), Literal (StringLit "x"), Literal (StringLit "y")), Literal (StringLit "a")),
        Ok (Base String);
    "Nested if mismatch", IfExp (Literal (BoolLit false), IfExp (Literal (BoolLit true), Literal (IntLit 3), Literal (IntLit 4)), Literal (StringLit "a")),
        Error "Types Base Int and Base String are not compatable";
    "Plus", FuncApp (FuncApp (BuiltInFunc Plus, Literal (IntLit 2)), Literal (IntLit 3)),
        Ok (Base Int);
    "Plus mismatch", FuncApp (FuncApp (BuiltInFunc Plus, Literal (IntLit 2)), Literal (BoolLit true)),
        Error "Types Base Int and Base Bool are not compatable";
    "Plus bools", FuncApp (FuncApp (BuiltInFunc Plus, Literal (BoolLit false)), Literal (BoolLit true)),
        Error "Types Base Int and Base Bool are not compatable"
]

let testTypeChecker (description, ast, expected) =
    testCase description <| fun () ->
        let actual = typeCheck ast
        Expect.equal actual expected ""

[<Tests>]
let tests = testList "Type checker test" <| List.map testTypeChecker testCases

let testAll() =
    runTestsInAssembly defaultConfig [||] |> ignore

[<EntryPoint>]
let main argv =
    testAll()
    0