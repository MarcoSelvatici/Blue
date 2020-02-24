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
        Error "Types Base Int and Base Bool are not compatable";
    "Greater than", FuncApp (FuncApp (BuiltInFunc Greater, Literal (IntLit 2)), Literal (IntLit 3)),
        Ok (Base Bool);
    "Less than mismatch", FuncApp (FuncApp (BuiltInFunc Less, Literal (BoolLit false)), Literal (IntLit 3)),
        Error "Types Base Int and Base Bool are not compatable";
    "Logical and", FuncApp (FuncApp (BuiltInFunc And, Literal (BoolLit false)), Literal (BoolLit true)),
        Ok (Base Bool);
    "Logical or mismatch", FuncApp (FuncApp (BuiltInFunc Or, Literal (BoolLit false)), Literal (IntLit 3)),
        Error "Types Base Bool and Base Int are not compatable";
    "Complex if exp `if 2<3 then 2-3 else 2/3`", IfExp (FuncApp (FuncApp (BuiltInFunc Less, Literal (IntLit 2)), Literal (IntLit 3)), FuncApp (FuncApp (BuiltInFunc Minus, Literal (IntLit 2)), Literal (IntLit 3)), FuncApp (FuncApp (BuiltInFunc Div, Literal (IntLit 2)), Literal (IntLit 3))),
        Ok (Base Int);
    "Simple lambda", buildLambda "x" (Identifier "x"),
        Ok (Fun(Gen 0, Gen 0));
    "Simple lambda int", buildLambda "x" (Literal (IntLit 1)),
        Ok (Fun(Gen 0, Base Int));
    "Simple lambda int plus", buildLambda "x" (FuncApp (FuncApp (BuiltInFunc Plus, Identifier "x"), Literal (IntLit 3))),
        Ok (Fun(Base Int, Base Int));
    "Simple lambda plus", buildLambda "x" (FuncApp (FuncApp (BuiltInFunc Plus, Identifier "x"), Identifier "x")),
        Ok (Fun(Base Int, Base Int));
    "Simple lambda unbound", buildLambda "x" (FuncApp (FuncApp (BuiltInFunc Plus, Identifier "x"), Identifier "y")),
        Error "Identifier y is not bound";
    "Nested lambdas", buildLambda "x" (buildLambda "y" (Identifier "y")),
        Ok (Fun(Gen 0, Fun(Gen 1, Gen 1)));
    "Simple partial application (\x. \y. y*x) 2", FuncApp (buildLambda "x" (buildLambda "y" (FuncApp (FuncApp (BuiltInFunc Mult, Identifier "y"), Identifier "x"))), Literal(IntLit 2)),
        Ok (Fun(Base Int, Base Int));
    "Simple partial application bool (\x. \y. y<x) 2", FuncApp (buildLambda "x" (buildLambda "y" (FuncApp (FuncApp (BuiltInFunc Less, Identifier "y"), Identifier "x"))), Literal(IntLit 2)),
        Ok (Fun(Base Int, Base Bool));
    "Simple partial application mismatch (\x. \y. y && x) 2", FuncApp (buildLambda "x" (buildLambda "y" (FuncApp (FuncApp (BuiltInFunc And, Identifier "y"), Identifier "x"))), Literal(IntLit 2)),
        Error "Types Base Bool and Base Int are not compatable";
    "Simple partial application not bound (\x. \y. y*x) x", FuncApp (buildLambda "x" (buildLambda "y" (FuncApp (FuncApp (BuiltInFunc Mult, Identifier "y"), Identifier "x"))), Identifier "x"),
        Error "Identifier x is not bound";
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