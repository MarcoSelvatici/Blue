open System
open Expecto
open Parser
open TokeniserStub

// TODO It does not make sense to test all the failure messages now because they
// are not completely designed yet.
let testCases = [
    "Simple identifier", [TIdentifier "a"],
        Ok (Identifier "a");
    "Simple literal", [TLiteral (IntLit 42)],
        Ok (Literal (IntLit 42));
    "Double literal", [TLiteral (IntLit 42); TLiteral (IntLit 3)],
        Ok (FuncApp (Literal (IntLit 42), Literal (IntLit 3)));
    "Simple roundExp", [KOpenRound; TLiteral (IntLit 42); KCloseRound],
        Ok (RoundExp (Literal (IntLit 42)));
    "Nested roundExp", [KOpenRound; KOpenRound; TLiteral (IntLit 42); KCloseRound; KCloseRound],
        Ok (RoundExp (RoundExp (Literal (IntLit 42))));
    "Simple lambda", [KLambda; TIdentifier "x"; KDot; TLiteral (IntLit 42)],
        Ok (buildLambda "x" (Literal (IntLit 42)));
    "Curried lambda", [KLambda; TIdentifier "x";  TIdentifier "y"; KDot; TLiteral (IntLit 42)],
        Ok (buildLambda "x" (buildLambda "y" (Literal (IntLit 42))));
    "Invalid lambda, no arguments", [KLambda; KDot; TLiteral (IntLit 2)],
        buildError "failed: parseLambda. Invalid empty argument list" [] [];
    //"Lambda and Dot", [KLambda; TIdentifier "x"; KDot; TLiteral (IntLit 42); KDot],
    //    buildError "failed: top level" [KDot] [buildLambda "x" (Literal (IntLit 42))];
    "IfExp", [KIf; TIdentifier "x"; KThen; TIdentifier "y"; KElse; TIdentifier "z"; KFi],
        Ok (IfExp (Identifier "x", Identifier "y", Identifier "z"));
    //"Miss Cond exp", [KIf; KThen; TIdentifier "y"; KElse; TIdentifier "z"; KFi],
    //    Error...
    "Simple SeqExp", [KOpenSquare; TIdentifier "x"; KComma; TIdentifier "y"; KCloseSquare],
        Ok (SeqExp (Identifier "x", Identifier "y"));
    //"Miss first Exp", [KOpenSquare; KComma; TIdentifier "y"; KCloseSquare],
    //    Error...
    "Simple FuncApp Lit", [TIdentifier "f"; TLiteral (IntLit 42)],
        Ok (FuncApp (Identifier "f", Literal (IntLit 42)));
    "Simple FuncApp Ident", [TIdentifier "f"; TIdentifier "x"],
        Ok (FuncApp (Identifier "f", Identifier "x"));
    "Simple FuncApp Bracket", [KOpenRound; TIdentifier "f"; TLiteral (IntLit 42); KCloseRound],
        Ok (RoundExp (FuncApp (Identifier "f", Literal (IntLit 42))));
    "Bracketed arg", [TIdentifier "g"; KOpenRound; TIdentifier "f"; TLiteral (IntLit 42); KCloseRound],
        Ok (FuncApp (Identifier "g", RoundExp (FuncApp (Identifier "f", Literal (IntLit 42)))));
    "Bracketed fun", [KOpenRound; TIdentifier "f"; TLiteral (IntLit 42); KCloseRound; TIdentifier "g"],
        Ok (FuncApp (RoundExp (FuncApp (Identifier "f", Literal (IntLit 42))), Identifier "g"));
    "Long exp list", List.replicate 5 (TIdentifier "f"), // Wrong associativity!
        Ok (FuncApp (Identifier "f", FuncApp (Identifier "f", FuncApp (Identifier "f", (FuncApp (Identifier "f", Identifier "f"))))));
]

let testParser (description, tkns, expected) =
    testCase description <| fun () ->
        let actual = parse tkns
        Expect.equal actual expected ""

[<Tests>]
let tests = testList "Parser test" <| List.map testParser testCases

let testAll() =
    runTestsInAssembly defaultConfig [||] |> ignore

[<EntryPoint>]
let main argv =
    testAll()
    0
