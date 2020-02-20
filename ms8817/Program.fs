open System
open Expecto
open Parser
open TokeniserStub

// TODO It does not make sense to test all the failure messages now because they
// are not completely designed yet.
let testCases = [
    "Single identifier", [TIdentifier "a"],
        Ok (Identifier "a");
    "Single literal", [TLiteral (IntLit 42)],
        Ok (Literal (IntLit 42));
    "Double literal", [TLiteral (IntLit 42); TLiteral (IntLit 3)],
        buildError "failed: top level" [TLiteral (IntLit 3)] [Literal (IntLit 42)];
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
    "Lambda and Dot", [KLambda; TIdentifier "x"; KDot; TLiteral (IntLit 42); KDot],
        buildError "failed: top level" [KDot] [buildLambda "x" (Literal (IntLit 42))];
    "IfExp", [KIf; TIdentifier "x"; KThen; TIdentifier "y"; KElse; TIdentifier "z"; KFi],
        Ok (IfExp (Identifier "x", Identifier "y", Identifier "z"));
    //"Miss Cond exp", [KIf; KThen; TIdentifier "y"; KElse; TIdentifier "z"; KFi],
    //    Error...
    "SeqExp", [KOpenSquare; TIdentifier "x"; KComma; TIdentifier "y"; KCloseSquare],
        Ok (SeqExp (Identifier "x", Identifier "y"));
    //"Miss first Exp", [KOpenSquare; KComma; TIdentifier "y"; KCloseSquare],
    //    Error...
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
