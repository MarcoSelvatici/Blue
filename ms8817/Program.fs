open System
open Expecto
open Parser
open TokeniserStub

/// Like buildError, but allows to manually set the trace.
let buildErrorManually msg trace unmatchedTokens currentAsts = 
    Error {
        msg = msg;
        parseTrace = trace;
        unmatchedTokens = unmatchedTokens;
        currentAsts = currentAsts;
    }

// TODO It does not make sense to test all the failure messages now because they
// are not completely designed yet.
let testCases = [
    "Empty", [],
        buildErrorManually "failed: buildFuncAppTree. Expected expression" "" [] [];
    "Simple identifier", [TIdentifier "a"],
        Ok (Identifier "a");
    "Simple literal", [TLiteral (IntLit 42)],
        Ok (Literal (IntLit 42));
    "Double literal", [TLiteral (IntLit 42); TLiteral (IntLit 3)],
        Ok (FuncApp (Literal (IntLit 42), Literal (IntLit 3)));
    "Simple roundExp", [KOpenRound; TLiteral (IntLit 42); KCloseRound],
        Ok (Literal (IntLit 42));
    "Nested roundExp", [KOpenRound; KOpenRound; TLiteral (IntLit 42); KCloseRound; KCloseRound],
        Ok (Literal (IntLit 42));
    "Simple lambda", [KLambda; TIdentifier "x"; KDot; TLiteral (IntLit 42)],
        Ok (buildLambda "x" (Literal (IntLit 42)));
    "Curried lambda", [KLambda; TIdentifier "x";  TIdentifier "y"; KDot; TLiteral (IntLit 42)],
        Ok (buildLambda "x" (buildLambda "y" (Literal (IntLit 42))));
    "Invalid lambda, no arguments", [KLambda; KDot; TLiteral (IntLit 2)],
        buildErrorManually "failed: pLambdaExp. Invalid empty argument list" "" [] [];
    "Lambda and Dot", [KLambda; TIdentifier "x"; KDot; TLiteral (IntLit 42); KDot],
        buildErrorManually "failed: pToken KLet" "in x " [KDot] [Literal (IntLit 42); IdentifierList ["x"]];
    "IfExp", [KIf; TIdentifier "x"; KThen; TIdentifier "y"; KElse; TIdentifier "z"; KFi],
        Ok (IfExp (Identifier "x", Identifier "y", Identifier "z"));
    "Miss Cond exp", [KIf; KThen; TIdentifier "y"; KElse; TIdentifier "z"; KFi],
        buildErrorManually "failed: buildFuncAppTree. Expected expression" "" [KThen; TIdentifier "y"; KElse; TIdentifier "z"; KFi] [];
    "Miss Then exp", [KIf; TIdentifier "y"; KThen; KElse; TIdentifier "z"; KFi],
        buildErrorManually "failed: buildFuncAppTree. Expected expression" "" [KElse; TIdentifier "z"; KFi] [Identifier "y"];
    "Miss Else exp", [KIf; TIdentifier "y"; KThen; TIdentifier "z"; KElse; KFi],
        buildErrorManually "failed: buildFuncAppTree. Expected expression" "" [KFi] [Identifier "z"; Identifier "y"];
    "Simple SeqExp", [KOpenSquare; TIdentifier "x"; KComma; TIdentifier "y"; KCloseSquare],
        Ok (SeqExp (Identifier "x", Identifier "y"));
    "Miss first Exp", [KOpenSquare; KComma; TIdentifier "y"; KCloseSquare],
        buildErrorManually "failed: buildFuncAppTree. Expected expression" "" [KComma; TIdentifier "y"; KCloseSquare] []
    "Simple FuncApp Lit", [TIdentifier "f"; TLiteral (IntLit 42)],
        Ok (FuncApp (Identifier "f", Literal (IntLit 42)));
    "Simple FuncApp Ident", [TIdentifier "f"; TIdentifier "x"],
        Ok (FuncApp (Identifier "f", Identifier "x"));
    "Simple FuncApp Bracket", [KOpenRound; TIdentifier "f"; TLiteral (IntLit 42); KCloseRound],
        Ok (FuncApp (Identifier "f", Literal (IntLit 42)));
    "Bracketed arg", [TIdentifier "g"; KOpenRound; TIdentifier "f"; TLiteral (IntLit 42); KCloseRound],
        Ok (FuncApp (Identifier "g", FuncApp (Identifier "f", Literal (IntLit 42))));
    "Bracketed fun", [KOpenRound; TIdentifier "f"; TLiteral (IntLit 42); KCloseRound; TIdentifier "g"],
        Ok (FuncApp (FuncApp (Identifier "f", Literal (IntLit 42)), Identifier "g"));
    "Empty brackets", [KOpenRound; KCloseRound],
        buildErrorManually "failed: buildFuncAppTree. Expected expression" "" [KCloseRound] [];
    "Long exp list, left associative", List.map (fun i -> (TIdentifier <| sprintf "%d" i)) [1; 2; 3; 4; 5],
        Ok (FuncApp (FuncApp (FuncApp (FuncApp (Identifier "1", Identifier "2"), Identifier "3"), Identifier "4" ), Identifier "5"))
    "Simple let in", [KLet; TIdentifier "f"; KEq; TLiteral (IntLit 2); KIn; TIdentifier "f"; KNi],
        Ok (FuncDefExp {FuncName = "f"; FuncBody = Literal (IntLit 2); Rest = Identifier "f";});
    "Double let in", [KLet; TIdentifier "f"; KEq; TLiteral (IntLit 2); KIn; KLet; TIdentifier "g"; KEq; TLiteral (StringLit "aaa"); KIn; TIdentifier "z"; KNi; KNi],
        Ok (FuncDefExp {FuncName = "f"; FuncBody = Literal (IntLit 2); Rest = FuncDefExp {FuncName = "g"; FuncBody = Literal (StringLit "aaa"); Rest = Identifier "z";};});
    "Triple let in", [KLet; TIdentifier "f"; KEq; KLet; TIdentifier "fun"; KEq; TLiteral (IntLit 2); KIn; TIdentifier "p"; KNi; KIn; KLet; TIdentifier "g"; KEq; TLiteral (StringLit "aaa"); KIn; TIdentifier "z"; KNi; KNi],
        Ok (FuncDefExp {FuncName = "f"; FuncBody = FuncDefExp {FuncName = "fun"; FuncBody = Literal (IntLit 2); Rest = Identifier "p";}; Rest = FuncDefExp {FuncName = "g"; FuncBody = Literal (StringLit "aaa"); Rest = Identifier "z";};});
    "Lambda let in", [KLet; TIdentifier "x"; TIdentifier "y"; TIdentifier "z"; KEq; TLiteral (IntLit 2); KIn; TIdentifier "x"; KNi],
        Ok (FuncDefExp {FuncName = "x"; FuncBody = buildLambda "y" (buildLambda "z" (Literal (IntLit 2))); Rest = Identifier "x";});
    "Simple addition 2+3", [TLiteral (IntLit 2); TBuiltInFunc Plus; TLiteral (IntLit 3)],
        Ok (FuncApp (FuncApp (BuiltInFunc Plus, Literal (IntLit 2)), Literal (IntLit 3)));
    "Simple arithmetic 2+3*4-5<6", [TLiteral (IntLit 2); TBuiltInFunc Plus; TLiteral (IntLit 3); TBuiltInFunc Mult; TLiteral (IntLit 4); TBuiltInFunc Minus; TLiteral (IntLit 5); TBuiltInFunc Less; TLiteral (IntLit 6)],
        Ok (FuncApp (FuncApp (BuiltInFunc Less, FuncApp (FuncApp (BuiltInFunc Plus,Literal (IntLit 2)), FuncApp (FuncApp (BuiltInFunc Minus, FuncApp (FuncApp (BuiltInFunc Mult,Literal (IntLit 3)), Literal (IntLit 4))),Literal (IntLit 5)))), Literal (IntLit 6)));
    "Simple arithmetic 1<2&&3>=4", [TLiteral (IntLit 1); TBuiltInFunc Less; TLiteral (IntLit 2); TBuiltInFunc And; TLiteral (IntLit 3); TBuiltInFunc GreaterEq; TLiteral (IntLit 4);],
        Ok (FuncApp ( FuncApp (BuiltInFunc And, FuncApp ( FuncApp (BuiltInFunc Less, Literal (IntLit 1)), Literal (IntLit 2))), FuncApp ( FuncApp (BuiltInFunc GreaterEq, Literal (IntLit 3)), Literal (IntLit 4))));
    "Simple arithmetic 1-2+3", [TLiteral (IntLit 1); TBuiltInFunc Minus; TLiteral (IntLit 2); TBuiltInFunc Plus; TLiteral (IntLit 3)],
        Ok (FuncApp ( FuncApp (BuiltInFunc Minus, Literal (IntLit 1)), FuncApp ( FuncApp (BuiltInFunc Plus, Literal (IntLit 2)), Literal (IntLit 3))));
    "Simple arithmetic 1=2<=3", [TLiteral (IntLit 1); TBuiltInFunc Equal; TLiteral (IntLit 2); TBuiltInFunc LessEq; TLiteral (IntLit 3)],
        Ok (FuncApp ( FuncApp (BuiltInFunc Equal, Literal (IntLit 1)), FuncApp (FuncApp (BuiltInFunc LessEq, Literal (IntLit 2)), Literal (IntLit 3))));
    "Incomplete arithmetic 1+", [TLiteral (IntLit 1); TBuiltInFunc Plus],
        buildErrorManually "failed: buildFuncAppTree. Expected expression" "" [] [];
    "Incomplete arithmetic /1", [TBuiltInFunc Div; TLiteral (IntLit 1)],
        buildErrorManually "failed: buildFuncAppTree. Expected expression" "" [] [];
    "Incomplete arithmetic *1&&", [TBuiltInFunc Mult; TLiteral (IntLit 1); TBuiltInFunc And],
        buildErrorManually "failed: buildFuncAppTree. Expected expression" "" [] [];
    "Simple builtin", [TBuiltInFunc Head; TIdentifier "l"],
        Ok (FuncApp (BuiltInFunc Head, Identifier "l"));
    "Double builtin", [TBuiltInFunc Head; TBuiltInFunc Tail; TIdentifier "l"],
        Ok (FuncApp (FuncApp (BuiltInFunc Head, BuiltInFunc Tail), Identifier "l"));
    "Bracketed Double builtin", [TBuiltInFunc Head; KOpenRound; TBuiltInFunc Tail; TIdentifier "l"; KCloseRound],
        Ok (FuncApp (BuiltInFunc Head, (FuncApp (BuiltInFunc Tail, Identifier "l"))));
    "Double trace error", [KLet; TIdentifier "x"; KEq; TLiteral (IntLit 2); KIn; KLet; TIdentifier "y"; KEq; TLiteral (IntLit 3); KIn; KDot; KNi; KNi],
        buildErrorManually "failed: pToken KLet" "in y in x " [KDot; KNi; KNi] [Literal (IntLit 3); IdentifierList ["y"]; Literal (IntLit 2); IdentifierList ["x"]]
    "Single trace error", [KLet; TIdentifier "x"; KEq; TLiteral (IntLit 2); KIn; KLet; TIdentifier "y"; KEq; TLiteral (IntLit 3); KIn; TIdentifier "y"; KNi; KDot; KNi],
        buildErrorManually "failed: pToken KLet" "in x " [KDot; KNi] [FuncDefExp {FuncName = "y"; FuncBody = Literal (IntLit 3); Rest = Identifier "y" }; Literal (IntLit 2); IdentifierList ["x"]]
    "Simple program", [KLet; TIdentifier "x"; TIdentifier "y"; KEq; TIdentifier "x"; TBuiltInFunc Plus; TIdentifier "y"; KIn; KLet; TIdentifier "z"; KEq; KLambda; TIdentifier "a"; TIdentifier "b"; KDot; TIdentifier "a"; TBuiltInFunc Less; TIdentifier "b"; TBuiltInFunc And; TIdentifier "z"; KIn; TIdentifier "x"; KOpenRound; TIdentifier "z"; TLiteral (IntLit 1); TLiteral (IntLit 2); KCloseRound; KNi; KNi],
        Ok (FuncDefExp {FuncName = "x"; FuncBody = buildLambda "y" (FuncApp (FuncApp (BuiltInFunc Plus, Identifier "x"), Identifier "y")); Rest =        FuncDefExp {FuncName = "z"; FuncBody = buildLambda "a" ( buildLambda "b" (FuncApp (FuncApp (BuiltInFunc And, FuncApp (FuncApp (BuiltInFunc Less, Identifier "a"), Identifier "b")), Identifier "z")) ); Rest = FuncApp(Identifier "x", FuncApp(FuncApp (Identifier "z", Literal (IntLit 1)), Literal (IntLit 2)))}});
    "Longest match", [KLet; TIdentifier "x"; TIdentifier "y"; KEq; TIdentifier "x"; TBuiltInFunc Div; TIdentifier "y"; KNi; TIdentifier "x"; KNi],
        buildErrorManually "failed: pToken KIn" "in x " [KNi; TIdentifier "x"; KNi] [FuncApp (FuncApp (BuiltInFunc Div, Identifier "x"), Identifier "y"); IdentifierList ["x"; "y"]];
    "Lambda with let in", [KLambda; TIdentifier "x"; KDot; KLet; TIdentifier "y"; KEq; TLiteral (BoolLit true); KIn; TIdentifier "x"; TBuiltInFunc Mult; TLiteral (IntLit 2); KNi; TIdentifier "y"],
        Ok (buildLambda "x" (FuncApp ((FuncDefExp {FuncName="y"; FuncBody=Literal (BoolLit true); Rest=FuncApp (FuncApp (BuiltInFunc Mult, Identifier "x"), Literal (IntLit 2))}), Identifier "y")));
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
