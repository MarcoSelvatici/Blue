open Lexer
open Expecto

// tokenise test
[<Tests>]
let toktest1 =
    testCase "testing keywords" <| fun () ->
        Expect.equal (tokeniseT3 ". , () [] \\") [KDot; KComma; KOpenRound; KCloseRound; KOpenSquare; KCloseSquare; KLambda] 
                      "testing lexing for keywords: . , () [] \\"

[<Tests>]
let toktest2 =
    testCase "testing arithmetic" <| fun () ->
        Expect.equal (tokeniseT3 "a +b* c/ d-e") [TIdentifier "a"; TBuiltInFunc BPlus; TIdentifier "b"; TBuiltInFunc BMult;
                                                  TIdentifier "c"; TBuiltInFunc BDiv; TIdentifier "d"; TBuiltInFunc BMinus; 
                                                  TIdentifier "e"] 
                      "testing lexing for arithmetic: a +b* c/ d-e"

[<Tests>]
let toktest3 =
    testCase "testing comparisons" <| fun () ->
        Expect.equal (tokeniseT3 "a > b<=c>=d== e>f") [TIdentifier "a"; TBuiltInFunc BGreater; TIdentifier "b"; TBuiltInFunc BLessEq;
                                                       TIdentifier "c"; TBuiltInFunc BGreaterEq; TIdentifier "d"; TBuiltInFunc BEqual;
                                                       TIdentifier "e"; TBuiltInFunc BGreater; TIdentifier "f"]
                      "testing lexing for comparisons: a > b<=c>=d== e>f"

[<Tests>]
let toktest4 =
    testCase "testing logic" <| fun () ->
        Expect.equal (tokeniseT3 "!a && b & c| d|| e") [TBuiltInFunc BNot; TIdentifier "a"; TBuiltInFunc BAnd; TIdentifier "b";
                                                        TBuiltInFunc BBitAnd; TIdentifier "c"; TBuiltInFunc BBitOr; TIdentifier "d";
                                                        TBuiltInFunc BOr; TIdentifier "e"]
                      "testing lexing for logic: !a && b & c| d|| e"

[<Tests>]
let toktest5 =
    testCase "testing number literals" <| fun () ->
        Expect.equal (tokeniseT3 "\\. 9 a 0.9 11.2340239") [KLambda; KDot; TLiteral (IntLit 9); TIdentifier "a"; TLiteral (FloatLit 0.9);
                                                               TLiteral (FloatLit 11.2340239)]
                      "testing lexing for number literals: 9 \\. a 0.9 11.2340239023"

[<Tests>]
let toktest6 =
    testCase "testing list functions" <| fun () ->
        Expect.equal (tokeniseT3 "let hd = head lst
                                  let tl = tail lst") [KLet; TIdentifier "hd"; KEq; TBuiltInFunc BHead; TIdentifier "lst"; KLet;
                                                       TIdentifier "tl"; KEq; TBuiltInFunc BTail; TIdentifier "lst"]
                      "testing lexing for head/tail: let hd = head lst
                                                     let tl = tail lst"



// Run this to run all current tests
let runAllTests =
    let testLst =
        testList "Tests" [
            toktest1
            toktest2
            toktest3
            toktest4
            toktest5
            toktest6
        ]
    runTests defaultConfig testLst |> ignore