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

[<Tests>]
let toktest7 =
    testCase "testing rowCount and comments functionalities" <| fun () ->
        Expect.equal (tokeniseT3 "let hd = head lst
                                  //dfmdsfodpmfdpfmdpfmdsfmdpmfodsfmpdsfmdsomf;;;d20391-30239#'#zx.;
                                  (* 23-94\\\\\\qwlpewwewewpelq[eweeeeee
                                  
                                  fjdkfj #### sdsd### MULTILINE COMMENT ### 
                                  let i = wow.
                                  *)

                                  let tl_ = tail lst
                                  let hd' = head lst
                                  let _tl = tail lst
                                  let tl_2 = tail lst
                                  let hd_a = head lst
                                  let tl2a = tail lst
                                  let tl
                                    = 
                                    tail lst") [KLet; TIdentifier "hd"; KEq; TBuiltInFunc BHead; TIdentifier "lst"; KLet;
                                                TIdentifier "tl_"; KEq; TBuiltInFunc BTail; TIdentifier "lst"; KLet;
                                                TIdentifier "hd'"; KEq; TBuiltInFunc BHead; TIdentifier "lst"; KLet;
                                                TIdentifier "_tl"; KEq; TBuiltInFunc BTail; TIdentifier "lst"; KLet;
                                                TIdentifier "tl_2"; KEq; TBuiltInFunc BTail; TIdentifier "lst"; KLet;
                                                TIdentifier "hd_a"; KEq; TBuiltInFunc BHead; TIdentifier "lst"; KLet;
                                                TIdentifier "tl2a"; KEq; TBuiltInFunc BTail; TIdentifier "lst"; KLet;
                                                TIdentifier "tl"; KEq; TBuiltInFunc BTail; TIdentifier "lst"]
                      "testing lexing for head/tail: let hd = head lst
                                                     //dfmdsfodpmfdpfmdpfmdsfmdpmfodsfmpdsfmdsomf;;;d20391-30239#'#zx.;
                                                     let tl_ = tail lst
                                                     let hd' = head lst
                                                     let _tl = tail lst
                                                     let tl_2 = tail lst
                                                     let hd_a = head lst
                                                     let tl2a = tail lst
                                                     let tl
                                                       = 
                                                       tail lst"

[<Tests>]
let toktest8 =
    testCase "testing small program" <| fun () ->
        Expect.equal (tokeniseT3 "let a =
                                  let b = \x. x * 2 in
                                    \c. b (b c)
                                  in
                                  a 5") [KLet; TIdentifier "a"; KEq; KLet; TIdentifier "b"; KEq; KLambda;
                                         TIdentifier "x"; KDot; TIdentifier "x"; TBuiltInFunc BMult; TLiteral (IntLit 2);
                                         KIn; KLambda; TIdentifier "c"; KDot; TIdentifier "b"; KOpenRound;
                                         TIdentifier "b"; TIdentifier "c"; KCloseRound; KIn; TIdentifier "a";
                                         TLiteral (IntLit 5)]
                      "testing lexing for small program: let a =
                                                         let b = \x. x * 2 in
                                                            \c. b (b c)
                                                         in
                                                         a 5"

[<Tests>]
let toktest9 =
    testCase "testing list builtin functions" <| fun () ->
        Expect.equal (tokeniseT3 "let a = ['a'; 'b']
                                  let b = implode a
                                  let c = \"ab\"
                                  let d = strEq b c
                                  let e = explode c
                                  let f = size e
                                  let rec yo lst = 
                                    let hd = head lst
                                    append hd global
                                    let tl = tail lst
                                    yo tl") 
                                        [KLet; TIdentifier "a"; KEq; KOpenSquare; TLiteral (CharLit 'a'); KSemiColon;
                                         TLiteral (CharLit 'b'); KCloseSquare; KLet; TIdentifier "b"; KEq;
                                         TBuiltInFunc BImplode; TIdentifier "a"; KLet; TIdentifier "c"; KEq;
                                         TLiteral (StringLit "ab"); KLet; TIdentifier "d"; KEq; TBuiltInFunc BStrEq;
                                         TIdentifier "b"; TIdentifier "c"; KLet; TIdentifier "e"; KEq;
                                         TBuiltInFunc BExplode; TIdentifier "c"; KLet; TIdentifier "f"; KEq;
                                         TBuiltInFunc BSize; TIdentifier "e"; KLet; KRec; TIdentifier "yo";
                                         TIdentifier "lst"; KEq; KLet; TIdentifier "hd"; KEq; TBuiltInFunc BHead;
                                         TIdentifier "lst"; TBuiltInFunc BAppend; TIdentifier "hd"; TIdentifier "global";
                                         KLet; TIdentifier "tl"; KEq; TBuiltInFunc BTail; TIdentifier "lst";
                                         TIdentifier "yo"; TIdentifier "tl"]
                        "testing lexing for list builtins: let a = ['a'; 'b']
                                                           let b = implode a
                                                           let c = \"ab\"
                                                           let d = strEq b c
                                                           let e = explode c
                                                           let f = size e
                                                           let recl yo lst = 
                                                             let hd = head lst
                                                             append hd global
                                                             let tl = tail lst
                                                             yo t"

[<Tests>]
let toktest10=
    testCase "testing if then else and booleans" <| fun () ->
        Expect.equal (tokeniseT3 "let a = true
                                  if a 
                                  then true 
                                  else false
                                  fi") [KLet; TIdentifier "a"; KEq; TLiteral (BoolLit true); KIf; TIdentifier "a";
                                        KThen; TLiteral (BoolLit true); KElse; TLiteral (BoolLit false); KFi]
                    "testing if then else and booleans: let a = true
                                                        if a 
                                                        then true 
                                                        else false
                                                        fi"

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
            toktest7
            toktest8
            toktest10
        ]
    runTests defaultConfig testLst |> ignore