open System
open Expecto
open TestLib
open Preprocessor
open PreprocessorTest
open Lexer
open LexerTest
open Parser
open ParserTest
open TypeChecker
open TypeCheckerTest
open BetaEngine
open TestBetaEngine
open SKIRuntime
open TestSKIRuntime
open E2EBetaTest
open E2ESKITest
 

// Unit testing.
[<Tests>]
let preprocessorTest = createTestList "Preprocessor Tests" preprocess testCasesPreprocessor
[<Tests>]
let lexerTest = createTestList "Lexer Tests" tokeniseT3 testCasesLexer
[<Tests>]
let parserTest = createTestList "Parser Tests" parse testCasesParser
[<Tests>]
let typeCheckerTest = createTestList "Type Checker Tests" typeCheck testCasesTypeChecker
[<Tests>]
let prettyPrintTypeTest = createTestList "Pretty Print Type" type2String testCasesPrettyPrintType
[<Tests>]
let betaEngineTest = createTestList "Beta Engine Tests" runAst testCasesBetaEngine
[<Tests>]
let skiRuntimeTest = createTestList "SKI Runtime Tests" combinatorRuntime testCasesSKIRuntime
[<Tests>]
let e2eSKITests = e2eCreateTestList "E2E SKI Tests" combinatorRuntime typeCheck parse tokeniseT3 testCasesSKIE2E
[<Tests>]
let e2eBetaTests = e2eCreateTestList "E2E Beta Tests" runAst typeCheck parse tokeniseT3 testCasesBetaE2E

let print x =
    printfn "%A" x
    x

let printParsedAst input =
    input
    |> tokeniseT3
    |> Result.bind parse
    |> print
    |> ignore

[<EntryPoint>]
let main argv =
    runTests()
    0
