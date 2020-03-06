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

[<Tests>]
let preprocessorTest = createTestList "Preprocessor Tests" preprocess testCasesPreprocessor
[<Tests>]
let lexerTest = createTestList "Lexer Tests" tokeniseT3 testCasesLexer
[<Tests>]
let parserTest = createTestList "Parser Tests" parse testCasesParser
[<Tests>]
let typeCheckerTest = createTestList "Type Checker Tests" typeCheck testCasesTypeChecker
[<Tests>]
let betaEngineTest = createTestList "Beta Engine Tests" runAst testCasesBetaEngine
[<Tests>]
let skiRuntimeTest = createTestList "SKI Runtime Tests" combinatorRuntime testCasesSKIRuntime



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
