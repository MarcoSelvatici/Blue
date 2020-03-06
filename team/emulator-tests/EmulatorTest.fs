open System
open Expecto
open TestLib
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
open E2ETest
 

// Unit testing.
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
[<Tests>]
let e2eTests = e2eCreateTestList "E2E Tests" combinatorRuntime parse tokeniseT3 testCasesE2E




[<EntryPoint>]
let main argv =
    runTests()
    Console.ReadKey() |> ignore
    0
