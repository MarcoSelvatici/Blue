open System
open Expecto
open TestLib
open Parser
open ParserTest
open TypeChecker
open TypeCheckerTest

open BetaEngine
open TestBetaEngine

[<Tests>]
let parserTest = createTestList "Parser Tests" parse testCasesParser
[<Tests>]
let TypeCheckerTest = createTestList "Type Checker Tests" typeCheck testCasesTypeChecker
[<Tests>]
let BetaEngineTest = createTestList "Beta Engine Tests" runAst testCasesBetaEngine
// TODO (oliver, fabio): Add your own unit tests.

[<EntryPoint>]
let main argv =
    runTests()
    Console.ReadKey() |> ignore
    0
