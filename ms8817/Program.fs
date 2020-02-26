// Author: ms8817 (Marco Selvatici)

open Expecto
open TestLib
open Parser
open ParserTest
open TypeChecker
open TypeCheckerTest

[<Tests>]
let t1 = createTestList "Parser Tests" parse testCasesParser
[<Tests>]
let t2 = createTestList "Type Checker Tests" typeCheck testCasesTypeChecker

[<EntryPoint>]
let main argv =
    runTests()
    0
