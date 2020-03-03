open Expecto
open TestLib
open Parser
open ParserTest
open TypeChecker
open TypeCheckerTest

[<Tests>]
let parserTest = createTestList "Parser Tests" parse testCasesParser
[<Tests>]
let TypeCheckerTest = createTestList "Type Checker Tests" typeCheck testCasesTypeChecker
// TODO (oliver, fabio, szymon): Add your own unit tests.

[<EntryPoint>]
let main argv =
    runTests()
    0
