module EmulatorTop

open System
open Expecto
open TestLib
open Parser
open ParserTest
open TypeChecker
open TypeCheckerTest
open BetaEngine
open TestBetaEngine
open SKIRuntime
open TestSKIRuntime

//[<Tests>]
//let lexerTest = createTestList "Lexer Tests" tokeniseT3 testCasesLexer
//[<Tests>]
//let parserTest = createTestList "Parser Tests" parse testCasesParser
//[<Tests>]
//let typeCheckerTest = createTestList "Type Checker Tests" typeCheck testCasesTypeChecker
//[<Tests>]
//let betaEngineTest = createTestList "Beta Engine Tests" runAst testCasesBetaEngine
//[<Tests>]
//let skiRuntimeTest = createTestList "SKI Runtime Tests" combinatorRuntime testCasesSKIRuntime

let maybeTypeCheck checkTypes ast =
    let printAndReturnUnchanged t =
        printfn "\nTYPE: %A" t |> ignore
        Ok ast // Return the unchanged ast
    if not checkTypes
    then Ok ast
    else typeCheck ast |> Result.bind printAndReturnUnchanged 

let reduce ast =
    runAst ast//, combinatorRuntime ast

let end2end checkTypes input =
    input
    //|> tokeniseT3
    //|> Result.bind parse
    |> parse
    |> Result.bind (maybeTypeCheck checkTypes)
    |> Result.bind reduce

//[<EntryPoint>]
//let main argv =
//    runTests()
//    //end2end true "(\x.x+10) 12"
//    Console.ReadKey() |> ignore
//    0
