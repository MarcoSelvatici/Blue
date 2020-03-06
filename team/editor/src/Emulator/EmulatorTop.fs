module EmulatorTop

open System
open Expecto
open TestLib
open Preprocessor
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

let selectRuntime runtime ast =
    if runtime // true == beta, false == ski
    then runAst ast
    else combinatorRuntime ast

let end2end checkTypes runtime input =
    input
    |> preprocess 
    |> Result.bind tokeniseT3
    |> Result.bind parse
    |> Result.bind (maybeTypeCheck checkTypes)
    |> Result.bind (selectRuntime runtime)
    |> prettyPrint

let getAst input =
    input
    |> tokeniseT3
    |> Result.bind parse

let getType input =
    input
    |> tokeniseT3
    |> Result.bind parse
    |> Result.bind typeCheck

//[<EntryPoint>]
//let main argv =
//    runTests()
//    //end2end true "(\x.x+10) 12"
//    Console.ReadKey() |> ignore
//    0
