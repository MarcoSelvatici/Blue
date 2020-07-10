module TestLib

open Expecto


/// Create a testcase for unit testing a function.
let testFunction f testData =
    let descr, inp, expectedOut = testData
    testCase descr <| fun () ->
        let actual = f inp
        Expect.equal actual expectedOut ""

/// Create an expecto testList from a list of testCases.
let createTestList descr f testCases =
    testList descr <| List.map (testFunction f) testCases

/// Helper function for e2e testing.
let e2eTest runtime typecheck parse lex testData =
    let typeCheckAndReturnAst ast =
        typecheck ast |> Result.bind (fun _ -> Ok ast) 
    let descr, inp, expectedOut = testData
    testCase descr <| fun () ->
        let actual = lex inp
                     |> Result.bind parse
                     |> Result.bind typeCheckAndReturnAst
                     |> Result.bind runtime
        Expect.equal actual expectedOut ""

/// Create an expecto testList for e2e testing.
let e2eCreateTestList descr runtime typecheck parse lex testCases =
    testList descr <| List.map (e2eTest runtime typecheck parse lex) testCases

/// Run all the previously defined tests.
let runTests () =
    runTestsInAssembly defaultConfig [||] |> ignore
