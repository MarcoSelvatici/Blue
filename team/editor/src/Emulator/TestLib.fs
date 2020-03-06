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

// Helper function for e2e testing. 
let e2eTest runtime parse lex testData =
    let descr, inp, expectedOut = testData
    testCase descr <| fun () ->
        match lex inp with
        | Ok toklst -> 
            match parse toklst with 
            | Ok ast -> 
                Expect.equal (runtime ast) expectedOut ""
            | Error err -> Expect.equal (Error err) expectedOut ""
        | Error err -> Expect.equal (Error err) expectedOut ""

/// Create an expecto testList for e2e testing.
let e2eCreateTestList descr runtime parse lex testCases =
    testList descr <| List.map (e2eTest runtime parse lex) testCases

/// Run all the previously defined tests.
let runTests () =
    runTestsInAssembly defaultConfig [||] |> ignore
