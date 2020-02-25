open Lexer
open Expecto

// tokenise test
[<Tests>]
let toktest1 =
    testCase "test 1 for tokenise function" <| fun () ->
        Expect.equal (tokeniseT3 "\"4s\tdsd\"") [] 
                      "testing tokenising for ([(...)])"

[<Tests>]
let toktest2 =
    testCase "test 2 for tokenise function" <| fun () ->
        Expect.equal (tokeniseT3 "=a=4.5") [] "testing tokenising for u.u"


// Run this to run all current tests
let runAllTests =
    let testLst =
        testList "Tests" [
            toktest1
            toktest2
        ]
    runTests defaultConfig testLst |> ignore