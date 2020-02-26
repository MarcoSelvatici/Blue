// Author: oss1017 (Oliver Stiff)
// Run testbench and individual programs using the SKI runtime

open Testbench

[<EntryPoint>]
let main argv =

    //// Evaluate a single program (Ast supplied as input) ////
    // let prog = Ok (...Ast...)
    //
    // singleEval prog

    //// RUN ALL EXPECTO TESTS ////
    testAll()

    0