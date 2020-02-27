open System
open TokeniserParserStub
open BetaEngine
open TestBetaEngine

[<EntryPoint>]
let main argv =
    //allTests()

    //ackermanAST 0 0
    
    // IDEA 

    //doesnt work add as test
    FuncApp (FuncApp ( lam "m" (lam "n" (Identifier "m") ), intL 10), intL 20)
    |> runAst
    |> print
    |> ignore
   
    Console.ReadKey() |> ignore
    0 

