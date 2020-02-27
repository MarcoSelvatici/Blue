open System
open TokeniserParserStub
open BetaEngine
open TestBetaEngine

[<EntryPoint>]
let main argv =
    allTests()

    FuncApp (lam "k" (def "k" (trueL) (Identifier "k")), intL 11)
    |> runAst
    //|> print
    |> ignore
   
    Console.ReadKey() |> ignore
    0 

