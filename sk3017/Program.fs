open System
open TokeniserParserStub
open BetaEngine
open TestBetaEngine

[<EntryPoint>]
let main argv =
    allTests()

    def "id" (lam "x" (Identifier "x")) (F (Identifier "id") (Identifier "id"))
    |> runAst
    //|> print
    |> ignore
   
    Console.ReadKey() |> ignore
    0 

