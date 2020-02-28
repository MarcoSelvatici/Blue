open System
open TokeniserParserStub
open BetaEngine
open TestBetaEngine

[<EntryPoint>]
let main argv =
    allTests()

    F (def "double" (lam "a" (F2builtIn Mult (intL 2) (idn "a"))) (idn "double"))  (intL 18)
    |> runAst
    //|> print
    |> ignore
   
    Console.ReadKey() |> ignore
    0 

// TODO: 3. update README