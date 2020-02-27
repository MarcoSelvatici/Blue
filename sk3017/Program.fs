open System
open TokeniserParserStub
open BetaEngine
open TestBetaEngine

[<EntryPoint>]
let main argv =
    allTests()

    def "c" (intL 10) (binaryBuiltin Plus ( Identifier "c") ( Identifier "c"))
    |> runAst
    |> print
    |> ignore
   
    Console.ReadKey() |> ignore
    0 

