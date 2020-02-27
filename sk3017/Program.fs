open System
open TokeniserParserStub
open BetaEngine
open TestBetaEngine

[<EntryPoint>]
let main argv =
    allTests()

    F2 
      (F2 (lam "a" (lam "b" (lam "c" (lam "d" 
           (F2builtIn Plus 
             (F2builtIn Mult (Identifier "a") (Identifier "c") ) 
             (F2builtIn Div (Identifier "d") (Identifier "b"))
           ))))) 
        (intL 3) (intL 4 )) (intL 5) (intL 6)
    |> runAst
    //|> print
    |> ignore
   
    Console.ReadKey() |> ignore
    0 

