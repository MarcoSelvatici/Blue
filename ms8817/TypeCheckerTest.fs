open TokeniserStub
open Parser
open TypeChecker


[<EntryPoint>]
let main argv =
    Literal (IntLit 1)
    |> typeCheck
    0