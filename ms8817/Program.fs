open System

open Parser

open TokeniserStub

let printAst ast =
    printfn "%A" ast

[<EntryPoint>]
let main argv =
    let tkns = [TIdentifier "a"]
    let tkns1 = [TLiteral (IntLit 2)]
    let tkns2 = [KOpenRound; TLiteral (IntLit 2); KCloseRound]
    tkns2
    |> parse
    |> printAst
    0
