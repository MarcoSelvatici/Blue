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
    let tkns3 = [KLambda; TIdentifier "x"; KDot; TLiteral (IntLit 2)]
    let tkns4 = [KLambda; TIdentifier "x";  TIdentifier "y"; KDot; TLiteral (IntLit 2)]
    tkns4
    |> parse
    |> printAst
    0
