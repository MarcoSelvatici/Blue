module BetaEngine

open TokeniserParserStub

type Enviourment = Map<string, Ast>

let rec reduceAst env ast =
    match ast with
    | FuncDefExp {FuncName = name; FuncBody = body; Rest = rest}    -> 
        reduceAst (Map.add name body env) rest
    | Lambda l          -> failwithf "Lambda - Not implemented %A" ast  // if no outermost can be called we call inside // closures?
    | FuncApp (f,x)     -> failwithf "FuncApp - Not implemented %A" ast 
    | RoundExp x        -> reduceAst env x
    | IfExp (bool,bTrue,bFalse) ->
        match reduceAst env bool with
        | Ok (Literal (BoolLit true))  -> reduceAst env bTrue
        | Ok (Literal (BoolLit false)) -> reduceAst env bFalse
        | error -> error
    | Identifier i -> 
        Map.tryFind i env |>
        function
        | Some def -> reduceAst env def
        | None -> Error <| sprintf "Identifier \'%s\' is not defined" i
    | Null | Literal _ | BuiltInFunc _ | SeqExp _ 
                       -> Ok ast
    | FuncAppList _     -> Error "What? parser returned FuncAppList"  
    | IdentifierList _  -> Error "What? parser returned IdentifierList"


// top level function
let runAst ast =
    reduceAst Map.empty ast