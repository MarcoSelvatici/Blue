module BetaEngine

open TokeniserParserStub

type Enviourment = string list * Map<string, Ast>

let extendEnv (boundVariables, variableMap) name body =
    name::boundVariables, Map.add name body variableMap

let extendVarMap (boundVariables, variableMap) name body = 
    boundVariables, Map.add name body variableMap

let extendBoundVar (boundVariables, variableMap) name = 
    name::boundVariables, variableMap


// TODO: delete - use result.map
let passError f res = 
    match res with
    | Ok x -> Ok (f x)
    | Error x -> res

(*
BuiltInFunc
    / UnaryOp
    | Not
    | Head
    | Tail
    | Size
    // ComparisonOp
    | Greater
    | GreaterEq
    | Less
    | LessEq
    | Equal
    // LogicalOp
    | And
    | Or
    // AdditiveOp
    | Plus
    | Minus
    // MultiplicativeOp
    | Mult
    | Div
*)

let rec functionApplication env f x =
    match evaluate env f, evaluate env x with
    | Error e, _ | _, Error e -> Error e
    | Ok (Identifier i), Ok( ast ) -> Ok ( FuncApp (Identifier i, ast) )    // if function is an indetifier - don't evaluate
    | Ok( ast ), Ok (Identifier i) -> Ok ( FuncApp (ast, Identifier i) )    // if argument is an indetifier - don't evaluate
    | Ok (Lambda  { LambdaParam = name; LambdaBody = body;}), Ok( ast ) ->  // reduce lambda terms
        evaluate (extendVarMap env name ast) body
    // add FuncDefExp
    // add FuncApp
    // add BuiltInFunc
    // add Null / Literal / IdentifierList
    | _ -> Error "Not impemented" 
     

(*
type Ast =
    | FuncDefExp of FuncDefExpType // function definition(s) followed by expression
x   | Lambda of LambdaType // anonymous function
    | FuncApp of Ast * Ast
    | FuncAppList of Ast list
    | Null // used with pair to make lists
    | Literal of Literal
x    | Identifier of string
    | IdentifierList of string list
    | BuiltInFunc of BuiltInFunc // E.g. builtinTimes, builtinPlus
    | RoundExp of Ast // possibly needed see techical note
    | IfExp of Ast * Ast * Ast
    | SeqExp of Ast * Ast // A pair of two elements [a, b]. TODO: (syntactic sugar) Extend this to (untyped) lists [a, b, c, d] -> Seq(a, Seq(b, ...))
*)

and evaluate env ast =
    match ast with
    | FuncDefExp {FuncName = name; FuncBody = body; Rest = rest} -> 
        evaluate (extendEnv env name body) rest
    | Lambda  { LambdaParam = name; LambdaBody = body;}-> 
        evaluate (extendBoundVar env name) body
    | FuncApp (f,x)     -> functionApplication env f x 
    | RoundExp x        -> evaluate env x
    | IfExp (bool,bTrue,bFalse) ->
        match evaluate env bool with
        | Ok (Literal (BoolLit true))  -> evaluate env bTrue
        | Ok (Literal (BoolLit false)) -> evaluate env bFalse
        | error -> error    
    | Identifier i -> 
        match Map.tryFind i (snd env) with
        | Some ast -> evaluate env ast
        | None when List.contains i (fst env) -> Ok (Identifier i)
        | None -> Error <| sprintf "Identifier \'%s\' is not defined" i;
    | Null | Literal _ | BuiltInFunc _ | SeqExp _ 
                       -> Ok ast
    | FuncAppList _     -> Error "What? parser returned FuncAppList"  
    | IdentifierList _  -> Error "What? parser returned IdentifierList"


// top level function
let runAst ast =
    evaluate ([],Map.empty) ast