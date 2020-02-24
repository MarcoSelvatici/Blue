module BetaEngine

open TokeniserParserStub

let print a = 
    printf "%A\n" a 
    a

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
*)

/// PAP for Literals, used as building block for builtin functions
let (|INTLIT|_|) x = 
    match x with
    | IntLit v -> Some v
    | _ -> None
let (|BOOLLIT|_|) x = 
    match x with
    | BoolLit v -> Some v
    | _ -> None
let (|STRINGLIT|_|) x = 
    match x with
    | StringLit v -> Some v
    | _ -> None

// can put all the lists into one
// TODO: refactor
let binaryIntToBoolOperators = 
    [
    Greater,    (>); 
    GreaterEq,  (>=); 
    Less,       (<); 
    LessEq,     (<=); 
    Equal,      (=);  
    ] |> Map, (|INTLIT|_|), BoolLit

let binaryBoolToBoolOperators = 
    [
    And, (&&); 
    Or,  (||);
    ] |> Map, (|BOOLLIT|_|), BoolLit

let binaryInttoIntOperators = 
    [
    Plus, (+);
    Minus,(-);   
    Mult, (*); 
    Div,  (/);
    ] |> Map, (|INTLIT|_|), IntLit

/// func: (|BINBUILTIN|_|)
/// > PAP for binary built-in operators
/// > if 'full match' is detected - the function is evaluated and result returned
/// > if 'full match' is detected but the types are incorrect - Error is returned
/// > if 1-ary application is detected - the whole expresion is returned
/// > otherwise - the pattern doesn't match
/// 
/// - map - Map from Builtin token to F# function
/// - (|INTYPE|_|) - PAP for matching the input type (both have to be the same) // TODO: change?
/// - output - function that packages the output into desider type              // TODO: merge with map ?
/// - f,x - left- and righthandside of function application
let (|BINBUILTIN|_|) (map,(|INTYPE|_|),output) (f, x)  =
    match (f, x) with
    | FuncApp (BuiltInFunc b, Literal (INTYPE l)), Literal (INTYPE r) when Map.containsKey b map 
        -> (Map.find b map) l r |> output |> Literal |> Ok |> Some
    | FuncApp (BuiltInFunc b, lArg), rArg  when Map.containsKey b map
        -> Error <| sprintf "%A is unsuported for %A, %A" b lArg rArg |> Some
    | BuiltInFunc b, _ when Map.containsKey b map
        -> FuncApp (f,x) |> Ok |> Some
    | _ -> None


// TODO : possibly delay evaluation of f or x
let rec functionApplication env f x =
    match evaluate env f, evaluate env x with
    | Error e, _ | _, Error e -> Error e
    | Ok fnc, Ok inp -> Ok (fnc , inp)
    |> 
        print
    |> Result.map (function
        // pass on - if funtion / argument is an identifier / non-reducable ifExp
        | Identifier _, _ | _, Identifier _ | IfExp _, _ | _, IfExp _ as nonReducable
            -> Ok (FuncApp (nonReducable))
        // reduce - lambda (CHANGE HERE FOR NORMAL REDUCTION)
        | Lambda  { LambdaParam = name; LambdaBody = body;}, ast
            -> evaluate (extendVarMap env name ast) body
        | BINBUILTIN binaryIntToBoolOperators  res -> res
        | BINBUILTIN binaryBoolToBoolOperators res -> res
        | BINBUILTIN binaryInttoIntOperators   res -> res
        

        // match l, r now ?


        // add FuncApp
        // add BuiltInFunc

        // fail on
        |  Null , _ | Literal _ , _ | SeqExp _ , _ -> Error "Null / Literal / SeqExp non-reducable" // TODO: make better error        
        | _ -> Error "What? RoundExp / IdentifierList / FuncAppList / FuncDefExp in functionApplication"  
        )
        |> function
        | Ok ( Ok ast ) -> Ok ast
        | Ok ( Error e) -> Error e
        | Error e -> Error e

(*
type Ast =
# f?   | FuncDefExp of FuncDefExpType // function definition(s) followed by expression
# r    | Lambda of LambdaType // anonymous function
prf    | FuncApp of Ast * Ast
# f    | FuncAppList of Ast list
# f    | Null // used with pair to make lists
# f    | Literal of Literal
# p    | Identifier of string
# f    | IdentifierList of string list
  p/f  | BuiltInFunc of BuiltInFunc // E.g. builtinTimes, builtinPlus
# f    | RoundExp of Ast // possibly needed see techical note
# p    | IfExp of Ast * Ast * Ast
# f    | SeqExp of Ast * Ast // A pair of two elements [a, b]. TODO: (syntactic sugar) Extend this to (untyped) lists [a, b, c, d] -> Seq(a, Seq(b, ...))
*)

and evaluate env ast =
    match ast with
    | FuncDefExp {FuncName = name; FuncBody = body; Rest = rest} -> 
        evaluate (extendEnv env name body) rest
    | Lambda  { LambdaParam = name; LambdaBody = body;} as l->
        //Ok l // (CHANGE HERE FOR NORMAL REDUCTION)
        match evaluate (extendBoundVar env name) body with
        | Ok b -> Ok (Lambda  { LambdaParam = name; LambdaBody = b } )
        | Error e -> Error e
    | FuncApp (f,x)     -> functionApplication env f x 
    | RoundExp x        -> evaluate env x
    | IfExp (bool,bTrue,bFalse) ->                                  // TODO: change to result map ?
        match evaluate env bool with
        | Ok (Literal (BoolLit true))  -> evaluate env bTrue
        | Ok (Literal (BoolLit false)) -> evaluate env bFalse
        | Error e -> Error e
        | Ok ( exp ) -> Ok (IfExp (exp,bTrue,bFalse))
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