module  BetaEngine

open SharedTypes

// introspection tools
let print a = 
    printf "%A\n" a 
    a

let printFA env f x = 
    printfn "functionApplication: " 
    printf "f: "
    print f |> ignore
    printf "x: "
    print x |> ignore
    printf "env: "
    print env |> ignore
    printf "\n"

let printEval env ast =
    printf "evaluate: " 
    print ast |> ignore
    printf "env: "
    print env |> ignore
    printf "\n"

//////////////
/// ERRORS ///
//////////////

let buildError message ast : Result<Ast,BetaEngineError>=
    Error {msg= message; trace=[]; ast=ast }

let errorAddTrace (err:BetaEngineError) traceMsg =
    Error {err with trace = traceMsg::err.trace}

let buildErrorS string ast = 
    buildError string ast |> Some

//////////////////////
// BUILIN FUNCTIONS //
//////////////////////

//  PAP matching on Ast - building block for builtIn functions
//  used for type-checking and unpacking the values

// D.U. defined types
let (|INTLIT|_|)    = function Literal (IntLit    v) -> Some v | _ -> None
let (|BOOLLIT|_|)   = function Literal (BoolLit   v) -> Some v | _ -> None
let (|STRINGLIT|_|) = function Literal (StringLit v) -> Some v | _ -> None
let (|SEQEXP|_|)    = function SeqExp (l,r) ->   Some (l,r) | _ -> None

// additionaly defined types

// match Null or SeqExp 
let (|LISTLAZY|_|) x =
    match x with
    | Null | SeqExp _ -> Some x
    | _ -> None
// match whole list    
let rec (|LIST|_|) x =
    match x with
    | Null -> Some []
    | SeqExp (hd, LIST tlLst) -> Some (hd::tlLst)
    | _ -> None
// match string list
let rec (|STRLIST|_|) x =
    match x with
    | Null -> Some []
    | SeqExp (STRINGLIT s, STRLIST tlLst) -> Some (s::tlLst)
    | _ -> None

// helper function 
let rec buildList list =
    match list with
    | [] -> Null
    | ele::rest -> SeqExp (ele, buildList rest)

/// PAP buildier for unary built-in operators
/// * if 'full match' is detected - the function is evaluated and result returned (Pass)
/// * if one of the arguments can't be evaluated - the original Ast is returned   (Delay)
/// * if 'full match' is detected but the types are incorrect - Error is returned (Fail)
/// * otherwise - What? error is returned (should not happen)
/// 
/// parameters:
/// - b - the function token (key in the map) - used for error reporting
/// - f - function to be run in the case of match
/// - (|InType|_|) - PAP for matching (and unpacking) the left  Ast input type
/// - argLst - list of arguments to be matched
/// - originalAst - to be returned if evaluatian needs to be delayed
let buildUnaryBuiltIn b f (|InType|_|) (argLst, originalAst) =
    match argLst with
    | (InType x)::[] -> Ok (f x)
    | (Identifier _)::_ | (FuncApp _)::_ | (IfExp _)::_ -> Ok originalAst // TODO: CAN DELETE THIS ROW ?
    | arg::[] 
        -> buildError (sprintf "%A is unsuported for %A" b arg) originalAst
    | _ -> buildError (sprintf "What? buildUnaryBuiltIn %A %A" b argLst) originalAst

let mapInputOutputUnary inputTransformer outputTransformer lstBind =
    let mapOutputUnary lst =
        List.map (fun (n,fn) -> n, fn >> outputTransformer) lst
    let mapInputUnary lst =
        List.map (fun (name,fn) -> (name,buildUnaryBuiltIn name fn inputTransformer) ) lst
    
    lstBind |> mapOutputUnary |> mapInputUnary 

/// PAP buildier for binary built-in operators
/// * if 'full match' is detected - the function is evaluated and result returned (Pass)
/// * if one of the arguments can't be evaluated - the original Ast is returned   (Delay)
/// * if 'full match' is detected but the types are incorrect - Error is returned (Fail)
/// * otherwise - What? error is returned (should not happen)
/// 
/// parameters:
/// - b - the function token (key in the map) - used for error reporting
/// - f - function to be run in the case of match
/// - (|InType1|_|) - PAP for matching (and unpacking) the left  Ast input type
/// - (|InType2|_|) - PAP for matching (and unpacking) the right Ast input type
/// - argLst - list of arguments to be matched
/// - originalAst - to be returned if evaluatian needs to be delayed
let buildBinaryBuiltIn b f (|InType1|_|) (|InType2|_|) (argLst, originalAst) =
    match argLst with
    | (InType2 val2)::(InType1 val1)::[] -> Ok (f val1 val2)
    //| (Identifier _)::_ | (FuncApp _)::_ | (IfExp _)::_    // TODO: CAN DELETE THIS ROW ?
    //| _::(Identifier _)::_ | _::(FuncApp _)::_ | _::(IfExp _)::_
    | _::[] | _::_::_::_
        -> Ok originalAst
    | arg2::arg1::[] 
        -> buildError (sprintf "%A is unsuported for %A, %A" b arg1 arg2) originalAst
    | _ -> buildError (sprintf "What? buildBinaryBuiltIn %A %A" b argLst) originalAst

let mapInputOutputBin inputTransformer1 inputTransformer2 outputTransformer lstBind =
    let mapOutputBin lst =
        List.map (fun (name,fn) -> name, (fun val1 val2 -> outputTransformer (fn val1 val2 ))) lst
    let mapInputBin lst =
        List.map (fun (name,fn) -> (name,buildBinaryBuiltIn name fn inputTransformer1 inputTransformer2) ) lst
    
    lstBind |> mapOutputBin |> mapInputBin 

/// Map from BuiltInFunc token to Builtin Functions
let BuiltIn = 
    [   // BINARY
        mapInputOutputBin (|BOOLLIT|_|) (|BOOLLIT|_|) (BoolLit>>Literal)
         [ // bool -> bool -> bool
            And, (&&); 
            Or,  (||);
         ];
        
        mapInputOutputBin (|STRINGLIT|_|) (|STRINGLIT|_|) (BoolLit>>Literal) 
         [   StrEq, (=) ]; // string -> string -> bool
        
        mapInputOutputBin (|INTLIT|_|) (|INTLIT|_|) (BoolLit>>Literal)
         [  // int -> int -> bool
            Greater,   (>); 
            GreaterEq, (>=); 
            Less,      (<); 
            LessEq,    (<=); 
            Equal,     (=);  
         ];
        
        mapInputOutputBin (|INTLIT|_|) (|INTLIT|_|) (IntLit>>Literal)
         [ // int -> int -> int
            Plus, (+);
            Minus,(-);   
            Mult, (*); 
            Div,  (/);
         ]; 
         //fun x -> Some x
        
        mapInputOutputBin (fun x -> Some x) (|LISTLAZY|_|)  id
         [  Append, (fun l r -> SeqExp (l,r)); ];
         
        // UNARY
        mapInputOutputUnary (|BOOLLIT|_|) (BoolLit>>Literal)
         [ Not, not ]; // bool -> bool

        mapInputOutputUnary (|LIST|_|) (IntLit>>Literal)
         [ Size, List.length ] // List -> int

        mapInputOutputUnary (|SEQEXP|_|) id
         [  // SeqExp -> ast 
            Head, (fun (hd,tl) -> hd);
            Tail, (fun (hd,tl) -> tl);         
         ];

         mapInputOutputUnary (|STRINGLIT|_|) id
         [ // String -> ast 
            Explode, Seq.toList 
                     >> List.map (string >> StringLit >> Literal) 
                     >> buildList ;
         ];

         mapInputOutputUnary (|STRLIST|_|) (StringLit>>Literal)
          [ //  [String] -> 
            Implode, Seq.fold (+) ""
          ];

    ] |> List.concat |> Map
   
type Enviourment = string list * Map<string, Ast>

let extendEnv map name body =
     Map.add name body map

let (|UnexpectedTypes|) ast =
    match ast with
    | FuncAppList _   -> buildError "What? FuncAppList"    ast
    | IdentifierList _-> buildError "What? IdentifierList" ast
    | Combinator _    -> buildError "What? Combinator"     ast
    | _ -> buildError "What? UnexpectedTypes didn't match" ast

// subsititute value for the variable
let rec lambdaBetaReduction variable value ast =
    let rCall = lambdaBetaReduction variable value
    match ast with
    | FuncDefExp ({FuncName = name; FuncBody = body; Rest = rest}) when name <> variable
        -> FuncDefExp ({FuncName = name; FuncBody = rCall body; Rest = rCall rest})
    | LambdaExp  {LambdaParam = name; LambdaBody = body; } when name <> variable           
        -> LambdaExp { LambdaParam = name; LambdaBody = rCall body}
    | FuncApp (l ,r)
        -> FuncApp (rCall l, rCall r)
    | IfExp (b,t,e) -> IfExp (rCall b, rCall t, rCall e)
    | SeqExp (l,r) -> SeqExp (rCall l, rCall r)
    | Identifier i when i = variable -> value
    | Identifier _ | FuncDefExp _ | LambdaExp _ | Null | Literal _ | BuiltInFunc _ -> ast
    | _ -> ast  // TODO : delete

let rec decodeIdentifier env name = 
    match Map.tryFind name env with
        | Some (Identifier i) -> decodeIdentifier env i
        | Some ast -> Ok ast
        | None -> buildError (sprintf "Identifier \'%s\' is not defined" name) (Identifier name);


/// Builds PAP for matching build-in expressions in the map 
/// * if the (list of arguments) and (function token) is succesfuly extracted from the tree
/// * and (function token) is a key in the map
/// * then the function in the map is run with the (list of arguments)
/// * otherwise the PAP doesn't match
/// 
/// parameters:
/// - n - maximal number of args in (list of arguments) [at least arity of the functions in the map]
/// - map - Map from Builtin token to F# function
/// - env - envoiurment needed if x is identifier
/// - f,x - left- and righthandside of function application
let rec FlatAndMatch n map env (f, x) = 
    /// flattens nested FuncApp to list of arguments and the builin function token
    /// retruns (function token), (list of arguments)
    let rec (|FlatArg|_|) n (f, x) =
        let (|FlatArgNless1|_|) = (|FlatArg|_|) (n-1)
        match f, evaluate env x with  // ugly dependence on evaluate - TODO: get rid of this
        | _ when n = 0 -> None
        | BuiltInFunc b, Ok ex ->  (b, [ex]) |> Some
        | FuncApp (FlatArgNless1 (b, argLst )), Ok ex -> (b, ex::argLst ) |> Some
        | _ -> None
    
    let (|FlatArgN|_|) = (|FlatArg|_|) n
    match (f,x) with
    | FlatArgN (b, argLst) when Map.containsKey b map
        -> (Map.find b map) (argLst, FuncApp (f,x)) |> Some
    | _ -> None

and functionApplication env f x =
    let (|BultinMatchWEnv|_|) = FlatAndMatch 2 BuiltIn env
    //printFA env f x
    // match evaluate env f, evaluate env x with
    match evaluate env x with
    | Error e -> Error e
    | Ok inp -> Ok (f , inp)
    |> Result.map (function
        | BultinMatchWEnv res -> res   
        | (Identifier _ as uf), _ 
        | (IfExp _ as uf), _  
        | (FuncApp _ as uf), _ 
        | (FuncDefExp _ as uf), _-> 
            match evaluate env uf with
            | Error e -> Error e
            | Ok evalf -> functionApplication env evalf x
        | LambdaExp  { LambdaParam = name; LambdaBody = body}, ast
            -> lambdaBetaReduction name ast body |> evaluate env      
        | (Null as ast), _  | (Literal _ as ast), _ | (SeqExp _ as ast), _ 
            -> buildError (sprintf "%A non-reducable" ast ) ast
        | (f, x) -> buildError (sprintf "What? %A in functionApplication" (f,x)) (FuncApp (f,x))
        )
    |> function
    | Ok ( Ok ast ) -> Ok ast
    | Ok ( Error e)
    | Error e -> Error e

and evaluate env ast =
    //printEval env ast
    match ast with
    | FuncDefExp {FuncName = name; FuncBody = body; Rest = rest} -> 
        evaluate (extendEnv env name body) rest
    | LambdaExp  { LambdaParam = name; LambdaBody = body } as l
        -> Ok l
    | FuncApp (f,x) -> functionApplication env f x
    | IfExp (bool,bTrue,bFalse) ->
        match evaluate env bool with
        | Ok (Literal (BoolLit true))  -> evaluate env bTrue
        | Ok (Literal (BoolLit false)) -> evaluate env bFalse
        | Ok ( exp ) -> Ok (IfExp (exp,bTrue,bFalse))
        | Error e -> Error e
    | Identifier i -> decodeIdentifier env i
    | Null | Literal _ | BuiltInFunc _ | SeqExp _ 
        -> Ok ast
    | UnexpectedTypes e -> e

let upcastError =
    function
    | Error e -> Error <| BetaEngineError e
    | Ok x -> Ok x

/// top level function for reducing the AST
let runAst ast =
    ast
    |> evaluate Map.empty 
    |> upcastError