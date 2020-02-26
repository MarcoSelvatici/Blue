module BetaEngine

open TokeniserParserStub

let print a = 
    printf "%A\n" a 
    a

//////////////////////
// BUILIN FUNCTIONS //
//////////////////////

//  PAP matching on Ast - building block for builtIn functions
//  used for type-checking and unpacking the values

// D.U. defined types
let (|INTLIT|_|)    = function Literal (IntLit    v) -> Some v | _ -> None
let (|BOOLLIT|_|)   = function Literal (BoolLit   v) -> Some v | _ -> None
let (|STRINGLIT|_|) = function Literal (StringLit v) -> Some v | _ -> None
let (|SEQEXP|_|)    = function SeqExp (l,r) ->      Some (l,r) | _ -> None

// additionaly defined types
let (|LISTLAZY|_|) x =
    match x with
    | Null | SeqExp _ -> Some x
    | _ -> None
let rec (|LIST|_|) x = 
    match x with
    | Null -> Some []
    | SeqExp (hd, LIST tlLst) -> Some (hd::tlLst)
    | _ -> None

let rec buildList list =
    match list with
    | [] -> Null
    | ele::rest -> SeqExp(ele, buildList rest)

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
    // | (Identifier _)::_ | (FuncApp _)::_ | (IfExp _)::_ -> Ok originalAst // TODO: CAN DELETE THIS ROW ?
    | arg::[] 
        -> Error <| sprintf "%A is unsuported for %A" b arg
    | _ -> Error <| sprintf "What? buildUnaryBuiltIn %A %A" b argLst 

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
/// * NB: list is reversed
/// 
/// parameters:
/// - b - the function token (key in the map) - used for error reporting
/// - f - function to be run in the case of match
/// - (|InTypeL|_|) - PAP for matching (and unpacking) the left  Ast input type
/// - (|InTypeR|_|) - PAP for matching (and unpacking) the right Ast input type
/// - argLst - list of arguments to be matched
/// - originalAst - to be returned if evaluatian needs to be delayed
let buildBinaryBuiltIn b f (|InTypeL|_|) (|InTypeR|_|) (argLst, originalAst) =
    match argLst with
    | (InTypeL val2)::(InTypeR val1)::[] -> Ok (f val1 val2)
    // | (Identifier _)::_ | (FuncApp _)::_ | (IfExp _)::_    // TODO: CAN DELETE THIS ROW ?
    | _::(Identifier _)::_ | _::(FuncApp _)::_ | _::(IfExp _)::_ | _::[] | _::_::_::_
        -> Ok originalAst
    | arg2::arg1::[] 
        -> Error <| sprintf "%A is unsuported for %A, %A" b arg1 arg2
    | _ -> Error <| sprintf "What? buildBinaryBuiltIn %A %A" b argLst 

let mapInputOutputBin inputTransformer1 inputTransformer2 outputTransformer lstBind =
    let mapOutputBin lst =
        List.map (fun (name,fn) -> name, (fun val1 val2 -> outputTransformer (fn val1 val2 ))) lst
    let mapInputBin lst =
        List.map (fun (name,fn) -> (name,buildBinaryBuiltIn name fn inputTransformer1 inputTransformer2) ) lst
    
    lstBind |> mapOutputBin |> mapInputBin 

let BuiltIn = 
    [   // BINARY
        mapInputOutputBin(|BOOLLIT|_|) (|BOOLLIT|_|) (BoolLit>>Literal)
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
        
        mapInputOutputBin Some (|LISTLAZY|_|) id
         [  Append, (fun l r -> SeqExp (l,r)); ]; // Ast -> SeqExp -> Ast
         
        // UNARY
        mapInputOutputUnary (|BOOLLIT|_|) (BoolLit>>Literal)
         [ Not, not ]; // bool -> bool

        mapInputOutputUnary (|LIST|_|) (IntLit>>Literal)
         [ Size, List.length ] // List -> int

        mapInputOutputUnary (|SEQEXP|_|) id
         [  // Seq -> Ast 
            Head, (fun (hd,tl) -> hd);
            Tail, (fun (hd,tl) -> tl);         
         ];

         mapInputOutputUnary (|STRINGLIT|_|) id
         [ // String -> AST 
            Explode, ( fun s -> s 
                                |> Seq.toList 
                                |> List.map (string >> StringLit >> Literal) 
                                |> buildList ) 
         ];

    ] |> List.concat |> Map
   
/// Builds PAP for matching build-in expressions in the map 
/// * if the (list of arguments) and (function token) is succesfuly extracted from the tree
/// * and (function token) is a key in the map
/// * then the function in the map is run with the (list of arguments)
/// * otherwise the PAP doesn't match
/// 
/// parameters:
/// - n - maximal number of args in (list of arguments) [at least arity of the functions in the map]
/// - map - Map from Builtin token to F# function
/// - f,x - left- and righthandside of function application
let FlatAndMatch n map (f, x) = 
    /// flattens nested FuncApp to list of arguments and the builin function token
    /// retruns (function token), (list of arguments)
    let rec (|FlatArg|_|) n (f, x)  =
        let (|FlatArgNless1|_|) = (|FlatArg|_|) (n-1)
        match f, x with 
        | _ when n = 0 -> None
        | BuiltInFunc b, _ ->  (b, [x]) |> Some // add n = 1 if delayed evaluation
        | FuncApp (FlatArgNless1 (b, argLst )), _ -> (b, x::argLst ) |> Some
        | _ -> None
    
    let (|FlatArgN|_|) = (|FlatArg|_|) n
    match (f,x) with
    | FlatArgN (b, argLst) when Map.containsKey b map
        -> (Map.find b map) (argLst, FuncApp (f,x)) |> Some
    | _ -> None

let (|BuiltinMatch|_|) = FlatAndMatch 4 BuiltIn

///////////////////
// END OF BUILIN //
///////////////////

type Enviourment = string list * Map<string, Ast>

//  3 function for maipulating the Envoiurment - boundVariables and varaibleMap

/// adds name to boundVariables and (name,body) pair to the variableMap
let extendEnv (boundVariables, variableMap) name body : Enviourment =
    name::boundVariables, Map.add name body variableMap

/// adds name to boundVariables, removes name from map
/// * used to keep track of bound Variables in lambdas
/// * since the new name is not yet tied to an AST it should be removed from the map
let extendBoundVar (boundVariables, variableMap) name : Enviourment = 
    name::boundVariables, Map.remove name variableMap

/// adds name,body pair to the variableMap
/// * used to assign value to bound Variables in lambdas
/// * should be used after extendBoundVar
let extendVarMap (boundVariables, variableMap) name body : Enviourment= 
    boundVariables, Map.add name body variableMap

// TODO : possibly delay evaluation of f or x
let rec functionApplication env f x =
    (*
    printfn "functionApplication: " 
    printf "f: "
    print f |> ignore
    printf "x: "
    print x |> ignore
    printf "env: "
    print env |> ignore
    printf "\n"
    *)

    match evaluate env f, evaluate env x with
    | Error e, _ | _, Error e -> Error e
    | Ok fnc, Ok inp -> Ok (fnc , inp)
    |> Result.map (function
        // pass on - if funtion / argument is an identifier / non-reducable ifExp
        | Identifier _, _ | _, Identifier _ | IfExp _, _ | _, IfExp _ as nonReducable
            ->  Ok (FuncApp (nonReducable))
        // reduce - lambda (CHANGE HERE FOR NORMAL REDUCTION)
        | Lambda  { LambdaParam = name; LambdaBody = body;}, ast
            -> evaluate (extendVarMap env name ast) body
        | BuiltinMatch res -> res
        //| UnaryBuiltinMatch res -> res
        // add FuncApp (done ? )
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
    (*
    printf "evaluate: " 
    print ast |> ignore
    printf "env: "
    print env |> ignore
    printf "\n"
    *)

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
        | Ok ( exp ) -> Ok (IfExp (exp,bTrue,bFalse))
        | Error e -> Error e
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

 // TODO: add error contructor
 
 // can write more generic types like:
 //type Builitn<'A> = int -> 'A
