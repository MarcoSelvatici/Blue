module  BetaEngine

open SharedTypes

let print a = 
    printf "%A\n" a 
    a

// introspection tools
let printFA env f x = 
    printfn "functionApplication: " 
    printf "f: "
    print f |> ignore
    printf "x: "
    print x |> ignore
    printf "env: "
    print env |> ignore
    printf "\n"

let printEval env art =
    printf "evaluate: " 
    print art |> ignore
    printf "env: "
    print env |> ignore
    printf "\n"

////////////////////
/// TREE MAPPING ///
////////////////////

/// from Ast to Art (Ast Run Time)
/// Curently useless - planned to support memoisation

type Counter() =
  static let mutable x = int64 0
  static member getID = 
    x <- x + (int64 1); 
    Some x
  static member getIDstub =
    Some (int64 -1)

type Art =
    | Def of FuncDefArt
    | Lam of LambdaR
    | App of (Art * Art * int64 option)
    | Nul
    | Lit of Literal
    | Idn of string
    | BIF of BuiltInFunc
    | IfE of Art * Art * Art
    | Seq of (Art * Art * int64 option)

and FuncDefArt = {
    Name: string;
    Body: Art;
    Rest: Art;
}

and LambdaR = {
    Var: string;
    Body: Art;
}

let newIDstub = int64 0

// TODO: add better error reporting - for now function just return None
let AstToArt ast =
    let rec (|MapAstArt|_|) ast =
        match ast with
        | FuncDefExp {FuncName = name; FuncBody = MapAstArt body ; Rest = MapAstArt rest} 
            -> Def ({Name = name; Body = body; Rest = rest}) |> Some
        | LambdaExp  { LambdaParam = name; LambdaBody = MapAstArt body}
            -> Lam {Var = name; Body = body;} |> Some
        | FuncApp (MapAstArt l, MapAstArt r)
            -> App (l, r, Counter.getIDstub) |> Some
        | Null -> Nul |> Some
        | Literal lit -> Lit lit |> Some
        | Identifier i -> Idn i |> Some
        | BuiltInFunc b -> BIF b |> Some
        | IfExp (MapAstArt b, MapAstArt t, MapAstArt e)
            -> IfE (b,t,e) |> Some
        | SeqExp (MapAstArt l, MapAstArt r)
            -> Seq (l,r, Counter.getIDstub) |> Some
        | FuncAppList _
        | IdentifierList _ -> None
        | _ -> None
    (|MapAstArt|_|) ast

let ArtToAst art = 
    let rec (|MapArtAst|_|) art = 
        match art with
        | Def ({Name = name; Body = MapArtAst body; Rest = MapArtAst rest}) 
            -> FuncDefExp {FuncName = name; FuncBody = body ; Rest = rest} |> Some
        | Lam  {Var = name; Body = MapArtAst body; }               
            -> LambdaExp { LambdaParam = name; LambdaBody =  body} |> Some
        | App (MapArtAst l ,MapArtAst r,_)
            -> FuncApp (l, r) |> Some
        | Nul -> Null |> Some
        | Lit l -> Literal l |> Some
        | Idn i -> Identifier i |> Some
        | BIF b -> BuiltInFunc b |> Some
        | IfE (MapArtAst b,MapArtAst t, MapArtAst e) -> IfExp (b,t,e) |> Some
        | Seq (MapArtAst l, MapArtAst r,_) -> SeqExp (l,r) |> Some
        | _ -> None
    (|MapArtAst|_|) art

//////////////
/// ERRORS ///
//////////////

let buildError (message:string) (art:Art) =
     Error (message, art)

let buildErrorS string art = 
    buildError string art |> Some

//////////////////////
// BUILIN FUNCTIONS //
//////////////////////

//  PAP matching on Ast - building block for builtIn functions
//  used for type-checking and unpacking the values

// D.U. defined types
let (|INTLIT|_|)    = function Lit (IntLit    v) -> Some v | _ -> None
let (|BOOLLIT|_|)   = function Lit (BoolLit   v) -> Some v | _ -> None
let (|STRINGLIT|_|) = function Lit (StringLit v) -> Some v | _ -> None
let (|SEQEXP|_|)    = function Seq (l,r,_) ->   Some (l,r) | _ -> None

// additionaly defined types

// match Null or Seq 
let (|LISTLAZY|_|) x =
    match x with
    | Nul | Seq _ -> Some x
    | _ -> None
// match whole list    
let rec (|LIST|_|) x =
    match x with
    | Nul -> Some []
    | Seq (hd, LIST tlLst, _) -> Some (hd::tlLst)
    | _ -> None
// match string list
let rec (|STRLIST|_|) x =
    match x with
    | Nul -> Some []
    | Seq (STRINGLIT s, STRLIST tlLst, _) -> Some (s::tlLst)
    | _ -> None

// helper function 
let rec buildList list =
    match list with
    | [] -> Nul
    | ele::rest -> Seq (ele, buildList rest, Counter.getIDstub)

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
    | (Idn _)::_ | (App _)::_ | (IfE _)::_ -> Ok originalAst // TODO: CAN DELETE THIS ROW ?
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
    //| (Idn _)::_ | (App _)::_ | (IfE _)::_    // TODO: CAN DELETE THIS ROW ?
    //| _::(Idn _)::_ | _::(App _)::_ | _::(IfE _)::_
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
        mapInputOutputBin (|BOOLLIT|_|) (|BOOLLIT|_|) (BoolLit>>Lit)
         [ // bool -> bool -> bool
            And, (&&); 
            Or,  (||);
         ];
        
        mapInputOutputBin (|STRINGLIT|_|) (|STRINGLIT|_|) (BoolLit>>Lit) 
         [   StrEq, (=) ]; // string -> string -> bool
        
        mapInputOutputBin (|INTLIT|_|) (|INTLIT|_|) (BoolLit>>Lit)
         [  // int -> int -> bool
            Greater,   (>); 
            GreaterEq, (>=); 
            Less,      (<); 
            LessEq,    (<=); 
            Equal,     (=);  
         ];
        
        mapInputOutputBin (|INTLIT|_|) (|INTLIT|_|) (IntLit>>Lit)
         [ // int -> int -> int
            Plus, (+);
            Minus,(-);   
            Mult, (*); 
            Div,  (/);
         ]; 
         //fun x -> Some x
        
        mapInputOutputBin (fun x -> Some x) (|LISTLAZY|_|)  id
         [  Append, (fun l r -> Seq (l,r,Counter.getIDstub)); ];
         
        // UNARY
        mapInputOutputUnary (|BOOLLIT|_|) (BoolLit>>Lit)
         [ Not, not ]; // bool -> bool

        mapInputOutputUnary (|LIST|_|) (IntLit>>Lit)
         [ Size, List.length ] // List -> int

        mapInputOutputUnary (|SEQEXP|_|) id
         [  // Seq -> Art 
            Head, (fun (hd,tl) -> hd);
            Tail, (fun (hd,tl) -> tl);         
         ];

         mapInputOutputUnary (|STRINGLIT|_|) id
         [ // String -> Art 
            Explode, Seq.toList 
                     >> List.map (string >> StringLit >> Lit) 
                     >> buildList ;
         ];

         mapInputOutputUnary (|STRLIST|_|) (StringLit>>Lit)
          [ //  [String] -> 
            Implode, Seq.fold (+) ""
          ];

    ] |> List.concat |> Map
   
type Enviourment = string list * Map<string, Art>

let extendEnv map name body =
     Map.add name body map

// subsititute value for the variable
let rec lambdaBetaReduction variable value art =
    let rCall = lambdaBetaReduction variable value
    match art with
    | Def ({Name = name; Body = body; Rest = rest}) when name <> variable
        -> Def ({Name = name; Body = rCall body; Rest = rCall rest})
    | Lam  {Var = name; Body = body; } when name <> variable           
        -> Lam { Var = name; Body = rCall body}
    | App (l ,r, _)
        -> App (rCall l, rCall r, Counter.getIDstub)
    | IfE (b,t,e) -> IfE (rCall b, rCall t, rCall e)
    | Seq (l,r,_) -> Seq (rCall l, rCall r, Counter.getIDstub)
    | Idn i when i = variable -> value
    | Idn _ | Def _ | Lam _ | Nul | Lit _ | BIF _ -> art

let rec decodeIdentifier env name = 
    match Map.tryFind name env with
        | Some (Idn i) -> decodeIdentifier env i
        | Some art -> Ok art
        | None -> buildError (sprintf "Idn \'%s\' is not defined" name) (Idn name);

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
    let rec (|FlatArg|_|) n (f, x, _) =
        let (|FlatArgNless1|_|) = (|FlatArg|_|) (n-1)
        match f, evaluate env x with  // ugly dependence on evaluate - TODO: get rid of this
        | _ when n = 0 -> None
        | BIF b, Ok ex ->  (b, [ex]) |> Some
        | App (FlatArgNless1 (b, argLst )), Ok ex -> (b, ex::argLst ) |> Some
        | _ -> None
    
    let (|FlatArgN|_|) = (|FlatArg|_|) n
    match (f,x,None) with
    | FlatArgN (b, argLst) when Map.containsKey b map
        -> (Map.find b map) (argLst, App (f,x,None)) |> Some
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
        | (Idn _ as uf), _ 
        | (IfE _ as uf), _  
        | (App _ as uf), _ 
        | (Def _ as uf), _-> 
            match evaluate env uf with
            | Error e -> Error e
            | Ok evalf -> functionApplication env evalf x
        | Lam  { Var = name; Body = body }, art
            -> lambdaBetaReduction name art body |> evaluate env      
        | (Nul as art), _  | (Lit _ as art), _ | (Seq _ as art), _ 
            -> buildError (sprintf "%A non-reducable" art ) art
        | (f, x) -> buildError (sprintf "What? %A in functionApplication" (f,x)) (App (f,x,None))
        )
    |> function
    | Ok ( Ok ast ) -> Ok ast
    | Ok ( Error e)
    | Error e -> Error e

and evaluate env art =
    //printEval env art
    match art with
    | Def {Name = name; Body = body; Rest = rest} -> 
        evaluate (extendEnv env name body) rest
    | Lam  { Var = name; Body = body } as l
        -> Ok l
    | App (f,x,id) -> functionApplication env f x
    | IfE (bool,bTrue,bFalse) ->
        match evaluate env bool with
        | Ok (Lit (BoolLit true))  -> evaluate env bTrue
        | Ok (Lit (BoolLit false)) -> evaluate env bFalse
        | Ok ( exp ) -> Ok (IfE (exp,bTrue,bFalse))
        | Error e -> Error e
    | Idn i -> decodeIdentifier env i
    | Nul | Lit _ | BIF _ | Seq _ 
        -> Ok art

/// top level function for reducing the AST
let runAst ast =
    AstToArt ast
    |> function
    | None -> buildError "What? couldn't transform Ast to Art (AST Run Time)" Nul
    | Some art -> evaluate (Map.empty) art
    |> (fun art -> (Result.map ArtToAst art, art))
    |> function
    | Ok (Some ast), _ -> Ok ast
    | Ok (None), Ok (art) -> buildError "What? couldn't transform Art back to Ast" art
    | Error e, _ -> Error e
    | _ -> buildError "What? runAst" Nul
    

