module BetaEngine

open TokeniserParserStub

let print a = 
    printf "%A\n" a 
    a

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


//  PAP for AST, building block for builtIn functions
//  used for type-checking and unpacking the values
//  boilerplate code 
// TODO: how to pass DU as PAPs ?

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

(*
   | Explode   String -> AST 
*)

let mapOutputUnary outputTransformer lst  =
    List.map (fun (n,f) -> n, f >> outputTransformer) lst

let bBool =
    // bool -> Ast
    mapOutputUnary (BoolLit>>Literal)
     [ // bool -> bool
        Not, not
     ]
    |> Map, (|BOOLLIT|_|)

let bSeq =
    [   // Seq -> Ast 
        Head, (fun (l,r) -> l); // addnotation needed to make the function non-generic
        Tail, (fun (l,r) -> r);         
    ] 
    |> Map, (|SEQEXP|_|)

let bList =
     // List -> Ast
    [
        mapOutputUnary (IntLit>>Literal)
         [ // List -> int
            Size, (fun (l:Ast list) -> List.length l)
         ];
    (*
        mapUn (Implode) | Implode   List -> String (tricky?) -> AST // coz its like typing the thing
    *) 
    ] |> List.concat  
    |> Map, (|LIST|_|)

/// Binary Builtin
let mapBin outputTransformer lst =
    List.map (fun (n,fn) -> n, (fun f g l r -> g (f l r)) fn outputTransformer) lst

let bIntInt = 
    [  // int -> int -> Ast   
        mapBin (BoolLit>>Literal)
         [  // int -> int -> bool
            Greater,   (>); 
            GreaterEq, (>=); 
            Less,      (<); 
            LessEq,    (<=); 
            Equal,     (=);  
        ];          
        mapBin (IntLit>>Literal)
         [ // int -> int -> int
            Plus, (+);
            Minus,(-);   
            Mult, (*); 
            Div,  (/);
         ]; 
    ] |> List.concat
    |> Map, (|INTLIT|_|), (|INTLIT|_|)

let bStringString =
    // string -> string -> Ast
    mapBin (BoolLit>>Literal)
         // string -> string -> bool
     [ StrEq, (fun (a:string) b -> a = b)]
    |> Map, (|STRINGLIT|_|), (|STRINGLIT|_|)

let bBoolBool = 
    // bool -> bool -> Ast
    mapBin (BoolLit>>Literal)
     [ // bool -> bool -> bool
        And, (&&); 
        Or,  (||);
     ] 
    |> Map, (|BOOLLIT|_|), (|BOOLLIT|_|)

let bASTList =
    //  Ast -> List -> Ast
    [ Append, (fun l r -> SeqExp (l,r)) ]
    |> Map, (fun (x:Ast) -> Some x), (|LISTLAZY|_|)

/// PAP buildier for unary built-in operators
/// * if 'full match' is detected - the function is evaluated and result returned
/// * if 'full match' is detected but the types are incorrect - Error is returned
/// * otherwise - the pattern doesn't match
/// 
/// parameters:
/// - map - Map from Builtin token to F# function
/// - (|INTYPE|_|) - PAP for matching (and unpacking) the input type
/// - f,x - left- and righthandside of function application
let (|UNBUILTIN|_|) (map,(|INTYPE|_|)) (f, x) = 
    match (f, x) with
    | BuiltInFunc b, INTYPE value when Map.containsKey b map 
        -> (Map.find b map) value |> Ok |> Some
    | BuiltInFunc b, arg when Map.containsKey b map 
        -> Error <| sprintf "%A is unsuported for %A" b arg |> Some
    | _ -> None

/// PAP buildier for binary built-in operators
/// * if 'full match' is detected - the function is evaluated and result returned
/// * if 'full match' is detected but the types are incorrect - Error is returned
/// * if 1-ary application is detected - the whole expresion is returned
/// * otherwise - the pattern doesn't match
/// 
/// parameters:
/// - map - Map from Builtin token to F# function
/// - (|InTypeL|_|) - PAP for matching (and unpacking) the left Ast input type
/// - (|InTypeR|_|) - PAP for matching (and unpacking) the right Ast input type
/// - f,x - left- and righthandside of function application
let (|BINBUILTIN|_|) (map,(|InTypeL|_|),(|InTypeR|_|)) (f, x)  =
    match (f, x) with
    | FuncApp (BuiltInFunc b, InTypeL l), InTypeR r when Map.containsKey b map 
        -> (Map.find b map) l r |> Ok |> Some
    | FuncApp (BuiltInFunc b, Identifier l), InTypeR r when Map.containsKey b map 
        -> FuncApp (f,x) |> Ok |> Some
    | FuncApp (BuiltInFunc b, lArg), rArg  when Map.containsKey b map
        -> Error <| sprintf "%A is unsuported for %A, %A" b lArg rArg |> Some
    | BuiltInFunc b, _ when Map.containsKey b map
        -> FuncApp (f,x) |> Ok |> Some
    | _ -> None

// b passed for error - reporting
// list is reversed
let buildBinaryBuiltIn b f (|InTypeL|_|) (|InTypeR|_|) (argLst, originalAst) =
    match argLst with
    | (InTypeL r)::(InTypeR l)::[] -> Ok (f l r)
    | (Identifier _)::_ | (FuncApp _)::_ | (IfExp _)::_    // TODO: CAN DELETE THIS ROW ?
    | _::(Identifier _)::_ | _::(FuncApp _)::_ | _::(IfExp _)::_ | _::[]  
        -> Ok originalAst
    | rArg::lArg::[] 
        -> Error <| sprintf "%A is unsuported for %A, %A" b lArg rArg
    | _ -> Error <| sprintf "What? BINBUILTIN2 %A %A" b argLst 

let mapInputOutputBin inputTransformer1 inputTransformer2 outputTransformer lstBind =
    let mapOutputBin outputTransformer lstBind =
        List.map (fun (n,fn) -> n, (fun f g l r -> g (f l r)) fn outputTransformer) lstBind
    let mapInputBin inputTransformer1 inputTransformer2 lstBind =
        List.map (fun (name,fn) -> (name,buildBinaryBuiltIn name fn inputTransformer1 inputTransformer2) ) lstBind
        
    mapOutputBin outputTransformer lstBind
    |> mapInputBin inputTransformer1 inputTransformer2

let BinaryBuiltIn = 
    [   
        mapInputOutputBin(|BOOLLIT|_|) (|BOOLLIT|_|) (BoolLit>>Literal)
         [ // bool -> bool -> bool
            And, (&&); 
            Or,  (||);
         ];

        mapInputOutputBin (|STRINGLIT|_|) (|STRINGLIT|_|) (BoolLit>>Literal) 
         [  // string -> string -> bool
            StrEq, (=)
         ];
        
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
         [ //  Ast -> SeqExp -> Ast
            Append, (fun l r -> SeqExp (l,r));
         ];
    ] 
    |> List.concat |> Map
   
    
let (|BuiltInMatch|_|) map (b,argLst,orginalAst) =
    if Map.containsKey b map 
    then Some ( (Map.find b map) (argLst, orginalAst))
    else None

// todo add descrciption
// list of n or less
let rec (|FlatArgBuiltIn|_|) n (f, x)  =
    let (|FlatArgBuiltInNless1|_|)= (|FlatArgBuiltIn|_|) (n-1)
    match f, x with 
    | _ when n = 0 -> None
    //| _, Identifier _ | _, FuncApp _ | _, IfExp _ -> None // cant do that since it has to delay
    | BuiltInFunc b, _ ->  (b, [x], FuncApp (f,x)) |> Some // add n = 1 if delayed evaluation
    | FuncApp (FlatArgBuiltInNless1 (b, argLst, _)), _ -> (b, x::argLst, FuncApp (f,x)) |> Some
    | _ -> None

let FlatAndMatch n map (f, x) = 
    let (|FlatArgBuiltInN|_|) = (|FlatArgBuiltIn|_|) n
    match (f,x) with
    | FlatArgBuiltInN (b, argLst, orginalAst) when Map.containsKey b map
        -> (Map.find b map) (argLst, orginalAst) |> Some
    | _ -> None

let (|BinaryBuiltinMatch|_|) = FlatAndMatch 2 BinaryBuiltIn


let (|BUILTIN|_|) (f,x)= 
    match (f,x) with 
    | UNBUILTIN  bBool         res 
    | UNBUILTIN  bSeq          res -> Some (res)
    //| BinaryBuiltinMatch res -> Some (res)
    //| D res -> Some (res)
    //| FlatArgBuiltIn 2 (BuiltInMatch BinaryBuiltIn res) -> Some (res)
    | _ -> None

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
        | BUILTIN res -> res
        | BinaryBuiltinMatch res -> res
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
