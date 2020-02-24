// Module: SKI combinator Runtime
// Author: oss1017 (Oliver Stiff)-

module rec ski_combinator

open TokeniserStub
open Parser

/// Print function
let print x =
    printfn "%A" x


 /// Print and return (used in pipeline)
let pipePrint x =
    print x; x
 

/// Bracket abstraction and substituting functions when they are called by making use of function name to body bindings. 
let bracketAbstract (input: Ast) (bindings: Map<string, Ast>): Result<Ast,string> =
    match input with
    | Combinator _ | Literal _ | BuiltInFunc _ | SeqExp _ | Null -> input |> Ok

    //    1.  T[x] => x
    // Check in bindings to see if that identfier has been defined
    | Identifier x ->
        if bindings.ContainsKey x
        then bindings.[x] |> Ok
        // An error could be returned if identifier is not in bindings: possible err msg is below
        //else Error <| sprintf "Undefined identifier: \'%s\'" x
        else Identifier x |> Ok

    //substitute expressions into lambdas
    | FuncApp ( Lambda { LambdaParam = name; LambdaBody = exp1 },  exp2) ->
        match bracketAbstract exp2 bindings with
        | Ok x ->
            bracketAbstract exp1 (bindings.Add(name, x))
        | Error x -> Error x
        
    //    2.  T[(E₁ E₂)] => (T[E₁] T[E₂])
    | FuncApp (exp1, exp2) -> 
        match bracketAbstract exp1 bindings, bracketAbstract exp2 bindings with
        | Ok x, Ok y       -> FuncApp (x, y) |> Ok
        | Error x, Error y -> Error <|  x + ", " + y
        | Error x, _       -> Error x
        | _, Error y       -> Error y

    | Lambda x ->
        match x with
        //    3.  T[λx.E] => (K T[E]) (if x does not occur free in E)
        | { LambdaParam = name; LambdaBody = exp } when not (isFree name exp) ->
            match bracketAbstract exp bindings with
            | Ok x -> FuncApp (Combinator K, x) |> Ok
            | Error x -> Error x

        //    4.  T[λx.x] => I
        | { LambdaParam = name; LambdaBody = Identifier exp } when name = exp ->
            Combinator I |> Ok

        //    5.  T[λx.λy.E] => T[λx.T[λy.E]] (if x occurs free in E) 
        | { LambdaParam = name1; LambdaBody = Lambda { LambdaParam = name2; LambdaBody = exp } } when isFree name1 exp ->
            match bracketAbstract (Lambda { LambdaParam = name2; LambdaBody = exp } ) bindings with
            | Ok x -> bracketAbstract (Lambda { LambdaParam = name1; LambdaBody = x } ) bindings
            | Error x -> Error x
            
        //    6.  T[λx.(E₁ E₂)] => (S T[λx.E₁] T[λx.E₂]) (if x occurs free in E₁ or E₂)
        | { LambdaParam = name; LambdaBody = FuncApp (exp1, exp2) } when isFree name exp1 || isFree name exp2 ->
            let x = bracketAbstract ( Lambda { LambdaParam = name; LambdaBody = exp1 }) bindings
            let y = bracketAbstract ( Lambda { LambdaParam = name; LambdaBody = exp2 }) bindings
            match x, y with
            | Ok x, Ok y       -> FuncApp (FuncApp (Combinator S, x ), y ) |> Ok 
            | Error x, Error y -> Error <| x + ", " + y
            | Error x, _       -> Error x
            | _, Error y       -> Error y
            
        | _ ->
            Error <| sprintf "Unable to bracket abstract lambda %A" x
    
    //function definitiions and bindings
    | FuncDefExp { FuncName = name; FuncBody = body; Rest = exp } ->
        match bracketAbstract body bindings with
        | Ok x ->
            bracketAbstract exp (bindings.Add(name, x))
        | Error x -> Error x

    | IfExp (condition,expT,expF) ->
            match bracketAbstract condition bindings with
            | Ok x -> 
                match eval (x) with
                | Ok (Literal (BoolLit true))  -> 
                    bracketAbstract expT bindings
                | Ok (Literal (BoolLit false)) -> 
                    bracketAbstract expF bindings
                | _ -> Error "Unexpected value in ifThenElse condition"
            | Error x -> Error x

    | RoundExp _        -> Error "RoundExp should not be returned by parser"
    | FuncAppList _     -> Error "FuncAppList should not be returned by parser"
    | IdentifierList _  -> Error "IdentifierList should not be returned by parser"


/// Determine if a variable is free in an expression
let isFree (var:string) (exp: Ast): bool =
    let rec free (exp: Ast): string List =
        match exp with
        | Identifier x 
            -> [x]
        | FuncApp (exp1, exp2)
            -> free exp1 @ free exp2
        | Lambda { LambdaParam = name; LambdaBody = exp1 }
            -> List.filter (fun x -> x <> name) (free exp1)

        // also need to check if var is in seq and other Ast type....

        | _ 
            -> []

    List.contains var (free exp)


/// Evaluate Ast built-in functions
let eval (input:Ast) : Result<Ast,string> =
    match input with
    
    // implode string list
    | FuncApp( BuiltInFunc Implode, exp) -> 
        let rec imp lst =
            match lst with
            | SeqExp ( Literal (StringLit head) , Null) ->
                Some head 
            | SeqExp (Literal (StringLit head), tail) ->
                match imp tail with
                | None -> None
                | Some x -> Some (head + x)              
            | _ ->
                None
        
        match eval exp with
        | Ok x -> 
            match imp x with
            | None -> Error "Cannot implode argument of type which is not string list"
            | Some x -> x |> StringLit |> Literal |> Ok
            
        | Error x -> Error x 
            
            
    // explode string
    | FuncApp( BuiltInFunc Explode, x) -> 
        match eval x with
        | Ok (Literal (StringLit x)) ->
            Seq.toList x |> List.map (string) |> buildList |> Ok
        | _ ->
            Error "cannot explode argument of type which is not string"

    //head
    | FuncApp( BuiltInFunc Head, x) -> 
        match eval x with
        | Ok (SeqExp (head, tail)) ->
            head |> Ok
        | _ ->
            Error "Error getting head of list/sequence"

    //tail
    | FuncApp( BuiltInFunc Tail, x) -> 
        match eval x with
        | Ok (SeqExp (head, tail)) ->
            tail |> Ok
        | _ ->
            Error "Error getting tail of list/sequence"

    //size of list
    | FuncApp( BuiltInFunc Size, exp) -> 
        let rec sizeOf x =
            match x with
            | SeqExp (Null, Null) ->
                0
            | SeqExp (head, Null) ->
                1 
            | SeqExp (head, tail) ->
                1 + (sizeOf tail)
            | _ ->
               -1

        match eval exp with
        | Ok x ->
            let len = sizeOf x
            if len <> -1
            then len |> IntLit |> Literal |> Ok
            else Error <| sprintf "Error getting size of list: Invalid input: %A" x
        | Error x -> Error x            

    //unary built-in functions
    | FuncApp( BuiltInFunc op, x) -> 
        let x' = eval x
        match op, x' with
        
        //boolean op
        | Not, Ok (Literal (BoolLit n)) -> 
            Literal (BoolLit (not n)) |> Ok
        | _ ->
            Error <| sprintf "Error evaluating built-in function with 1 argument: opertor \'%A\'" op

    // Built-in functon w/ 2 args
    | FuncApp( FuncApp( BuiltInFunc op, x), y) -> 
        let x' = eval x
        let y' = eval y
        match op, x', y' with
        
        //arithmetic ops
        | Mult, Ok (Literal (IntLit n)), Ok (Literal (IntLit m))  -> 
            Literal (IntLit (n * m)) |> Ok
        | Div,Ok (Literal (IntLit n)), Ok (Literal (IntLit m))    ->
            if m = 0
            then Error "Division by 0"
            else Literal (IntLit (n / m)) |> Ok     
        | Plus, Ok (Literal (IntLit n)), Ok (Literal (IntLit m))  -> 
            Literal (IntLit (n + m)) |> Ok
        | Minus, Ok (Literal (IntLit n)), Ok (Literal (IntLit m)) ->
            Literal (IntLit (n - m)) |> Ok
        
        //boolean ops
        | And, Ok (Literal (BoolLit n)), Ok (Literal (BoolLit m)) ->
            Literal (BoolLit (n && m)) |> Ok
        | Or, Ok (Literal (BoolLit n)), Ok (Literal (BoolLit m))  ->
            Literal (BoolLit (n || m)) |> Ok

        //comparaison ops (bool)
        | Greater, Ok (Literal (IntLit n)), Ok (Literal (IntLit m))   ->
            Literal (BoolLit (n > m)) |> Ok
        | GreaterEq, Ok (Literal (IntLit n)), Ok (Literal (IntLit m)) ->
            Literal (BoolLit (n >= m)) |> Ok
        | Less, Ok (Literal (IntLit n)), Ok (Literal (IntLit m))      ->
            Literal (BoolLit (n < m)) |> Ok
        | LessEq, Ok (Literal (IntLit n)), Ok (Literal (IntLit m))    ->
            Literal (BoolLit (n <= m)) |> Ok
        | Equal, Ok (Literal (IntLit n)), Ok (Literal (IntLit m))     ->
            Literal (BoolLit (n = m)) |> Ok

        //Error  
        // Passthrough for partially applied functions
        // if we only wanted fully evaluated and reduced expressions this should return an error
        | _ -> input |> Ok

    | Combinator _ | SeqExp _ | Identifier _ | FuncApp _ | BuiltInFunc _ | Null | Literal _ -> input |> Ok

    | IfExp _ -> input |> Ok    // this should be dealt with in bracket abstraction
                                // but if for some reason it can't evaluate it,
                                // it should be passed to output

    | Lambda _          -> Error "Lambda should not exist after bracket abstraction"
    | FuncDefExp _      -> Error "FuncDefExp should not exist after bracket abstraction"
    | RoundExp _        -> Error "RoundExp should not be returned by parser"
    | FuncAppList _     -> Error "FuncAppList should not be returned by parser"
    | IdentifierList _  -> Error "IdentifierList should not be returned by parser"


/// Builds a list in our language out of an f# list
let rec buildList lst =
    match lst with
    | [] ->  SeqExp (Null, Null)
    | [x] -> SeqExp (Literal (StringLit x), Null)
    | head::tail -> SeqExp ( Literal (StringLit head), buildList tail )


//evaluate/simplify SKI exp
/// SKI combinator reduction
let rec interpret (exp:Ast) :Ast =
    match exp with
    | Combinator x ->
        exp
    | FuncApp( Combinator I, x) ->
        interpret x
    | FuncApp(FuncApp (Combinator K, x), y) ->
        interpret x
    | FuncApp (FuncApp (FuncApp (Combinator S, x), y), z) ->
        interpret (FuncApp (FuncApp (x, z), FuncApp (y, z)))
    | FuncApp (exp1, exp2) ->             
        let exp1' = interpret exp1
        let exp2' = interpret exp2
        if exp1 = exp1' && exp2 = exp2'
        then FuncApp (exp1, exp2)
        else interpret (FuncApp (exp1', exp2'))
    | _ -> exp


/// Evaluate the output of the parser using bracket abstraction, S K I Y combinators and the evaluation of built-in functions
let combinatorRuntime (input: Result<Ast,string>): Result<Ast,string> = 
    match input with
    | Ok x -> 
        let bindings = Map []
        match bracketAbstract x bindings with
        | Ok x ->
            match eval (interpret x) with
            | Ok x -> Ok x
            | Error x -> Error <| "SKI runtime error: Built-in function evaluation Error: \n" + x
        | Error x -> Error <| "SKI runtime error: Bracket Abstraction Error: \n" + x
    | Error x -> Error <| "Invalid Ast supplied as input to SKI Runtime: \n" + x