// Module: SKI combinator Runtime
// Author: oss1017 (Oliver Stiff)

module rec SKIRuntime

open SharedTypes

/// Print function
let print x =
    printfn "%A" x


 /// Print and return (used in pipeline)
let pipePrint x =
    print x; x 


/// build error msg
let buildErrorSKI =
    SKIRuntimeError >> Error


/// Bracket abstraction and substituting functions when they are called (by making use of function name to body fn bindings) 
let bracketAbstract (input: Ast) (bindings: Map<string, Ast>): Result<Ast,ErrorT> =
    match input with
    | Combinator _ | Literal _ | BuiltInFunc _ | SeqExp _ | Null -> input |> Ok

    //    1.  T[x] => x
    // Check in bindings to see if that identfier has been defined
    | Identifier x ->
        if bindings.ContainsKey x
        then
            match bracketAbstract bindings.[x] bindings with
            | Ok x -> Ok x
            | Error x -> Error x
        else Identifier x |> Ok
        // An error could be returned if identifier is not in bindings: possible error mesage is below
        //else Error <| sprintf "Undefined identifier: \'%s\'" x


    // The following two matches fix an issue with ifThenElse evaluation 
    // order when it contains variables when in a lambda.
    // This in turn allows for recursion to work without knowing if
    // the function is recursive in advance (current parser does not indicate if a fn is recursive).
    | FuncApp (Identifier exp1, exp2) ->
        if bindings.ContainsKey exp1
        then bracketAbstract (FuncApp (bindings.[exp1], exp2)) bindings
        else buildErrorSKI <| sprintf "Undefined identifier: \'%s\'" exp1 

    | FuncDefExp { FuncName = name; FuncBody = LambdaExp lam; Rest = exp } ->
        bracketAbstract exp (bindings.Add(name, LambdaExp lam))

    //substitute expressions into lambdas
    | FuncApp ( LambdaExp { LambdaParam = name; LambdaBody = exp1 },  exp2) ->
        match bracketAbstract exp2 bindings with
        | Ok x ->
            bracketAbstract exp1 (bindings.Add(name, x))
        | Error x -> Error x
        
    //    2.  T[(E₁ E₂)] => (T[E₁] T[E₂])
    | FuncApp (exp1, exp2) -> 
        match bracketAbstract exp1 bindings, bracketAbstract exp2 bindings with
        | Ok x, Ok y       -> FuncApp (x, y) |> Ok
        | Error (SKIRuntimeError x), Error (SKIRuntimeError y) -> buildErrorSKI <|  x + ", " + y
        | Error x, _       -> Error x
        | _, Error y       -> Error y

    | LambdaExp x ->
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
        | { LambdaParam = name1; LambdaBody = LambdaExp { LambdaParam = name2; LambdaBody = exp } } when isFree name1 exp ->
            match bracketAbstract (LambdaExp { LambdaParam = name2; LambdaBody = exp } ) bindings with
            | Ok x -> bracketAbstract (LambdaExp { LambdaParam = name1; LambdaBody = x } ) bindings
            | Error x -> Error x
            
        //    6.  T[λx.(E₁ E₂)] => (S T[λx.E₁] T[λx.E₂]) (if x occurs free in E₁ or E₂)
        | { LambdaParam = name; LambdaBody = FuncApp (exp1, exp2) } when isFree name exp1 || isFree name exp2 ->
            let x = bracketAbstract ( LambdaExp { LambdaParam = name; LambdaBody = exp1 }) bindings
            let y = bracketAbstract ( LambdaExp { LambdaParam = name; LambdaBody = exp2 }) bindings
            match x, y with
            | Ok x, Ok y       -> FuncApp (FuncApp (Combinator S, x ), y ) |> Ok 
            | Error (SKIRuntimeError x), Error (SKIRuntimeError y) -> buildErrorSKI <| x + ", " + y
            | Error x, _       -> Error x
            | _, Error y       -> Error y
            
        | _ ->
            buildErrorSKI <| sprintf "Unable to bracket abstract lambda %A" x

    //function definitiions and bindings
    | FuncDefExp { FuncName = name; FuncBody = body; Rest = exp } ->
        match bracketAbstract body bindings with
        | Ok x ->
            bracketAbstract exp (bindings.Add(name, x))
        | Error x -> Error x

    // Only bracket abstract one branch after having evaluated the condition
    | IfExp (condition,expT,expF) ->
            match bracketAbstract condition bindings with
            | Ok x -> 
                match evalBuiltin (x) with
                | Ok (Literal (BoolLit true))  -> 
                    bracketAbstract expT bindings
                | Ok (Literal (BoolLit false)) -> 
                    bracketAbstract expF bindings
                | _ -> buildErrorSKI "Unexpected value in ifThenElse condition"
            | Error x -> Error x

    | FuncAppList _     -> buildErrorSKI "FuncAppList should not be returned by parser"
    | IdentifierList _  -> buildErrorSKI "IdentifierList should not be returned by parser"


/// Determine if a variable is free in an expression
let isFree (var:string) (exp: Ast): bool =
    let rec free (exp: Ast): string List =
        match exp with
        | Identifier x -> [x]
        | FuncApp (exp1, exp2) ->
            free exp1 @ free exp2
        | LambdaExp { LambdaParam = name; LambdaBody = exp1 } ->
            List.filter (fun x -> x <> name) (free exp1)
        | _ -> []

    List.contains var (free exp)


/// Evaluate Ast built-in functions
let evalBuiltin (input:Ast) : Result<Ast,ErrorT> =
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
        
        match evalBuiltin exp with
        | Ok x -> 
            match imp x with
            | None -> buildErrorSKI "Cannot implode argument of type which is not string list"
            | Some x -> x |> StringLit |> Literal |> Ok
            
        | Error x -> Error x 
            
            
    // explode string
    | FuncApp( BuiltInFunc Explode, x) -> 
        match evalBuiltin x with
        | Ok (Literal (StringLit x)) ->
            Seq.toList x |> List.map (string) |> buildList |> Ok
        | _ ->
            buildErrorSKI "cannot explode argument of type which is not string"

    // head
    | FuncApp( BuiltInFunc Head, x) -> 
        match evalBuiltin x with
        | Ok (SeqExp (head, tail)) ->
            head |> Ok
        | Ok Null -> Ok Null // empty list case
        | _ ->
            buildErrorSKI "Error getting head of list/sequence"

    // tail
    | FuncApp( BuiltInFunc Tail, x) -> 
        match evalBuiltin x with
        | Ok (SeqExp (head, tail)) ->
            tail |> Ok
        | Ok Null -> Ok Null // empty list case
        | _ ->
            buildErrorSKI "Error getting tail of list/sequence"

    // size of list
    | FuncApp( BuiltInFunc Size, exp) -> 
        let rec sizeOf x =
            match x with
            | Null -> 0 // empty list case
            | SeqExp (head, Null) ->
                1 
            | SeqExp (head, tail) ->
                1 + (sizeOf tail)
            | _ ->
               -1

        match evalBuiltin exp with
        | Ok x ->
            let len = sizeOf x
            if len <> -1
            then len |> IntLit |> Literal |> Ok
            else buildErrorSKI <| sprintf "Error getting size of list: Invalid input: %A" x
        | Error x -> Error x            

    //unary built-in functions
    | FuncApp( BuiltInFunc op, x) -> 
        let x' = evalBuiltin x
        match op, x' with
        
        //boolean op
        | Not, Ok (Literal (BoolLit n)) -> 
            Literal (BoolLit (not n)) |> Ok
        | _ ->
            buildErrorSKI <| sprintf "Error evaluating built-in function with 1 argument: opertor \'%A\'" op

    // Built-in functon w/ 2 args
    | FuncApp( FuncApp( BuiltInFunc op, x), y) -> 
        let x' = evalBuiltin x
        let y' = evalBuiltin y
        match op, x', y' with
        
        //arithmetic ops
        | Mult, Ok (Literal (IntLit n)), Ok (Literal (IntLit m))  -> 
            Literal (IntLit (n * m)) |> Ok
        | Div,Ok (Literal (IntLit n)), Ok (Literal (IntLit m))    ->
            if m = 0
            then buildErrorSKI "Division by 0"
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

        // Passthrough for partially applied functions
        // if we only wanted fully evaluated and reduced expressions this should return an error
        | _ -> input |> Ok

    | Combinator _ | SeqExp _ | Identifier _ | FuncApp _ | BuiltInFunc _ | Null | Literal _ -> input |> Ok

    | IfExp _           -> buildErrorSKI "IfExp should not exist after bracket abstraction"
    | LambdaExp _       -> buildErrorSKI "LambdaExp should not exist after bracket abstraction"
    | FuncDefExp _      -> buildErrorSKI "FuncDefExp should not exist after bracket abstraction"
    | FuncAppList _     -> buildErrorSKI "FuncAppList should not be returned by parser"
    | IdentifierList _  -> buildErrorSKI "IdentifierList should not be returned by parser"


/// Builds a list in our language out of an F# list
let rec buildList lst =
    match lst with
    | [] ->  Null
    | [x] -> SeqExp (Literal (StringLit x), Null)
    | head::tail -> SeqExp ( Literal (StringLit head), buildList tail )


/// SKI combinator reduction
let rec combinatorReduc (exp:Ast) : Ast =
    match exp with
    | Combinator x ->
        exp
    | FuncApp( Combinator I, x) ->
        combinatorReduc x
    | FuncApp(FuncApp (Combinator K, x), y) ->
        combinatorReduc x
    | FuncApp (FuncApp (FuncApp (Combinator S, x), y), z) ->
        combinatorReduc (FuncApp (FuncApp (x, z), FuncApp (y, z)))
    // if exp is fully simplified we need to stop calling interpret
    // check if it has changed from what it was at prev "iteration"
    | FuncApp (exp1, exp2) ->             
        let exp1' = combinatorReduc exp1
        let exp2' = combinatorReduc exp2
        if exp1 = exp1' && exp2 = exp2'
        then FuncApp (exp1, exp2)
        else combinatorReduc (FuncApp (exp1', exp2'))
    | _ -> exp  // let built-in functions pass through to then be evaluated in evalBuiltin


/// Evaluate the output of the parser using bracket abstraction, S K I Y combinators and the evaluation of built-in functions
let combinatorRuntime (input: Ast): Result<Ast,ErrorT> = 
    let bindings = Map []
    match bracketAbstract input bindings with
    | Ok x ->
        match evalBuiltin (combinatorReduc x) with
        | Ok x -> Ok x
        | Error (SKIRuntimeError x) -> buildErrorSKI <| "SKI runtime error: Built-in function evaluation Error: \n" + x
        | Error x -> Error x // will never occur
    | Error (SKIRuntimeError x) -> buildErrorSKI <| "SKI runtime error: Bracket Abstraction Error: \n" + x
    | Error x -> Error x // will never occur
