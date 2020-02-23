// Module: SKI combinator Runtime
// Author: oss1017 (Oliver Stiff)

module rec ski_combinator

open TokeniserStub
open Parser

/// Print function
let print x =
    printfn "%A" x

 /// Print and return (used in pipeline)
let pipePrint x =
    print x; x


///Determine if a variable is free in an expression
let isFree (var:string) (exp: Ast): bool =
    let rec free (exp: Ast): string List =
        match exp with
        | Identifier x 
            -> [x]
        | FuncApp (exp1, exp2)
            -> free exp1 @ free exp2
        | Lambda { LambdaParam = name; LambdaBody = exp1 }
            -> List.filter (fun x -> x <> name) (free exp1)
        | _ 
            -> []

    List.contains var (free exp)


/// Builds a list in our language out of an f# list
let rec buildList lst =
    match lst with
    | [] ->  SeqExp (Null, Null)
    | [x] -> SeqExp (Literal (StringLit x), Null)
    | head::tail -> SeqExp ( Literal (StringLit head), buildList tail )



/// Evaluate Ast built-in functions
let eval (input:Ast) : Ast =
    match input with
    
    // implode string list
    | FuncApp( BuiltInFunc Implode, x) -> 
        let rec imp lst =
            match lst with
            | SeqExp ( Literal (StringLit head) , Null) ->
                head
            | SeqExp (Literal (StringLit head), tail) ->
                head + imp tail
            | _ ->
                failwith "Error: cannot implode argument of type which is not string list"
        x |> eval |> imp |> StringLit |> Literal

    // explode string
    | FuncApp( BuiltInFunc Explode, x) -> 
        match eval x with
        | Literal (StringLit x) ->
            Seq.toList x |> List.map (string) |> buildList
        | _ ->
            failwith "Error: cannot explode argument of type which is not string"

    //head
    | FuncApp( BuiltInFunc Head, x) -> 
        match eval x with
        | SeqExp (head, tail) ->
            head
        | _ ->
            failwith "Error getting head of list/sequence"

    //tail
    | FuncApp( BuiltInFunc Tail, x) -> 
        match eval x with
        | SeqExp (head, tail) ->
            tail
        | _ ->
            failwith "Error getting tail of list/sequence"

    //size of list
    | FuncApp( BuiltInFunc Size, x) -> 
        let rec sizeOf x =
            match x with
            | SeqExp (Null, Null) ->
                0
            | SeqExp (head, Null) ->
                1 
            | SeqExp (head, tail) ->
                1 + (sizeOf tail)
            | _ ->
                failwith "Error getting size of list"
        Literal (IntLit (x |> eval |> sizeOf))

    //unary built-in functions
    | FuncApp( BuiltInFunc op, x) -> 
        let x' = eval x
        match op, x' with
        
        //boolean op
        | Not, Literal (BoolLit n) -> 
            Literal (BoolLit (not n))
        | _ ->
            failwith "Error evaluating built-in function with 1 argument"

    // Built-in functon w/ 2 args
    | FuncApp( FuncApp( BuiltInFunc op, x), y) -> 
        let x' = eval x
        let y' = eval y
        match op, x', y' with
        
        //arithmetic ops
        | Mult, Literal (IntLit n), Literal (IntLit m)  -> 
            Literal (IntLit (n * m))
        | Div, Literal (IntLit n), Literal (IntLit m)   ->
            Literal (IntLit (n / m))
        | Plus, Literal (IntLit n), Literal (IntLit m)  -> 
            Literal (IntLit (n + m))
        | Minus, Literal (IntLit n), Literal (IntLit m) ->
            Literal (IntLit (n - m))
        
        //boolean ops
        | And, Literal (BoolLit n), Literal (BoolLit m) ->
            Literal (BoolLit (n && m))
        | Or, Literal (BoolLit n), Literal (BoolLit m)  ->
            Literal (BoolLit (n || m))

        //comparaison ops (bool)
        | Greater, Literal (IntLit n), Literal (IntLit m)   ->
            Literal (BoolLit (n > m))
        | GreaterEq, Literal (IntLit n), Literal (IntLit m) ->
            Literal (BoolLit (n >= m))
        | Less, Literal (IntLit n), Literal (IntLit m)      ->
            Literal (BoolLit (n < m))
        | LessEq, Literal (IntLit n), Literal (IntLit m)    ->
            Literal (BoolLit (n <= m))
        | Equal, Literal (IntLit n), Literal (IntLit m)     ->
            Literal (BoolLit (n = m))

        //Error
        | _ -> input

    //////////////////  END: BUILT-IN FUNCTIONS  //////////////////
    | Combinator _ | SeqExp _ | Identifier _ | FuncApp _ | BuiltInFunc _ | Null | Literal _ -> input

    | IfExp _ -> input // this should be dealt with in bracket abstraction
                       // but if for some reason it can't evaluate it,
                       // it should be passed to output

    | Lambda _          -> failwith "Lambda should not exist after bracket abstraction"
    | FuncDefExp _      -> failwith "FuncDefExp should not exist after bracket abstraction"
    | RoundExp _        -> failwith "RoundExp should not be returned by parser"
    | FuncAppList _     -> failwith "FuncAppList should not be returned by parser"
    | IdentifierList _  -> failwith "IdentifierList should not be returned by parser"


/// Bracket abstraction and substituting functions when they are called by making use of function name to body bindings. 
let bracketAbstract (input: Ast) (bindings: Map<string, Ast>): Ast =
    match input with
    | Combinator _ | Literal _ | BuiltInFunc _ | SeqExp _ | Null ->  input

    //    1.  T[x] => x
    // Check in bindings to see if that identfier has been defined
    | Identifier x ->
        if bindings.ContainsKey x
        then bindings.[x]
        else Identifier x    // Could also return an error "Unkown ID"

    //substitute expressions into lambdas
    | FuncApp ( Lambda { LambdaParam = name; LambdaBody = exp1 },  exp2) ->
        let bindings = bindings.Add(name, bracketAbstract exp2 bindings)
        bracketAbstract exp1 bindings

    //    2.  T[(E₁ E₂)] => (T[E₁] T[E₂])
    | FuncApp (exp1, exp2) -> 
        FuncApp (bracketAbstract exp1 bindings, bracketAbstract exp2 bindings)
    
    | Lambda x ->
        match x with
        //    3.  T[λx.E] => (K T[E]) (if x does not occur free in E)
        | { LambdaParam = name; LambdaBody = exp } when not (isFree name exp) ->
            FuncApp (Combinator K, bracketAbstract exp bindings)

        //    4.  T[λx.x] => I
        | { LambdaParam = name; LambdaBody = Identifier exp } when name = exp ->
            Combinator I

        //    5.  T[λx.λy.E] => T[λx.T[λy.E]] (if x occurs free in E) 
        | { LambdaParam = name1; LambdaBody = Lambda { LambdaParam = name2; LambdaBody = exp } } when isFree name1 exp ->
            bracketAbstract (Lambda { LambdaParam = name1; LambdaBody = bracketAbstract (Lambda { LambdaParam = name2; LambdaBody = exp } ) bindings } ) bindings

        //    6.  T[λx.(E₁ E₂)] => (S T[λx.E₁] T[λx.E₂]) (if x occurs free in E₁ or E₂)
        | { LambdaParam = name; LambdaBody = FuncApp (exp1, exp2) } when isFree name exp1 || isFree name exp2 ->
            FuncApp (FuncApp (Combinator S, bracketAbstract ( Lambda { LambdaParam = name; LambdaBody = exp1 }) bindings ), bracketAbstract ( Lambda { LambdaParam = name; LambdaBody = exp2 }) bindings )
        | _ ->
            failwith "Error doing bracket abstraction"
    
    //function definitiions and bindings
    | FuncDefExp { FuncName = name; FuncBody = body; Rest = exp } ->
        let bindings = bindings.Add(name, bracketAbstract body bindings)
        bracketAbstract exp bindings

    | IfExp (condition,expT,expF) ->
            match eval (bracketAbstract condition bindings) with
            | Literal (BoolLit true)  -> 
                bracketAbstract expT bindings
            | Literal (BoolLit false) -> 
                bracketAbstract expF bindings
            | _ -> failwith "Unexpected value in ifThenElse condition"

    | RoundExp _        -> failwith "should not be returned by parser"
    | FuncAppList _     -> failwith "should not be returned by parser"
    | IdentifierList _  -> failwith "should not be returned by parser"


//evaluate/simplify SKI exp
///SKI combinator reduction
let rec interpret (exp:Ast) :Ast =
    //funcapp is used as a tree
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


let combinatorRuntime (input: Ast): Ast = 
    let bindings = Map []
    (input, bindings)
    ||> bracketAbstract
    |> interpret
    |> eval