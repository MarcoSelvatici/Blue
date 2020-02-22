// Module: SKI combinator Runtime
// Author: oss1017 (Oliver Stiff)

module rec ski_combinator

open TokeniserStub
open Parser

let print x =
    printfn "%A" x

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


///Evaluate Ast built-in functions
let eval (input:Ast) : Ast =
    match input with
    //literals
    | Literal x ->
        Literal x

    ////////////////// START: BUILT-IN FUNCTIONS //////////////////
    
    //head
    | FuncApp( BuiltInFunc Head, x) -> 
        match x with
        | SeqExp (head, tail) ->
            head
        | _ ->
            failwith "Error getting head of list/sequence"

    //tail
    | FuncApp( BuiltInFunc Tail, x) -> 
        match x with
        | SeqExp (head, tail) ->
            tail
        | _ ->
            failwith "Error getting tail of list/sequence"

    //size: works on lists created by using nested pairs, e.g. SeqExp ( Literal (IntLit 1), SeqExp ( Literal (IntLit 2),  SeqExp ( Literal (IntLit 3), Null ))) 
    | FuncApp( BuiltInFunc Size, x) -> 
        let rec sizeOf x =
            match x with
            | SeqExp (head, Null) ->
                1
            | SeqExp (head, tail) ->
                1 + (sizeOf tail)
            | _ ->
                failwith "Error getting size of list"
        Literal (IntLit (sizeOf x))


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
        
        //replace with church bools if needed
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
        | _ -> pipePrint input

    //////////////////  END: BUILT-IN FUNCTIONS  //////////////////
            

    | _ -> input


let tmp = FuncDefExp {FuncName = "f"; FuncBody = Literal (IntLit 2); Rest = FuncDefExp {FuncName = "g"; FuncBody = Literal (StringLit "aaa"); Rest = Identifier "z";};}

let bracketAbstract (input: Ast) (bindings: Map<string, Ast>): Ast =
    match input with
    //    1.  T[x] => x
    //Identifier
    | Identifier x ->
        if bindings.ContainsKey x
        then bindings.[x]
        else Identifier x
        

    //sub literals into lambdas
    | FuncApp ( Lambda { LambdaParam = name; LambdaBody = exp1 },  exp2) ->
        let bindings  = bindings.Add(name, bracketAbstract exp2 bindings)
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
            FuncApp (FuncApp (Combinator I, bracketAbstract ( Lambda { LambdaParam = name; LambdaBody = exp1 }) bindings ), bracketAbstract ( Lambda { LambdaParam = name; LambdaBody = exp2 }) bindings )
        | _ ->
            failwith "Error doing bracket abstraction"
    
    //function definitiions and bindings
    | FuncDefExp { FuncName = name; FuncBody = body; Rest = exp } ->
        let bindings = bindings.Add(name, bracketAbstract body bindings)
        bracketAbstract exp bindings

    // S K I Y
    | Combinator x ->
        Combinator x

    | _ 
        -> input


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
    |> pipePrint
    |> interpret
    |> pipePrint
    |> eval