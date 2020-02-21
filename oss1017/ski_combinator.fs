// Module: SKI combinator Runtime
// Author: oss1017 (Oliver Stiff)

module rec ski_combinator

open TokeniserStub
open Parser

let print x =
    printfn "%A" x

let pipePrint x =
    print x; x


let eval (input:Ast): Ast =
    match input with
    //literals
    | Literal x ->
        Literal x
       
    //Identifier
    | Identifier x ->
        Identifier x

    //Lambdas










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
    |FuncApp( BuiltInFunc op, x) -> 
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
        | Greater, Literal (BoolLit n), Literal (BoolLit m)   ->
            Literal (BoolLit (n > m))
        | GreaterEq, Literal (BoolLit n), Literal (BoolLit m) ->
            Literal (BoolLit (n >= m))
        | Less, Literal (BoolLit n), Literal (BoolLit m)      ->
            Literal (BoolLit (n < m))
        | LessEq, Literal (BoolLit n), Literal (BoolLit m)    ->
            Literal (BoolLit (n <= m))
        | Equal, Literal (BoolLit n), Literal (BoolLit m)     ->
            Literal (BoolLit (n = m))

        //Error
        | _ -> failwith "Error evaluating built-in fucntion with two arguments"

        //////////////////  END: BUILT-IN FUNCTIONS  //////////////////

    | _ -> failwith "Error evaluating"