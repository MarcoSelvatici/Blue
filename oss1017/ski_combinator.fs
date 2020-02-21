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

    //unary built-in functions
    |FuncApp( BuiltInFunc op, x) -> 
        let x' = eval x
        match op, x' with
        
        //boolean op
        | Not, Literal (BoolLit n) -> 
            Literal (BoolLit (not n))

        //head (lst)

        //tail (lst)

        //size
        | _ ->
            failwith "Error evaluating built-in function with 1 argument"
    | _ -> 







