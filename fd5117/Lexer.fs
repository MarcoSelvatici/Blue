module Lexer

type BuiltInFunc =
    // Builtin with no special treatment
    | BNot
    | BAssign
    | BHead
    | BTail
    | BSize
    | BImplode
    | BExplode
    | BAppend
    | BStrEq
    // ComparisonOp
    | BGreater
    | BGreaterEq
    | BLess
    | BLessEq
    | BEqual
    // LogicalOp
    | BAnd
    | BOr
    // BitwiseOp
    | BBitAnd
    | BBitOr
    // AdditiveOp
    | BPlus
    | BMinus
    // MultiplicativeOp
    | BMult
    | BDiv

type Literal =
    | IntLit of int
    | BoolLit of bool
    | CharLit of char
    | StringLit of string

type Token =
    | TLiteral of Literal
    | TIdentifier of string
    | TBuiltInFunc of BuiltInFunc
    
    // Keywords
    | KLet
    | KRec
    | KEq
    | KIn
    | KNi
    | KComma
    | KOpenRound
    | KCloseRound
    | KOpenSquare
    | KCloseSquare
    | KLambda
    | KDot
    | KIf
    | KThen
    | KElse
    | KFi
    | KNull

// If other, build recursively a single token.
let rec buildComment input =
    match input with 
    | currChar::tl when not <| currChar.Equals('\n') -> buildComment tl
    | _ -> input

let rec buildString str input =
    match input with 
    | currChar::tl when not <| List.contains currChar ['\\';'\"'] -> buildString (str + string currChar) tl
    | currChar::tl when currChar.Equals('\\') ->      
                        match tl with
                        | esc::tl' when List.contains esc ['a';'b';'f';'n';'r';'t';'v';'\\';'\"';'\'']
                            -> buildString (str + string ('\\' + esc)) tl'
                
                        | _ -> failwithf "lexing error, expected valid ESC sequence" 
    | currChar::tl when currChar.Equals('\"') -> str, tl
    | _ -> failwithf "lexing error, expecting \""

let rec buildChar input =
    match input with 
    | currChar::tl when not <| List.contains currChar ['\\';'\''] -> 
                   match tl with 
                   |'\''::tl' -> currChar, tl'
                   | _ -> failwithf "lexing error, expecting \'"
    | currChar::tl when currChar.Equals('\\') ->      
                        match tl with
                        | esc::tl' when List.contains esc ['a';'b';'f';'n';'r';'t';'v';'\\';'\"';'\'']
                            -> ('\\' + esc), tl'
                        | _ -> failwithf "lexing error, expected valid ESC sequence" 
    | _ -> failwithf "lexing error, char cannot be empty"

let tokeniseT3 (str: string) : Token list =
    // Recursively trying to match a token. 
    let rec tokenise (input: char list) : Token list =
        match input with
        | ' '::tl -> tokenise tl
        | '.'::tl -> [KDot] @ tokenise tl
        | '('::tl -> [KOpenRound] @ tokenise tl
        | ')'::tl -> [KCloseRound] @ tokenise tl
        | '\\'::tl -> [KLambda] @ tokenise tl
        | '+'::tl -> [BPlus |> TBuiltInFunc] @ tokenise tl
        | '-'::tl -> [BMinus |> TBuiltInFunc] @ tokenise tl
        | '*'::tl -> [BMult |> TBuiltInFunc] @ tokenise tl
        | '/'::tl -> 
           match tl with 
           | '/'::tl' -> 
                let rest = buildComment tl'
                tokenise rest
           | _ -> [BDiv |> TBuiltInFunc] @ tokenise tl 
        | '!'::tl -> [BNot |> TBuiltInFunc] @ tokenise tl
        | '>'::tl -> 
           match tl with
           | '='::tl' -> [BGreaterEq |> TBuiltInFunc] @ tokenise tl'
           | _ -> [BGreater |> TBuiltInFunc] @ tokenise tl
        | '<'::tl -> 
           match tl with
           | '='::tl' -> [BLessEq |> TBuiltInFunc] @ tokenise tl'
           | _ -> [BLess |> TBuiltInFunc] @ tokenise tl
        | '='::tl -> 
           match tl with
           | '='::tl' -> [BEqual |> TBuiltInFunc] @ tokenise tl'
           | _ -> [BAssign |> TBuiltInFunc] @ tokenise tl
        | '&'::tl -> 
           match tl with
           | '&'::tl' -> [BAnd |> TBuiltInFunc] @ tokenise tl'
           | _ -> [BBitAnd |> TBuiltInFunc] @ tokenise tl
        | '|'::tl -> 
           match tl with
           | '|'::tl' -> [BOr |> TBuiltInFunc] @ tokenise tl'
           | _ -> [BBitOr |> TBuiltInFunc] @ tokenise tl
        | '\"'::tl -> 
            let str, rest = buildString "" tl
            [str |> StringLit |> TLiteral] @ tokenise rest
        | '\''::tl -> 
            let c, rest = buildChar tl
            [c |> CharLit |> TLiteral] @ tokenise rest
        | [] -> []
        | _ -> []
            // Other case, lets match the whole string and return it.
            
    tokenise <| Seq.toList str 
