module Lexer

type BuiltInFunc =
    // Builtin with no special treatment
    | BNot        // OK
    | BHead       // OK
    | BTail       // OK
    | BSize       // OK
    | BImplode    // OK
    | BExplode    // OK
    | BAppend     // OK
    | BStrEq      // OK
    // ComparisonOp
    | BGreater    // OK
    | BGreaterEq  // OK
    | BLess       // OK
    | BLessEq     // OK
    | BEqual      // OK
    // LogicalOp
    | BAnd        // OK
    | BOr         // OK
    // BitwiseOp
    | BBitAnd     // OK
    | BBitOr      // OK
    // AdditiveOp
    | BPlus       // OK
    | BMinus      // OK
    // MultiplicativeOp
    | BMult       // OK
    | BDiv        // OK

type Literal =
    | IntLit of int        // OK
    | FloatLit of float    // OK
    | BoolLit of bool      // OK
    | CharLit of char      // OK
    | StringLit of string  // OK

type Token =
    | TLiteral of Literal       
    | TIdentifier of string
    | TBuiltInFunc of BuiltInFunc
    
    // Keywords 
    | KLet           // OK
    | KRec           // OK
    | KEq            // OK
    | KIn            // OK
    | KNi            // OK
    | KComma         // OK
    | KOpenRound     // OK
    | KCloseRound    // OK
    | KOpenSquare    // OK
    | KCloseSquare   // OK
    | KLambda        // OK
    | KDot           // OK
    | KIf            // OK
    | KThen          // OK
    | KElse          // OK
    | KFi            // OK
    | KNull          // OK

let esc = ['\a';'\b';'\f';'\n';'\r';'\t';'\v';'\\';'\"';'\'']
// If other, build recursively a single token.
let rec buildNumber isFloat number input =
    match input with 
    | currChar::tl when List.contains currChar (['0'..'9']) -> buildNumber isFloat (number + string currChar) tl
    | currChar::tl when (currChar.Equals('.') && (not isFloat)) -> 
                   match tl with 
                   | num::tl' when List.contains num (['0'..'9']) ->
                        buildNumber true (number + string currChar) tl
                   | _ -> failwithf "lexing error, expecting decimal digit after dot"
    | currChar::tl when List.contains currChar (['+';'-';'*';'/';'=';'<';'>';'&';'|';' ']) 
                   -> isFloat, number, input
    | [] -> isFloat, number, input
    | _ -> failwithf "lexing error, number contains non numeric char"

let rec buildWord word input =
    match input with
    | currChar::tl when List.contains currChar (['a'..'z']@['A'..'Z']) -> buildWord (word + string currChar) tl
    | currChar::tl when List.contains currChar (['+';'-';'*';'/';'=';'<';'>';'&';'|';' ']@esc) 
                  -> word, input
    | [] -> word, input
    | currChar::tl -> failwithf "lexing error, unrecognised non-alphabetic character: %c" currChar
    | _ -> failwithf "lexing error, unexpected behaviour" 

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
        | '.'::tl -> [KDot] @ tokenise tl
        | ','::tl -> [KComma] @ tokenise tl
        | '('::tl -> [KOpenRound] @ tokenise tl
        | ')'::tl -> [KCloseRound] @ tokenise tl
        | '['::tl -> [KOpenSquare] @ tokenise tl
        | ']'::tl -> [KCloseSquare] @ tokenise tl
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
           | _ -> [KEq] @ tokenise tl
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
        | currChar::tl  when List.contains currChar ([' ']@esc) -> tokenise tl
        | currChar::tl when List.contains currChar (['0'..'9']) ->
            let isFloat, number, rest = buildNumber false "" input
            if isFloat then [float number |> FloatLit |> TLiteral] @ tokenise rest
            else [int number |> IntLit |> TLiteral] @ tokenise rest
        | currChar::tl when List.contains currChar (['a'..'z']@['A'..'Z']) -> 
            let word, rest = buildWord "" input
            match (string word) with 
            | "true" -> [true |> BoolLit |> TLiteral] @ tokenise rest
            | "false" -> [false |> BoolLit |> TLiteral] @ tokenise rest
            | "head" -> [BHead |> TBuiltInFunc] @ tokenise rest
            | "tail" -> [BTail |> TBuiltInFunc] @ tokenise rest
            | "size" -> [BSize |> TBuiltInFunc] @ tokenise rest
            | "implode" -> [BImplode |> TBuiltInFunc] @ tokenise rest
            | "explode" -> [BExplode |> TBuiltInFunc] @ tokenise rest
            | "append" -> [BAppend |> TBuiltInFunc] @ tokenise rest
            | "strEq" -> [BStrEq |> TBuiltInFunc] @ tokenise rest
            | "let" -> [KLet] @ tokenise rest
            | "rec" -> [KRec] @ tokenise rest
            | "in" -> [KIn] @ tokenise rest
            | "ni" -> [KNi] @ tokenise rest
            | "if" -> [KIf] @ tokenise rest
            | "then" -> [KThen] @ tokenise rest
            | "else" -> [KElse] @ tokenise rest
            | "fi" -> [KFi] @ tokenise rest
            | "null" -> [KNull] @ tokenise rest
            | _ -> [word |> TIdentifier] @ tokenise rest
        | currChar::tl -> failwithf "lexing error, unrecognised character %c" currChar
        | _ -> failwithf "lexing error, unexpected behaviour"
           
    tokenise <| Seq.toList str 
