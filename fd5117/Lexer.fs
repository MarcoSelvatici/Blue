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

let escSeq = ['\a';'\b';'\f';'\n';'\r';'\t';'\v';'\\';'\"';'\'']

// If other, build recursively a single token.
let rec buildNumber rowCount isFloat number input =
    match input with 
    | currChar::tl when List.contains currChar (['0'..'9']) -> buildNumber rowCount isFloat (number + string currChar) tl
    | currChar::tl when (currChar.Equals('.') && (not isFloat)) -> 
                   match tl with 
                   | num::tl' when List.contains num (['0'..'9']) ->
                        buildNumber rowCount true (number + string currChar) tl
                   | _ -> failwithf "lexing error, expecting decimal digit after dot on line %i" rowCount
    | currChar::tl when List.contains currChar (['+';'-';'*';'/';'=';'<';'>';'&';'|';' ';'\n';'\t']) 
                   -> isFloat, number, input
    | [] -> isFloat, number, input
    | currChar::tl -> failwithf "lexing error, number contains non numeric char: '%c' on line %i" currChar rowCount
    | _ -> failwithf "lexing error, unexpected behaviour in building the number token on line %i" rowCount

let rec buildWord rowCount word input =
    match input with
    | currChar::tl when List.contains currChar (['a'..'z']@['A'..'Z']@['0'..'9']@['_';'\'']) -> buildWord rowCount (word + string currChar) tl
    | currChar::tl when List.contains currChar (['+';'-';'*';'/';'=';'<';'>';'&';'|';' ';'\n';'\t']) 
                  -> word, input
    | [] -> word, input
    | currChar::tl -> failwithf "lexing error, unrecognised non-alphabetic character: '%c' on line: %i" currChar rowCount
    | _ -> failwithf "lexing error, unexpected behaviour on line %i." rowCount 

let rec buildComment input =
    match input with 
    | currChar::tl when not <| currChar.Equals('\n') -> buildComment tl
    | _ -> input

let rec buildString rowCount str input =
    match input with 
    | currChar::tl when not <| List.contains currChar ['\\';'\"'] -> buildString rowCount (str + string currChar) tl
    | currChar::tl when currChar.Equals('\\') ->      
                        match tl with
                        | esc::tl' when List.contains esc ['a';'b';'f';'n';'r';'t';'v';'\\';'\"';'\'']
                            -> buildString rowCount (str + string ('\\' + esc)) tl'
                
                        | _ -> failwithf "lexing error, expected valid ESC sequence on line %i" rowCount 
    | currChar::tl when currChar.Equals('\"') -> str, tl
    | _ -> failwithf "lexing error, expecting closing quotation mark: \" on line %i" rowCount

let rec buildChar rowCount input =
    match input with 
    | currChar::tl when not <| List.contains currChar ['\\';'\''] -> 
                   match tl with 
                   |'\''::tl' -> currChar, tl'
                   | _ -> failwithf "lexing error, expecting \' on line %i" rowCount
    | currChar::tl when currChar.Equals('\\') ->      
                        match tl with
                        | esc::tl' when List.contains esc ['a';'b';'f';'n';'r';'t';'v';'\\';'\"';'\'']
                            -> ('\\' + esc), tl'
                        | _ -> failwithf "lexing error, expected valid ESC sequence on line %i" rowCount 
    | _ -> failwithf "lexing error, char definition cannot be empty on line %i" rowCount

let tokeniseT3 (str: string) : Token list =
    // Recursively trying to match a token. 
    let rec tokenise rowCount (input: char list) : Token list =
        match input with
        | '.'::tl -> [KDot] @ tokenise rowCount tl
        | ','::tl -> [KComma] @ tokenise rowCount tl
        | '('::tl -> [KOpenRound] @ tokenise rowCount tl // add multiline comments!
        | ')'::tl -> [KCloseRound] @ tokenise rowCount tl
        | '['::tl -> [KOpenSquare] @ tokenise rowCount tl
        | ']'::tl -> [KCloseSquare] @ tokenise rowCount tl
        | '\\'::tl -> [KLambda] @ tokenise rowCount tl
        | '+'::tl -> [BPlus |> TBuiltInFunc] @ tokenise rowCount tl
        | '-'::tl -> [BMinus |> TBuiltInFunc] @ tokenise rowCount tl
        | '*'::tl -> [BMult |> TBuiltInFunc] @ tokenise rowCount tl
        | '/'::tl -> 
           match tl with 
           | '/'::tl' -> 
                let rest = buildComment tl'
                tokenise rowCount rest
           | _ -> [BDiv |> TBuiltInFunc] @ tokenise rowCount tl 
        | '!'::tl -> [BNot |> TBuiltInFunc] @ tokenise rowCount tl
        | '>'::tl -> 
           match tl with
           | '='::tl' -> [BGreaterEq |> TBuiltInFunc] @ tokenise rowCount tl'
           | _ -> [BGreater |> TBuiltInFunc] @ tokenise rowCount tl
        | '<'::tl -> 
           match tl with
           | '='::tl' -> [BLessEq |> TBuiltInFunc] @ tokenise rowCount tl'
           | _ -> [BLess |> TBuiltInFunc] @ tokenise rowCount tl
        | '='::tl -> 
           match tl with
           | '='::tl' -> [BEqual |> TBuiltInFunc] @ tokenise rowCount tl'
           | _ -> [KEq] @ tokenise rowCount tl
        | '&'::tl -> 
           match tl with
           | '&'::tl' -> [BAnd |> TBuiltInFunc] @ tokenise rowCount tl'
           | _ -> [BBitAnd |> TBuiltInFunc] @ tokenise rowCount tl
        | '|'::tl -> 
           match tl with
           | '|'::tl' -> [BOr |> TBuiltInFunc] @ tokenise rowCount tl'
           | _ -> [BBitOr |> TBuiltInFunc] @ tokenise rowCount tl
        | '\"'::tl -> 
            let str, rest = buildString rowCount "" tl
            [str |> StringLit |> TLiteral] @ tokenise rowCount rest
        | '\''::tl -> 
            let c, rest = buildChar rowCount tl
            [c |> CharLit |> TLiteral] @ tokenise rowCount rest
        | [] -> []
        | currChar::tl  when List.contains currChar (['\n']) -> 
                        tokenise (rowCount + 1) tl
        | currChar::tl  when List.contains currChar ([' ']@escSeq) -> tokenise rowCount tl
        | currChar::tl when List.contains currChar (['0'..'9']) ->
            let isFloat, number, rest = buildNumber rowCount false "" input
            if isFloat then [float number |> FloatLit |> TLiteral] @ tokenise rowCount rest
            else [int number |> IntLit |> TLiteral] @ tokenise rowCount rest
        | currChar::tl when List.contains currChar (['a'..'z']@['A'..'Z']@['_']) -> 
            let word, rest = buildWord rowCount "" input
            match (string word) with 
            | "true" -> [true |> BoolLit |> TLiteral] @ tokenise rowCount rest
            | "false" -> [false |> BoolLit |> TLiteral] @ tokenise rowCount rest
            | "head" -> [BHead |> TBuiltInFunc] @ tokenise rowCount rest
            | "tail" -> [BTail |> TBuiltInFunc] @ tokenise rowCount rest
            | "size" -> [BSize |> TBuiltInFunc] @ tokenise rowCount rest
            | "implode" -> [BImplode |> TBuiltInFunc] @ tokenise rowCount rest
            | "explode" -> [BExplode |> TBuiltInFunc] @ tokenise rowCount rest
            | "append" -> [BAppend |> TBuiltInFunc] @ tokenise rowCount rest
            | "strEq" -> [BStrEq |> TBuiltInFunc] @ tokenise rowCount rest 
            | "let" -> [KLet] @ tokenise rowCount rest
            | "rec" -> [KRec] @ tokenise rowCount rest
            | "in" -> [KIn] @ tokenise rowCount rest
            | "ni" -> [KNi] @ tokenise rowCount rest
            | "if" -> [KIf] @ tokenise rowCount rest
            | "then" -> [KThen] @ tokenise rowCount rest
            | "else" -> [KElse] @ tokenise rowCount rest
            | "fi" -> [KFi] @ tokenise rowCount rest
            | "null" -> [KNull] @ tokenise rowCount rest
            | _ -> [word |> TIdentifier] @ tokenise rowCount rest
        | currChar::tl -> failwithf "lexing error, unrecognised character '%c' on line %i" currChar rowCount
        | _ -> failwithf "lexing error, unexpected behaviour... nothing was matched on line %i" rowCount
           
    tokenise 0 (Seq.toList str) 
