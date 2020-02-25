open System

// type ESC =  
//     | "\\a" // alert
//     | "\\b" // backspace
//     | "\\f" // form feed
//     | "\\n" // new line
//     | "\\r" // carriage return
//     | "\\t" // tab
//     | "\\v" // vertical tab
//     | "\\\\" // backslash
//     | "\\" + '\"' // quotation mark
//     | "\\\'" // apostrophe

type Keyword = 
    | KLet
    | KRec
    | KOpenRound // char OK
    | KCloseRound // char OK
    | KLambda // char OK
    | KDot // char OK
    | KIf
    | KThen
    | KElse
    | KFi
    | KIn
    | KPlus // char OK
    | KMinus // char OK
    | KMult // char OK
    | KDiv // char OK
    | KGreater // char OK
    | KGreaterEq // OK
    | KLess // char OK
    | KLessEq // OK
    | KEqual // OK
    | KAssign // char OK
    | KBitWiseAnd  // char OK
    | KBitwiseOr // char OK
    | KCompareAnd // OK
    | KCompareOr // OK
    | KNot // char OK
    | KHead 
    | KTail
    | KSize
    | KAppend
    | KNull
    | KStrEq
    | KImplode
    | KExplode

type Literal =
    | BoolLit of bool
    | IntLit of int
    | StrLit of string

type Token =
    | TIdentifier of string
    | TLit of Literal
    | TKey of Keyword



// If other, build recursively a single token.
let rec buildComment input =
    match input with 
    | currChar::tl when not <| currChar.Equals('\n') -> buildComment tl
    | _ -> input

let tokeniseT3 (str: string) : Token list =
    // Recursively trying to match a token. 
    let rec tokenise (input: char list) : Token list =
        match input with
        | ' '::tl -> tokenise tl
        | '.'::tl -> [KDot |> TKey] @ tokenise tl
        | '('::tl -> [KOpenRound |> TKey] @ tokenise tl
        | ')'::tl -> [KCloseRound |> TKey] @ tokenise tl
        | '\\'::tl -> [KLambda |> TKey] @ tokenise tl
        | '+'::tl -> [KPlus |> TKey] @ tokenise tl
        | '-'::tl -> [KMinus |> TKey] @ tokenise tl
        | '*'::tl -> [KMult |> TKey] @ tokenise tl
        | '/'::tl -> 
           match tl with 
           | '/'::tl' -> 
                let rest = buildComment tl'
                tokenise rest
           | _ -> [KDiv |> TKey] @ tokenise tl 
        | '!'::tl -> [KNot |> TKey] @ tokenise tl
        | '>'::tl -> 
           match tl with
           | '='::tl' -> [KGreaterEq |> TKey] @ tokenise tl'
           | _ -> [KGreater |> TKey] @ tokenise tl
        | '<'::tl -> 
           match tl with
           | '='::tl' -> [KLessEq |> TKey] @ tokenise tl'
           | _ -> [KLess |> TKey] @ tokenise tl
        | '='::tl -> 
           match tl with
           | '='::tl' -> [KEqual |> TKey] @ tokenise tl'
           | _ -> [KAssign |> TKey] @ tokenise tl
        | '&'::tl -> 
           match tl with
           | '&'::tl' -> [KCompareAnd |> TKey] @ tokenise tl'
           | _ -> [KBitWiseAnd |> TKey] @ tokenise tl
        | '|'::tl -> 
           match tl with
           | '|'::tl' -> [KCompareOr |> TKey] @ tokenise tl'
           | _ -> [KBitwiseOr |> TKey] @ tokenise tl
        | [] -> []
        | _ -> []
            // Other case, lets match the whole string and return it.
            
    tokenise <| Seq.toList str 
