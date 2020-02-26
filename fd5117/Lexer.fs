module Lexer

type BuiltInFunc =
    // Builtin with no special treatment.
    | BNot        
    | BHead       
    | BTail       
    | BSize       
    | BImplode    
    | BExplode    
    | BAppend     
    | BStrEq      
    // ComparisonOp.
    | BGreater    
    | BGreaterEq  
    | BLess       
    | BLessEq     
    | BEqual      
    // LogicalOp.
    | BAnd        
    | BOr         
    // BitwiseOp
    | BBitAnd     
    | BBitOr      
    // AdditiveOp.
    | BPlus       
    | BMinus      
    // MultiplicativeOp.
    | BMult       
    | BDiv       

type Literal =
    | IntLit of int        
    | FloatLit of float    
    | BoolLit of bool      
    | CharLit of char      
    | StringLit of string  

type Token =
    | TLiteral of Literal       
    | TIdentifier of string
    | TBuiltInFunc of BuiltInFunc
    
    // Keywords. 
    | KLet          
    | KEq           
    | KRec          
    | KIn           
    | KNi           
    | KComma        
    | KSemiColon       
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

// List of escape sequences supported in f#.
let escSeq = ['a';'b';'f';'n';'r';'t';'v';'\\';'\"';'\'']


// If the current character is a number this function is called, trying to parse an int or a float.
let rec buildNumber rowCount isFloat number input =
    match input with 
    // While the current char is a number, keep adding it to the result.
    | currChar::tl when List.contains currChar (['0'..'9']) -> 
        buildNumber rowCount isFloat (number + string currChar) tl
    // If char = '.' either is a Float (if it is the first dot encountered) or throw an error.
    | currChar::tl when (currChar.Equals('.') && (not isFloat)) ->  
        match tl with 
        | num::tl' when List.contains num (['0'..'9']) ->
            buildNumber rowCount true (number + string currChar) tl
        | _ -> failwithf "lexing error, expecting decimal digit after dot on line %i" rowCount
    // If current char is a valid char that breaks the number, return the result.
    | currChar::tl when List.contains currChar (['+';'-';'*';'/';'=';'<';'>';'&';'|';' ';'\n';'\t']) 
        -> isFloat, number, input
    // Analoguely, if string is over, return valid result.
    | [] -> isFloat, number, input
    // Else return an error, as specific as possible.
    | currChar::tl -> failwithf "lexing error, number contains non numeric char: '%c' on line %i" currChar rowCount
    | _ -> failwithf "lexing error, unexpected behaviour in building the number token on line %i" rowCount


// If the current character is a string this function is called, trying to parse an identifier or a keyword/builtin function.
let rec buildWord rowCount word input =
    match input with
    // While the current character is a valid identifier character, keep adding it to the result.
    | currChar::tl when List.contains currChar (['a'..'z']@['A'..'Z']@['0'..'9']@['_';'\'']) 
        -> buildWord rowCount (word + string currChar) tl
    // If is a valid character to break the sequence, return valid result.
    | currChar::tl when List.contains currChar (['+';'-';'*';'/';'=';'<';'>';'&';'|';' ';'.';'\n';'\t';'(';'[';')';']']) 
        -> word, input
    // Analoguely, if string is over, return valid result.
    | [] -> word, input
    // Else return an error, as specific as possible.
    | currChar::tl -> failwithf "lexing error, unrecognised non-alphabetic character: '%c' on line: %i" currChar rowCount
    | _ -> failwithf "lexing error, unexpected behaviour on line %i." rowCount 

// Function called when inline comment "//" is encountered. 
let rec buildInlineComment input =
    match input with 
    // Discard everything up to newline.
    | currChar::tl when not <| currChar.Equals('\n') -> buildInlineComment tl
    | _ -> input

// Function called when multiline comment "(*" is encountered. 
let rec buildMultilineComment rowCount input =
    match input with 
    // Discard everything up to newline.
    | currChar::tl when currChar.Equals ('\n') -> buildMultilineComment (rowCount + 1) tl
    | currChar::tl when not <| currChar.Equals ('*') -> buildMultilineComment rowCount tl
    | currChar::tl when currChar.Equals ('*') -> 
        match tl with 
        | nextChar::tl' when nextChar.Equals(')') -> rowCount, tl'
        | _ -> buildMultilineComment rowCount tl
    | _ -> rowCount, input

// Function called when open quotation marks are encountered '"'.
let rec buildString rowCount str input =
    match input with 
    // Match everything in the string that is not a closing quotation mark or an escape character.
    | currChar::tl when not <| List.contains currChar ['\\';'\"'] -> buildString rowCount (str + string currChar) tl
    // If an escape character is matched, try to match a valid escape sequence or throw an error.
    | currChar::tl when currChar.Equals('\\') ->      
        match tl with
        | esc::tl' when List.contains esc escSeq
            -> buildString rowCount (str + string ('\\' + esc)) tl'
        | esc::_ -> failwithf "lexing error, expected valid ESC sequence on line %i: \\%c is not valid" rowCount esc 
        | _ -> failwithf "lexing error, expected valid ESC sequence on line %i" rowCount  
    // If closing quotation mark is encountered, return valid result. Else throw an error.
    | currChar::tl when currChar.Equals('\"') -> str, tl
    | _ -> failwithf "lexing error, expecting closing quotation mark: '\"' on line %i" rowCount

// Function called when open apostrophe is met "'".
let rec buildChar rowCount input =
    // Match everything that is not a closing apostrophe or an escape character.
    match input with 
    | currChar::tl when not <| List.contains currChar ['\\';'\''] -> 
        // If closing apostrophe is then met, char is valid. Else throw error.
        match tl with 
        |'\''::tl' -> currChar, tl'
        | _ -> failwithf "lexing error, expecting closing apostrophe: '\'' on line %i" rowCount
    // If escape character is met, try to match valid escape sequence or throw an error.
    | currChar::tl when currChar.Equals('\\') ->      
        match tl with
        | esc::tl' when List.contains esc escSeq
            -> ('\\' + esc), tl'
        | esc::_ -> failwithf "lexing error, expected valid ESC sequence on line %i: \\%c is not valid" rowCount esc 
        | _ -> failwithf "lexing error, expected valid ESC sequence on line %i" rowCount 
    | _ -> failwithf "lexing error, char definition cannot be empty on line %i" rowCount

// Tokenise function: takes the program in string form and returns a list of Tokens or a lexing error.
let tokeniseT3 (str: string) =
    // Recursively trying to match a token. 
    let rec tokenise rowCount (input: char list) =
        match input with
        // Single/Coupled character -> Token matching, no helper functions needed.
        | '.'::tl -> [KDot] @ tokenise rowCount tl
        | ','::tl -> [KComma] @ tokenise rowCount tl
        | ';'::tl -> [KSemiColon] @ tokenise rowCount tl
        | '('::tl -> 
           // If the '(' char is followed by '*' -> start of a multiline comment. 
           match tl with 
           | '*'::tl' -> 
                let rowCount', rest = buildMultilineComment rowCount tl'
                tokenise rowCount' rest
           | _ -> [KOpenRound] @ tokenise rowCount tl
        | ')'::tl -> [KCloseRound] @ tokenise rowCount tl
        | '['::tl -> [KOpenSquare] @ tokenise rowCount tl
        | ']'::tl -> [KCloseSquare] @ tokenise rowCount tl
        | '\\'::tl -> [KLambda] @ tokenise rowCount tl
        // Arithmetic Builtin Functions
        | '+'::tl -> [BPlus |> TBuiltInFunc] @ tokenise rowCount tl
        | '-'::tl -> [BMinus |> TBuiltInFunc] @ tokenise rowCount tl
        | '*'::tl -> [BMult |> TBuiltInFunc] @ tokenise rowCount tl
        | '/'::tl -> 
           // If the '/' char is followed by another '/' -> start of an inline comment. 
           match tl with 
           | '/'::tl' -> 
                let rest = buildInlineComment tl'
                tokenise rowCount rest
           | _ -> [BDiv |> TBuiltInFunc] @ tokenise rowCount tl 
        // Logic and comparisons
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
        // String helper function is called when a quotation mark is detected.
        | '\"'::tl -> 
            let str, rest = buildString rowCount "" tl
            [str |> StringLit |> TLiteral] @ tokenise rowCount rest
        // Similarly for chars with apostrophe.
        | '\''::tl -> 
            let c, rest = buildChar rowCount tl
            [c |> CharLit |> TLiteral] @ tokenise rowCount rest
        // Isolate newline case just for row counting purposes.
        | currChar::tl  when List.contains currChar (['\n']) -> 
            tokenise (rowCount + 1) tl
        // Discard all spaces and tabs.
        | currChar::tl  when List.contains currChar ([' ';'\t';'\v']) -> tokenise rowCount tl
        // Number matching.
        | currChar::tl when List.contains currChar (['0'..'9']) ->
            let isFloat, number, rest = buildNumber rowCount false "" input
            if isFloat then [float number |> FloatLit |> TLiteral] @ tokenise rowCount rest
            else [int number |> IntLit |> TLiteral] @ tokenise rowCount rest
        // Identifier/Keyword - Builtin Function matching.
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
        // End case, the whole string has been succesfully matched.
        | [] -> []
        // Error throwing due to unrecognised character.
        | currChar::tl -> failwithf "lexing error, unrecognised character '%c' on line %i" currChar rowCount
        | _ -> failwithf "lexing error, unexpected behaviour... nothing was matched on line %i" rowCount
    try 
        tokenise 0 (Seq.toList str)
        |> Ok
    with
        Failure msg -> Error msg  
