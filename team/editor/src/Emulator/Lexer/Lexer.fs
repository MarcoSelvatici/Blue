module Lexer

open SharedTypes

// List of escape sequences supported in f#.
let escSeq = ['a';'b';'f';'n';'r';'t';'v';'\\';'\"';'\'']
let breakNum = ['+';'-';'*';'/';'=';'<';'>';'(';')';'[';']';'&';'|';' ';',';';';'\n';'\t';'#';'\r']
let breakId = ['+';'-';'*';'/';'=';'<';'>';'&';'|';' ';'.';',';';';'\n';'\t';'(';'[';')';']';'#';'\r']

// If the current character is a number this function is called, trying to parse an int or a float.
let rec buildNumber rowCount number input =
    match input with 
    // While the current char is a number, keep adding it to the result.
    | currChar::tl when List.contains currChar (['0'..'9']) -> 
        buildNumber rowCount (number + string currChar) tl
    // If current char is a valid char that breaks the number, return the result.
    | currChar::_ when List.contains currChar (breakNum) -> number, input
    // Analoguely, if string is over, return valid result.
    | [] -> number, input
    // Else return an error, as specific as possible.
    | currChar::_ -> failwithf "lexing error, number contains non numeric char: '%c' on line %i" currChar rowCount
    | _ -> failwithf "lexing error, unexpected behaviour in building the number token on line %i" rowCount


// If the current character is a string this function is called, trying to parse an identifier or a keyword/builtin function.
let rec buildWord rowCount word input =
    match input with
    // While the current character is a valid identifier character, keep adding it to the result.
    | currChar::tl when List.contains currChar (['a'..'z']@['A'..'Z']@['0'..'9']@['_';'\'']) 
        -> buildWord rowCount (word + string currChar) tl
    // If is a valid character to break the sequence, return valid result.
    | currChar::_ when List.contains currChar (breakId) 
        -> word, input
    // Analoguely, if string is over, return valid result.
    | [] -> word, input
    // Else return an error, as specific as possible.
    | currChar::_ -> failwithf "lexing error, unrecognised non-alphabetic character: '%c' on line: %i" currChar rowCount
    | _ -> failwithf "lexing error, unexpected behaviour on line %i." rowCount 

// Function called when inline comment "//" is encountered. 
let rec buildInlineComment input =
    match input with 
    // Discard everything up to newline.
    | currChar::tl when not <| List.contains currChar ['\n'] -> buildInlineComment tl
    | _ -> input

// Function called when multiline comment "(*" is encountered. 
let rec buildMultilineComment rowCount input =
    match input with 
    // Discard everything up to newline.
    | currChar::tl when List.contains currChar ['\n'] -> buildMultilineComment (rowCount + 1) tl
    | currChar::tl when not <| List.contains currChar ['*'] -> buildMultilineComment rowCount tl
    | currChar::tl when List.contains currChar ['*'] -> 
        match tl with 
        | nextChar::tl' when List.contains nextChar [')'] -> rowCount, tl'
        | _ -> buildMultilineComment rowCount tl
    | _ -> rowCount, input

// Function called when open quotation marks are encountered '"'.
let rec buildString rowCount str input =
    match input with 
    // Match everything in the string that is not a closing quotation mark or an escape character.
    | currChar::tl when not <| List.contains currChar ['\\';'\"'] -> buildString rowCount (str + string currChar) tl
    // If an escape character is matched, try to match a valid escape sequence or throw an error.
    | currChar::tl when List.contains currChar ['\\'] ->      
        match tl with
        | esc::tl' when List.contains esc escSeq
            -> buildString rowCount (str + string ('\\' + esc)) tl'
        | esc::_ -> failwithf "lexing error, expected valid ESC sequence on line %i: \\%c is not valid" rowCount esc 
        | _ -> failwithf "lexing error, expected valid ESC sequence on line %i" rowCount  
    // If closing quotation mark is encountered, return valid result. Else throw an error.
    | currChar::tl when List.contains currChar ['\"'] -> str, tl
    | _ -> failwithf "lexing error, expecting closing quotation mark: '\"' on line %i" rowCount


// Tokenise function: takes the program in string form and returns a list of Tokens or a lexing error.
let tokeniseT3 (str: string) =
    // Recursively trying to match a token. 
    let rec tokenise tokenList rowCount (input: char list) =
        match input with
        // Single/Coupled character -> Token matching, no helper functions needed.
        | '.'::tl -> tokenise (KDot::tokenList) rowCount tl
        | ','::tl -> tokenise (KComma::tokenList) rowCount tl
        | ';'::tl -> tokenise (KSemiColon::tokenList) rowCount tl
        | '('::'*'::tl -> 
            let rowCount', rest = buildMultilineComment rowCount tl
            tokenise tokenList rowCount' rest
        | '('::tl -> tokenise (KOpenRound::tokenList) rowCount tl
        | ')'::tl -> tokenise (KCloseRound::tokenList) rowCount tl
        | '['::tl -> tokenise (KOpenSquare::tokenList) rowCount tl
        | ']'::tl -> tokenise (KCloseSquare::tokenList) rowCount tl
        | '\\'::tl -> tokenise (KLambda::tokenList) rowCount tl
        // Arithmetic Builtin Functions
        | '+'::tl ->  
            tokenise ([TBuiltInFunc <| Plus] @ tokenList) rowCount tl
        | '-'::tl -> tokenise ([TBuiltInFunc <| Minus] @ tokenList) rowCount tl
        | '*'::tl -> tokenise ([TBuiltInFunc <| Mult] @ tokenList) rowCount tl
        | '#'::tl -> 
            let rest = buildInlineComment tl
            tokenise tokenList rowCount rest
        | '/'::tl -> tokenise ([TBuiltInFunc <| Div] @ tokenList) rowCount tl
        // Logic and comparisons
        | '!'::tl -> tokenise ([TBuiltInFunc <| Not] @ tokenList) rowCount tl
        | '>'::'='::tl -> tokenise ([TBuiltInFunc <| GreaterEq] @ tokenList) rowCount tl
        | '>'::tl -> tokenise ([TBuiltInFunc <| Greater] @ tokenList) rowCount tl
        | '<'::'='::tl -> tokenise ([TBuiltInFunc <| LessEq] @ tokenList) rowCount tl
        | '<'::tl -> tokenise ([TBuiltInFunc <| Less] @ tokenList) rowCount tl
        | '='::'='::tl -> tokenise ([TBuiltInFunc <| Equal] @ tokenList) rowCount tl
        | '='::tl -> tokenise (KEq::tokenList) rowCount tl
        | '&'::'&'::tl -> tokenise ([TBuiltInFunc <| And] @ tokenList) rowCount tl
        | '|'::'|'::tl -> tokenise ([TBuiltInFunc <| Or] @ tokenList) rowCount tl
        // String helper function is called when a quotation mark is detected.
        | '\"'::tl -> 
            let str, rest = buildString rowCount "" tl
            tokenise ([TLiteral <| (StringLit <| str)] @ tokenList) rowCount rest
        // Isolate newline case just for row counting purposes.
        | '\n'::tl -> tokenise tokenList (rowCount + 1) tl
        | '\r'::tl -> tokenise tokenList (rowCount + 1) tl
        | '\r'::'\n'::tl -> tokenise tokenList (rowCount + 1) tl
        // Discard all spaces and tabs.
        | currChar::tl  when List.contains currChar ([' ';'\t';'\v']) -> tokenise tokenList rowCount tl
        // Number matching.
        | currChar::_ when List.contains currChar (['0'..'9']) ->
            let number, rest = buildNumber rowCount "" input
            tokenise ([int number |> IntLit |> TLiteral] @ tokenList) rowCount rest
        // Identifier/Keyword - Builtin Function matching.
        | currChar::_ when List.contains currChar (['a'..'z']@['A'..'Z']@['_']) -> 
            let word, rest = buildWord rowCount "" input
            match (string word) with 
            | "true" -> tokenise ([TLiteral <| (BoolLit <| true)] @ tokenList) rowCount rest
            | "false" -> tokenise ([TLiteral <| (BoolLit <| false)] @ tokenList) rowCount rest
            | "head" -> tokenise ([TBuiltInFunc <| Head] @ tokenList) rowCount rest
            | "tail" -> tokenise ([TBuiltInFunc <| Tail] @ tokenList) rowCount rest
            | "size" -> tokenise ([TBuiltInFunc <| Size] @ tokenList) rowCount rest
            | "implode" -> tokenise ([TBuiltInFunc <| Implode]  @ tokenList) rowCount rest
            | "explode" -> tokenise ([TBuiltInFunc <| Explode] @ tokenList) rowCount rest
            | "append" -> tokenise ([TBuiltInFunc <| Append] @ tokenList) rowCount rest
            | "strEq" -> tokenise ([TBuiltInFunc <| StrEq] @ tokenList) rowCount rest
            | "let" -> tokenise (KLet::tokenList) rowCount rest
            | "in" -> tokenise (KIn::tokenList) rowCount rest
            | "ni" -> tokenise (KNi::tokenList) rowCount rest
            | "if" -> tokenise (KIf::tokenList) rowCount rest
            | "then" -> tokenise (KThen::tokenList) rowCount rest
            | "else" -> tokenise (KElse::tokenList) rowCount rest
            | "fi" -> tokenise (KFi::tokenList) rowCount rest
            | "test" -> tokenise ([TBuiltInFunc <| Test] @ tokenList) rowCount rest
            | "print" -> tokenise ([TBuiltInFunc <| Print] @ tokenList) rowCount rest
            | _ -> tokenise ([TIdentifier <| word] @ tokenList) rowCount rest
        // End case, the whole string has been succesfully matched.
        | [] -> tokenList
        // Error throwing due to unrecognised character.
        | currChar::_ -> failwithf "lexing error, unrecognised character '%c' on line %i" currChar rowCount
        | _ -> failwithf "lexing error, unexpected behaviour... nothing was matched on line %i" rowCount
    try 
        Ok <| (List.rev <| (tokenise [] 1 (Seq.toList str)))
        
    with
        Failure msg -> Error (LexerError msg)  
