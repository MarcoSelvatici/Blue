// Module: parser
// Author: ms8817 (Marco Selvatici)

module Parser

open TokeniserStub

type Ast =
    | FuncDefExp of FuncDefExpType // function definition(s) followed by expression
    | Lambda of LambdaType // anonymous function
    | FuncApp of Ast * Ast
    | Pair of Ast * Ast
    | Null // used with pair to make lists
    | Literal of Literal
    | Identifier of string
    | BuiltInFunc of BuiltinFunc
    // | Combinator of CombinatorType // Y combinator? ignore for now
    | RoundExp of Ast // possibly needed see techical note
    | Binop of Ast * Ast * Ast // possibly needed see technical note

// curried version
// let <FuncName> <FuncParam> = <FuncBody> in <Rest>
and FuncDefExpType = {
    FuncName: string; // Ast Literal.
    FuncParam: string; // Ast Literal.
    FuncBody: Ast;
    Rest: Ast;
}

// Curried
and LambdaType = {
    LambdaParam: string;
    LambdaBody: Ast;
}


type ErrorT = {
    parseTrace: string;
    unmatchedTokens: Token list;
    currentAsts: Ast list;
}

let buildError parseTrace unmatchedTokens currentAsts = 
    Error {
        parseTrace = parseTrace;
        unmatchedTokens = unmatchedTokens;
        currentAsts = currentAsts;
    }

type ParseRule =
    Result<Ast list * Token list, ErrorT> -> Result<Ast list * Token list, ErrorT>

////// Parse rules combinators.

// Both are left associative, + higher precedence than |.

/// Alternative combination (one rule OR the other).
/// Test rule 1 first, and then rule 2.
let (.|.) (pRule1 : ParseRule) (pRule2 : ParseRule) : ParseRule =
    fun (parseState : Result<Ast list * Token list, ErrorT>) ->
        let tryMatchRules parseState =
            // If both rules failed, make sure we return the error with the
            // least unmatched tokens (longest match).
            match pRule1 parseState with
            | Ok parseState' -> Ok parseState'
            | Error err1 ->
                match pRule2 parseState with
                | Ok parseState' -> Ok parseState'
                | Error err2 ->
                    if err1.unmatchedTokens.Length < err2.unmatchedTokens.Length
                    then err1 else err2
                    |> Error
        match parseState with
        | Ok _ -> tryMatchRules parseState
        | Error _ -> parseState

/// Sequential combination: rule 1 AND THEN rule 2.
/// Only matches if both rules match.
let (.+.) (pRule1 : ParseRule) (pRule2 : ParseRule) : ParseRule =
    fun (parseState : Result<Ast list * Token list, ErrorT>) ->
        let tryMatchRules parseState =
            match pRule1 parseState with
            | Error e -> Error e
            | Ok parseState' -> pRule2 (Ok parseState')
        match parseState with
        | Ok _ -> tryMatchRules parseState
        | Error _ -> parseState

////// Parse rules.

// Base rules.
// Tries to parse a token and, if successful, returns the Tokens list without
// that token.
let parseToken (tokenToMatch : Token) : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, token :: tokenlist) when token = tokenToMatch ->
        Ok (asts, tokenlist)
    | Ok (asts, tokenlist) ->
        buildError (sprintf "failed: parseToken %A" tokenToMatch) tokenlist asts

let parseIdentifier : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, TIdentifier id :: tokenlist) ->
        Ok (Identifier id :: asts, tokenlist)
    | Ok (asts, tokenlist) ->
        buildError "failed: parseIdentifier" tokenlist asts

let parseLiteral : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, TLiteral lit :: tokenlist) ->
        Ok(Literal lit :: asts, tokenlist)
    | Ok (asts, tokenlist) ->
        buildError "failed: parseLiteral" tokenlist asts

// Parse builtinFunc?

// Combnied rules.
// Every rule has two parts:
// - parse structure: defined as a series of combined parse rules;
// - ast reduction: take the Asts matched in the previous phase and reduce them.

let rec parseRoundExp parseState =
    let parseState' =
        parseState
        |> (parseToken KOpenRound .+. parseExp .+. parseToken KCloseRound)
    match parseState' with
        | Error e -> Error e
        | Ok (ast :: asts, tkns) -> Ok (RoundExp ast :: asts, tkns)
        | _ -> failwithf "What? This case is impossible."

and parseExp parseState =
    let parseState' =
        parseState
        |> (parseLiteral .|. parseIdentifier .|. parseRoundExp)
    match parseState' with
        | Error e -> Error e
        | Ok (Identifier _ :: _, _)
        | Ok (Literal _ :: _, _)
        | Ok (RoundExp _ :: _, _) -> parseState' // "Forward" the match.
        | Ok (asts, tkns) ->
            // Could not match this rule.
            buildError "failed: parseExp" tkns asts

let parse (tkns : Token list) : Result<Ast, ErrorT> =
    let parseState = Ok ([], tkns)
    match parseExp parseState with
        | Error e -> Error e 
        | Ok ([ast], []) -> Ok ast
        | Ok (asts, unmatchedTokens) ->
            buildError "failed: top level" unmatchedTokens asts
