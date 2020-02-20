// Module: parser
// Author: ms8817 (Marco Selvatici)

module Parser

open TokeniserStub

//==========//
// Ast type //
//==========//

type Ast =
    | FuncDefExp of FuncDefExpType // function definition(s) followed by expression
    | Lambda of LambdaType // anonymous function
    | FuncApp of Ast * Ast
    | Pair of Ast * Ast
    | Null // used with pair to make lists
    | Literal of Literal
    | Identifier of string
    | IdentifierList of string list
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

let buildLambda lambdaParam lambdaBody =
    Lambda {
        LambdaParam = lambdaParam;
        LambdaBody = lambdaBody;
    }

//===================//
// Parser error type //
//===================//

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

//=============//
// Parse rules //
//=============//

type ParseRule =
    Result<Ast list * Token list, ErrorT> -> Result<Ast list * Token list, ErrorT>

// Both combinators are left associative, + higher precedence than |.

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

// Base rules.

/// Tries to parse a token and, if successful, returns the Tokens list without
/// that token.
let parseToken (tokenToMatch : Token) : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, token :: tokenlist) when token = tokenToMatch ->
        Ok (asts, tokenlist)
    | Ok (asts, tokenlist) ->
        buildError (sprintf "failed: parseToken %A" tokenToMatch) tokenlist asts

/// Tries to parse a token and, if successful, returns the Tokens list unchanged
/// and a Null Ast.
let parseNull (tokenToMatch : Token) : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, token :: tokenlist) when token = tokenToMatch ->
        Ok (Null :: asts, token :: tokenlist)
    | Ok (asts, tokenlist) ->
        buildError (sprintf "failed: parseNull %A" tokenToMatch) tokenlist asts

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

let impossible ruleName = failwithf "What? %A: this case is impossible." ruleName

/// Simple recursive function that transforms transforms a lambda with a series
/// of arguments into a series of curried lambdas.
/// e.g. \x y z.body   becomes    \x.\y.\z.body
let rec buildCarriedLambda identifierList lambdaBody =
    match identifierList with
    | [] -> lambdaBody
    | id :: identifierList' ->
        buildLambda id <| buildCarriedLambda identifierList' lambdaBody

let rec parseRoundExp parseState =
    let parseState' =
        parseState
        |> (parseToken KOpenRound .+. parseExp .+. parseToken KCloseRound)
    match parseState' with
        | Error e -> Error e
        | Ok (ast :: asts, tkns) -> Ok (RoundExp ast :: asts, tkns)
        | _ -> impossible "parseRoundExp"

and parseIdentifierList parseState =
    // We expect the identifier list to finish with a dot? Very lambda specific.
    // TODO: define other ways to end an identifier list as needed.
    let parseState' =
        parseState
        |> (parseNull KDot .|. (parseIdentifier .+. parseIdentifierList))
    match parseState' with
        | Error e -> Error e
        | Ok (Null :: asts, tkns) -> // Finsihed the identifier list.
            Ok (IdentifierList [] :: asts, tkns)
        | Ok (IdentifierList idList :: Identifier id :: asts, tkns) -> // Append identifier.
            Ok (IdentifierList (id :: idList) :: asts, tkns)
        | _ -> impossible "parseIdentifierList"

and parseLambda parseState =
    let parseState' =
        parseState
        |> (parseToken KLambda .+. parseIdentifierList .+. parseToken KDot .+. parseExp)
    match parseState' with
        | Error e -> Error e
        | Ok (lambdaBody :: IdentifierList lambdaParams :: asts, tkns) ->
            Ok (buildCarriedLambda lambdaParams lambdaBody :: asts, tkns)
        | _ -> impossible "parseLambda"

and parseExp parseState =
    let parseState' =
        parseState
        |> (parseLiteral .|. parseIdentifier .|. parseRoundExp .|. parseLambda)
    match parseState' with
        | Error e -> Error e
        | Ok (Identifier _ :: _, _)
        | Ok (Literal _ :: _, _)
        | Ok (RoundExp _ :: _, _)
        | Ok (Lambda _ :: _, _) -> parseState' // "Forward" the match.
        | _ -> impossible "parseExp"

let parse (tkns : Token list) : Result<Ast, ErrorT> =
    let parseState = Ok ([], tkns)
    match parseExp parseState with
        | Error e -> Error e 
        | Ok ([ast], []) -> Ok ast
        | Ok (asts, unmatchedTokens) ->
            buildError "failed: top level" unmatchedTokens asts
