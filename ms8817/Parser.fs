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
    | FuncAppList of Ast list
    | Null // used with pair to make lists
    | Literal of Literal
    | Identifier of string
    | IdentifierList of string list
    | BuiltInFunc of BuiltInFunc // E.g. builtinTimes, builtinPlus
    | RoundExp of Ast // possibly needed see techical note
    | IfExp of Ast * Ast * Ast
    | SeqExp of Ast * Ast // A pair of two elements [a, b]. TODO: (syntactic sugar) Extend this to (untyped) lists [a, b, c, d] -> Seq(a, Seq(b, ...))

// curried version
// let <FuncName> <FuncParam> = <FuncBody> in <Rest>
and FuncDefExpType = {
    FuncName: string;
    FuncBody: Ast; // Contains <FuncParam>, <FuncBody>
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

/// Tries to match the next token with a list of tokens, and, if successful,
/// returns the Tokens list unchanged and a Null Ast.
/// This parsing rule works also with no more tokens.
let parseNull (tokensToMatch : Token list) : ParseRule =
    let matches token =
        match List.tryFind ((=) token) tokensToMatch with
        | Some _ -> true
        | None _ -> false
    function
    | Error e -> Error e
    | Ok (asts, []) ->
        Ok (Null :: asts, [])
    | Ok (asts, token :: tokenlist) when matches token ->
        Ok (Null :: asts, token :: tokenlist)
    | Ok (asts, tokenlist) ->
        buildError (sprintf "failed: parseNull %A" tokensToMatch) tokenlist asts

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

let parseBuiltin : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, TBuiltInFunc func :: tokenlist) ->
        Ok (BuiltInFunc func :: asts, tokenlist)
    | Ok (asts, tokenlist) ->
        buildError "failed: parseLiteral" tokenlist asts

// Combnied rules.
// Every rule has two parts:
// - parse structure: defined as a series of combined parse rules;
// - ast reduction: take the Asts matched in the previous phase and reduce them.

let impossible ruleName = failwithf "What? %A: this case is impossible." ruleName

/// Simple recursive function that transforms a lambda with a series
/// of arguments into a series of curried lambdas.
/// e.g. `\x y z.body` becomes `\x.\y.\z.body`
let rec buildCarriedLambda identifierList lambdaBody =
    match identifierList with
    | [] -> lambdaBody
    | id :: identifierList' ->
        buildLambda id <| buildCarriedLambda identifierList' lambdaBody

/// Transform a function definition with a list of arguments into a "named"
/// lambda.
/// e.g. `let x y z = body in rest ni` becomes `let x = \y.\z.body in rest ni`
let buildCarriedFunc funcParams funcBody rest =
    FuncDefExp {
        FuncName = List.head funcParams;
        FuncBody = buildCarriedLambda (List.tail funcParams) funcBody;
        Rest = rest;
    }

/// Transforms a list into a tree of expressions.
/// TODO: make this in a way that considers operators precedence.
let rec buildFuncAppTree itemsList =
    match itemsList with
    | [] -> impossible "buildFuncAppTree" // Caller should make sure this cannot happen.
    | [item] -> item
    | itemsList ->
        let itemsList', lastEl = List.splitAt (itemsList.Length - 1) itemsList
        FuncApp ((buildFuncAppTree itemsList'), lastEl.[0]) // TODO: this is a bit hacky.

// TODO: support sequence lists.
and parseSeqExp parseState =
    let parseState' =
        parseState
        |> (parseToken KOpenSquare .+. parseExp .+. parseToken KComma .+.
            parseExp .+. parseToken KCloseSquare)
    match parseState' with
    | Error e -> Error e
    | Ok (secondAst :: firstAst :: asts, tkns) ->
        Ok ( SeqExp (firstAst, secondAst) :: asts, tkns)
    | _ -> impossible "parseSeqExp"

and parseIfExp parseState =
    let parseState' =
        parseState
        |> (parseToken KIf .+. parseExp .+.
            parseToken KThen .+. parseExp .+.
            parseToken KElse .+. parseExp .+.
            parseToken KFi)
    match parseState' with
    | Error e -> Error e
    | Ok (elseAst :: thenAst :: condAst :: asts, tkns) ->
        Ok ( IfExp (condAst, thenAst, elseAst) :: asts, tkns)
    | _ -> impossible "parseIfExp"

and parseIdentifierList parseState =
    let idListTerminators = [KDot; KEq]
    let parseState' =
        parseState
        |> (parseNull idListTerminators .|. (parseIdentifier .+. parseIdentifierList))
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
    | Ok (_ :: IdentifierList [] :: asts, tkns) ->
        buildError (sprintf "failed: parseLambda. Invalid empty argument list") tkns asts
    | Ok (lambdaBody :: IdentifierList lambdaParams :: asts, tkns) ->
        Ok (buildCarriedLambda lambdaParams lambdaBody :: asts, tkns)
    | _ -> impossible "parseLambda"

and parseLetInExp parseState =
    let parseState' =
        parseState
        |> (parseToken KLet .+. parseIdentifierList .+. parseToken KEq .+.
            parseExp .+. parseToken KIn .+. parseExp .+. parseToken KNi) // TODO: is KNi even required?
    match parseState' with
    | Error e -> Error e
    | Ok (_ :: IdentifierList [] :: asts, tkns) ->
        buildError (sprintf "failed: parseLetInExp. Invalid empty argument list") tkns asts
    | Ok (rest :: funcBody :: IdentifierList funcParams :: asts, tkns) ->
        Ok (buildCarriedFunc funcParams funcBody rest :: asts, tkns)
    | _ -> impossible "parseLetInExp"

and parseRoundExp parseState =
    let parseState' =
        parseState
        |> (parseToken KOpenRound .+. parseExp .+. parseToken KCloseRound)
    match parseState' with
    | Error e -> Error e
    | Ok (ast :: asts, tkns) -> Ok (RoundExp ast :: asts, tkns)
    | _ -> impossible "parseRoundExp"

and parseItemExp parseState =
    let parseState' =
        parseState
        |> (parseLiteral .|. parseIdentifier .|. parseBuiltin .|.
            parseRoundExp .|. parseIfExp .|. parseSeqExp .|.
            parseLambda .|. parseLetInExp)
    match parseState' with
    | Error e -> Error e
    | Ok _ -> parseState' // No reduction at this level. TODO: remove the reduction bit altogether?

and parseAppExpList parseState =
    let appExpListTerminators =
        [KComma; KCloseSquare; KCloseRound; KThen; KElse; KFi; KIn; KNi] // TODO: What terminates lambdas?
    let parseState' =
        parseState
        |> (parseNull appExpListTerminators .|. (parseItemExp .+. parseAppExpList))
    match parseState' with
    | Error e -> Error e
    | Ok (Null :: asts, tkns) ->
        Ok (FuncAppList [] :: asts, tkns)
    | Ok (FuncAppList fAppList :: itemExp :: asts, tkns) ->
        Ok (FuncAppList (itemExp :: fAppList) :: asts, tkns)
    | _ -> impossible "parseAppExpList"

and parseExp parseState =
    let parseState' =
        parseState
        |> parseAppExpList
    match parseState' with
    | Error e -> Error e
    | Ok (FuncAppList [] :: asts, tkns) ->
        buildError (sprintf "failed: parseExp. Invalid empty exp list") tkns asts
    | Ok (FuncAppList fAppList :: asts, tkns) ->
        Ok (buildFuncAppTree fAppList :: asts, tkns)
    | _ -> impossible "parseExp"

let parse (tkns : Token list) : Result<Ast, ErrorT> =
    let parseState = Ok ([], tkns)
    match parseExp parseState with
    | Error e -> Error e
    | Ok ([ast], []) -> Ok ast
    | Ok (asts, unmatchedTokens) ->
        buildError "failed: top level" unmatchedTokens asts

// TODO: use result.map?
// TODO: fix associativity for func application
// TODO: fix operator precedence.
// TODOs:
// - revise how closely this resembles the grammar.
// - finish up cases
// - implement proper errors
// - write loads of tests
// - make sure we are efficient (should be)
// - clean up as much as possible