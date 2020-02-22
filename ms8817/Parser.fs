// Module: parser
// Author: ms8817 (Marco Selvatici)

module Parser

open TokeniserStub

//=======//
// Types //
//=======//

type Ast =
    | FuncDefExp of FuncDefExpType
    | Lambda of LambdaType
    | IfExp of Ast * Ast * Ast
    | SeqExp of Ast * Ast
    | FuncApp of Ast * Ast
    | FuncAppList of Ast list
    | Identifier of string
    | IdentifierList of string list
    | Literal of Literal
    | BuiltInFunc of BuiltInFunc
    | Null

// Curried.
and FuncDefExpType = {
    FuncName: string;
    FuncBody: Ast;
    Rest: Ast;
}

// Curried.
and LambdaType = {
    LambdaParam: string;
    LambdaBody: Ast;
}

type ErrorT = {
    parseTrace: string;
    unmatchedTokens: Token list;
    currentAsts: Ast list;
}

type ParseRule =
    Result<Ast list * Token list, ErrorT> -> Result<Ast list * Token list, ErrorT>

//========================//
// Parse rule combinators //
//========================//

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

//==================//
// Helper functions //
//==================//

let buildLambda lambdaParam lambdaBody =
    Lambda {
        LambdaParam = lambdaParam;
        LambdaBody = lambdaBody;
    }

let buildError parseTrace unmatchedTokens currentAsts = 
    Error {
        parseTrace = parseTrace;
        unmatchedTokens = unmatchedTokens;
        currentAsts = currentAsts;
    }

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

/// Given a list of operators groups, returns an option with the position and
/// type of the first operator that matches, or None if no operator was found in
/// the itemsList.
/// Operators in the same group have the same precedence (order within a group
/// does not matter).
/// TODO: this function should be simplified somehow.
let matchAnyOp opGroups itemsList =
    let matchAnyOpInGroup acc opGroup =
        let folder acc item =
            match acc with
            | _, Some _ -> acc // Aleready found a match.
            | idx, None ->
                // Try to match the item at the current index, otherwise continue.
                if List.contains item opGroup then idx, Some item else idx + 1, None
        match acc with
        | Some _ -> acc // Found an operator in a group with higher precedence.
        | None -> // Try to match the current operator group.
            let inGroup = ((0, None), itemsList) ||> List.fold folder
            match inGroup with
            | (idx, Some op) -> Some (idx, op) // Found.
            | (_, None) -> None 
    (None, opGroups) ||> List.fold matchAnyOpInGroup

/// Transforms a list of Items into a tree of left associative function
/// applications, respecting operators ordeing.
let rec buildFuncAppTree (itemsList : Ast list): Ast =
    let opGroups = 
        List.map (List.map BuiltInFunc) <| [
            [And; Or]; // Logical.
            [Greater; GreaterEq; Less; LessEq; Equal]; // Comparison.
            [Plus; Minus]; // Additive.
            [Mult; Div]; // Multiplicative.
        ]
    match itemsList with
    | [] -> impossible "buildFuncAppTree" // Caller should make sure this cannot happen. TODO: this may need to change now.
    | [item] -> item
    | itemsList ->
        match matchAnyOp opGroups itemsList with
        | Some (idx, op) -> // Split at the operator and recur on both sides.
            let lhs, rhs = List.splitAt idx itemsList
            FuncApp (FuncApp (op, buildFuncAppTree lhs), buildFuncAppTree (List.tail rhs))
        | None -> // No arithmetic operator was found, use normal function application associativity.
            let itemsList', lastEl = List.splitAt (itemsList.Length - 1) itemsList
            FuncApp ((buildFuncAppTree itemsList'), lastEl.[0]) // TODO: this is a bit hacky.

//=============//
// Parse rules //
//=============//

// Base rules.

/// Tries to parse a token and, if successful, consumes that token.
let parseToken (tokenToMatch : Token) : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, token :: tokenlist) when token = tokenToMatch ->
        Ok (asts, tokenlist)
    | Ok (asts, tokenlist) ->
        buildError (sprintf "failed: parseToken %A" tokenToMatch) tokenlist asts

/// Tries to match the next token with a list of tokens, and, if successful,
/// returns the unchanged Tokens list and a Null Ast.
/// This parsing rule matches also the empty token list.
let parseNull (tokensToMatch : Token list) : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, []) ->
        Ok (Null :: asts, [])
    | Ok (asts, token :: tokenlist) when List.contains token tokensToMatch ->
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
//                  This may not be necessary for all the rules.

// TODO: support sequence lists.
let rec parseSeqExp parseState =
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
        |> (parseToken KIf .+. parseExp .+. parseToken KThen .+. parseExp .+.
            parseToken KElse .+. parseExp .+. parseToken KFi)
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
    parseState
    |> (parseToken KOpenRound .+. parseExp .+. parseToken KCloseRound)

and parseItemExp parseState =
    parseState
    |> (parseLiteral .|. parseIdentifier .|. parseBuiltin .|. parseRoundExp .|.
        parseIfExp .|. parseSeqExp .|. parseLambda .|. parseLetInExp)

and parseAppExpList parseState =
    let appExpListTerminators =
        [KComma; KCloseSquare; KCloseRound; KThen; KElse; KFi; KIn; KNi]
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
// TODOs:
// - implement proper errors
// - write loads of tests
// - make sure we are efficient (should be)
// - clean up as much as possible