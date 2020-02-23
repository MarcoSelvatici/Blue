// Module: parser
// Author: ms8817 (Marco Selvatici)

module Parser

open TokeniserStub

//=======//
// Types //
//=======//

type Ast =
    | FuncDefExp of FuncDefExpType
    | LambdaExp of LambdaType
    | IfExp of Ast * Ast * Ast
    | SeqExp of Ast * Ast
    | FuncApp of Ast * Ast
    | FuncAppList of Ast list // Transformed into a tree of FuncApp.
    | Identifier of string
    | IdentifierList of string list // Transformed into a list of Identifier.
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
    msg: string;
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
    LambdaExp {
        LambdaParam = lambdaParam;
        LambdaBody = lambdaBody;
    }

/// Examines the parse state in search for identfier lists. An idenfier list is
/// only present there is a named function or a lambda that has not been
/// completely parsed yet, hence the error is "within" those.
let rec buildParseTrace (asts : Ast list) : string =
    match asts with
    | [] -> ""
    | IdentifierList ids :: asts' when not (List.isEmpty ids) ->
        sprintf "in %s " (List.head ids) + (buildParseTrace asts')
    | _ :: asts' -> buildParseTrace asts'

let buildError msg unmatchedTokens currentAsts = 
    Error {
        msg = msg;
        parseTrace = buildParseTrace currentAsts;
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
/// If such transformation is not possible (e.g. due to incomplete arithmetic
/// expression like `1+`), return a string with the error message.
let rec buildFuncAppTree (itemsList : Ast list): Result<Ast, string> =
    let opGroups =
        List.map (List.map BuiltInFunc) <| [
            [And; Or]; // Logical.
            [Greater; GreaterEq; Less; LessEq; Equal]; // Comparison.
            [Plus; Minus]; // Additive.
            [Mult; Div]; // Multiplicative.
        ]
    match itemsList with
    | [] -> Error "failed: buildFuncAppTree. Expected expression"
    | [item] -> Ok item
    | itemsList -> // More than one item.
        match matchAnyOp opGroups itemsList with
        | Some (idx, op) ->
            // Split at the operator and recur on both sides.
            let lhs, rhs = List.splitAt idx itemsList
            match buildFuncAppTree lhs, buildFuncAppTree (List.tail rhs) with
            | Error msg, _
            | _, Error msg -> Error msg
            | Ok lTree, Ok rTree -> Ok <| FuncApp (FuncApp (op, lTree), rTree)
        | None ->
            // No arithmetic operator was found, use normal funcApp associativity.
            let itemsList', lastEl = List.splitAt (itemsList.Length - 1) itemsList
            match buildFuncAppTree itemsList' with
            | Error msg -> Error msg
            | Ok tree -> Ok <| FuncApp (tree, lastEl.[0])

//=============//
// Parse rules //
//=============//

// Base rules.

/// Tries to parse a token and, if successful, consumes that token.
let pToken (tokenToMatch : Token) : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, token :: tokenlist) when token = tokenToMatch ->
        Ok (asts, tokenlist)
    | Ok (asts, tokenlist) ->
        buildError (sprintf "failed: pToken %A" tokenToMatch) tokenlist asts

/// Tries to match the next token with a list of tokens, and, if successful,
/// returns the unchanged Tokens list and a Null Ast.
/// This parsing rule matches also the empty token list.
let pNull (tokensToMatch : Token list) : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, []) ->
        Ok (Null :: asts, [])
    | Ok (asts, token :: tokenlist) when List.contains token tokensToMatch ->
        Ok (Null :: asts, token :: tokenlist)
    | Ok (asts, tokenlist) ->
        buildError (sprintf "failed: pNull %A" tokensToMatch) tokenlist asts

let pIdentifier : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, TIdentifier id :: tokenlist) ->
        Ok (Identifier id :: asts, tokenlist)
    | Ok (asts, tokenlist) ->
        buildError "failed: pIdentifier" tokenlist asts

let pLiteral : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, TLiteral lit :: tokenlist) ->
        Ok(Literal lit :: asts, tokenlist)
    | Ok (asts, tokenlist) ->
        buildError "failed: pLiteral" tokenlist asts

let pBuiltin : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, TBuiltInFunc func :: tokenlist) ->
        Ok (BuiltInFunc func :: asts, tokenlist)
    | Ok (asts, tokenlist) ->
        buildError "failed: pBuiltin" tokenlist asts

// Combnied rules.
// Every rule has two parts:
// - parse structure: defined as a series of combined parse rules;
// - ast reduction: take the Asts matched in the previous phase and reduce them.
//                  This may not be necessary for all the rules.

let rec pSeqListExp pState =
    let seqExpListTerminators = [KCloseSquare]
    // At least one element will be present in the sequence, since empty
    // sequences are matched in pSeqExp.
    let pState' =
        pState
        |> (pExp .+. (pNull seqExpListTerminators .|. pToken KComma .+. pSeqListExp))
    match pState' with
    | Error e -> Error e
    | Ok (Null :: exp :: asts, tkns) ->
        Ok (SeqExp (exp, Null) :: asts, tkns)
    | Ok (SeqExp (fst, snd) :: exp :: asts, tkns) ->
        Ok (SeqExp (exp, SeqExp (fst, snd)) :: asts, tkns)
    | _ -> impossible "pSeqListExp"

and pSeqExp pState =
    let pState' =
        pState
        |> (pToken KOpenSquare .+. pToken KCloseSquare .|. // Empty sequence.
            pToken KOpenSquare .+. pSeqListExp .+. pToken KCloseSquare)
    match pState' with
    | Error e -> Error e
    | Ok (SeqExp _ :: _, _) -> pState' // Non empty sequence.
    | Ok (asts, tkns) -> Ok (SeqExp (Null, Null) :: asts, tkns) // Empty sequence.

and pIfExp pState =
    let pState' =
        pState
        |> (pToken KIf .+. pExp .+. pToken KThen .+. pExp .+.
            pToken KElse .+. pExp .+. pToken KFi)
    match pState' with
    | Error e -> Error e
    | Ok (elseAst :: thenAst :: condAst :: asts, tkns) ->
        Ok ( IfExp (condAst, thenAst, elseAst) :: asts, tkns)
    | _ -> impossible "pIfExp"

and pIdentifierList pState =
    let idListTerminators = [KDot; KEq]
    let pState' =
        pState |> (pNull idListTerminators .|. (pIdentifier .+. pIdentifierList))
    match pState' with
    | Error e -> Error e
    | Ok (Null :: asts, tkns) -> // Finsihed the identifier list.
        Ok (IdentifierList [] :: asts, tkns)
    | Ok (IdentifierList idList :: Identifier id :: asts, tkns) -> // Append identifier.
        Ok (IdentifierList (id :: idList) :: asts, tkns)
    | _ -> impossible "pIdentifierList"

and pLambdaExp pState =
    let pState' =
        pState |> (pToken KLambda .+. pIdentifierList .+. pToken KDot .+. pExp)
    match pState' with
    | Error e -> Error e
    | Ok (_ :: IdentifierList [] :: asts, tkns) ->
        buildError "failed: pLambdaExp. Invalid empty argument list" tkns asts
    | Ok (lambdaBody :: IdentifierList lambdaParams :: asts, tkns) ->
        Ok (buildCarriedLambda lambdaParams lambdaBody :: asts, tkns)
    | _ -> impossible "pLambdaExp"

and pFuncDefExp pState =
    let pState' =
        pState
        |> (pToken KLet .+. pIdentifierList .+. pToken KEq .+.
            pExp .+. pToken KIn .+. pExp .+. pToken KNi)
    match pState' with
    | Error e -> Error e
    | Ok (_ :: _ :: IdentifierList [] :: asts, tkns) ->
        buildError "failed: pFuncDefExp. Invalid empty argument list" tkns asts
    | Ok (rest :: funcBody :: IdentifierList funcParams :: asts, tkns) ->
        Ok (buildCarriedFunc funcParams funcBody rest :: asts, tkns)
    | _ -> impossible "pFuncDefExp"

and pRoundExp pState =
    pState |> (pToken KOpenRound .+. pExp .+. pToken KCloseRound)

and pItemExp pState =
    pState
    |> (pLiteral .|. pIdentifier .|. pBuiltin .|. pRoundExp .|.
        pIfExp .|. pSeqExp .|. pLambdaExp .|. pFuncDefExp)

and pAppExpList pState =
    let appExpListTerminators =
        [KComma; KCloseSquare; KCloseRound; KThen; KElse; KFi; KIn; KNi]
    let pState' =
        pState |> (pNull appExpListTerminators .|. (pItemExp .+. pAppExpList))
    match pState' with
    | Error e -> Error e
    | Ok (Null :: asts, tkns) ->
        Ok (FuncAppList [] :: asts, tkns)
    | Ok (FuncAppList fAppList :: itemExp :: asts, tkns) ->
        Ok (FuncAppList (itemExp :: fAppList) :: asts, tkns)
    | _ -> impossible "pAppExpList"

and pExp pState =
    match pAppExpList pState with
    | Error e -> Error e
    | Ok (FuncAppList fAppList :: asts, tkns) ->
        match buildFuncAppTree fAppList with
        | Error msg -> buildError msg tkns asts
        | Ok tree -> Ok (tree :: asts, tkns)
    | _ -> impossible "pExp"

let parse (tkns : Token list) : Result<Ast, ErrorT> =
    let parseState = Ok ([], tkns)
    match pExp parseState with
    | Error e -> Error e
    | Ok ([ast], []) -> Ok ast
    | Ok (asts, unmatchedTokens) ->
        buildError "failed: top level" unmatchedTokens asts

// TODO: use result.map?
// TODOs:
// - write loads of tests
// - make sure we are efficient (should be)
// - clean up as much as possible