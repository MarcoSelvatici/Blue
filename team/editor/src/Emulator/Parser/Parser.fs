// Author: ms8817 (Marco Selvatici)

module Parser

open SharedTypes

type ParseRule =
    Result<Ast list * Token list, ParserError> -> Result<Ast list * Token list, ParserError>

//========================//
// Parse rule combinators //
//========================//

// Both combinators are left associative, + higher precedence than |.

/// Alternative combination (one rule OR the other).
/// Test rule 1 first, and then rule 2.
let (.|.) (pRule1 : ParseRule) (pRule2 : ParseRule) : ParseRule =
    fun (parseState : Result<Ast list * Token list, ParserError>) ->
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
    fun (parseState : Result<Ast list * Token list, ParserError>) ->
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
/// only present if there is a named function or a lambda that has not been
/// completely parsed yet, hence the error is "within" those.
let rec buildParseTrace (asts : Ast list) : string =
    match asts with
    | [] -> ""
    | IdentifierList ids :: asts' when not (List.isEmpty ids) ->
        sprintf "in %s " (List.head ids) + (buildParseTrace asts')
    | _ :: asts' -> buildParseTrace asts'

let buildParserError msg unmatchedTokens currentAsts = 
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
/// lambda. Caller ensures there is at least one entry in funcParams.
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
let rec matchAnyOp opGroups itemsList =
    /// Check if an item is present in an opGroup, and if so, return the item
    /// and its index in the itemsList.
    let itemInOpGroup opGroup acc item =
        match acc with
        | _, Some _ -> acc // Aleready found a match.
        | idx, None ->
            // Try to match the item at the current index, otherwise continue.
            if List.contains item opGroup then idx, Some item else idx + 1, None
    match opGroups with
    | [] -> None // No operator present in the itemsList.
    | opGroup :: opGroups' ->
        let idxAndOp = ((0, None), itemsList) ||> List.fold (itemInOpGroup opGroup)
        match idxAndOp with
        | (idx, Some op) -> Some (idx, op) // Found.
        | (_, None) -> matchAnyOp opGroups' itemsList // Try next group.

/// Transforms a list of Items into a tree of left associative function
/// applications, respecting operator precedences.
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
            | Error msg, _ | _, Error msg -> Error msg
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

let pToken' addToAst (tokenToMatch : Token) : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, token :: tokenlist) when token = tokenToMatch && addToAst ->
        Ok (Token token :: asts, tokenlist)
    | Ok (asts, token :: tokenlist) when token = tokenToMatch && not addToAst ->
        Ok (asts, tokenlist)
    | Ok (asts, tokenlist) ->
        buildParserError (sprintf "failed: pToken %A" tokenToMatch) tokenlist asts

/// Tries to parse a token and, if successful, consumes that token.
let pToken    = pToken' false
/// Tries to parse a token and, if successful, consumes that token and adds it
/// to the ast.
let pTokenAdd = pToken' true

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
        buildParserError (sprintf "failed: pNull %A" tokensToMatch) tokenlist asts

let pIdentifier : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, TIdentifier id :: tokenlist) ->
        Ok (Identifier id :: asts, tokenlist)
    | Ok (asts, tokenlist) ->
        buildParserError "failed: pIdentifier" tokenlist asts

let pLiteral : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, TLiteral lit :: tokenlist) ->
        Ok(Literal lit :: asts, tokenlist)
    | Ok (asts, tokenlist) ->
        buildParserError "failed: pLiteral" tokenlist asts

let pBuiltin : ParseRule =
    function
    | Error e -> Error e
    | Ok (asts, TBuiltInFunc func :: tokenlist) ->
        Ok (BuiltInFunc func :: asts, tokenlist)
    | Ok (asts, tokenlist) ->
        buildParserError "failed: pBuiltin" tokenlist asts

// Combnied rules.
// Every rule has two parts:
// - parse structure: defined as a series of combined parse rules;
// - ast reduction: take the Asts matched in the previous phase and reduce them.
//                  This may not be necessary for all the rules.
// When possible, Result.map is used, but some rules may generate an error from
// an Ok result, hence require a normal pattern matching.

let rec pSeqList pState =
    pState
    |> (pExp .+. (pTokenAdd KCloseSquare .|. pTokenAdd KComma .+. pSeqList))
    |> Result.map (
        function
        | SeqExp (h, t) :: Token KComma :: exp :: asts, tkns ->
            SeqExp (exp, SeqExp (h, t)) :: asts, tkns
        | Token KCloseSquare :: exp :: asts, tkns ->
            SeqExp (exp, EmptySeq) :: asts, KCloseSquare :: tkns
        | state -> impossible <| sprintf "pSeqList %A" state
    )

and pSeqExp pState =
    pState
    |> (pTokenAdd KOpenSquare .+. pTokenAdd KCloseSquare .|. // Empty sequence.
        pTokenAdd KOpenSquare .+. pSeqList .+. pTokenAdd KCloseSquare)
    |> Result.map (
        function
        | Token KCloseSquare :: SeqExp (l,r) :: Token KOpenSquare :: asts, tkns -> 
            // Non empty sequence.
            SeqExp (l, r) :: asts, tkns
        | Token KCloseSquare :: Token KOpenSquare :: asts, tkns ->
            // Empty sequence.
            EmptySeq :: asts, tkns
        | _ -> impossible "pSeqExp"
    )

and pIfExp pState =
    pState
    |> (pToken KIf .+. pExp .+. pToken KThen .+. pExp .+.
        pToken KElse .+. pExp .+. pToken KFi)
    |> Result.map (
        function
        | elseAst :: thenAst :: condAst :: asts, tkns ->
            IfExp (condAst, thenAst, elseAst) :: asts, tkns
        | _ -> impossible "pIfExp"
    )

and pIdentifierList pState =
    let idListTerminators = [KDot; KEq]
    pState
    |> (pNull idListTerminators .|. (pIdentifier .+. pIdentifierList))
    |> Result.map (
        function
        | Null :: asts, tkns -> // Finsihed the identifier list.
            IdentifierList [] :: asts, tkns
        | IdentifierList idList :: Identifier id :: asts, tkns -> // Append identifier.
            IdentifierList (id :: idList) :: asts, tkns
        | _ -> impossible "pIdentifierList"
    )

and pLambdaExp pState =
    pState
    |> (pToken KLambda .+. pIdentifierList .+. pToken KDot .+. pExp)
    |> function
       | Error e -> Error e
       | Ok (_ :: IdentifierList [] :: asts, tkns) ->
           buildParserError "failed: pLambdaExp. Invalid empty argument list" tkns asts
       | Ok (lambdaBody :: IdentifierList lambdaParams :: asts, tkns) ->
           Ok (buildCarriedLambda lambdaParams lambdaBody :: asts, tkns)
       | _ -> impossible "pLambdaExp"

and pFuncDefExp pState =
    pState
    |> (pToken KLet .+. pIdentifierList .+. pToken KEq .+.
        pExp .+. pToken KIn .+. pExp .+. pToken KNi)
    |> function
       | Error e -> Error e
       | Ok (_ :: _ :: IdentifierList [] :: asts, tkns) ->
           buildParserError "failed: pFuncDefExp. Invalid empty argument list" tkns asts
       | Ok (rest :: funcBody :: IdentifierList funcParams :: asts, tkns) ->
           Ok (buildCarriedFunc funcParams funcBody rest :: asts, tkns)
       | _ -> impossible "pFuncDefExp"

and pRoundExp pState =
    pState
    |> (pToken KOpenRound .+. pExp .+. pToken KCloseRound)

and pItemExp pState =
    pState
    |> (pLiteral .|. pIdentifier .|. pBuiltin .|. pRoundExp .|.
        pIfExp .|. pSeqExp .|. pLambdaExp .|. pFuncDefExp)

and pAppExpList pState =
    let appExpListTerminators =
        [KComma; KCloseSquare; KCloseRound; KThen; KElse; KFi; KIn; KNi]
    pState
    |> (pNull appExpListTerminators .|. (pItemExp .+. pAppExpList))
    |> Result.map (
        function
        | Null :: asts, tkns ->
            FuncAppList [] :: asts, tkns
        | FuncAppList fAppList :: itemExp :: asts, tkns ->
            FuncAppList (itemExp :: fAppList) :: asts, tkns
        | _ -> impossible "pAppExpList"
    )

and pExp pState =
    match pAppExpList pState with
    | Error e -> Error e
    | Ok (FuncAppList fAppList :: asts, tkns) ->
        match buildFuncAppTree fAppList with
        | Error msg -> buildParserError msg tkns asts
        | Ok tree -> Ok (tree :: asts, tkns)
    | _ -> impossible "pExp"

let parse (tkns : Token list) : Result<Ast, ErrorT> =
    let parseState = Ok ([], tkns)
    match pExp parseState with
    | Error e -> Error <| ParserError e
    | Ok ([ast], []) -> Ok ast
    | Ok (asts, unmatchedTokens) ->
        Result.mapError ParserError (buildParserError "failed: top level" unmatchedTokens asts)
