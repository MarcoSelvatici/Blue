// Types that are shared among different modules.

module SharedTypes

// TODO: change this to actual tokens type.
type BuiltInFunc =
    // Builtin with no special treatment
    | Not
    | Head // 'a list -> 'a
    | Tail // 'a list -> 'a list
    | Size // 'a list -> int
    | Append // Append to list. 'a -> 'a List -> 'a List
    | StrEq // Comparison between two strings. string -> string -> bool
    | Explode // string -> string List
    | Implode // string List -> string
    // ComparisonOp
    | Greater
    | GreaterEq
    | Less
    | LessEq
    | Equal
    // LogicalOp
    | And
    | Or
    // AdditiveOp
    | Plus
    | Minus
    // MultiplicativeOp
    | Mult
    | Div

// TODO: change this to actual tokens type.
type Literal =
    | IntLit of int
    | BoolLit of bool
    | StringLit of string

// TODO: change this to actual tokens type.
type Token =
    | TLiteral of Literal
    | TIdentifier of string
    | TBuiltInFunc of BuiltInFunc
    // Keywords
    | KLet
    | KRec
    | KEq
    | KIn
    | KNi
    | KComma
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

type Art =
    | Def of FuncDefArt
    | Lam of LambdaR
    | App of (Art * Art * int64 option)
    | Nul
    | Lit of Literal
    | Idn of string
    | BIF of BuiltInFunc
    | IfE of Art * Art * Art
    | Seq of (Art * Art * int64 option)

and FuncDefArt = {
    Name: string;
    Body: Art;
    Rest: Art;
}

and LambdaR = {
    Var: string;
    Body: Art;
}

//========//
// Errors //
//========//

type ParserError = {
    msg: string;
    parseTrace: string;
    unmatchedTokens: Token list;
    currentAsts: Ast list;
}

// TODO: add other types of errors.
type ErrorT =
    | ParserError of ParserError
    | TypeCheckerError of string
    | BetaEngineError of string*Art
