
module TokeniserParserStub

//=============//
// Token types //
//=============//
type BuiltInFunc =
    // UnaryOp
    | Not
    | Head
    | Tail
    | Size
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

type Literal =
    | IntLit of int
    | BoolLit of bool
    | StringLit of string

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
    | KAppend
    | KNull
    | KStrEq
    | KImplode
    | KExplode

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