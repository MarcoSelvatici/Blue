// Author: Marco Selvatici (ms8817)

// Note: this is not supposed to be an official version of the tokeniser types.
// I just need some types to work on the parser.
// Fabio will take over and finalise the type definition.

module TokeniserStub

type BuiltInFunc =
    // Builtin with no special treatment
    | Not
    | Head // 'a list -> 'a
    | Tail // 'a list -> 'a list
    | Size // 'a list -> int
    | Append // Append to list. 'a -> 'a List -> 'a List
    | StrEq // Comparison between two strings. string -> string -> bool
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
    | KNull


