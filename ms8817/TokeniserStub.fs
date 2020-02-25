// Note: this is in way supposed to be an official version.
// I just need something to get my types to work in the parser.
// Many changes are probably required.

module TokeniserStub

let print x =
    printfn "%A" x

type BuiltInFunc =
    // Builtin with no special treatment
    | Not
    | Head // list -> head
    | Tail // list -> list
    | Size // list -> int
    | Implode // Reduce a list of chars into a string. string list -> string
    | Explode // Expand a string into a list of chars. string -> string list
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


