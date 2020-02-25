// Note: this is in way supposed to be an official version.
// I just need something to get my types to work in the parser.
// Many changes are probably required.

module TokeniserStub

let print x =
    printfn "%A" x

type BuiltInFunc =
    // Builtin with no special treatment
    | Not
    | Head
    | Tail
    | Size
    | Implode
    | Explode
    | Append
    | StrEq
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


