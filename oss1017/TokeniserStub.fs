// Note: this is in no way supposed to be an official version.
// I just need something to get my types to work in the parser.
// Many changes are probably required.
// Author: ms8817 (Marco Selvatici)

module TokeniserStub

let print x =
    printfn "%A" x

type BuiltinFunc =
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
    | BuiltinFun of BuiltinFunc
    // Keywords
    | KLet
    | KRec
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