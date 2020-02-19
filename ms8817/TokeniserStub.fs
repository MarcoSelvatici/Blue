// Note: this is in way supposed to be an official version.
// I just need something to get my types to work in the parser.
// Many changes are probably required.

module TokeniserStub

type UnaryOp =
    | Not
    | Head
    | Tail
    | Size

type ComparisonOp =
    | Greater
    | GreaterEq
    | Less
    | LessEq
    | Equal

type LogicalOp =
    | And
    | Or

type AdditiveOp =
    | Plus
    | Minus

type MultiplicativeOp =
    | Mult
    | Div

type Keyword =
    | KLet
    | KRec
    | KOpenRound
    | KCloseRound
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

type Literal =
    | IntLit of int
    | BoolLit of bool
    | StringLit of string

type Token =
    | TLiteral of Literal
    | TIdentifier of string
    | UnaryOp of UnaryOp
    | OtherKeyword of Keyword
