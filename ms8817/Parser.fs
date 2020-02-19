// Module: parser
// Author: ms8817 (Marco Selvatici)

module Parser

open TokeniserStub

type Block =
    | BlockExp of Exp // The final expression in a block.
    | NamedBlockSeq of (IdentifierList * Block) * Block // Let <id-list> = <block> in <block>

and Exp =
    | LiteralExp of Literal
    | IdentifierExp of string
    | RoundExp of Exp
    | LambdaExp of IdentifierList * Exp
    | BuiltinExp of BuiltinExp

and IdentifierList =
    | Identifier of string
    | IdentifierSeq of string * IdentifierList

and BuiltinExp =
    | IfExp of Exp * Exp * Exp
    | SeqExp of Exp * Exp
    | UnaryExp of UnaryOp * Exp
    | LogicalExp of LogicalExp

and LogicalExp =
    | ComparisonExp of ComparisonExp
    | LogicalNotExp of Exp
    | BinaryLogicalExp of LogicalOp * ComparisonExp * LogicalExp

and ComparisonExp =
    | AdditiveExp of AdditiveExp
    | BinaryComparisonExp of ComparisonOp * AdditiveExp * ComparisonExp

and AdditiveExp =
    | MultiplicativeExp of MultiplicativeExp
    | BinaryAdditiveExp of AdditiveOp * MultiplicativeExp * AdditiveExp

and MultiplicativeExp =
    | ApplicativeExp of ApplicativeExp
    | BinaryMultiplicativeExp of MultiplicativeOp * ApplicativeExp * MultiplicativeExp

and ApplicativeExp =
    | Exp of Exp
    | ExpSeq of Exp * ApplicativeExp

