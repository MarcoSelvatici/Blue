module TokeniserParserStub

//=============//
// Token types //
//=============//

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

//==========//
// Ast type //
//==========//

type Ast =
    | FuncDefExp of FuncDefExpType
    | Lambda of LambdaType 
    | FuncApp of (Ast * Ast)
    | Null
    | Literal of Literal
    | Identifier of string
    | BuiltInFunc of BuiltInFunc
    | IfExp of Ast * Ast * Ast
    | SeqExp of Ast * Ast
    | FuncAppList of Ast list       // should not
    | IdentifierList of string list // be passed
    | RoundExp of Ast               // from parser

and FuncDefExpType = {
    FuncName: string;
    FuncBody: Ast;
    Rest: Ast;
}

and LambdaType = {
    LambdaParam: string;
    LambdaBody: Ast;
}