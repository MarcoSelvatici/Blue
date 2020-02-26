module TokeniserParserStub

//=============//
// Token types //
//=============//

type BuiltInFunc =
    // Builtin with no special treatment
    | Not
    | Head // 'a list -> 'a
    | Tail // 'a list -> 'a list
    | Size // 'a list -> int
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

//==========//
// Ast type //
//==========//

type Ast =
    | FuncDefExp of FuncDefExpType // function definition(s) followed by expression
    | Lambda of LambdaType // anonymous function
    | FuncApp of (Ast * Ast)
    | FuncAppList of Ast list
    | Null // used with pair to make lists
    | Literal of Literal
    | Identifier of string
    | IdentifierList of string list
    | BuiltInFunc of BuiltInFunc // E.g. builtinTimes, builtinPlus
    | RoundExp of Ast // possibly needed see techical note
    | IfExp of Ast * Ast * Ast
    | SeqExp of Ast * Ast // A pair of two elements [a, b].

and FuncDefExpType = {
    FuncName: string;
    FuncBody: Ast;
    Rest: Ast;
}

and LambdaType = {
    LambdaParam: string;
    LambdaBody: Ast;
}