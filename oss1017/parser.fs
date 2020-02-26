// Module: parser
// Author: ms8817 (Marco Selvatici)
// This may not be the most recent version and was edited to include Combinators

module Parser

open TokeniserStub

type Ast =
    | FuncDefExp of FuncDefExpType // function definition(s) followed by expression
    | Lambda of LambdaType // anonymous function
    | FuncApp of Ast * Ast
    | FuncAppList of Ast list
    | Null // used with pair to make lists
    | Literal of Literal
    | Identifier of string
    | IdentifierList of string list
    | BuiltInFunc of BuiltinFunc // E.g. builtinTimes, builtinPlus
    | RoundExp of Ast // possibly needed see techical note
    | IfExp of Ast * Ast * Ast
    | SeqExp of Ast * Ast // A pair of two elements
    | Combinator of CombinatorType

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

and CombinatorType =
    | S
    | K
    | I

let buildLambda lambdaParam lambdaBody =
    Lambda {
        LambdaParam = lambdaParam;
        LambdaBody = lambdaBody;
    }
