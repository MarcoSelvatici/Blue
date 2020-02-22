// Module: parser
// Author: ms8817 (Marco Selvatici)

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
    | SeqExp of Ast * Ast // A pair of two elements [a, b]. TODO: (syntactic sugar) Extend this to (untyped) lists [a, b, c, d] -> Seq(a, Seq(b, ...))
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
    | Y

let buildLambda lambdaParam lambdaBody =
    Lambda {
        LambdaParam = lambdaParam;
        LambdaBody = lambdaBody;
    }
