// Types that are shared among different modules.

module SharedTypes

type BuiltInFunc =
    // side-effects
    | Print
    // Builtin with no special treatment.
    | Not
    | Head
    | Tail
    | Size
    | Implode
    | Explode
    | Append
    | StrEq
    | Test
    // ComparisonOp.
    | Greater
    | GreaterEq
    | Less
    | LessEq
    | Equal
    // LogicalOp.
    | And
    | Or
    // AdditiveOp.
    | Plus
    | Minus
    // MultiplicativeOp.
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
    // Keywords. 
    | KLet
    | KEq
    | KIn
    | KNi
    | KComma
    | KSemiColon
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

type Ast =
    | FuncDefExp of FuncDefExpType
    | LambdaExp of LambdaType
    | IfExp of Ast * Ast * Ast
    | SeqExp of Ast * Ast
    | FuncApp of (Ast * Ast)
    | FuncAppList of Ast list // Transformed into a tree of FuncApp.
    | Identifier of string
    | IdentifierList of string list // Transformed into a list of Identifier.
    | Literal of Literal
    | BuiltInFunc of BuiltInFunc
    | Null // Used in the EmptySeq, which terminates the sequences.
    | Combinator of CombinatorType
    | Token of Token // Should not appear in the final ast.

// Curried.
and FuncDefExpType = {
    FuncName: string;
    FuncBody: Ast;
    Rest: Ast;
}

// Curried.
and LambdaType = {
    LambdaParam: string;
    LambdaBody: Ast;
}

and CombinatorType =
    | S 
    | K
    | I    

// Handy definition of EmptySeq [] and PAP to match it.
let EmptySeq = SeqExp (Null, Null)
let (|EMPTYSEQ|_|) =
    function
    | SeqExp(Null, Null) -> Some 0
    | _ -> None

//========//
// Errors //
//========//

type ParserError = {
    msg: string;
    parseTrace: string;
    unmatchedTokens: Token list;
    currentAsts: Ast list;
}

type BetaEngineError = {
    msg: string;
    trace: string list;
    ast: Ast;
}

type ErrorT =
    | PreprocessorError of string
    | LexerError of string
    | ParserError of ParserError
    | TypeCheckerError of string
    | BetaEngineError of BetaEngineError
    | SKIRuntimeError of string

/// Printing
type Printer() =
    static let mutable outBuffer = ""
    static member Print s =
        if outBuffer <> ""
        then outBuffer <- outBuffer + "<br /> > " + s
        else outBuffer <- outBuffer + " > " + s
    static member ReturnClear =
        let ret = outBuffer
        outBuffer <- ""
        ret

/// Pretty print ASTs
let prettyPrint (inp: Result<Ast,ErrorT>): string =
    let toString x =
        sprintf "%A" x
    let rec printList (inp: Ast) : string =
        match inp with
        | SeqExp (Null, Null) -> ""
        | SeqExp (head,SeqExp (Null, Null)) -> (printFormat  <| head |> sprintf "%A")
        | SeqExp (head,tail) -> (printFormat  <| head |> sprintf "%A,") + (printList <| tail |> sprintf "%A")
        | _ ->  "Error: PrettyPrint: PrintList is broken"
    and printFormat inp = 
        match inp with
        | Identifier x  -> toString x
        | Combinator x -> toString x
        | Literal (IntLit x) -> toString x 
        | Literal (BoolLit x) -> toString x 
        | Literal (StringLit x) -> toString x 
        | Null -> "Null" // should this be an error ?
        | LambdaExp { LambdaParam = name; LambdaBody = exp } -> 
            sprintf "(\\%A.%A)" name (printFormat exp)
        | SeqExp _ -> "[" + printList inp + "]"
        | FuncApp (x , y) -> "(" + (printFormat  <| x |> sprintf "%A") + " " + (printFormat <| y |> sprintf "%A") + ")" // should this be an error ?
        | FuncDefExp _ | FuncAppList _ | IdentifierList _ | Token _ | IfExp _ | BuiltInFunc _ -> toString inp
    match inp with
    | Error x -> toString x
    | Ok x -> printFormat x
