# HLP project

## Task

Implement a functional language:

- weakly typed.
- two runtimes (beta reduction and combinators reduction).

### Main Features

#### builtin types
- int
- bool
- string
- pair 
- list (add on, can be built on top of pairs but may have a nicer syntax)
- NULL (list terminator)

#### builtin operators/functions
| function | symbol | type |
| --- | --- | --- |
| plus | + | (int -> int -> int) |
| minus | - | (int -> int -> int) |
| multiply | * | (int -> int -> int) |
| divide | / | (int -> int -> int) |
| greater/less than/equal to | > >= < <= | (int -> int -> bool) |
| logical and | && | (bool -> bool -> bool) |
| logical or | \|\| | (bool -> bool -> bool) |
| logical not | ! | (bool -> bool) |
| head | | ('a pair -> 'a)
| tail | | ('a pair -> 'a pair)
| size | | ('a list -> int)
| append | | ('a list -> 'a -> 'a list)
| strEq | | (string -> string -> bool)
| ifThenElse | | (bool -> 'a -> 'a -> 'a)
| implode | | (string list -> string)
| explode | | (string -> string list)
| test | | ('a -> bool)
(print ?) this would have side effects.

#### Functions

Anonymus functions:
```
\x.x  # f# equivalent of fun x -> x
\xy.x y  # f# equivalent of fun x y -> x y 
```
Named functions:
```
let name = \x. x in # Assign a name to the lambda function.
let name x = x in   # Syntactic sugar for the above named expression.
```

### Basic goals

- Implement lexer.
- Implement parser.
- Implement a combinators reduction engine.
- Implement a beta reduction engine.

### Stretch goals
- Extensive and precise error reporting.
- Implement lists.
- Implement pattern matching.
- Implement self/mutual recursion.
- Optimize reduction algorithms (do not evaluate expression multiple times etc...).
- Implement type inference / checking.
- Implement complex data types, like Discrimiated unions, records etc...
- GUI ide.

## Code structure

### Modules
The code will be composed of 5 core modules:
- Lexer
- Parser
- Type checker (add on)
- Beta reduction engine
- Combinators reduction engine

Every module will have one corresponding top level function, with the specified inputs and outputs (see table below).

The table below does not list the possible errors that can be thrown at the various stages, but only the return types resulting from a correct execution.

Ideally, every module will have a specific error type, but a basic common version is showed in the `Types` section below.

Then a module output would be a `Result<CorrectT, ErrorT>`.

| Module | Inputs | Outputs | Comments |
| --- | --- | --- | --- |
| Lexer | Source code: `string` | Tokens list: `Token list` | Extend on top of LexNGram. Possibly use only pure functional style in order to make it easier to reimplement it in the language itself (e.g. do not use f# parseInt function). |
| Parser | Token list: `Token list` | Ast: `Block` | Take inspiration from tick 3, but will require extra complexity. |
| Type checker (Add on) | Ast: `Block` | Ast: `Block` | Type check. Not included in the minimal implementation. |
| Combinator reductor | Ast: `Block` | value: `Exp` | Perform SKI (and other builtin functions) reduction. This happens in two steps: 1. extraction of a constant expression, 2. evaluation. The returned expression may be a literal or a partially applied function. |
| Beta reductor | Ast: `Block` | value: `Exp` | Perform Beta reduction (lazy evaluation). The returned expression may be a literal or a partially applied function. |

### Types
These are the basic types, we will probably need to encapsulate those inside results or options.

```
// Lexer.

Keyword =
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
    | KPlus
    | KMinus
    | KMult
    | KDiv
    | KGreater
    | KGreaterEq
    | KLess
    | KLessEq
    | KEqual
    | KAnd
    | KOr
    | KNot
    | KHead
    | KTail
    | KSize
    | KAppend
    | KNull
    | KStrEq
    | KImplode
    | KExplode

Literal =
    | IntLit of int
    | BoolLit of bool
    | StringLit of string 

Token =
    | TLit of Literal
    | TIdentifier of string
    | TKeyword of Keyword

// Parser.

// A block is a series of let expressions, followed by a single final expressions
// (that can make use of the previously defined functions).
// Possibly use a map instead of a list, for performance.
Block = (string * Block) list * Exp

Exp =
    | LiteralExp of Literal
    | FuncApp of Exp * Exp
    | LambaExp of TIdentifier * Exp
    | BuiltinFunExp of BuiltinFun

BuiltinFun =
    | IfThenElse of Exp * Exp * Exp
    | Head of Exp
    | Tail of Exp
    | Size of Exp
    | Append of Exp * Exp
    | Implode of Exp
    | Explode of Exp
    | Pair of Exp * Exp
    | Test of Exp
    | BracketedExp of Exp
    | Comparison of ComparisonExp

ComparisonExp =
    | LessThan of Exp * Exp
    | LessEqualTo of Exp * Exp
    | GreaterThan of Exp * Exp
    | GreaterEqualTo of Exp * Exp
    | EqualTo of Exp * Exp
    | Logical of LogicalExp

LogicalExp =
    | LogicalAnd of Exp * Exp
    | LogicalOr of Exp * Exp
    | LogicalNot of Exp
    | Multiplicative of MultiplicativeExp

MultiplicativeExp =    
    | Times of Exp * Exp
    | Divide of Exp * Exp
    | Additive of AdditiveExp

AdditiveExp =
    | Plus of Exp * Exp  
    | Minus of Exp * Exp


# General error message.
# Module-specific error messages will be introduces as other
# nice-to-have features.

ModuleT =
    | Lexer
    | Parser
    | TypeChecker
    | CombinatorsEngine
    | BetaEngine

ErrorT = {
    Module: ModuleT;
    Descripton: string;
}

```

## Testing

1. Lexer
    - Unit testing the lexing of each specific token
    - Test robustness against incorrect inputs
2. Parser
    - hard-code possible Tokens List and
      - if valid, test they get parsed in the expected Asts.
      - if invalid, check the parser fails with the expected error message.
3. Type checker
    - hard code possible Asts and
      - if valid, test the inferred types are as expected.
      - if invalid, check the type-inference fails with the expected error message.
4. SKI Engine
    - hard code possible Asts and
      - if valid, test the engine can reduce the ast to a constant expression. Then check the evaluation of this expression happens correctly.
      - if invalid, check that the reduction to constant expression and the evaluation fail as expected.
5. Beta-reduction Engine
    - hard-coded independent (as possible) tests of language features:
        - constants
        - function application
        - implicit recursion (Y-combinator written in lambda expression)
        - recursion
        - mutual recursion
        

## Splits
| Part | Estimated difficulty (1-10) | Assigned to |
| --- | --- | --- |
| lexer | 4 | Fabio |
| parser | 6 | Marco |
| type checker | 8 | - |
| SKI+ Engine | 9 | Oliver |
| B-red Engine | 8 | Szymon |

Members doing lexer and parser may help other team members in debugging and testing.

Also the type checker can be implemented from team members that manage to finish their tasks in advance. The work on the type checker should be performed in an independent module.

## Code Examples
```
- Simple lambda:
(\y. \y. x + y) 1 2
(\x y. x + y) 1 2

-Named expression:
let x = \y. y in
x 3

- Equivalent to:
let x y = y in
x 3

- More complex:
let a c =
  let b x = x * 2 in
  b (b c)
in
a 5

- Equivalent to:
let a =
  let b = \x. x * 2 in
  \c. b (b c)
in
a 5

- Equivalent to:
( \a. a 5 ) ( ( \b. \c. b (b c) ) ( \x. x * 2 ) )

- Other example:
let z a b =
  if a then b else b + 2 fi
in
z False 1
```
