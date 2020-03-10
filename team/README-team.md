# Sample README

NB - replace this file by your overall team README.

This directory is used for:

* Team phase code submission
* Team phase individual team contribution statements (named `team-login.md`)
* overall team readme

# Documentation

This documentation will contain:
- how to run the code
- features of the language with some code examples (TODO)
- implementative details of our system (TODO)

## Run the code

### Expecto tests

Enter the `emulator-tests` directory and run with `dotnet run`.

### GUI

Enter the editor directory and follow the instructions in the README. 

## Features

We implemented a weakly typed language with two runtimes (beta reduction and combinators reduction). It also allows comments, builtin types and operators/functions.
#### Comments:
- inline: # --> \n
- multiline: (* --> *)
  
#### builtin types
The language supports all main built-in types, as listed below:
- int
- bool
- string
- pair 
- list ( parsed as a concatenation of pairs ).
- NULL ( list endpoint )

#### builtin operators/functions
As for the builtin functions, the language is able to perform the following:
| function | symbol | type |
| --- | --- | --- |
| plus | + | (int -> int -> int) |
| minus | - | (int -> int -> int) |
| multiply | * | (int -> int -> int) |
| divide | / | (int -> int -> int) |
| greater | > | (int -> int -> bool) |
| greater or equal to | >= | (int -> int -> bool) |
| less than | < | (int -> int -> bool) |
| less than or equal to |<= | (int -> int -> bool) |
| equal to | == | (int -> int -> bool) |
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
| print| | 

#### Functions
The language supports recursion and both anonymous and named functions, here are some practicle examples for both classes.
- Anonymus functions:
```
\x.x  # f# equivalent of fun x -> x
\xy.x y  # f# equivalent of fun x y -> x y 
```
- Named functions:
```
let name = \x. x in # Assign a name to the lambda function.
let name x = x in   # Syntactic sugar for the named expression above.
```

#### Additional Features
We developed two runtime systems:
  - Beta Reduction
  - SKI Combinator

The engines support all the above features, and have been tested on ~400 tests for both :
  - Unit Testing
  - End To End Testing
This process has been made particularly frictionless by developing a Test library, that massively eased the job of adding new tests and allowed us to do Regression Testing. <br>
Moreover, an editor in a VisUAL2 environment has been setup, that makes writing in our language very easy and intuitive. <br>
For Demo purposes, a library of functions has been developed and added to a Preprocessor, such that is imported before the test code is ran. The library contains the following functions to massively facilitate lists and string processing for the end user:
  - listMap
  - listReduce
  - listFold
  - listFindInt
  - listFind
  - stringLength
  - listConcat
  - listReverse
  - listSort
  - listItem
  - TODO WHEN MERGING ADD THE FUNCTIONS THAT ARE IN THE PRINT BRANCH

Some of these functions have also been used in a further demo project, which is a Lexer for a simple language. <br>
The lexer, contained in the demo-code folder under the name lexngram.s, correctly lexes strings, bools and ints, as well as all the builtin functions and list and string functions we included in our language. <br>

## Implementation details
The code is divided in a series of modules.

- `TestLib`: provides a basic testing library, built on top of Expecto.
- `SharedTypes`: contains the types which are reused across different modules,
like `Token`, `Ast` and error types.
- `Tokeniser`, `Parser`, `TypeChecker`, `SKIEngine`, `BetaEngine`: these are the
core modules of our language. Their top level functions all return a
`Result<someType, ErrorT>` where `someType` is the type returning for a correct
execution of the program.

Note: the type checker is best effort for functions `Implode` and `Explode`.
Since our lists are untyped it is impractical to check the list types.

### Tokeniser
The tokeniser module acts first, taking as input the raw string that contains the program to be run. <br>
Strings can contain escape sequences, preceeded by the classical backwards slash char: `\`.
- Escape Sequences:
  - \a : alert
  - \b : backspace
  - \f : formfeed
  - \n : newline
  - \r : carriage return
  - \t : tab
  - \v : vertical tab
  - \\\\ : backslash
  - \\" : quotation mark
  - \\' : apostrophe

The tokeniser tries to match valid tokens as it goes through the input string. The error checking performed by the tokeniser is __exclusively__ on the validity of a single token, not on the logic sense of the expression. <br>
The Tokens that the tokeniser would match and return as valid are defined as follow, using regexes. <br>
It is also specified a (non exahustive in any way) list of cases in which the program would instead throw an error or just tokenise it differently.

| Token | Regex |
| --- | ----- |
| Bool | ['true', 'false'] |
| String| [ " ][anychar, escape sequence]*[ " ] |
| Identifier |  [ _ ]* [a - z A - Z ] [a - z A - Z _ ']* |

<br>

| Token | Expected Result  | Actual Result |
| --- | --- | --- |
| 0 | Bool | intLit|
| 1 | Bool | intLit|
| False | Bool | TIdentifier |
| True | Bool | TIdentifier |
| .9 | Float ( 0.9 ) | KDot; intLit | 
| 1. | Float ( 1.0 ) | Error (invalid float number)
| "\z" | stringLit( "\\z" ) | Error (invalid escape sequence)
| " _ " | TIdentifier (" _ ") | Error (invalid character)
| "a-b" | TIdentifier (" a-b ") | Error (invalid character)
| "'a" | TIdentifier (" 'a ") | Error (invalid char)

#### Lexing examples
```
1. let a = 2 + 3
    -> [KLet; TIdentifier "a"; KEq; TLiteral (IntLit 2); TBuiltInFunc BPlus; TLiteral (IntLit 3)]

2. let a =
   let b = \x. x * 2 in
     \c. b (b c)
   in
   a 5
   -> [KLet; TIdentifier "a"; KEq; KLet; TIdentifier "b"; KEq; KLambda;         TIdentifier "x"; KDot; TIdentifier "x"; TBuiltInFunc BMult; TLiteral     (IntLit 2); KIn; KLambda; TIdentifier "c"; KDot; TIdentifier "b";        KOpenRound; TIdentifier "b"; TIdentifier "c"; KCloseRound; KIn;          TIdentifier "a"; TLiteral (IntLit 5)]
```

### Parser
### TypeChecker
### SKI Engine
### Beta Engine


