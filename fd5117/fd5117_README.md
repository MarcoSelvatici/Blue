# Lexer
Author: __Fabio Deo fd5117__.

This is my individual readme, where I quickly explain the structure of the lexer and how to run the testbench.

## General Info
__How will your code be used by team (if all goes well) including what order are modules in your team project compiled?__

My individual part was lexing. Therefore my module will be compiled first, taking as input the raw program string and outputting a Result<Token list, Error> type that will feed as input of the Parser module, developed by Marco. 

__Which parts if any are code written for other people?__

None.

__Which parts if any of code you use is written by others?__

Because Marco started the parser before I started the lexer, I have tried to use as much as possible his tokeniser stub in order to minimise the inconsistencies once the two pieces of code will be joined. 
However I did make some changes, adding some tokens and modifying his suggested structure.

__What help have you obtained/given others debugging or doing code review?__

We discuss daily on the groupchat in order to reduce as much as possible the future adjustents we will need to make each individual code consistent with its complimentary part.
The biggest interractions I had were with Marco, as the output of my module feeds directly into his, so we longly discussed and proofread each others tokeniser structures.

__How did you work out (who decided what - how do you revise) the types that interface your code with others?__

Most of the grammar we needed to be able to lex had been discussed by everyone during the project plan. However, as I mentioned, I added some new features. Lastly I changed the output type from Token list to Result<Token list, Error> in order to test for faulty inputs as well as correct ones.


## Instructions
1. Clone the repository
2. Navigate to .../fd5117
3. Run: ```dotnet run --project lexer.fsproj ``` to run the testbench
   
## Token List
```
type BuiltInFunc =
    // Builtin with no special treatment.
    | BNot        
    | BHead       
    | BTail       
    | BSize       
    | BImplode    
    | BExplode    
    | BAppend     
    | BStrEq      
    // ComparisonOp.
    | BGreater    
    | BGreaterEq  
    | BLess       
    | BLessEq     
    | BEqual      
    // LogicalOp.
    | BAnd        
    | BOr         
    // BitwiseOp
    | BBitAnd     
    | BBitOr      
    // AdditiveOp.
    | BPlus       
    | BMinus      
    // MultiplicativeOp.
    | BMult       
    | BDiv       

type Literal =
    | IntLit of int        
    | FloatLit of float    
    | BoolLit of bool      
    | CharLit of char      
    | StringLit of string  

type Token =
    | TLiteral of Literal       
    | TIdentifier of string
    | TBuiltInFunc of BuiltInFunc
    
    // Keywords. 
    | KLet          
    | KEq           
    | KRec          
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
    | KNull         
```

## Arbitrary Grammar Decisions

- Escape Sequences:
  - \a : alert
  - \b : backspace
  - \f : formfeed
  - \n : newline
  - \r : carriage return
  - \t : tab
  - \v : vertical tab
  - \\\\ : backslash
  - \" : quotation mark
  - \' : apostrophe

- Booleans: 
   - true
   - false
   -  __NOT ALLOWED__ ( 0; 1; False; True )

- Integers:
  - Regex spec: [0 - 9]+ 
  - e.g ( 0; 0003; 1240) 

- Floats:
  - Regex spec: [0 - 9]+ [ . ] [0 - 9]+
  - e.g ( 0.9; 1234.56789 )
  -  __NOT ALLOWED__ ( .9; 13. )

- Char:
  - [ ' ][anychar || escape sequence][ ' ]
  - e.g ( ' '; '\v' '$'; '1' )
  -  __NOT ALLOWED__ ( ''; '\z')
  
- String:
  - [ " ][anychar || escape sequence]*[ " ]
  - e.g ( ""; " "; "$"; "az19"; "\a\b\n" )
  -  __NOT ALLOWED__ ( "\z"; "\c")
  
- Identifier:
  - [ _ ]* [a - z A - Z ] [a - z A - Z _ ']*
  - e.g ( a; A; abcAbc; _a; A\_; az19' ; a'\_'a )
  -  __NOT ALLOWED__ ( " _ "; "a-b"; "'a")
  
- Comments:
  - inline: // --> \n
  - multiline: (* --> *)
  
## Lexing examples
```
1. let a = 2 + 3
    -> [KLet; TIdentifier "a"; KEq; TLiteral (IntLit 2); TBuiltInFunc BPlus; TLiteral (IntLit 3)]

2. let a =
   let b = \x. x * 2 in
     \c. b (b c)
   in
   a 5
   -> [KLet; TIdentifier "a"; KEq; KLet; TIdentifier "b"; KEq; KLambda; TIdentifier "x"; KDot; TIdentifier "x"; TBuiltInFunc BMult; TLiteral (IntLit 2); KIn; KLambda;       TIdentifier "c"; KDot; TIdentifier "b"; KOpenRound; TIdentifier "b"; TIdentifier "c"; KCloseRound; KIn; TIdentifier "a"; TLiteral (IntLit 5)]
```