# Lexer
Author: __Fabio Deo fd5117__

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
  

## Lexing examples
```
1. let a = 2 + 3
    -> [KLet; TIdentifier "a"; KEq; TLiteral (IntLit 2); TBuiltInFunc BPlus;        TLiteral (IntLit 3)]

2. let a =
   let b = \x. x * 2 in
     \c. b (b c)
   in
   a 5
   -> [KLet; TIdentifier "a"; KEq; KLet; TIdentifier "b"; KEq; KLambda;
       TIdentifier "x"; KDot; TIdentifier "x"; TBuiltInFunc BMult; TLiteral (IntLit 2); KIn; KLambda; TIdentifier "c"; KDot; TIdentifier "b"; KOpenRound; TIdentifier "b"; TIdentifier "c"; KCloseRound; KIn; TIdentifier "a"; TLiteral (IntLit 5)]
```