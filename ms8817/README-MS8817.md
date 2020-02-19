# Parser
Author: Marco Selvatici (ms8817)

## BNF Grammar

```
<applicative-exp> ::= <exp> | <exp> <applicative-exp>
<multiplicative-exp> ::= <applicative-exp> | <applicative-exp> KMultiplicativeOp <multiplicative-exp>
<additive-exp> ::= <multiplicative-exp> | <multiplicative-exp> KAdditiveOp <additive-exp>
<comparison-exp> ::= <additive-exp> | <additive-exp> KComparisonOp <comparison-exp>
<logical-exp> ::= <comparison-exp> | LogicalNotOp <comparison-exp> | <comparison-exp> KLogicalOp <logical-exp> 
<unary-exp> ::= KUnaryOp <exp>
<seq-exp> ::= KOpenSquare <exp> KComma <exp> KCloseSquare
<if-exp> ::= KIf <exp> KThen <exp> KElse <exp> KFi
<builtin-exp> ::= <if-exp> | <seq-exp> | <unary-exp> | <comparison-exp>
<identifier-list> ::= TIdentifier | TIdentfier <identifier-list>
<lambda-exp> ::= KLambda <identifier-list> KDot <exp>  // \x.x+1
<round-exp> ::= KOpenRound <exp> KCloseRound
<exp> ::= TLit | TIdentifier | <round-exp> | <lambda-exp> | <builtin-exp> 
<named-let-in-exp> ::= KLet <identifier-list> KEq <block> KIn
<block> ::= <exp> | <named-let-in-exp> <block>
```

## Parsing expamples
```
> Example 1
f 2 + 3

> Becomes
AdditiveExp (
  ApplicativeExp (
    TIdentifier "f",
    TLiteral 2
  ),
  TLiteral 3
)

> Example 2
let a =
  let b = \x. x * 2 in
  \c. b (b c)
in
a 5

> Becomes
Block(
  NamedLetInExp (
    IdentifierList (
      TIdentifier "a"
    ),
    Block (
      NamedLetInExp (
        IdentifierList (
          TIdentfier "b"  
        ),
        Block (
          LambdaExp (
            IdentifierList (
              TIdentifier "x"
            ),
            MultiplicativeExp (
              TIdentifier "x",
              TLiteral 2
            )
          )
        ) # "b" Block
      ),
      LambdaExp (
        IdentifierList (
          TIdentifier "c"
        ),
        ApplicativeExp (
          TIdentifier "b",
          ApplicativeExp (
            TIdentifier "b",
            TIdentifier "c"  
          )
        )
      )
    ) # "a" block
  ),
  ApplicativeExp (
    TIdentifier "a",
    TLit 5
  )
) # Top level block 
```
