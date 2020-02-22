# Parser
Author: Marco Selvatici (ms8817)

## BNF Grammar (not working since left recursive...)

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

## BNF Grammar (proper)

```
<identifier-list> ::= TIdentifier | TIdentfier <identifier-list>
<item-exp> ::= TLit | TIdentifier | BuiltinFunc | <round-exp> | <lambda-exp> | <defn-exp> | <if-exp> | <seq-exp>
<app-exp-list> ::= <item-exp> | <item-exp> <app-exp-list>
<round-exp> ::= "(" <exp> ")"
<lambda-exp> ::= "\" <identifier-list> "." <exp>
<defn-exp> ::= "let" <identifier-list> "=" <exp> "in" <exp> "ni"
<if-exp> ::= "if" <exp> "then" <exp> "else" <exp> "fi"
<seq-exp> ::= "[" <exp> "," <exp> "]"
<exp> ::= <app-exp-list>
```

## Operator precendences examples
```
2 + 3 * 4 - 5
Try find operator in the list, in orderof associativity: && ||, then <, > then +, - then * /
When operator is found, split the list there.
Create: FuncApp( FuncApp(Operator, recur LHS), recur RHS)
If no operator is found, use normal function application associativity.
2 + 3 * 4 - 5 < 6
              ^
FA ( FA (<, 2 + 3 * 4 - 5), 6)
2 + 3 * 4 - 5   (LHS)
  ^
FA ( FA (+, 2), 3 * 4 - 5)
3 * 4 - 5   (RHS)
      ^
FA ( FA (-, 3 * 4), 5)
3 * 4    (LHS)
  ^
FA (FA (*, 3), 4))

FA ( FA (<, FA ( FA (+, 2), FA ( FA (-, FA (FA (*, 3), 4)), 5))), 6)
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
