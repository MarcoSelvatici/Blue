# Parser
Author: Marco Selvatici (ms8817)

## BNF Grammar

```
<identifier-list> ::= TIdentifier | TIdentfier <identifier-list>
<item-exp> ::= TLit | TIdentifier | BuiltinFunc | <round-exp> | <lambda-exp> | <defn-exp> | <if-exp> | <seq-exp>
<app-exp-list> ::= <item-exp> | <item-exp> <app-exp-list>
<round-exp> ::= "(" <exp> ")"
<lambda-exp> ::= "\" <identifier-list> "." <exp>
<defn-exp> ::= "let" <identifier-list> "=" <exp> "in" <exp> "ni"
<if-exp> ::= "if" <exp> "then" <exp> "else" <exp> "fi"
<seq-exp> ::= "[" <seq-list-exp> "]"
<seq-list-exp> ::= <exp> | <exp> "," <seq-list-exp> 
<exp> ::= <app-exp-list>
```

## Operator precendences logic

Try find operator in the list, in order of associativity:
- logical
- comparsison
- additive
- multiplicative

When operator is found, split the list there and create
`FuncApp( FuncApp(Operator, recur LHS), recur RHS)`
If no operator is found, use normal function application associativity.
```
> Example 1
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

Total:
FA ( FA (<, FA ( FA (+, 2), FA ( FA (-, FA (FA (*, 3), 4)), 5))), 6)

> Example 2
1 < 2 && 3 >= 4
      ^
FA ( FA(&&, 1 < 2), 3 >= 4)
1 < 2   (LHS)
  ^
FA ( FA (<, 1), 2)

3 >= 4   (RHS)
  ^
FA ( FA (>=, 3), 4)

Total:
FA ( FA(&&, FA ( FA (<, 1), 2)), FA ( FA (>=, 3), 4))


> Example 3
1 - 2 + 3
  ^
FA ( FA (-, 1), 2 + 3)
2 + 3   (RHS)
  ^
FA ( FA (+, 2), 3)

Total:
FA ( FA (-, 1), FA ( FA (+, 2), 3))
```

## Wroth noting tests
<u>Simple program</u>. Tests:
- Curried functions and lambas,
- operators associativity,
- function application associativity.
```
let x y = x + y in
    let z = \a b. a < b && z in
        x (z 1 2)
    ni
ni
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
