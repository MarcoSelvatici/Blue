Beta reduction engine for the ASTs

For now the runtime system is using the strict reduction order. 

```
(\a. function of a) B ->
(\a. function of a) C ->
function of C

where b is beta-reducible and c is not
```

Lambda expressions bodies are evaluated innermost first. 
(TODO: understand implications)

Lists are defined as :
```
[ ] - Null
[ e1 ] - SeqExp (e1, Null)
[ e1; e2 ] - SeqExp (e1,  SeqExp (e2, Null))
```

Function application 
* -> run f if its function application.
* -> reduce if f is a lambda expression

Grammar changes:
FuncApp Ast * Ast -> | FuncApp of (Ast * Ast)

add this if not successful
 (*
    | FuncApp (l,r) -> (    // workaround "FuncApp FLATBUILTIN" doesn't work
        match (l,r) with    // as FuncApp requires 2 values
        | FLATBUILTIN (b, argLst) -> (b, x::argLst) |> Some
        | _ -> None
        )
    *)  

Assumptions:
1. No built in functions operate on identifiers

Ideas:
1. Environment consists of variable name list and variable mapping - this is used for evaluation of lambda expressions where names may be bound but not yet mapped.

Questions:
1. Should Booleans in the tree be reduced to literals ?
2. Can the runtime have lazy beta reduction for lambdas but eager for let-expressions ?