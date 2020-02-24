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


Function application 
* -> run f if its function application.
* -> reduce if f is a lambda expression

Ideas:
1. Environment consists of variable name list and variable mapping - this is used for evaluation of lambda expressions where names may be bound but not yet mapped.

Questions:
1. Should Booleans in the tree be reduced to literals ?
2. Can the runtime have lazy beta reduction for lambdas but eager for let-expressions ?