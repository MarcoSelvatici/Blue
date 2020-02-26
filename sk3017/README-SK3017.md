# Instructions 
```
How will your code be used by team (if all goes well) including what order are modules in your team project compiled?

Which parts if any are code written for other people?

Which parts if any of code you use is written by others?

What help have you obtained/given others debugging or doing code review?

How did you work out the types that interface your code with others?

 
I do not want a report-style statement, just the above info as concisely as possible.
```

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

Assumptions:
1. No built in functions operate directly on identifiers

Ideas:
1. Environment consists of variable name list and variable mapping - this is used for evaluation of lambda expressions where names may be bound but not yet mapped.

Questions:
1. Can the runtime have lazy beta reduction for lambdas but eager for let-expressions ?

## Built-In functions 

Builtin functions are organised into hierarchy:
1. Input type and arity
2. Output type