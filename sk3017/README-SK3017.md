Beta reduction engine for ASTs

For now using the strict reduction order.

Function application 
* -> run f if its function application.
* -> reduce if f is a lambda expression

Ideas:
1. Environment consists of variable name list and variable mapping - this is used for evaluation of lambda expressions where names may be bound but not yet mapped.

Questions:
1. Should Booleans in the tree be reduced to literals ?