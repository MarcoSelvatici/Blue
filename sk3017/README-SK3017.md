# Quetions for Individual Part 

> 1. How will your code be used by team (if all goes well) including what order are modules in your team project compiled?

 My part of is **Beta Reduction Runtime** which is the last in the chain of modules:
```                
TOKENISER ═> PARSER ═> (TYPE CHECKER) ═╦═> BETA RUNTIME
                                       ╚═> COMBINATOR RUNTIME
``` 
 
 It will be used to reduce the AST produced by the parser. It interprets the structure of the AST and performs the computations transforming it until it's irreducible.

 > 2. Which parts if any are code written for other people?

 None.

 > 3. Which parts if any of code you use is written by others?

 The type definition followed our language grammar and defining it was a collaborative work so the types in `TokeniserParserStub` were done together.

 > 4. What help have you obtained/given others debugging or doing code review?

We mainly communicated in terms of compatibility and how our decisions will influence other parts - such that any assumptions made were clear to all.

> 5. How did you work out the types that interface your code with others?

We have had several sessions in which we defined grammar together which was later revised. The mapping to the types was done by Marco which we all read and accepted. Types themselves underwent some minor changes which mainly 'trickled down'  from parser and tokeniser.
