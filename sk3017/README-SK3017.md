# Instructions 

```
1. How will your code be used by team (if all goes well) including what order are modules in your team project compiled?

2. Which parts if any are code written for other people?

3. Which parts if any of code you use is written by others?

4. What help have you obtained/given others debugging or doing code review?

5. How did you work out the types that interface your code with others?
```

# Answers

```
1. Beta reduction engine for the ASTs. It uses applicative order and has general built-in capabilities. My part of is the last in the chain of 

TOKENISER -> PARSER -> TYPE CHECKER* -> BETA RUNTIME
                                     -> SKI COMBINATOR RUNTIME

Thus the 
```
```
2. None - I haven't wrote any code for anyone.
```
```
3. None - No one wrote any code for me.
```
```
4. We mainly communicated in terms of compatibility and how our decisions will influence other parts. 
```

```
5. We have had several sessions in which we defined grammar together which was later revised and we share that - although it underwent some changes it was mostly the same and they 'trickled down' mainly from parser and tokeniser.
```

