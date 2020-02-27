# SKI Combinator Runtime

Author: oss1017 (Oliver Stiff)

### Overview

###### How will your code be used by the team including what order are modules in your team project compiled?
If all goes well, the combinator runtime which I wrote should be able to take in an AST created by the parser (Marco's part). 
This AST is then reduced and evaluated using SKI combinators as well as the evaluation of built-in functions (Mult, Head, Size...).
My module, being dependent on both the tokensiser and parser must be compiled last.   
Szymon's code and mine, both being reduction/evaluation engines can be run in parallel to compare results and test the implementations.  

###### Which parts if any are code written for other people?
None.   

###### Which parts if any of code you use is written by others?
None of the combinator runtime code was written by other team members. 
The type definitions in `TokeniserStub.fs` and `parser.fs` were written up by Marco based on our work for the team work plan. 
I extended the Ast type slightly to include the S, K and I combinators. 
I also added implode and explode built-in functions to the tokeniser stub.    


###### What help have you obtained/given others debugging or doing code review?
Most of the collaboration with other members was centred around making the output of one module match as closely as possible with the input of the next.
The main aim being to reduce incompatibilities during the group stage of the  project. 
I did not obtain/give any help as far as code or debugging is concerned.   


###### How did you work out (who decided what - how do you revise) the types that interface your code with others?
The grammar for our language was extensively discussed during the team work plan phase of the project. 
Further reflection led us to change most of the grammar, with Marco (parser) defining the new Ast type. 
Based on his type definition and further discussion with him and Szymon, I was able to get a reasonable idea of what the Ast pased to the runtime would look like.   


###### What works, what does not work?
As far as what was defined in the team work plan, the basic goals were implemented.
Mutual recursion was not implemented as it would require knowing which functions are recursive which is not currently the case in our language.  

What works:
- Converting Lambdas into S,K,I combinators (in `bracketAbstract` function)
- Simplifying SKI combinator expressions (in `combinatorReduc` function)
- Function definitions (in `bracketAbstract` function)
- Function applications (in `bracketAbstract` function)
- Evaluating built-in functions (in `evalBuiltin` function):  
    - implode
    - explode
    - head
    - tail
    - size
    - basic arithmetic and logical operators
- ifThenElse expressions, only evaluating a branch if needed
- Error reporting which indicates at which stage of the runtime an error occured
- Recursion (not using the Y combinator as it would require knowing if a function is recursive)   


### Converting from Lambda to SKI

`LambdaToSKI.fs` takes in a pure lambda expression and returns a reduced SKI expression 
(I wrote it before we decided on the final AST but it was still useful and part of the code was used in the combinator runtime `ski_combinator.fs`).
It follows the rules defined below. [Source](https://en.wikipedia.org/wiki/Combinatory_logic)   


```
   
       T[ ] may be defined as follows:
   
       1.  T[x] => x
       2.  T[(E₁ E₂)] => (T[E₁] T[E₂])
       3.  T[λx.E] => (K T[E]) (if x does not occur free in E)
       4.  T[λx.x] => I
       5.  T[λx.λy.E] => T[λx.T[λy.E]] (if x occurs free in E)
       6.  T[λx.(E₁ E₂)] => (S T[λx.E₁] T[λx.E₂]) (if x occurs free in E₁ or E₂)
         
```

### Testbench

The module `Testbench` contains a series of lists of testcases classified based on which feature they evaluate (recursion, arithmetic operations...).
Each test case is a tuple containing a test name, the input to the runtime as well as the expected output,
whether it be a literal, a partially applied function, or an error. 
The Testbench is run by calling `testAll()`.  
A single program/AST can be evaluated by applying an input tree to the function `singleEval`.