# SKI Combinator


### Converting from Lambda to SKI

`LambdaToSKI.fs` takes in a pure lambda expression and returns a reduced SKI expression 
(I wrote it before I knew what the AST would look like but it might still be useful. 
Contains code for bracket abstraction...).
It follows the rules defined below. [Source](https://en.wikipedia.org/wiki/Combinatory_logic)

```
   
       T[ ] may be defined as follows:
   
       1.  T[x] => x
       2.  T[(E₁ E₂)] => (T[E₁] T[E₂])
       3.  T[λx.E] => (K T[E]) (if x does not occur free in E)
       4.  T[λx.x] => I
       5.  T[λx.λy.E] => T[λx.T[λy.E]] (if x occurs free in E)
       6.  T[λx.(E₁ E₂)] => (S T[λx.E₁] T[λx.E₂]) (if x occurs free in E₁ or E₂)
         
         T[λx.λy.(y x)]
         = T[λx.T[λy.(y x)]] (by 5)
         = T[λx.(S T[λy.y] T[λy.x])] (by 6)
         = T[λx.(S I T[λy.x])] (by 4)
         = T[λx.(S I (K T[x]))] (by 3)
         = T[λx.(S I (K x))] (by 1)
         = (S T[λx.(S I)] T[λx.(K x)]) (by 6)
         = (S (K (S I)) T[λx.(K x)]) (by 3)
         = (S (K (S I)) (S T[λx.K] T[λx.x])) (by 6)
         = (S (K (S I)) (S (K K) T[λx.x])) (by 3)
         = (S (K (S I)) (S (K K) I)) (by 4)
   
   
         If we apply this combinatorial term to any two terms x and y 
         (by feeding them in a queue-like fashion into the combinator 'from the right'),
         it reduces as follows:
         
         (S (K (S I)) (S (K K) I) x y)
         = (K (S I) x (S (K K) I x) y)
         = (S I (S (K K) I x) y)
         = (I y (S (K K) I x y))
         = (y (S (K K) I x y))
         = (y (K K x (I x) y))
         = (y (K (I x) y))
         = (y (I x))
         = (y x)

```