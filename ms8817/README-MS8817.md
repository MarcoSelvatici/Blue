# Parser & Type Checker
Author: Marco Selvatici (ms8817)

This readme presents the main features and design choices for my individual
coding part of the project.

The initial goal was to implement just a parser, but I managed to get it
completed one week in advance the deadline, so I moved onto implementing a
Hindley Milner type checker, which is listed among the stretch goals in our
initial project plan.

Note: the folder also contains a basic test library, that can be resused to test
different modules. This will hopefully be used to test their code by the other
team memebers in the second part of the project.

## General info

<b>How will your code be used by team (if all goes well) including what order
are modules in your team project compiled?</b>

The code clearly performs the (essential) parsing and (nice-to-have)
type-checking operations.
The parser will depend on the tokeniser to be compiled first, and will be a
dependency for the reduction engines.
The type checker will depend on the tokeniser and parser, but will not be needed
by the reduction engines.

<b>Which parts if any are code written for other people?</b>

All of the code I wrote is directly used in either parser, type-checker or
tests (or combinations of those). Nonetheless, the types in the tokeniser stub I
wrote have been used by Fabio to write the tokeniser, while the Ast type in the
parser have been used by Oliver and Szymon as a base to implement their
reduction engines.

<b>Which parts if any of code you use is written by others?</b>

None.

<b>What help have you obtained/given others debugging or doing code review?</b>

We have a very active groupchat where we often discuss issues. For example,
after creating the Ast type, I had many conversations with Oliver and Szymon
about what should or should not be included in it.
I also discussed with Fabio the content of the tokeniser stub.

<b>How did you work out (who decided what - how do you revise) the types that
interface your code with others?</b>

As I mentioned above, I come up with the Ast type, and then revised it
multiple times with the suggestions from Oliver and Szymon.

Our tokeniser types where sort of defined already in the initial project plan,
but it did need several changes in order to be easier to use in the parser.

## Parser

My implementation uses parser combinators. Just two of them are needed:
- `.+.` sequential operator (matches parse rule 1 and then parse rule 2)
- `.|.` alternative operator (matches parse rule 1 or parse rule 2)

A careful implementation of the `.|.` operator allows to report the longest
match in case of parse error.

### BNF Grammar

The BNF grammar parsed is:

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

Parser combinators allow to map this grammar definition very closely into code.
For example, the function that parses an item expression is:
```
and pItemExp pState =
    pState
    |> (pLiteral .|. pIdentifier .|. pBuiltin .|. pRoundExp .|. pIfExp .|. pSeqExp .|. pLambdaExp .|. pFuncDefExp)
```

which is very similar to the definition of an item-exp in the grammar.

### Error reporting

The code presents some advanced error reporting features:
- when an expression fails to parse completely, the error regarding the longest
possible match is returned.
- the error contains a trace, which helps to understand the "scope" of where the
error happened. For example, the code:
```
let x = 2 in
  let y = 3 in
    .           // gives parse error 
  ni
ni
```
produces the error trace `in y in x`, while the code:

```
let x = 2 in
  let y = 3 in
    y
  ni
  .     // gives parse error 
ni
```
produces the error trace `in x` (since we are "outside" y).

(both examples are taken from tests in `ParserTest.fs`)

### Efficiency

Efficiency issues may arise when we need to parse the same expression at the
beginning of two alternative branches. For example
`<app-exp-list> ::= <item-exp> | <item-exp> <app-exp-list>` may require you to
parse the first item-exp once, and then again if the match in the first branch
failed.

This pattern can produce an exponential parsing time, when the same task could
be performed in linear time. My parsing rules are designed in a way that avoids
these inefficiencies.

### Currying

The parser automatically curries all function definitions and lambdas, to make
them ready-to-use in the reduction engines.

### Operator precedence

When the parser encounters an applicative expression list, it makes sure that
binary operators are treated with the correct precedence.

### Wroth noting test
If you are intersed in examining the tests in `ParserTest.fs` I suggest to pay
attention to the one called `Simple program`. It tests:
- curried functions and lambas,
- operators associativity,
- function application associativity.

The code it successfully parse is:
```
let x y = x + y in
    let z = \a b. a < b && z in
        x (z 1 2)
    ni
ni
```

## Type Checker

As a stretch goal, I implemented a fully working Hindley Milner type checker.

My solution is based on some key ideas:

- There exists a context with mappings from identifiers to their types (which
may be generic). This context may change at various stages of the inference
process.
- An inference function may return:
  - if successful, the type of the ast and the substitutions that has been
  applied in order to unify different types. For example, a substitution may be
  that the generic type `Gen 1` can specialised to `Base Int`.
  - if unsuccessful, the appropiate error.
- You can apply the above mentioned substutions to both types and context, as
needed.

With this in place, the resulting code has a nice recursive structure.

In the next sections, I will describe the two core functions of my type
inference implementation.

### Unify

This function takes two types (possibly containing wildcards) and tries to unify
them into a single type. To do so, it is often necessary to substitute some
wildcards with more specialised types (possibly simply other wildcards).

If the unification succeeds, the list of necessary substitutions is returned,
otherwise it returns an error explaining why such process is not possible.

### Apply

This function takes a list of substitutions, and apply them to a type. This is
a recursive process, since a type may contain substitutable wildcards.
