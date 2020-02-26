// Author: oss1017 (Oliver Stiff)
// Module: Pure Lambda to SKI (not used in the final program)

open System
open Expecto
open typeDefinitions


module rec Interpreter =
    //print function
    let print x =
        printfn "%A" x

    let pipePrint x =
        print x; x

    //lambda to SKIexp
    //
    //      T[ ] may be defined as follows:
    //
    //    1.  T[x] => x
    //    2.  T[(E₁ E₂)] => (T[E₁] T[E₂])
    //    3.  T[λx.E] => (K T[E]) (if x does not occur free in E)
    //    4.  T[λx.x] => I
    //    5.  T[λx.λy.E] => T[λx.T[λy.E]] (if x occurs free in E)
    //    6.  T[λx.(E₁ E₂)] => (S T[λx.E₁] T[λx.E₂]) (if x occurs free in E₁ or E₂)
    //      
    //      T[λx.λy.(y x)]
    //      = T[λx.T[λy.(y x)]] (by 5)
    //      = T[λx.(S T[λy.y] T[λy.x])] (by 6)
    //      = T[λx.(S I T[λy.x])] (by 4)
    //      = T[λx.(S I (K T[x]))] (by 3)
    //      = T[λx.(S I (K x))] (by 1)
    //      = (S T[λx.(S I)] T[λx.(K x)]) (by 6)
    //      = (S (K (S I)) T[λx.(K x)]) (by 3)
    //      = (S (K (S I)) (S T[λx.K] T[λx.x])) (by 6)
    //      = (S (K (S I)) (S (K K) T[λx.x])) (by 3)
    //      = (S (K (S I)) (S (K K) I)) (by 4)
    //
    //
    //      If we apply this combinatorial term to any two terms x and y 
    //      (by feeding them in a queue-like fashion into the combinator 'from the right'),
    //      it reduces as follows:
    //      
    //      (S (K (S I)) (S (K K) I) x y)
    //      = (K (S I) x (S (K K) I x) y)
    //      = (S I (S (K K) I x) y)
    //      = (I y (S (K K) I x y))
    //      = (y (S (K K) I x y))
    //      = (y (K K x (I x) y))
    //      = (y (K (I x) y))
    //      = (y (I x))
    //      = (y x)

    //SKI type definition

    type SKIexp =
        //| Fn of BuiltinFun
        | S
        | K
        | I
        | Tree of SKIexp * SKIexp


    // Lambda type definition

    type lambda =
      | Variable of string
      | Application of lambda * lambda
      | Abstraction of string * lambda


    // need an intermediate representation with both lambda and SKI terms based on rules above 

    type inter = 
        | SInter
        | KInter
        | IInter
        //| TreeInter of inter * inter
        | VariableInter of string
        | ApplicationInter of inter * inter
        | AbstractionInter of string * inter



    // need to destinguish betwen free variables and bound variables (rules 5 and 6 above)

    ///Determine if a variable is free in an expression
    let isFree (var:string) (exp: inter): bool =
        let rec free (exp: inter): string List =
            match exp with
            | VariableInter x 
                -> [x]
            | ApplicationInter (exp1, exp2) //| TreeInter (exp1, exp2) 
                -> free exp1 @ free exp2
            | AbstractionInter (name, exp1)
                -> List.filter (fun x -> x <> name) (free exp1)
            | _ 
                -> []

        List.contains var (free exp)

    // recursively turn a lamda into a intermedaite expression and simplify
    /// Simplifies intermediate expressions
    let rec translate (exp: inter): inter =
        match exp with
        //    1.  T[x] => x
        | VariableInter x
            -> VariableInter x

        //    2.  T[(E₁ E₂)] => (T[E₁] T[E₂])
        | ApplicationInter (exp1, exp2)
            -> ApplicationInter (translate exp1, translate exp2)
            
        //    3.  T[λx.E] => (K T[E]) (if x does not occur free in E)
        | AbstractionInter (name, exp1) when not (isFree name exp1)
            -> ApplicationInter (KInter, translate exp1)

        //    4.  T[λx.x] => I
        | AbstractionInter (name, VariableInter exp1) when name = exp1
            -> IInter

        //    5.  T[λx.λy.E] => T[λx.T[λy.E]] (if x occurs free in E)
        | AbstractionInter (name1, AbstractionInter (name2, exp1)) when isFree name1 exp1
            -> translate (AbstractionInter (name1, translate (AbstractionInter (name2, exp1))))

        //    6.  T[λx.(E₁ E₂)] => (S T[λx.E₁] T[λx.E₂]) (if x occurs free in E₁ or E₂)
        | AbstractionInter (name, ApplicationInter (exp1, exp2)) when isFree name exp1 || isFree name exp2
            -> ApplicationInter (ApplicationInter (SInter, translate (AbstractionInter (name, exp1))), translate (AbstractionInter (name, exp2)))
        // remaining cases
        | SInter 
            ->SInter
        | KInter
            -> KInter
        | IInter 
            -> IInter
        | _ 
            -> failwith "Error when simplifying intermediate language"


    let rec LamToInter (exp: lambda): inter =
        match exp with
        | Variable x
            -> VariableInter x
        | Application (exp1, exp2)
            -> ApplicationInter ( LamToInter (exp1), LamToInter (exp2))
        | Abstraction (name, exp1)
            -> AbstractionInter (name, LamToInter (exp1))

    let rec InterToSki (exp: inter): SKIexp =
        match exp with
        | SInter 
            -> S
        | KInter 
            -> K
        | IInter
            -> I
        | ApplicationInter (exp1, exp2)
            -> Tree (InterToSki exp1, InterToSki exp2)
        //| TreeInter (exp1, exp2)
        //    -> Tree (InterToSki exp1, InterToSki exp2)
        | VariableInter _ 
            -> failwith "Error: variable found in final intermediate representation"
        | AbstractionInter _ 
            -> failwith "Error: abstraction found in final intermediate representation"



    ///Turns a Lambda Expression into an SKI expression
    let LamToSKI (exp: lambda): SKIexp =
        exp
        //|> pipePrint
        |> LamToInter
        //|> pipePrint
        |> translate
        //|> pipePrint
        |> InterToSki


    //evaluate/simplify SKI exp
    ///SKI combinator reduction
    let rec interpret (exp:SKIexp) :SKIexp =
        match exp with
        | S | K | I 
            -> exp
        | Tree(I, x) 
            -> interpret x
        | Tree(Tree (K, x), y) 
            -> interpret x
        | Tree (Tree (Tree (S, x), y), z) 
            -> interpret (Tree (Tree (x, z), Tree (y, z)))
        | Tree (exp1, exp2)             
            ->
              let exp1' = interpret exp1
              let exp2' = interpret exp2
              if exp1 = exp1' && exp2 = exp2'
              then Tree (exp1, exp2)
              else interpret (Tree (exp1', exp2'))

    ///SKI combinator on input lambda expression
    let LambdaToSimplifiedSKI (exp:lambda) :SKIexp =
        exp
        |> LamToSKI
        |> interpret

    ///Remove Tree () from SKIexp
    let rec prettyFormat (input: SKIexp): string = 
        match input with
        | Tree (exp1, exp2)
            -> "(" + prettyFormat exp1 + " " + prettyFormat exp2 + ")"
        | S
            -> "S"
        | K
            -> "K"
        | I
            -> "I"


    [<Tests>]
    let LambdaSkiTest1 =
        testCase "Lambda to SKI 1" <| fun () ->
            let expected = I
            Expect.equal ( Abstraction ("x", Variable "x") |> LambdaToSimplifiedSKI ) expected (prettyFormat expected)

    [<Tests>]
    let LambdaSkiTest2 =
        testCase "Lambda to SKI 2" <| fun () ->
            let expected = Tree (Tree (S,Tree (K,Tree (S,I))),Tree (Tree (S,Tree (K,K)),I))
            Expect.equal ( Abstraction ("x", Abstraction ("y", Application (Variable "y", Variable "x"))) |> LambdaToSimplifiedSKI ) expected (prettyFormat expected)

    [<Tests>]
    let LambdaSkiTest3 =
        testCase "Lambda to SKI 3" <| fun () ->
            let expected = I
            Expect.equal ( Application (Abstraction ("x", Variable "x"), Abstraction ("y", Variable "y")) |> LambdaToSimplifiedSKI ) expected (prettyFormat expected)

            

    [<EntryPoint>]
    let main argv =
        
        runTestsInAssembly defaultConfig [||] |> ignore

        0 // return an integer exit code
    