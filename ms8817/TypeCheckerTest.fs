// Author: ms8817 (Marco Selvatici)

module TypeCheckerTest

open TokeniserStub
open Parser
open TypeChecker

let testCasesTypeChecker = [
    "Simple Int", Literal (IntLit 1),
        Ok (Base Int);
    "Simple Identifer", Identifier "x",
        Error "Identifier x is not bound";
    "Simple if", IfExp (Literal (BoolLit true), Literal (IntLit 1), Literal (IntLit 2)),
        Ok (Base Int);
    "Simple if mismatch", IfExp (Literal (BoolLit true), Literal (StringLit "a"), Literal (IntLit 2)),
        Error "Types Base String and Base Int are not compatable";
    "Simple if non-bool", IfExp (Literal (IntLit 1), Literal (StringLit "a"), Literal (StringLit "b")),
        Error "Types Base Int and Base Bool are not compatable";
    "Nested if", IfExp (Literal (BoolLit false), IfExp (Literal (BoolLit true), Literal (StringLit "x"), Literal (StringLit "y")), Literal (StringLit "a")),
        Ok (Base String);
    "Nested if mismatch", IfExp (Literal (BoolLit false), IfExp (Literal (BoolLit true), Literal (IntLit 3), Literal (IntLit 4)), Literal (StringLit "a")),
        Error "Types Base Int and Base String are not compatable";
    "Plus", FuncApp (FuncApp (BuiltInFunc Plus, Literal (IntLit 2)), Literal (IntLit 3)),
        Ok (Base Int);
    "Plus mismatch", FuncApp (FuncApp (BuiltInFunc Plus, Literal (IntLit 2)), Literal (BoolLit true)),
        Error "Types Base Int and Base Bool are not compatable";
    "Plus bools", FuncApp (FuncApp (BuiltInFunc Plus, Literal (BoolLit false)), Literal (BoolLit true)),
        Error "Types Base Int and Base Bool are not compatable";
    "Greater than", FuncApp (FuncApp (BuiltInFunc Greater, Literal (IntLit 2)), Literal (IntLit 3)),
        Ok (Base Bool);
    "Less than mismatch", FuncApp (FuncApp (BuiltInFunc Less, Literal (BoolLit false)), Literal (IntLit 3)),
        Error "Types Base Int and Base Bool are not compatable";
    "Logical and", FuncApp (FuncApp (BuiltInFunc And, Literal (BoolLit false)), Literal (BoolLit true)),
        Ok (Base Bool);
    "Logical or mismatch", FuncApp (FuncApp (BuiltInFunc Or, Literal (BoolLit false)), Literal (IntLit 3)),
        Error "Types Base Bool and Base Int are not compatable";
    "Complex if exp `if 2<3 then 2-3 else 2/3`", IfExp (FuncApp (FuncApp (BuiltInFunc Less, Literal (IntLit 2)), Literal (IntLit 3)), FuncApp (FuncApp (BuiltInFunc Minus, Literal (IntLit 2)), Literal (IntLit 3)), FuncApp (FuncApp (BuiltInFunc Div, Literal (IntLit 2)), Literal (IntLit 3))),
        Ok (Base Int);
    "Simple lambda", buildLambda "x" (Identifier "x"),
        Ok (Fun(Gen 0, Gen 0));
    "Simple lambda int", buildLambda "x" (Literal (IntLit 1)),
        Ok (Fun(Gen 0, Base Int));
    "Simple lambda int plus", buildLambda "x" (FuncApp (FuncApp (BuiltInFunc Plus, Identifier "x"), Literal (IntLit 3))),
        Ok (Fun(Base Int, Base Int));
    "Simple lambda plus", buildLambda "x" (FuncApp (FuncApp (BuiltInFunc Plus, Identifier "x"), Identifier "x")),
        Ok (Fun(Base Int, Base Int));
    "Simple lambda unbound", buildLambda "x" (FuncApp (FuncApp (BuiltInFunc Plus, Identifier "x"), Identifier "y")),
        Error "Identifier y is not bound";
    "Nested lambdas", buildLambda "x" (buildLambda "y" (Identifier "y")),
        Ok (Fun(Gen 0, Fun(Gen 1, Gen 1)));
    "Simple partial application (\x. \y. y*x) 2", FuncApp (buildLambda "x" (buildLambda "y" (FuncApp (FuncApp (BuiltInFunc Mult, Identifier "y"), Identifier "x"))), Literal(IntLit 2)),
        Ok (Fun(Base Int, Base Int));
    "Simple partial application bool (\x. \y. y<x) 2", FuncApp (buildLambda "x" (buildLambda "y" (FuncApp (FuncApp (BuiltInFunc Less, Identifier "y"), Identifier "x"))), Literal(IntLit 2)),
        Ok (Fun(Base Int, Base Bool));
    "Simple partial application mismatch (\x. \y. y && x) 2", FuncApp (buildLambda "x" (buildLambda "y" (FuncApp (FuncApp (BuiltInFunc And, Identifier "y"), Identifier "x"))), Literal(IntLit 2)),
        Error "Types Base Bool and Base Int are not compatable";
    "Simple partial application not bound (\x. \y. y*x) x", FuncApp (buildLambda "x" (buildLambda "y" (FuncApp (FuncApp (BuiltInFunc Mult, Identifier "y"), Identifier "x"))), Identifier "x"),
        Error "Identifier x is not bound";
    "Simple `let x = 1 in x`", FuncDefExp {FuncName="x"; FuncBody=Literal (IntLit 1); Rest=Identifier "x"},
        Ok (Base Int);
    "Partially applied `let x y = y in x`", buildCarriedFunc ["x"; "y"] (Identifier "y") (Identifier "x"),
        Ok (Fun(Gen 0, Gen 0));
    "Partially applied int `let x y = y + 1 in x`", buildCarriedFunc ["x"; "y"] (FuncApp (FuncApp (BuiltInFunc Plus, Identifier "y"), Literal (IntLit 1))) (Identifier "x"),
        Ok (Fun(Base Int, Base Int));
    "Fully applied int `let x y = y + 1 in x 4`", buildCarriedFunc ["x"; "y"] (FuncApp (FuncApp (BuiltInFunc Plus, Identifier "y"), Literal (IntLit 1))) (FuncApp (Identifier "x", Literal (IntLit 4))),
        Ok (Base Int);
    "Scoping `let x y = y in y`", buildCarriedFunc ["x"; "y"] (Identifier "y") (Identifier "y"),
        Error "Identifier y is not bound";
    "Lambda as input `let x y = y 1 in x \x.x+1`", buildCarriedFunc ["x"; "y"] (FuncApp (Identifier "y", Literal (IntLit 1))) (FuncApp (Identifier "x", buildLambda "x" (FuncApp (FuncApp (BuiltInFunc Plus, Identifier "x"), Literal (IntLit 1))))),
        Ok (Base Int);
    "Different types `let f = \x.x in let g = f True in f 3`", buildCarriedFunc ["f"] (buildLambda "x" (Identifier "x")) (buildCarriedFunc ["g"] (FuncApp (Identifier "f", Literal (BoolLit true))) (FuncApp (Identifier "f", Literal (IntLit 3)))),
        Ok (Base Int);
    "No recursion `let x = x x in 1`", buildCarriedFunc ["x"] (FuncApp (Identifier "x", Identifier "x")) (Literal (IntLit 1)),
        Error "Identifier x is not bound"; // Recursion is not supported (yet).
    "Simple StrEq", BuiltInFunc StrEq,
        Ok (Fun(Base String, Fun (Base String, Base Bool)));
    "Partially applied StrEq", FuncApp (BuiltInFunc StrEq, Literal (StringLit "hello")),
        Ok (Fun (Base String, Base Bool));
    "String equality in ifExp", IfExp ( FuncApp (FuncApp (BuiltInFunc StrEq, Literal (StringLit "s1")), Literal (StringLit "s2")), Literal (IntLit 1), Literal (IntLit 1)),
        Ok (Base Int);
    "Empty SeqExp", SeqExp (Null, Null),
        Ok (Pair (Base NullType, Base NullType));
    "Simple SeqExp", SeqExp (Literal (IntLit 1), Null),
        Ok (Pair (Base Int, Base NullType));
    "Simple SeqExp with base types `[1 , 'hello', true]`", SeqExp (Literal (IntLit 1), SeqExp (Literal (StringLit "hello"), SeqExp (Literal (BoolLit true), Null))),
        Ok (Pair (Base Int, Pair (Base String, Pair (Base Bool, Base NullType))));
    "Simple SeqExp with Lambda `[1 , \x.x]`", SeqExp (Literal (IntLit 1), SeqExp (buildLambda "x" (Identifier "x"), Null)),
        Ok (Pair (Base Int, Pair (Fun (Gen 0, Gen 0), Base NullType)));
    "Unique ids `[\x.x, \y.y]`", SeqExp (buildLambda "y" (Identifier "y"), SeqExp (buildLambda "x" (Identifier "x"), Null)),
        Ok (Pair (Fun (Gen 0, Gen 0), Pair ( Fun (Gen 1, Gen 1), Base NullType)));
    "Simple Head", BuiltInFunc Head,
        Ok (Fun (Pair (Gen 0, Gen 1), Gen 0));
    "Applied Head `head [1]`", FuncApp (BuiltInFunc Head, SeqExp (Literal (IntLit 4), Null)), 
        Ok (Base Int);
    "Applied Head `head ['hello', 1, true]`", FuncApp (BuiltInFunc Head, SeqExp (Literal (StringLit "hello"), SeqExp (Literal (IntLit 1), SeqExp (Literal (BoolLit true), Null)))), 
        Ok (Base String);
    "Applied Head `head [\x.x<1, 1, true]`", FuncApp (BuiltInFunc Head, SeqExp (buildLambda "x" (FuncApp (FuncApp (BuiltInFunc Less, Identifier "x"), Literal (IntLit 1))), SeqExp (Literal (IntLit 1), SeqExp (Literal (BoolLit true), Null)))), 
        Ok (Fun (Base Int, Base Bool));
    "Simple Tail", BuiltInFunc Tail,
        Ok (Fun (Pair (Gen 0, Gen 1), Gen 1));
    "Applied Tail `tail [1]`", FuncApp (BuiltInFunc Tail, SeqExp (Literal (IntLit 4), Null)), 
        Ok (Base NullType);
    "Applied Tail `tail ['hello', 1, true]`", FuncApp (BuiltInFunc Tail, SeqExp (Literal (StringLit "hello"), SeqExp (Literal (IntLit 1), SeqExp (Literal (BoolLit true), Null)))), 
        Ok (Pair (Base Int, Pair (Base Bool, Base NullType)));
    "Applied Tail `tail [1, \x.x<1]`", FuncApp (BuiltInFunc Tail, SeqExp (Literal (IntLit 1), SeqExp (buildLambda "x" (FuncApp (FuncApp (BuiltInFunc Less, Identifier "x"), Literal (IntLit 1))), Null))),
        Ok (Pair (Fun (Base Int, Base Bool), Base NullType));
    "Simple Size", BuiltInFunc Size,
        Ok (Fun (Pair (Gen 0, Gen 1), Base Int));
    "Applied Size `size [1]`", FuncApp (BuiltInFunc Size, SeqExp (Literal (IntLit 4), Null)), 
        Ok (Base Int);
    "Applied Size `tail ['hello', 1, true]`", FuncApp (BuiltInFunc Size, SeqExp (Literal (StringLit "hello"), SeqExp (Literal (IntLit 1), SeqExp (Literal (BoolLit true), Null)))), 
        Ok (Base Int);
    "Simple Append", BuiltInFunc Append,
        Ok (Fun (Gen 2, Fun (Pair (Gen 0, Gen 1), Pair (Gen 2, Pair (Gen 0, Gen 1)))));
    "Applied Append `append true [1]`", FuncApp (FuncApp (BuiltInFunc Append, Literal (BoolLit true)), SeqExp (Literal (IntLit 4), Null)), 
        Ok (Pair (Base Bool, Pair (Base Int, Base NullType)));
]
