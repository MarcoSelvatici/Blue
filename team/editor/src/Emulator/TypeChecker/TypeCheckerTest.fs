// Author: ms8817 (Marco Selvatici)

module TypeCheckerTest

open SharedTypes
open Parser
open TypeChecker

let buildTypeCheckerError err = Error <| TypeCheckerError err

let testCasesTypeChecker = [
    "Simple Int", Literal (IntLit 1),
        Ok (Base Int);
    "Simple Identifer", Identifier "x",
        buildTypeCheckerError "Identifier x is not bound";
    "Simple if", IfExp (Literal (BoolLit true), Literal (IntLit 1), Literal (IntLit 2)),
        Ok (Base Int);
    "Simple if mismatch", IfExp (Literal (BoolLit true), Literal (StringLit "a"), Literal (IntLit 2)),
        buildTypeCheckerError "Types String and Int are not compatable";
    "Simple if non-bool", IfExp (Literal (IntLit 1), Literal (StringLit "a"), Literal (StringLit "b")),
        buildTypeCheckerError "Types Int and Bool are not compatable";
    "Nested if", IfExp (Literal (BoolLit false), IfExp (Literal (BoolLit true), Literal (StringLit "x"), Literal (StringLit "y")), Literal (StringLit "a")),
        Ok (Base String);
    "Nested if mismatch", IfExp (Literal (BoolLit false), IfExp (Literal (BoolLit true), Literal (IntLit 3), Literal (IntLit 4)), Literal (StringLit "a")),
        buildTypeCheckerError "Types Int and String are not compatable";
    "Plus", FuncApp (FuncApp (BuiltInFunc Plus, Literal (IntLit 2)), Literal (IntLit 3)),
        Ok (Base Int);
    "Plus mismatch", FuncApp (FuncApp (BuiltInFunc Plus, Literal (IntLit 2)), Literal (BoolLit true)),
        buildTypeCheckerError "Types Int and Bool are not compatable";
    "Plus bools", FuncApp (FuncApp (BuiltInFunc Plus, Literal (BoolLit false)), Literal (BoolLit true)),
        buildTypeCheckerError "Types Int and Bool are not compatable";
    "Greater than", FuncApp (FuncApp (BuiltInFunc Greater, Literal (IntLit 2)), Literal (IntLit 3)),
        Ok (Base Bool);
    "Less than mismatch", FuncApp (FuncApp (BuiltInFunc Less, Literal (BoolLit false)), Literal (IntLit 3)),
        buildTypeCheckerError "Types Int and Bool are not compatable";
    "Logical and", FuncApp (FuncApp (BuiltInFunc And, Literal (BoolLit false)), Literal (BoolLit true)),
        Ok (Base Bool);
    "Logical or mismatch", FuncApp (FuncApp (BuiltInFunc Or, Literal (BoolLit false)), Literal (IntLit 3)),
        buildTypeCheckerError "Types Bool and Int are not compatable";
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
        buildTypeCheckerError "Identifier y is not bound";
    "Nested lambdas", buildLambda "x" (buildLambda "y" (Identifier "y")),
        Ok (Fun(Gen 0, Fun(Gen 1, Gen 1)));
    "Simple partial application (\x. \y. y*x) 2", FuncApp (buildLambda "x" (buildLambda "y" (FuncApp (FuncApp (BuiltInFunc Mult, Identifier "y"), Identifier "x"))), Literal(IntLit 2)),
        Ok (Fun(Base Int, Base Int));
    "Simple partial application bool (\x. \y. y<x) 2", FuncApp (buildLambda "x" (buildLambda "y" (FuncApp (FuncApp (BuiltInFunc Less, Identifier "y"), Identifier "x"))), Literal(IntLit 2)),
        Ok (Fun(Base Int, Base Bool));
    "Simple partial application mismatch (\x. \y. y && x) 2", FuncApp (buildLambda "x" (buildLambda "y" (FuncApp (FuncApp (BuiltInFunc And, Identifier "y"), Identifier "x"))), Literal(IntLit 2)),
        buildTypeCheckerError "Types Bool and Int are not compatable";
    "Simple partial application not bound (\x. \y. y*x) x", FuncApp (buildLambda "x" (buildLambda "y" (FuncApp (FuncApp (BuiltInFunc Mult, Identifier "y"), Identifier "x"))), Identifier "x"),
        buildTypeCheckerError "Identifier x is not bound";
    "Simple `let x = 1 in x`", FuncDefExp {FuncName="x"; FuncBody=Literal (IntLit 1); Rest=Identifier "x"},
        Ok (Base Int);
    "Partially applied `let x y = y in x`", buildCarriedFunc ["x"; "y"] (Identifier "y") (Identifier "x"),
        Ok (Fun(Gen 1, Gen 1));
    "Partially applied int `let x y = y + 1 in x`", buildCarriedFunc ["x"; "y"] (FuncApp (FuncApp (BuiltInFunc Plus, Identifier "y"), Literal (IntLit 1))) (Identifier "x"),
        Ok (Fun(Base Int, Base Int));
    "Fully applied int `let x y = y + 1 in x 4`", buildCarriedFunc ["x"; "y"] (FuncApp (FuncApp (BuiltInFunc Plus, Identifier "y"), Literal (IntLit 1))) (FuncApp (Identifier "x", Literal (IntLit 4))),
        Ok (Base Int);
    "Scoping `let x y = y in y`", buildCarriedFunc ["x"; "y"] (Identifier "y") (Identifier "y"),
        buildTypeCheckerError "Identifier y is not bound";
    "Lambda as input `let x y = y 1 in x \x.x+1`", buildCarriedFunc ["x"; "y"] (FuncApp (Identifier "y", Literal (IntLit 1))) (FuncApp (Identifier "x", buildLambda "x" (FuncApp (FuncApp (BuiltInFunc Plus, Identifier "x"), Literal (IntLit 1))))),
        Ok (Base Int);
    "Different types `let f = \x.x in let g = f True in f 3`", buildCarriedFunc ["f"] (buildLambda "x" (Identifier "x")) (buildCarriedFunc ["g"] (FuncApp (Identifier "f", Literal (BoolLit true))) (FuncApp (Identifier "f", Literal (IntLit 3)))),
        Ok (Base Int);
    "Simple StrEq", BuiltInFunc StrEq,
        Ok (Fun(Base String, Fun (Base String, Base Bool)));
    "Partially applied StrEq", FuncApp (BuiltInFunc StrEq, Literal (StringLit "hello")),
        Ok (Fun (Base String, Base Bool));
    "String equality in ifExp", IfExp ( FuncApp (FuncApp (BuiltInFunc StrEq, Literal (StringLit "s1")), Literal (StringLit "s2")), Literal (IntLit 1), Literal (IntLit 1)),
        Ok (Base Int);
    "Empty SeqExp", EmptySeq,
        Ok (Pair (Gen 0, Gen 1));
    "Simple SeqExp", SeqExp (Literal (IntLit 1), EmptySeq),
        Ok (Pair (Base Int, Pair (Gen 0, Gen 1)));
    "Simple SeqExp with base types `[1 , 'hello', true]`", SeqExp (Literal (IntLit 1), SeqExp (Literal (StringLit "hello"), SeqExp (Literal (BoolLit true), EmptySeq))),
        Ok (Pair (Base Int, Pair (Base String, Pair (Base Bool, Pair (Gen 0, Gen 1)))));
    "Simple SeqExp with Lambda `[1 , \x.x]`", SeqExp (Literal (IntLit 1), SeqExp (buildLambda "x" (Identifier "x"), EmptySeq)),
        Ok (Pair (Base Int, Pair (Fun (Gen 0, Gen 0), Pair (Gen 1, Gen 2))));
    "Unique ids `[\x.x, \y.y]`", SeqExp (buildLambda "y" (Identifier "y"), SeqExp (buildLambda "x" (Identifier "x"), EmptySeq)),
        Ok (Pair (Fun (Gen 0, Gen 0), Pair ( Fun (Gen 1, Gen 1), Pair (Gen 2, Gen 3))));
    "Simple Head", BuiltInFunc Head,
        Ok (Fun (Pair (Gen 0, Gen 1), Gen 0));
    "Applied Head `head [1]`", FuncApp (BuiltInFunc Head, SeqExp (Literal (IntLit 4), EmptySeq)), 
        Ok (Base Int);
    "Applied Head `head ['hello', 1, true]`", FuncApp (BuiltInFunc Head, SeqExp (Literal (StringLit "hello"), SeqExp (Literal (IntLit 1), SeqExp (Literal (BoolLit true), EmptySeq)))), 
        Ok (Base String);
    "Applied Head `head [\x.x<1, 1, true]`", FuncApp (BuiltInFunc Head, SeqExp (buildLambda "x" (FuncApp (FuncApp (BuiltInFunc Less, Identifier "x"), Literal (IntLit 1))), SeqExp (Literal (IntLit 1), SeqExp (Literal (BoolLit true), EmptySeq)))), 
        Ok (Fun (Base Int, Base Bool));
    "Simple Tail", BuiltInFunc Tail,
        Ok (Fun (Pair (Gen 0, Gen 1), Gen 1));
    "Applied Tail `tail [1]`", FuncApp (BuiltInFunc Tail, SeqExp (Literal (IntLit 4), EmptySeq)), 
        Ok (Pair (Gen 3, Gen 4));
    "Applied Tail `tail ['hello', 1, true]`", FuncApp (BuiltInFunc Tail, SeqExp (Literal (StringLit "hello"), SeqExp (Literal (IntLit 1), SeqExp (Literal (BoolLit true), EmptySeq)))), 
        Ok (Pair (Base Int, Pair (Base Bool, Pair (Gen 3, Gen 4))));
    "Applied Tail `tail [1, \x.x<1]`", FuncApp (BuiltInFunc Tail, SeqExp (Literal (IntLit 1), SeqExp (buildLambda "x" (FuncApp (FuncApp (BuiltInFunc Less, Identifier "x"), Literal (IntLit 1))), EmptySeq))),
        Ok (Pair (Fun (Base Int, Base Bool), Pair (Gen 6, Gen 7)));
    "Simple Size", BuiltInFunc Size,
        Ok (Fun (Pair (Gen 0, Gen 1), Base Int));
    "Applied Size `size [1]`", FuncApp (BuiltInFunc Size, SeqExp (Literal (IntLit 4), EmptySeq)), 
        Ok (Base Int);
    "Applied Size `size ['hello', 1, true]`", FuncApp (BuiltInFunc Size, SeqExp (Literal (StringLit "hello"), SeqExp (Literal (IntLit 1), SeqExp (Literal (BoolLit true), EmptySeq)))), 
        Ok (Base Int);
    "Simple Append", BuiltInFunc Append,
        Ok (Fun (Gen 2, Fun (Pair (Gen 0, Gen 1), Pair (Gen 2, Pair (Gen 0, Gen 1)))));
    "Applied Append to empty `append 1 []`", FuncApp (FuncApp (BuiltInFunc Append, Literal (IntLit 1)), EmptySeq), 
        Ok (Pair (Base Int, Pair (Gen 2, Gen 3)));
    "Applied Append to empty `append 2 (append 1 [])`",FuncApp (FuncApp (BuiltInFunc Append, Literal (IntLit 2)), FuncApp (FuncApp (BuiltInFunc Append, Literal (IntLit 1)), EmptySeq)), 
        Ok (Pair(Base Int, Pair (Base Int, Pair (Gen 7, Gen 8))));
    "Applied Append `append true [1]`", FuncApp (FuncApp (BuiltInFunc Append, Literal (BoolLit true)), SeqExp (Literal (IntLit 4), EmptySeq)), 
        Ok (Pair (Base Bool, Pair (Base Int, Pair (Gen 5, Gen 6))));
    "Head not list `head 1`", FuncApp (BuiltInFunc Head, Literal (IntLit 1)),
        buildTypeCheckerError "Types seq and Int are not compatable";
    "Tail not list `tail \x.x`", FuncApp (BuiltInFunc Tail, buildLambda "x" (Identifier "x")),
        buildTypeCheckerError "Types seq and 'a -> 'a are not compatable";
    "Size not list `size 4`", FuncApp (BuiltInFunc Head, Literal (IntLit 2)),
        buildTypeCheckerError "Types seq and Int are not compatable";
    "Append not list `append true 1`", FuncApp (FuncApp (BuiltInFunc Append, Literal (BoolLit true)), Literal (IntLit 4)),
        buildTypeCheckerError "Types seq and Int are not compatable";
    "Head of empty `head []`", FuncApp (BuiltInFunc Head, EmptySeq),
        Ok (Gen 1);
    "Tail of empty `tail []`", FuncApp (BuiltInFunc Tail, EmptySeq),
        Ok (Gen 2);
    "Size of empty `tail []`", FuncApp (BuiltInFunc Size, EmptySeq),
        Ok (Base Int);
    "Simple Implode", BuiltInFunc Implode,
        Ok (Fun (Pair (Gen 0, Gen 1), Base String));
    "Simple Explode", BuiltInFunc Explode,
        Ok (Fun (Base String, Pair (Gen 0, Gen 1)));
    "Applied Implode `implode ['a', 'b']`", FuncApp(BuiltInFunc Implode, SeqExp(Literal (StringLit "a"), SeqExp (Literal (StringLit "b"), EmptySeq))),
        Ok (Base String);
    "Applied Explode `explode 'ab'`", FuncApp(BuiltInFunc Explode, Literal (StringLit "ab")),
        Ok (Pair (Gen 1, Gen 2));
    "Simple Test", BuiltInFunc Test,
        Ok (Fun(Gen 0, Base Bool));
    "Applied Test `test \x.x`", FuncApp(BuiltInFunc Test, buildLambda "x" (Identifier "x")),
        Ok (Base Bool);
    "Unify empty list with populated list. `let lst = [1] in if true then [] else lst fi`", buildCarriedFunc ["lst"] (SeqExp(Literal (IntLit 1), EmptySeq)) (IfExp (Literal(BoolLit true), EmptySeq, Identifier "lst")),
        Ok (Pair (Base Int, Pair (Gen 1, Gen 2)));
    "Unused recursion `let x = x x in 1`", buildCarriedFunc ["x"] (FuncApp (Identifier "x", Identifier "x")) (Literal (IntLit 1)),
        Ok (Base Int);
    "Recursion `let x = x x in x`", buildCarriedFunc ["x"] (FuncApp (Identifier "x", Identifier "x")) (Identifier "x"),
        Ok (Gen 1);
    "List.map", (FuncDefExp { FuncName = "listMap"; FuncBody = LambdaExp { LambdaParam = "f"; LambdaBody = LambdaExp { LambdaParam = "lst"; LambdaBody = IfExp (FuncApp (FuncApp (BuiltInFunc Equal, FuncApp (BuiltInFunc Size, Identifier "lst")), Literal (IntLit 0)), SeqExp (Null,Null), FuncApp (FuncApp (BuiltInFunc Append, FuncApp (Identifier "f", FuncApp (BuiltInFunc Head, Identifier "lst"))), FuncApp (FuncApp (Identifier "listMap", Identifier "f"), FuncApp (BuiltInFunc Tail, Identifier "lst")))) } }; Rest = Identifier "listMap" }),
        Ok (Fun(Fun (Gen 22,Gen 8), Fun (Pair (Gen 6,Gen 7),Pair (Gen 8,Pair (Gen 12,Gen 13)))))
    "If then else empty list", IfExp(Literal (BoolLit true), EmptySeq, EmptySeq),
        Ok (Pair(Gen 0, Gen 1));
    "Recursion `let x i = x (tail i) in x ni", FuncDefExp {FuncName = "x"; FuncBody = LambdaExp {LambdaParam = "i"; LambdaBody = FuncApp (Identifier "x",FuncApp (BuiltInFunc Tail, Identifier "i"))}; Rest = (Identifier "x")},
        Ok (Fun (Pair (Gen 4,Gen 5),Gen 2))
    "Simple Print", BuiltInFunc Print,
        Ok (Fun (Gen 0, Gen 0));
    "Complex program with prints everywhere `let x y = print ((print y) (print 1)) in x \x.(print x)+(print 1)`", buildCarriedFunc ["x"; "y"] (FuncApp (BuiltInFunc Print, (FuncApp (FuncApp (BuiltInFunc Print, Identifier "y"), FuncApp (BuiltInFunc Print, Literal (IntLit 1)))))) (FuncApp (FuncApp (BuiltInFunc Print, Identifier "x"), buildLambda "x" (FuncApp (FuncApp (BuiltInFunc Plus, FuncApp (BuiltInFunc Print, Identifier "x")), FuncApp (BuiltInFunc Print, Literal (IntLit 1)))))),
        Ok (Base Int);
    "Head in function `let a lst = (head lst) + 1 in a ni`", buildCarriedFunc ["a"; "lst"] (FuncApp (FuncApp (BuiltInFunc Plus, FuncApp (BuiltInFunc Head, Identifier "lst")), Literal (IntLit 1))) (Identifier "a"),
        Ok (Fun (Pair (Base Int, Gen 6), Base Int));
    "Unary Not", BuiltInFunc Not,
        Ok (Fun (Base Bool, Base Bool));
    "Applied Unary Not", FuncApp (BuiltInFunc Not, Literal (BoolLit true)),
        Ok (Base Bool);
]

let testCasesPrettyPrintType = [
    "Base type int", Base Int,
        "Int";
    "Base type bool", Base Bool,
        "Bool";
    "Base type string", Base String,
        "String";
    "Simple Fun", Fun (Base Int, Base Int),
        "Int -> Int";
    "Double Fun", Fun (Base Int, Fun (Base Bool, Base String)),
        "Int -> Bool -> String";
    "Double Fun Left", Fun (Fun (Base Int, Base Bool), Base String),
        "(Int -> Bool) -> String";
    "Empty pair", Pair (Gen 0, Gen 1),
        "seq";
    "Int List", Pair (Base Int, Pair (Base Int, Pair (Gen 0, Gen 1))),
        "seq";
    "Mixed List", Pair (Fun (Base Int, Fun (Base Bool, Base String)), Pair (Base String, Pair (Gen 0, Gen 1))),
        "seq";
    "Nested List", Pair (Pair (Base Int, Pair (Base Bool, Pair (Gen 0, Gen 1))), Pair (Pair (Base String, Pair (Base String, Pair (Gen 0, Gen 1))), Pair (Gen 0, Gen 1))),
        "seq";
    "Nested Empty", Pair (Pair (Gen 0, Gen 1), Pair (Gen 0, Gen 1)),
        "seq";
    "Simple Gen", Gen 0,
        "'a";
    "Complex Gen `let a lst = (head lst) + 1 in a ni`", Fun (Pair (Base Int, Gen 6), Base Int),
        "seq -> Int";
]
