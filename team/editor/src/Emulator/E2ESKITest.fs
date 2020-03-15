module E2ESKITest

open SharedTypes

let testCasesSKIE2E : (string * string * Result<Ast, SharedTypes.ErrorT>) list = [
    "Small program", "\x.\y. y x",
        Ok (FuncApp (
                FuncApp (
                    Combinator S, 
                    FuncApp ( 
                        Combinator K,
                        FuncApp (
                            Combinator S,
                            Combinator I 
                        ) 
                    ) 
                ),
                FuncApp (
                    FuncApp (
                        Combinator S,
                        FuncApp (
                            Combinator K,
                            Combinator K
                        )
                    ), 
                    Combinator I 
                ) 
        ));
    "simple addition", "let x = 10 in 
           x + x 
         ni",
    Ok (Literal (IntLit 20));
    "Curried Lambda", "\xy.42",
    Ok (FuncApp (Combinator K, Literal (IntLit 42)));
    "StringLength1", "let StringLength str = 
                        size (explode str)
                      in
                        StringLength \"1sdsadsvfe\"
                      ni",
    Ok (Literal (IntLit 10));
    "I combinator", "\x.x",
    Ok (Combinator I);
    "I combinator with value", "(\x.x) 10",
    Ok (Literal (IntLit 10));
    "K combinator", "\x y. y",
    Ok (FuncApp(Combinator K,Combinator I));
    "K combinator 2nd with 1 arg", "(\x y. y) 5",
    Ok (Combinator I);
    "K combinator 2nd with 2 args", "(\x y. y) 1 5",
    Ok (Literal (IntLit 5));
    "Program", "let main =
                let first = (\x y.x) in
                let second = (\x y.y) in
                append \"a\" (append (first (second 1 \"b\") ([1,2,3])) [\"c\"]) ni ni
                in main ni",
    Ok(SeqExp (Literal (StringLit "a"),SeqExp (Literal (StringLit "b"),SeqExp (Literal (StringLit "c"),SeqExp (Null,Null)))));
]
  
