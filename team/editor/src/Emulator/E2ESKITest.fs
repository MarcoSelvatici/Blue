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
    //////////////////////////////////////////
    /// IF THEN ELSE BUG STILL TO BE FIXED ///
    //////////////////////////////////////////
     
    // "ListMap library function", "let listMap f lst = 
    //                             if size lst == 0
    //                             then []
    //                             else append (f (head lst))
    //                                  (listMap f (tail lst))
    //                             fi
    //                           in 
    //                           listMap (\x.x+1)[1, 2, 3]
    //                           ni",
    // Ok (FuncApp (Combinator K, Literal (IntLit 42)));
    // "ListReverse ", "let listReverse lst = 
    //                     let reverser lst revlst  = 
    //                       if size lst == 0
    //                       then revlst
    //                       else reverser (tail lst) (append (head lst) revlst)
    //                       fi
    //                     in
    //                       reverser lst []
    //                     ni
    //                   in
    //                     listReverse [\"a\", \"bcd\", \"e\"]
    //                   ni",
    // Ok
    //   (SeqExp
    //      (Literal (StringLit "e"),
    //       SeqExp
    //         (Literal (StringLit "bcd"),
    //             SeqExp (Literal (StringLit "a"),SeqExp (Null,Null)))));
]
  
