module E2EBetaTest

open SharedTypes

let testCasesBetaE2E : (string * string * Result<Ast, SharedTypes.ErrorT>) list = [
    "Small program", "\x.\y. y x",
    Ok
      (LambdaExp
         { LambdaParam = "x"
           LambdaBody =
                       LambdaExp
                         { LambdaParam = "y"
                           LambdaBody = FuncApp (Identifier "y", Identifier "x") } });
    "simple addition", "let x = 10 in 
           x + x 
         ni",
    Ok (Literal (IntLit 20));
    "Curried Lambda", "\xy.42",
    Ok (LambdaExp { LambdaParam = "xy"
                    LambdaBody = Literal (IntLit 42) });
    "ListMap add ", "let listMap f lst = 
                      if size lst == 0
                      then []
                      else append (f (head lst))
                           (listMap f (tail lst))
                      fi
                    in 
                    listMap (\x.x+1)[1, 2, 3]
                    ni",
    Ok (SeqExp
            (Literal (IntLit 2),
            SeqExp (Literal (IntLit 3),SeqExp (Literal (IntLit 4),SeqExp (Null,Null)))));
    "ListMap mult ", "let listMap f lst = 
                        if size lst == 0
                        then []
                        else append (f (head lst))
                             (listMap f (tail lst))
                        fi
                      in 
                      listMap (\x.x*2)[3, 5, 32]
                      ni",
    Ok (SeqExp
            (Literal (IntLit 6),
            SeqExp (Literal (IntLit 10),SeqExp (Literal (IntLit 64),SeqExp (Null,Null)))));
    "ListMap div ", "let listMap f lst = 
                        if size lst == 0
                        then []
                        else append (f (head lst))
                             (listMap f (tail lst))
                        fi
                      in 
                      listMap (\x.x/2)[10, 2, 84]
                      ni",
    Ok (SeqExp
            (Literal (IntLit 5),
            SeqExp (Literal (IntLit 1),SeqExp (Literal (IntLit 42),SeqExp (Null,Null)))))
    "ListMap sub ", "let listMap f lst = 
                        if size lst == 0
                        then []
                        else append (f (head lst))
                             (listMap f (tail lst))
                        fi
                      in 
                      listMap (\x.x-2)[2, 1, 11]
                      ni",
    Ok (SeqExp
            (Literal (IntLit 0),
            SeqExp (Literal (IntLit -1),SeqExp (Literal (IntLit 9),SeqExp (Null,Null)))));
    "ListFold add ", "let listFold f acc lst = 
                        if size lst == 0
                        then acc
                        else listFold f (f acc (head lst)) (tail lst)
                        fi
                      in
                          listFold(\x y.x+y) 0 [10, 2, 12]
                      ni",
    Ok  (Literal (IntLit 24));
    "ListFold sub ", "let listFold f acc lst = 
                        if size lst == 0
                        then acc
                        else listFold f (f acc (head lst)) (tail lst)
                        fi
                      in
                          listFold(\x y.x-y) 36 [10, 2, 12]
                      ni",
    Ok  (Literal (IntLit 12));
    "ListFold mul ", "let listFold f acc lst = 
                        if size lst == 0
                        then acc
                        else listFold f (f acc (head lst)) (tail lst)
                        fi
                      in
                          listFold(\x y.x*y) 1 [5, 2, 14]
                      ni",
    Ok  (Literal (IntLit 140));
    "ListFold div ", "let listFold f acc lst = 
                        if size lst == 0
                        then acc
                        else listFold f (f acc (head lst)) (tail lst)
                        fi
                      in
                          listFold(\x y.x/y) 120 [4, 3, 2]
                      ni",
    Ok  (Literal (IntLit 5));

]
  
