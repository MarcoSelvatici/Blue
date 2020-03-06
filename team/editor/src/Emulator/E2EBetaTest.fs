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
    "ListMap library function", "let listMap f lst = 
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
            SeqExp (Literal (IntLit 3),SeqExp (Literal (IntLit 4),SeqExp (Null,Null)))))

]
  
