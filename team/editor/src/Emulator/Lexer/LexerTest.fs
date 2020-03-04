module LexerTest

open SharedTypes
open Lexer


let testCasesLexer = [
    "Keywords", ". , () [] \\",
       Ok [KDot; KComma; KOpenRound; KCloseRound; KOpenSquare; KCloseSquare; KLambda];
    "Arithmetic", "a +b* c/ d-e",
        Ok [TIdentifier "a"; TBuiltInFunc Plus; TIdentifier "b"; TBuiltInFunc Mult; TIdentifier "c";
            TBuiltInFunc Div; TIdentifier "d"; TBuiltInFunc Minus; TIdentifier "e"];
    "Comparisons", "a > b<=c>=d== e>f",
        Ok [TIdentifier "a"; TBuiltInFunc Greater; TIdentifier "b"; TBuiltInFunc LessEq;
            TIdentifier "c"; TBuiltInFunc GreaterEq; TIdentifier "d"; TBuiltInFunc Equal;
            TIdentifier "e"; TBuiltInFunc Greater; TIdentifier "f"];
    "Logic", "!a && b & c| d|| e", 
        Ok [TBuiltInFunc Not; TIdentifier "a"; TBuiltInFunc And; TIdentifier "b";
            TBuiltInFunc BitAnd; TIdentifier "c"; TBuiltInFunc BitOr; TIdentifier "d";
            TBuiltInFunc Or; TIdentifier "e"];
    "Number literals", "\\. 9 a 0.9 11.2340239",
        Ok [KLambda; KDot; TLiteral (IntLit 9); TIdentifier "a"; TLiteral (FloatLit 0.9);
            TLiteral (FloatLit 11.2340239)];
    "Failing number literals", " 0.9 2 0. ", 
        Error (LexerError "lexing error, expecting decimal digit after dot on line 0");
    "List functions", "let hd = head lst
                       let tl = tail lst",
        Ok [KLet; TIdentifier "hd"; KEq; TBuiltInFunc Head; TIdentifier "lst"; KLet;
            TIdentifier "tl"; KEq; TBuiltInFunc Tail; TIdentifier "lst"];
    "Row count and comments functionalities", "let hd = head lst
                                               //dfmdsfodpmfdpfmdpfmdsfmdpmfodsfmpdsfmdsomf;;
                                               (* 23-94\\\\\\qwlpewwewewpelq[eweeeeee
                                  
                                               fjdkfj #### sdsd### MULTILINE COMMENT ### 
                                               let i = wow.
                                               *)

                                               let tl_ = tail lst
                                               let hd' = head lst
                                               let _tl = tail lst
                                               let tl_2 = tail lst
                                               let hd_a = head lst
                                               let tl2a = tail lst
                                               let tl
                                                = 
                                                tail lst",
        Ok [KLet; TIdentifier "hd"; KEq; TBuiltInFunc Head; TIdentifier "lst"; KLet;
            TIdentifier "tl_"; KEq; TBuiltInFunc Tail; TIdentifier "lst"; KLet;
            TIdentifier "hd'"; KEq; TBuiltInFunc Head; TIdentifier "lst"; KLet;
            TIdentifier "_tl"; KEq; TBuiltInFunc Tail; TIdentifier "lst"; KLet;
            TIdentifier "tl_2"; KEq; TBuiltInFunc Tail; TIdentifier "lst"; KLet;
            TIdentifier "hd_a"; KEq; TBuiltInFunc Head; TIdentifier "lst"; KLet;
            TIdentifier "tl2a"; KEq; TBuiltInFunc Tail; TIdentifier "lst"; KLet;
            TIdentifier "tl"; KEq; TBuiltInFunc Tail; TIdentifier "lst"];
    "Small program", "let a =
                          let b = \x. x * 2 in
                            \c. b (b c)
                          in
                          a 5",
        Ok [KLet; TIdentifier "a"; KEq; KLet; TIdentifier "b"; KEq; KLambda;
            TIdentifier "x"; KDot; TIdentifier "x"; TBuiltInFunc Mult; TLiteral (IntLit 2);
            KIn; KLambda; TIdentifier "c"; KDot; TIdentifier "b"; KOpenRound;
            TIdentifier "b"; TIdentifier "c"; KCloseRound; KIn; TIdentifier "a";
            TLiteral (IntLit 5)];
    "List builtin functions", "let a = ['a'; 'b']
                                  let b = implode a
                                  let c = \"ab\"
                                  let d = strEq b c
                                  let e = explode c
                                  let f = size e
                                  let rec yo lst = 
                                    let hd = head lst
                                    append hd global
                                    let tl = tail lst
                                    yo tl",
        Ok [KLet; TIdentifier "a"; KEq; KOpenSquare; TLiteral (CharLit 'a'); KSemiColon;
            TLiteral (CharLit 'b'); KCloseSquare; KLet; TIdentifier "b"; KEq;
            TBuiltInFunc Implode; TIdentifier "a"; KLet; TIdentifier "c"; KEq;
            TLiteral (StringLit "ab"); KLet; TIdentifier "d"; KEq; TBuiltInFunc StrEq;
            TIdentifier "b"; TIdentifier "c"; KLet; TIdentifier "e"; KEq;
            TBuiltInFunc Explode; TIdentifier "c"; KLet; TIdentifier "f"; KEq;
            TBuiltInFunc Size; TIdentifier "e"; KLet; KRec; TIdentifier "yo";
            TIdentifier "lst"; KEq; KLet; TIdentifier "hd"; KEq; TBuiltInFunc Head;
            TIdentifier "lst"; TBuiltInFunc Append; TIdentifier "hd"; TIdentifier "global";
            KLet; TIdentifier "tl"; KEq; TBuiltInFunc Tail; TIdentifier "lst";
            TIdentifier "yo"; TIdentifier "tl"];
    "If then else and booleans", "let a = true
                                  if a 
                                  then true 
                                  else false
                                  fi",
        Ok [KLet; TIdentifier "a"; KEq; TLiteral (BoolLit true); KIf; TIdentifier "a";
            KThen; TLiteral (BoolLit true); KElse; TLiteral (BoolLit false); KFi];
    "Failing number literal 2", " 0.9 2 23a ",
        Error (LexerError "lexing error, number contains non numeric char: 'a' on line 0");
    "Failing identifier", " let ab = 2
                            let a_b@ = 4",
        Error (LexerError "lexing error, unrecognised non-alphabetic character: '@' on line: 1");
    "Never closing multiline comment", " let ab = 2
                                         (* asdsapodsa
                                         sadsioajd
                                         let ciao = true
                                         fdsfjspodjf",
        Ok [KLet; TIdentifier "ab"; KEq; TLiteral (IntLit 2)];
    "Wrong esc sequence in string", " let ab = \"dasdjs\n\t\k\"", 
        Error (LexerError "lexing error, expected valid ESC sequence on line 0: \k is not valid");
    "Never closing string", " let ab = \"dasdjs\n\t", 
        Error (LexerError "lexing error, expecting closing quotation mark: '\"' on line 0");
    "Wrong esc sequence in char", " let ab = \'\h\'" ,
        Error (LexerError "lexing error, expected valid ESC sequence on line 0: \h is not valid");
    "Late closing char", " let ab = \'duh\'",
        Error (LexerError "lexing error, expecting closing apostrophe: '\'' on line 0");
    "Unrecognised char", " let $a = 2",
        Error (LexerError "lexing error, unrecognised character '$' on line 0");
    "lambda expression", "(\x.x+10) 12",
        Ok [KOpenRound; KLambda; TIdentifier "x"; KDot; TIdentifier "x"; TBuiltInFunc Plus; 
            TLiteral (IntLit 10); KCloseRound; TLiteral (IntLit 12)];
]
  
