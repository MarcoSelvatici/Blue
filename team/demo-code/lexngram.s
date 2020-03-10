### ATTEMPT TO LEX A SIMPLE LANGUAGE ###
### LEXER (STRING -> STRING LIST)    ###
### MISSING:                         ###
### 1. Match string                  ###
### 2. Match identifier              ###
### 3. Match lambda                  ###

# Library
let listFind f key lst = 
    if size lst == 0
    then false
    else 
        if f (head lst) key
        then true 
        else listFind f key (tail lst)
        fi
    fi
in
let listContain f keys lst =
    if size keys == 0 || size lst == 0
    then false 
    else 
        if (listFind f (head keys) lst)
        then true
        else  listContain f (tail keys) lst
        fi
    fi
in
let listItem idx lst =    
    let looper step idx lst =  
        if size lst == 0
        then "" # undefined behaviour
        else
            if step == idx
            then head lst
            else looper (step + 1) idx (tail lst)
            fi
        fi
    in
        looper 0 idx lst
    ni
in
let listConcat lhs rhs = 
    if size lhs == 0
    then rhs
    else append (head lhs) (listConcat (tail lhs) rhs)
    fi
in
let stringAppend lhs rhs =
    implode (listConcat (explode lhs) (explode rhs))
in
let listReverse lst = 
    let reverser lst revlst  = 
        if size lst == 0
        then revlst
        else reverser (tail lst) (append (head lst) revlst)
        fi
    in
        reverser lst []
    ni
in
let listSplitAt idx lst = 
    let splitter lhs rhs idx =
        if size lhs == idx || size rhs == 0
        then [lhs, rhs]
        else splitter (append (head rhs) lhs) (tail rhs) idx
        fi
    in 
        let halfReversed = splitter [] lst idx in
            (\ a b. [a , b] ) (listReverse (head halfReversed)) (head (tail halfReversed))
        ni
    ni
in
# Helper functions
let matchNum digits input =
    if size input == 0 
    then digits
    else 
        if listFind strEq (head input) ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
        then matchNum (digits+1) (tail input)
        else digits 
        fi
    fi
in

(*
    Doesnt work, no idea why... seems an infinite loop *)
let matchId chars input =
    if size input == 0 
    then chars
    else 
        if ! (listFind strEq (head input) ["+","-","*","/","=","<",">","&","|"," ",".",",","(","[",")","]","#",""])
        then matchId (chars+1) (tail input)
        else chars
        fi
    fi
in 


let matchLet input = 
    if strEq (head input) "l"
    then    if strEq (head (tail input)) "e"
            then    if strEq (head (tail (tail input))) "t"
                    then    if listFind strEq (head (tail(tail(tail input)))) [" "]
                            then true
                            else false
                            fi
                    else false
                    fi
            else false
            fi
    else false
    fi
in

let matchIn input = 
    if strEq (head input) "i"
    then    if strEq (head (tail input)) "n"
            then    if listFind strEq (head (tail(tail input))) [" ","("]
                    then true
                    else false
                    fi
            else false
            fi
    else false
    fi
in

let matchNi input = 
    if strEq (head input) "n"
    then    if strEq (head (tail input)) "i"
            then    if listFind strEq (head (tail(tail input))) [" ",")"]
                    then true
                    else false
                    fi
            else false
            fi
    else false
    fi
in

let matchIf input = 
    if strEq (head input) "i"
    then    if strEq (head (tail input)) "f"
            then    if listFind strEq (head (tail(tail input))) [" ","("]
                    then true
                    else false
                    fi
            else false
            fi
    else false
    fi
in

(*
let matchThen input = 
    if strEq (head input) "t"
    then    if strEq (head (tail input)) "h"
            then    if strEq (head (tail (tail input))) "e"
                    then    if strEq (head (tail (tail (tail input)))) "n"
                            then    if listFind strEq (head (tail(tail(tail(tail input))))) [" ","("]
                                    then true
                                    else false
                                    fi
                            else false
                            fi
                    else false
                    fi
            else false
            fi
    else false
    fi
in

let matchElse input = 
    if strEq (head input) "e"
    then    if strEq (head (tail input)) "l"
            then    if strEq (head (tail (tail input))) "s"
                    then    if strEq (head (tail (tail (tail input)))) "e"
                            then    if listFind strEq (head (tail(tail(tail(tail input))))) [" ","("]
                                    then true
                                    else false
                                    fi
                            else false
                            fi
                    else false
                    fi
            else false
            fi
    else false
    fi
in
*)

# LEXER
let lexer program lst = 
    let lexNgram input lst = 
        # Base case
        if size input == 0
        then lst 
        else
        # Keywords
        if strEq (head input) "."
        then (append "KDot") (lexNgram (tail input) lst)
        else
        if strEq (head input) ","
        then (append "KComma") (lexNgram (tail input) lst)
        else
        if strEq (head input) "("
        then (append "KOpenRound") (lexNgram (tail input) lst)
        else
        if strEq (head input) ")"
        then (append "KCloseRound") (lexNgram (tail input) lst)
        else
        if strEq (head input) "["
        then (append "KOpenSquare") (lexNgram (tail input) lst)
        else
        if strEq (head input) "]"
        then (append "KCloseSquare") (lexNgram (tail input) lst)
        else
        if strEq (head input) "\\" # Doesnt work
        then (append "KLambda") (lexNgram (tail input) lst)
        else
        # Arithmetics BuiltIn
        if strEq (head input) "+" 
        then (append "BPlus") (lexNgram (tail input) lst)
        else 
        if strEq (head input) "-" 
        then (append "BMinus") (lexNgram (tail input) lst)
        else
        if strEq (head input) "*" 
        then (append "BMult") (lexNgram (tail input) lst)
        else 
        if strEq (head input) "/" 
        then (append "BDiv") (lexNgram (tail input) lst)
        else 
        # Logic and Comparisons
        if strEq (head input) "!" 
        then (append "BNot") (lexNgram (tail input) lst)
        else 
        if strEq (head input) "&" && strEq (listItem 1 input) "&" 
        then let input' = tail input in  (append "BAnd") (lexNgram (tail input') lst) ni
        else 
        if strEq (head input) "|" && strEq (listItem 1 input) "|" 
        then let input' = tail input in  (append "BOr") (lexNgram (tail input') lst) ni
        else 
        if strEq (head input) ">" && strEq (listItem 1 input) "=" 
        then let input' = tail input in  (append "BGtEq") (lexNgram (tail input') lst) ni
        else 
        if strEq (head input) "<" && strEq (listItem 1 input) "=" 
        then let input' = tail input in  (append "BLEq") (lexNgram (tail input') lst) ni
        else
        if strEq (head input) "=" && strEq (listItem 1 input) "=" 
        then let input' = tail input in  (append "BEqTo") (lexNgram (tail input') lst) ni
        else 
        if strEq (head input) ">" 
        then (append "BGt") (lexNgram (tail input) lst)
        else 
        if strEq (head input) "<" 
        then (append "BLe") (lexNgram (tail input) lst)
        else 
        if strEq (head input) "=" 
        then (append "Eq") (lexNgram (tail input) lst)
        else
        if listFind strEq (head input) ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
        then let digits = matchNum 0 input in
            (append (stringAppend "IntLit " (implode (head (listSplitAt digits input))))) 
                    (lexNgram (head (tail (listSplitAt digits input))) lst)
            ni
        else 
        if strEq (head input) " " 
        then lexNgram (tail input) lst
        else
        if matchLet input
        then (append "KLet") (lexNgram (tail(tail(tail(tail input)))) lst)
        else
        if matchIn input
        then (append "KIn") (lexNgram (tail(tail(tail input))) lst)
        else
        if matchNi input
        then (append "KNi") (lexNgram (tail(tail(tail input))) lst)
        else
        if matchIf input
        then (append "KIf") (lexNgram (tail(tail(tail input))) lst)
        else
        (* USE SPLIT AT instead of tail(tail....
        if matchThen input
        then (append "KThen") (lexNgram (tail(tail(tail(tail(tail input))))) lst)
        else
        if matchElse input
        then (append "KElse") (lexNgram (tail(tail(tail(tail(tail input))))) lst)
        else
        *)

            let chars = matchId 0 input in
            (append (stringAppend "Id " (implode (head (listSplitAt chars input))))) 
                    (lexNgram (head (tail (listSplitAt chars input))) lst)
            ni
            #append "ERROR" lst

         fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi # fi fi for then and else
    in
        lexNgram (explode program) lst
    ni
in
    lexer "let x = 5 in x ni " []
ni

# Helper functions
ni ni ni ni ni ni  # ni ni for then and else 
# Library
ni ni ni ni ni ni ni


