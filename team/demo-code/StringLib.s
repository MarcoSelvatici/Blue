## String library ##

let stringItem idx str =
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
  listItem idx (explode str)
  ni
in
let stringLength str = 
  size (explode str)
in

0 ## Do nothing.

ni ni