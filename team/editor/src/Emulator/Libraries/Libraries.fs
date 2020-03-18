module Libraries

let libraries  =
    [ 
    "List",
    ["let listMap f lst =
        if size lst == 0
        then []
        else append (f (head lst)) (listMap f (tail lst))
        fi
      in";
    "let listReduce f lst =
      let reducer f acc lst =
          if size lst == 0
          then acc
          else reducer f (f acc (head lst)) (tail lst)
          fi
      in
          if size lst == 0
          then 0 
          else reducer f (head lst) (tail lst) 
          fi
      ni
    in";
    "let listFold f acc lst = 
        if size lst == 0
        then acc
        else listFold f (f acc (head lst)) (tail lst)
        fi
     in";
     "let listFindInt int lst = 
        if size lst == 0
        then false
        else 
          if (head lst) == int
          then true 
          else listFindInt int (tail lst)
          fi
        fi
      in";
      "let listFind f key lst = 
         if size lst == 0
         then false
         else 
           if f (head lst) key
           then true 
           else listFind f key (tail lst)
           fi
         fi
      in";
      "let listConcat lhs rhs = 
        if size lhs == 0
        then rhs
        else append (head lhs) (listConcat (tail lhs) rhs)
        fi
      in";
      "let listReverse lst = 
        let reverser lst revlst  = 
          if size lst == 0
          then revlst
          else reverser (tail lst) (append (head lst) revlst)
          fi
        in
          reverser lst []
        ni
      in";
      "let listSplitAt idx lst = 
        let splitter lhs rhs idx =
          if size lhs == idx || size rhs == 0
          then [lhs, rhs]
          else splitter (append (head rhs) lhs) (tail rhs) idx
          fi
        in 
        let halfReversed = splitter [] lst idx in
          (\\ a b. [a , b] ) (listReverse (head halfReversed)) (head (tail halfReversed))
        ni
      ni
      in";
      "let listSort f l = 
        let half l = listSplitAt (size l / 2) l in
          let merge la lb = 
            if size la == 0 
            then lb
            else
              if size lb == 0 
              then la 
              else
                if f (head la) (head lb)
                then append (head la) (merge (tail la) lb)
                else append (head lb) (merge la (tail lb))
                fi 
              fi 
            fi
          in
            if size l <= 1 
            then l 
            else 
              let halfed = half l in 
                let left = head halfed in
                  let right = head (tail halfed) in 
                    merge (listSort f left) (listSort f right)
                  ni 
                ni 
              ni 
            fi 
          ni 
        ni
      in";
      "let listItem idx lst =    
         let looper step idx lst =  
           if size lst == 0
           then 0 # undefined behaviour
           else
             if step == idx
             then head lst
             else looper (step + 1) idx (tail lst)
             fi
           fi
         in
           looper 0 idx lst
         ni
       in";
        "let listContains f keys lst =
            if size keys == 0 || size lst == 0
            then false 
            else 
                if (listFind f (head keys) lst)
                then true
                else  listContains f (tail keys) lst
                fi
            fi
        in";
    ];

    "String",
    ["let stringItem idx str =
        let listItem idx lst =    
         let looper step idx lst =  
           if size lst == 0
           then 0 # undefined behaviour
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
      in";
      "let stringLength str = 
        size (explode str)
      in";
    ];

    "Test",
    ["let test = 0 in"];
    ]
    |> List.map (fun (n,l) -> (n, (List.fold (fun s1 s2 -> s1 + "\n" + s2) "" l, List.length l)))
    |> Map

