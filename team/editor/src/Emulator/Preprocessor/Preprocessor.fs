module Preprocessor
open SharedTypes

let buildError s = s |> PreprocessorError |> Error

// TODO: extend support to windows
let isEndline = (=) '\n'
let addLine s1 s2 = s1 + "\n" + s2

/////////
let libraries  =
    [ 
    "List",
    ["let listMap f lst =
    if size lst == 0
    then []
    else append (f (head lst)) (listMap f (tail lst))
    fi in";
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
      "let listFind f int lst = 
         if size lst == 0
         then false
         else 
           if f (head lst) int
           then true 
           else listFind f int (tail lst)
           fi
         fi
      in";
      "let StringLength str = 
         size (explode str)
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
      if size la == 0 then lb else
      if size lb == 0 then la else
      if f (head la)  (head lb)
      then append (head la) (merge (tail la) lb)
      else append (head lb) (merge la (tail lb))
      fi fi fi
      in
      if size l <= 1 then l else 
      let halfed = half l in 
      let left = head halfed in
      let right = head (tail halfed) in 
      merge (listSort f left) (listSort f right)
      ni ni ni fi ni ni
      in
     "
    ];

    "Test",
    ["let test = 0 in"];
    ]
    |> List.map (fun (n,l) -> (n, (List.fold addLine "" l, List.length l)))
    |> Map

let findLib k = 
    Map.tryFind k libraries
    |>  function 
    | Some x -> Ok x
    | None -> sprintf "\'import %s\' error\nno such library\ndid you mean %A" 
                k (libraries |> Map.toList |> List.map fst) |> buildError
/////////




let charToString chrLst = chrLst |> (List.map string) |> List.fold (+) ""

let splitLines (s:string) : char list list= 
    let rec toLists l =
        match List.tryFindIndex isEndline l with
        | Some i -> List.splitAt i l
                    |> (fun (l1, l2) -> l1::(toLists (List.tail l2))) 
        | None -> [l]    

    Seq.toList s |> toLists

(*
open System.IO
let isBackslash = (=) '/'
let readFile name =
    let backDir (path:string) = 
        path |> Seq.toList 
        |> List.splitAt (Seq.findIndexBack isBackslash path)
        |> fst |> charToString
    let path = ( backDir __SOURCE_DIRECTORY__) + "/Libraries/" + name
    try
        match File.ReadAllLines(path) with
        | [||] | [|_|] -> ("",0) |> Ok
        | arr -> (Array.reduce addLine arr.[1..], int arr.[0] ) |> Ok
    with
    | :? System.IO.FileNotFoundException
        -> sprintf "\'import %s\' error \n file %s not found" name path |> buildError
    | :? System.IO.DirectoryNotFoundException
        -> sprintf "What? \'import %s\' \n directory %s not found\n" name path |> buildError
    | :? System.FormatException
        -> sprintf "\'import %s\' error \n first line should be number of funtions" name |> buildError
*)

let replaceImport (codeLine, countNi) (s:char list) =
    match s with
    | 'i'::'m'::'p'::'o'::'r'::'t'::' '::name ->  
            name |> charToString |> findLib 
            |> Result.map (fun (l,i) -> l::codeLine,countNi+i) 
    | _ -> ((charToString s)::codeLine, countNi) |> Ok

let concatNi (s,i) = addLine s (String.replicate i "ni ")
   
let mapFirst f (l,r) = (f l, r)
let bindFolder folder state el =
    match state with
    | Ok s -> folder s el
    | Error _ as err -> err

let print a = printf "%A\n" a ; a
let preprocess rawInput =
    rawInput
    |> splitLines
    |> List.fold (bindFolder replaceImport) ( Ok ([],0) )
    |> Result.map (
        mapFirst (List.rev >> List.toSeq >> String.concat "\n" )
        >> concatNi
        )
    
// TODO : test w empty lines