module Preprocessor
open SharedTypes
open Libraries

let buildError s = s |> PreprocessorError |> Error

let isEndlineUNIX = (=) '\010'
let isEndlineDOS = (=) '\013'
let addLine s1 s2 = s1 + "\n" + s2

/////////

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
        match List.tryFindIndex isEndlineUNIX l with
        | Some i -> List.splitAt i l
                    |> (fun (l1, l2) ->
                        match List.tryFindIndex isEndlineDOS l1 with
                        | Some i -> List.splitAt i l1
                                    |> (fun (l3, l4) ->
                                        l3::(toLists (List.tail l2))) 
                        | None -> l1::(toLists (List.tail l2))) 
                        
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