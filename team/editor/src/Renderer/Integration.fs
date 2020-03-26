/// integrate emulator code with renderer
module Integration

open Tabs
//open Views
open Refs
open Editors
open Fable.Core.JsInterop
open Fable.Import

open EmulatorTop
open TypeChecker
open SharedTypes

let resetEmulator() =
    //showVexAlert "Resetting..."
    (getHtml "out-text").innerHTML <- ""
    (getHtml "out-type").innerHTML <- "" 
    (getHtml "out-print").innerHTML <- ""
    Editors.removeEditorDecorations currentFileTabId
    Editors.enableEditors()

let getProgram () =
    textOfTId currentFileTabId
    |> List.fold (fun r s -> r + s + "\n") ""

let makeToolTip lineNumber hoverLst =
    let makeMarkDown textLst =
        textLst
        |> List.toArray
        |> Array.map (fun txt -> createObj [ "isTrusted" ==> true; "value" ==> txt ])
    editorLineDecorate
        Refs.editors.[currentFileTabId]
        lineNumber
        (createObj [
            "isWholeLine" ==> true
            "isTrusted" ==> true
            "inlineClassName" ==> ""
            "hoverMessage" ==> makeMarkDown hoverLst
         ])
        None

let makeTypesTooltips program =
    removeEditorDecorations currentFileTabId
    let rawText = textOfTId currentFileTabId
    let linesWithLet, _ =
        let folder state (currLine : string) =
            //let seqLine = Seq.toList line
            let stateLst, lineNum = state
            match currLine.IndexOf "let" with
            | -1 -> stateLst, lineNum + 1 // No match.
            | b ->
                let trimmed = currLine.Substring (b+4) // Skip "let ".
                match trimmed.IndexOf " " with
                | -1 -> stateLst, lineNum + 1 // No match.
                | e -> (trimmed.[..e-1], lineNum) :: stateLst, lineNum + 1
        let initialState = [], 1 // No lines with let, and examining line 1.
        (initialState, rawText) ||> List.fold folder

    match getFuncTypes program with
    | Error _ -> ()
    | Ok funcTypes ->
        let lookupType fName =
            match List.tryFind (fun (name,_)->fName = name) funcTypes with
            | Some (_,t) -> sprintf "val %s: %s" fName t
            | None -> "Could not find type"
        // For each line with let, create a tooltip with the corresponding type from
        // funcTypes.
        linesWithLet
        |> List.map (fun (fname, lineNum) -> makeToolTip lineNum [lookupType fname])
        |> ignore

/// Top-level simulation execute
/// If current tab is TB run TB if this is possible
let runCode () =
    let program = getProgram ()
    try
        if currentTypeCheck
        then makeTypesTooltips program
        else ()
        let res = end2end currentTypeCheck currentRuntime program
        (getHtml "out-text").innerHTML <-  sprintf "%A" (prettyPrint res)
        (getHtml "out-type").innerHTML <- if currentTypeCheck
                                          then getType program
                                          else "Type checking not enabled"
        (getHtml "out-print").innerHTML <- Printer.ReturnClear
    with
        // Some of the impossible cases has been triggered, or there was a stack
        // overflow.
        Failure msg -> showVexAlert <| sprintf "EXCEPTION:\n%A<br>This may be due to the fact that the expression cannot be type checked. Try turning off type checking." msg

