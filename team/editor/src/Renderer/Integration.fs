/// integrate emulator code with renderer
module Integration

open Tabs
//open Views
open Refs
open Editors
open Fable.Core.JsInterop
open Fable.Import

open EmulatorTop
open SharedTypes

let resetEmulator() =
    showVexAlert "Resetting..."
    Editors.removeEditorDecorations currentFileTabId
    Editors.enableEditors()

let getProgram () =
    textOfTId currentFileTabId
    |> List.fold (fun r s -> r + s + "\n") ""

/// Top-level simulation execute
/// If current tab is TB run TB if this is possible
let runCode () =
    let program = getProgram ()
    let res = end2end true program
    showVexAlert <| sprintf "%A" res
