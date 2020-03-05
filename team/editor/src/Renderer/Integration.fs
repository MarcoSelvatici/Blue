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

//let enableTypeCheck() =
//    showVexAlert "enable type check"

//let disableTypeCheck() =
//    showVexAlert "disable type check"

//let enableSki() =
//    showVexAlert "enable SKI"

//let enableBeta() =
//    showVexAlert "enable BETA"

let getProgram () =
    textOfTId currentFileTabId
    |> List.fold (fun r s -> r + s + "\n") ""

/// Top-level simulation execute
/// If current tab is TB run TB if this is possible
let runCode () =
    let program = getProgram ()
    let res = end2end currentTypeCheck currentRuntime program
    showVexAlert <| sprintf "%A" res

