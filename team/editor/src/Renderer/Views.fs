(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Views
    Description: Display registers, memory or symbol table in Views Panel
*)

/// implement views panel
module Views

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser
open Refs
open Fable

let maxSymbolWidth = 30
let maxDataSymbolLength = 16

let nameSquash maxW name =
    let nameLen = String.length name
    if nameLen <= maxW then name
    else
        let fp = (float maxW) * 0.65 |> int
        let lp = maxW - (fp + 3)
        name.[0..fp - 1] + "..." + name.[nameLen - lp..nameLen - 1]

let calcDashboardWidth() =
    let w =
        match currentRep, currentView with
        | Bin, _ -> "--dashboard-width-binrep"
        | _, Output -> "--dashboard-width-init-output"
        //| _ -> "--dashboard-width-init-other"
        |> getCustomCSS
    printf "Setting width to %s" w
    w |> setDashboardWidth


let setRepresentation rep =
    (// Disable the other button
    representation currentRep).classList.remove("btn-rep-enabled")
    |> ignore

    // Enable the newly pressed button
    let btnNew = representation rep
    btnNew.classList.add ("btn-rep-enabled");

    // Reassign currentRep, new mutability required
    // keep constants defining GUI sizes in CSS
    currentRep <- rep
    calcDashboardWidth()

let setTypeCheck setting =
    (// Disable the other button
    typeToggle currentTypeCheck).classList.remove("btn-rep-enabled")
    |> ignore

    // Enable the newly pressed button
    let btnNew = typeToggle setting
    btnNew.classList.add ("btn-rep-enabled");

    // Reassign currentRep, new mutability required
    // keep constants defining GUI sizes in CSS
    currentTypeCheck <- setting
    calcDashboardWidth()

let setRuntime setting =
    (// Disable the other button
    runtimeSelect currentRuntime).classList.remove("btn-rep-enabled")
    |> ignore

    // Enable the newly pressed button
    let btnNew = runtimeSelect setting
    btnNew.classList.add ("btn-rep-enabled");

    // Reassign currentRep, new mutability required
    // keep constants defining GUI sizes in CSS
    currentRuntime <- setting
    calcDashboardWidth()

/// make an HTML element
/// id = element name
/// css = css class names to add to classlist
/// inner = inner HTML (typically text) for element
let makeElement (id : string) (css : string) (inner : string) =
        let el = document.createElement id
        el.classList.add css
        el.innerHTML <- inner
        el

/// make an HTML element
/// id = element name
/// css = css class names to add to classlist
let makeEl (id : string) (css : string) =
        let el = document.createElement id
        el.classList.add css
        el
/// appends child node after last child in parent node, returns parent
/// operator is left associative
/// child: child node
/// node: parent node.
let (&>>) (node : Node) child =
    node.appendChild child |> ignore
    node

let createDOM (parentID : string) (childList : Node list) =
    let parent = document.createElement parentID
    List.iter (fun ch -> parent &>> ch |> ignore) childList
    parent

let addToDOM (parent : Node) (childList : Node list) =
    List.iter (fun ch -> parent &>> ch |> ignore) childList
    parent

/// Set View to view
let setView view =
    (// Change the active tab
    viewTab currentView).classList.remove("active")
    (viewTab view).classList.add("active")

    (// Change the visibility of the views
    viewView currentView).classList.add("invisible")
    (viewView view).classList.remove("invisible")

    // new mutability again, update the variable
    currentView <- view
    calcDashboardWidth()
