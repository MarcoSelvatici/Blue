(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.MenuBar
    Description: Code for the app menus
*)

/// implement menu functions
module MenuBar
open EEExtensions
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Node.Base
open Refs
open Settings
open Tabs

(****************************************************************************************************
 *
 *                                      MENU OPERATIONS
 *
 ****************************************************************************************************)


// Popup windows

let makePrefsWindow() =
    let url = "prefs.html"
    let mutable prefsWindow = (Core.Option.None : BrowserWindow option)

    // Register click on the About menu

    let options = createEmpty<BrowserWindowOptions>
    let webPrefs = createEmpty<WebPreferences>
    webPrefs?nativeWindowOpen <- Some true
    options?toolbar <- Some false
    options.resizable <- Some false
    options.show <- Some true
    options.height <- Some 600.
    options.width <- Some 400.
    options.modal <- Some true
    options.parent <- Some(electron.remote.getCurrentWindow())
    options.webPreferences <- Some webPrefs

    let prefs = electron.remote.BrowserWindow.Create(options)
    // For to remove the menu of the window
    prefs.setMenu (unbox null)

    prefs.on ("closed", unbox (fun () ->
        // Dereference the about window object.
        prefsWindow <- Option.None
    )) |> ignore

    prefs.loadURL (Node.Exports.path.join ("file://", Node.Globals.__dirname, url))

    prefsWindow <- prefs |> Some

/// Load the node Buffer into the specified tab
let loadFileIntoTab tId (fileData : Node.Buffer.Buffer) =
    //if currentFileTabId = tId then
    //    Integration.resetEmulator()
    let editor = editors.[tId]
    editor?setValue (fileData.toString ("utf8")) |> ignore
    setTabSaved tId

let openListOfFiles (fLst : string list) =
    let makeTab path =
        let tId = createNamedFileTab (Files.baseFilePath path) path
        (fun tId -> Files.setTabFilePath tId path) |> ignore
        (path, tId)
    let readPath (path, tId) =
        Node.Exports.fs.readFile (path, (fun err data -> // TODO: find out what this error does
            loadFileIntoTab tId data
            Files.setTabFilePath tId path
        ))
        |> ignore
        [ tId ]
    if debugLevel > 0 then printfn "File list to open is: %A" fLst
    fLst
    |> Files.resultUndefined()
    |> Result.map Files.updateCurrentPathFromList
    |> Result.map (List.map (fun p -> Files.filterBadName false p) >> List.concat)
    |> Result.map (List.map (makeTab >> readPath))
    |> Result.map List.concat
    |> Result.map (function | tId :: _ -> selectFileTab tId; () | [] -> ())

let openFile() =
    let options = createEmpty<OpenDialogOptions>
    options.properties <- ResizeArray([ "openFile"; "multiSelections" ]) |> Some
    options.filters <- Files.fileFilterOpts
    options.defaultPath <- Some vSettings.CurrentFilePath
    electron.remote.dialog.showOpenDialog (options)
    |> Seq.toList
    |> openListOfFiles
    |> ignore

let loadDemo() =
    Tabs.createFileTab()
    |> fun tId ->
        let sampleFileName = "karatsuba.s"
        printfn "Reading sample file: %s" sampleFileName
        Node.Exports.fs.readFile (sampleFileName, (fun _ data -> // TODO: find out what this error does
                                              loadFileIntoTab tId data
        ))
        Tabs.setTabSaved tId

let showQuitMessage (callBack : bool -> unit) =
    let mess = "You have unsaved changes. Are you sure you want to exit and lose changes?"
    let buttons = [ "Save"; "Exit without saving" ]
    Refs.showVexConfirm mess callBack

/// Check if there is any unsaved info. Display dialog asking for confirmation if there is.
/// Otherwise exit.
let ExitIfOK() =
    let close() = electron.ipcRenderer.send "doClose" |> ignore
    let callback (result : bool) =
        match result with
        | false -> ()
        | true -> close()
    let tabL = Tabs.unsavedTabs()
    if tabL <> [] then
        showQuitMessage callback
    else close()

(****************************************************************************************************
 *
 *                                  MENU HELPER FUNCTIONS
 *
 ****************************************************************************************************)


let menuSeparator =
    let sep = createEmpty<MenuItemOptions>
    sep.``type`` <- Some Separator
    sep

/// Make action menu item from name, opt key to trigger, and action.
let makeItem (label : string) (accelerator : string option) (iAction : unit -> unit) =
    let handlerCaster f = System.Func<MenuItem, BrowserWindow, unit> f |> Some
    let item = createEmpty<MenuItemOptions>
    item.label <- Some label
    item.accelerator <- accelerator
    item.click <- handlerCaster (fun _ _ -> iAction())
    item

/// Make role menu from name, opt key to trigger, and action.
let makeRoleItem label accelerator role =
    let item = makeItem label accelerator id
    item.role <- U2.Case1 role |> Some
    item

/// make conditional menu item from condition, name, opt key to trigger, and role
let makeCondRoleItem cond label accelerator role =
    let item = makeItem label accelerator id
    item.role <- U2.Case1 role |> Some
    item.visible <- Some cond
    item

/// make conditional menu item from condition, name, opt key to trigger, and action
let makeCondItem cond label accelerator action =
    let item = makeItem label accelerator action
    item.visible <- Some cond
    item

/// Make a new menu from a a list of menu items
let makeMenu (name : string) (table : MenuItemOptions list) =
    let subMenu = createEmpty<MenuItemOptions>
    subMenu.label <- Some name
    subMenu.submenu <-
        table
        |> ResizeArray<MenuItemOptions>
        |> U2.Case2 |> Some
    subMenu


(****************************************************************************************************
 *
 *                                         MENUS
 *
 ****************************************************************************************************)
let fileMenu() =
    makeMenu "File" [
            makeItem "New" (Some "CmdOrCtrl+N") (createFileTab >> ignore)
            menuSeparator
            makeItem "Save" (Some "CmdOrCtrl+S") Files.saveFile
            makeItem "Save As" (Some "CmdOrCtrl+Shift+S") Files.saveFileAs
            makeItem "Open" (Some "CmdOrCtrl+O") (openFile >> ignore)
            menuSeparator
            makeItem "Close" (Some "CmdOrCtrl+W") deleteCurrentTab
            menuSeparator
            makeItem "Quit" (Some "CmdOrCtrl+Q") ExitIfOK
        ]

let editMenu() =
    makeMenu "Edit" [
        makeItem "Undo" (Some "CmdOrCtrl+Z") Files.editorUndo
        makeItem "Redo" (Some "CmdOrCtrl+Shift+Z") Files.editorRedo
        menuSeparator
        makeRoleItem "Cut" (Some "CmdOrCtrl+X") MenuItemRole.Cut
        makeRoleItem "Copy" (Some "CmdOrCtrl+C") MenuItemRole.Copy
        makeRoleItem "Paste" (Some "CmdOrCtrl+V") MenuItemRole.Paste
        menuSeparator
        makeItem "Select All" (Some "CmdOrCtrl+A") Files.editorSelectAll
        menuSeparator
        makeItem "Find" (Some "CmdOrCtrl+F") Files.editorFind
        makeItem "Replace" (Some "CmdOrCtrl+H") Files.editorFindReplace
        menuSeparator
        makeItem "Increase Font Size" (Some "CmdOrCtrl+.") (fun () -> Settings.alterFontSize 2)
        makeItem "Decrease Font Size" (Some "CmdOrCtrl+,") (fun () -> Settings.alterFontSize -2)
        makeItem "Preferences" Core.Option.None createSettingsTab
    ]

let viewMenu() =
        let devToolsKey = if Node.Globals.``process``.platform = NodeJS.Platform.Darwin then "Alt+Command+I" else "Ctrl+Shift+I"
        makeMenu "View" [
            makeRoleItem "Toggle Fullscreen" (Some "F11") MenuItemRole.Togglefullscreen
            menuSeparator
            makeRoleItem "Zoom In" (Some "CmdOrCtrl+Plus") MenuItemRole.Zoomin
            makeRoleItem "Zoom Out" (Some "CmdOrCtrl+-") MenuItemRole.Zoomout
            makeRoleItem "Reset Zoom" (Some "CmdOrCtrl+0") MenuItemRole.Resetzoom
            menuSeparator
            makeCondItem (debugLevel > 0) "Toggle Dev Tools" (Some devToolsKey) (electron.remote.getCurrentWebContents()).toggleDevTools
        ]

let popupMenu (items) =
    let menu = electron.remote.Menu.Create()
    items
    |> List.map electron.remote.MenuItem.Create
    |> List.iter menu.append
    menu.popup (electron.remote.getCurrentWindow())
    ()


//let helpMenu() =
//    makeMenu "Help" (
//        [
//            makeItem "UAL instruction guide" Core.Option.None (runExtPage <| visualDocsPage "guide#content")
//            makeItem "VisUAL2 web pages" Core.Option.None (runExtPage <| visualDocsPage "")
//            makeItem "Testbenches" Core.Option.None (runExtPage <| visualDocsPage "testbench")
//            makeItem "Official ARM documentation" Core.Option.None (runExtPage "http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0234b/i1010871.html")
//            menuSeparator
//            makeItem "Load complex demo code" Core.Option.None (interlockAction "load code" loadDemo)
//            makeCondItem (debugLevel > 0) "Run dev tools FABLE checks" Core.Option.None (interlockAction "FABLE checks" Integration.runTestbench)
//            makeCondItem (debugLevel > 0) "Run Emulator Tests" Core.Option.None (interlockAction "run tests" Tests.runAllEmulatorTests)
//            menuSeparator
//            makeItem "About" Core.option.None (fun () ->
//                printfn "Directory is:%s" (Stats.dirOfSettings())
//                showVexAlert (sprintf "<h4>VisUAL2 ARM Simulator v%s</h4> " Refs.appVersion +
//                            "(c) 2018, Imperial College <br> Acknowledgements: Salman Arif (VisUAL), HLP 2018 class" +
//                            " (F# reimplementation), with special mention to Thomas Carrotti," +
//                            " Lorenzo Silvestri, and HLP Team 10"))
//        ])


/// Make all app menus
let mainMenu() =
    let template =
        ResizeArray<MenuItemOptions> [
            fileMenu()
            editMenu()
            viewMenu()
            //helpMenu()
        ]
    template
    |> electron.remote.Menu.buildFromTemplate
    |> electron.remote.Menu.setApplicationMenu
