module Program

open Gtk

type MainWindow (builder : Builder) as this =
    inherit Window(builder.GetObject("MainWindow").Handle)

    do
        this.DeleteEvent.Add(fun _ -> Application.Quit())

    new() = new MainWindow(new Builder("MainWindow.glade"))

module BNel = Psyche.Base.Nel
module BResult = Psyche.Base.Result

open System.IO

let run src =
    BResult.result {
        let! (ty, untypedAst) = Psyche.Parser.tryParse src
        printfn "Static type: %O" ty
        let! value = Runtime.eval (Primitive.primitives) untypedAst
        return value
    }

let interactive () =
    fun _ ->
        printf "> "
        System.Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.takeWhile (fun input -> not(isNull input) && input <> ":exit")
    |> Seq.iter (fun src ->
        match run src with
        | Ok value ->
            printfn "Result: %O\n" value
        | Error msg ->
            eprintfn "%s\n" msg)

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv
    then
        interactive ()
    else
        if argv.[0] = "gui"
            then
                Application.Init()
                let app = new Application("vision.kodai.psyche", GLib.ApplicationFlags.None)
                app.Register(GLib.Cancellable.Current) |> ignore
                let win = new MainWindow()
                app.AddWindow(win)
                win.Show()
                Application.Run()
            else
                let srcPath = argv.[0]
                let src = File.ReadAllText srcPath

                match run src with
                | Ok value ->
                    printfn "Result: %O" value
                | Error msg ->
                    eprintfn "%s" msg
                    exit 1
    0
