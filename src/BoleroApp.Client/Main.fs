module BoleroApp.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Bolero.Templating.Client

type Model =
    {
        content: string
    }

let initModel =
    {
        content = "Bolero App"
    }

type Message =
    | Ping

let update message model =
    match message with
    | Ping -> model

let view (model: Model) (dispatch: Dispatch<Message>) =
    text (model.content)

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override _.Program =
        Program.mkSimple (fun _ -> initModel) update view
#if DEBUG
        |> Program.withHotReload
#endif
