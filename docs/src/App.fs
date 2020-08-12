module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props

type Model = { X: int }

type Msg =
    | Increment
    | Decrement

let init (): Model * Cmd<Msg> =
    { X = 0 }, Cmd.none

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Increment ->
        { model with X = model.X + 1 }, Cmd.none
    | Decrement ->
        { model with X = model.X - 1}, Cmd.none

let view (model: Model) (dispatch: Dispatch<Msg>) =
    div []
        [ button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
        ; div [] [ str (string model.X) ]
        ; button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ]
        ]

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-react"
|> Program.withConsoleTrace
|> Program.run
