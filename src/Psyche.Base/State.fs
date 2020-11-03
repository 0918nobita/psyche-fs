module Psyche.Base.State

type State<'s, 'a> = State of ('s -> ('a * 's))

let runState ((State f): State<'s, 'a>) (init: 's) =
    f init

type StateBuilder() =
    member _.Return(x) = State(fun s -> (x, s))
    member _.ReturnFrom(m: State<_, _>) = m
    member _.Bind((State x), f) =
        State
        <| fun s ->
            let (v, s) = x s
            runState (f v) s

let state = StateBuilder()
