module Psyche.Base.State

open Psyche.Base.Monad

type State<'S, 'A> =
    | State of ('S -> ('A * 'S))

    static member runState ((State f): State<'S, 'A>) (init: 'S) =
        f init
    
    static member MonadImpl (_: State<_, _>) =
        {
            Bind = fun f (State x) ->
                State(fun s ->
                    let (v, s) = x s
                    State.runState (f v) s)
            Return = fun x -> State(fun s -> (x, s))
        }
