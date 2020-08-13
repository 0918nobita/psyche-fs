namespace Psyche

open System.Collections
open System.Collections.Generic

module Base =
    module Map =
        let keys (m: Map<_, _>) =
            Map.fold (fun keys key _ -> key :: keys) [] m

    type Nel<'a> =
        private
        | Nel of 'a * ('a list)

        interface IReadOnlyCollection<'a> with
            member this.Count =
                let (Nel(_, tail)) = this
                1 + List.length tail

            member this.GetEnumerator() =
                let (Nel(head, tail)) = this
                (head :: tail :> seq<_>).GetEnumerator()

            member this.GetEnumerator() =
                let (Nel(head, tail)) = this
                (head :: tail :> seq<_>).GetEnumerator() :> IEnumerator

    module Nel =
        module ActivePattern =
            let (|Nel|) (Nel(head, tail)) = (head, tail)

        let singleton v = Nel(v, [])
        let create head tail = Nel(head, tail)
        let head (Nel(head, _)) = head
        let tail (Nel(_, tail)) = tail
        let length (Nel(_, tail)) = 1 + List.length tail

    module Option =
        type OptionBuilder() =
            member _.Return(x) = Some x
            member _.ReturnFrom(m: _ option) = m
            member _.Bind(m, f) = Option.bind f m

        let option = OptionBuilder()

        let toResult =
            function
            | Some x -> Ok x
            | None -> Error()

    module Result =
        type ResultBuilder() =
            member _.Return(x) = Ok x
            member _.ReturnFrom(m: Result<_, _>) = m
            member _.Bind(m, f) = Result.bind f m

        let result = ResultBuilder()

        let fold
            (folder: 'State -> 'T -> Result<'State, 'Error>)
            (initialState: 'State)
            (list: 'T list)
            : Result<'State, 'Error>
            =
            let folder state elem =
                result {
                    let! s = state
                    return! folder s elem
                }
            List.fold folder (Ok initialState) list

    type State<'s, 'a> = State of ('s -> ('a * 's))

    module State =
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
