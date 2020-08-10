namespace Psyche.Base

open System.Collections
open System.Collections.Generic

module Map =
    let keys (m: Map<_, _>) = Map.fold (fun keys key _ -> key :: keys) [] m

module Nel =
    type Nel<'a> =
        private
        | Nel of 'a * list<'a>

        interface IReadOnlyCollection<'a> with

            member this.Count =
                let (Nel(_, tail)) = this
                1 + List.length tail

            member this.GetEnumerator() =
                let (Nel(head, tail)) = this
                (head :: tail :> _ seq).GetEnumerator()

            member this.GetEnumerator() =
                let (Nel(head, tail)) = this
                (head :: tail :> _ seq).GetEnumerator() :> IEnumerator

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
        member _.ReturnFrom(m: Option<_>) = m
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
        (list: list<'T>)
        : Result<'State, 'Error>
        =
        List.fold (fun state elem ->
            result {
                let! s = state
                return! folder s elem }) (Ok initialState) list
