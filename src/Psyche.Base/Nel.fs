namespace Psyche.Base

open System.Collections
open System.Collections.Generic

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
