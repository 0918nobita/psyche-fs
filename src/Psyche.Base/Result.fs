module Psyche.Base.Result

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
