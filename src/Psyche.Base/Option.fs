module Psyche.Base.Option

type OptionBuilder() =
    member _.Return(x) = Some x
    member _.ReturnFrom(m: _ option) = m
    member _.Bind(m, f) = Option.bind f m

let option = OptionBuilder()

let toResult =
    function
    | Some x -> Ok x
    | None -> Error()
