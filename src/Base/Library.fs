namespace Base

module Map =
    let keys (m: Map<_, _>) = Map.fold (fun keys key _ -> key :: keys) [] m

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
