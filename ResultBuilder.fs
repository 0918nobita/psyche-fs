module ResultBuilder

type ResultBuilder() =
    member _.Return(x) = Ok x
    member _.ReturnFrom(m: Result<_, _>) = m
    member _.Bind(m, f) = Result.bind f m

let result = ResultBuilder()
