module Psyche.Base.Functor

[<Struct>]
type FunctorClass<'a, 'b, 'Ma, 'Mb> = {
    Fmap: ('a -> 'b) -> 'Ma -> 'Mb
}

type FunctorBuiltin =
    | FunctorBuiltin

    static member FunctorImpl (_: _ option) =
        { Fmap = Option.map }

    static member FunctorImpl (_: Result<_, _>) =
        { Fmap = Result.map }

    static member FunctorImpl (_: _ list) =
        { Fmap = List.map }

    static member FunctorImpl (_: _[]) =
        { Fmap = Array.map }

let inline getImpl
    (builtin: ^Builtin)
    (dummy: FunctorClass< ^a, ^b, ^Ma, ^Mb >): FunctorClass< ^a, ^b, ^Ma, ^Mb > =
    ((^Builtin or ^Ma): (static member FunctorImpl: ^Ma -> FunctorClass< ^a, ^b, ^Ma, ^Mb >) (Unchecked.defaultof< ^Ma >))

let inline fmap (f: ^a -> ^b) (m: ^Ma): ^Mb =
    (getImpl FunctorBuiltin (Unchecked.defaultof< FunctorClass< ^a, ^b, ^Ma, ^Mb > >)).Fmap f m

let inline (<%>) (f: ^a -> ^b) (m: ^Ma): ^Mb = fmap f m
