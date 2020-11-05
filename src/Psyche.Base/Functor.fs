namespace Psyche.Base

module Functor =
    type Fmap() =
        static member (?) (_: Fmap, source) =
            fun f -> Option.map f source

        static member (?) (_: Fmap, source) =
            fun f -> List.map f source

        static member (?) (_: Fmap, source) =
            fun f -> Array.map f source

    let inline (<%>) f x = Fmap() ? (x) f
