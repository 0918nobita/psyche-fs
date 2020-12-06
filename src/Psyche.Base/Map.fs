module Psyche.Base.Map

let keys (m: Map<_, _>) =
    Map.fold (fun keys key _ -> key :: keys) [] m
