namespace Psyche.Base

module Map =
    let keys (m: Map<_, _>) =
        Map.fold (fun keys key _ -> key :: keys) [] m
