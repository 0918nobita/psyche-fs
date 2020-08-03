module Value

open UntypedExpr

type Value =
    | UnitVal
    | Closure of VarId * UntypedExpr * Env
    | BoolVal of bool
    | IntVal of int
    | RefVal of Ref<Value>

    override this.ToString() =
        match this with
        | UnitVal -> "#unit"
        | BoolVal true -> "true"
        | BoolVal false -> "false"
        | IntVal n -> string n
        | Closure _ -> "<Closure>"
        | RefVal r -> sprintf "<Ref %O>" !r

and Env = List<VarId * Value>
