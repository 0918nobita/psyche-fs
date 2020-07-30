module Value

open Syntax

type Value =
    | Closure of VarId * Expr * Env
    | BoolVal of bool
    | IntVal of int

    override this.ToString() =
        match this with
        | BoolVal true -> "true"
        | BoolVal false -> "false"
        | IntVal n -> string n
        | Closure(_) -> "<Closure>"

and Env = List<VarId * Ref<Value>>
