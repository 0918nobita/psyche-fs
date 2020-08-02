module Value

open Syntax

type Value =
    | Closure of VarId * Expr * Env
    | BoolVal of bool
    | IntVal of int
    | RefVal of Ref<Value>

    override this.ToString() =
        match this with
        | BoolVal true -> "true"
        | BoolVal false -> "false"
        | IntVal n -> string n
        | Closure _ -> "<Closure>"
        | RefVal r -> sprintf "<Ref %O>" !r

and Env = List<VarId * Value>
