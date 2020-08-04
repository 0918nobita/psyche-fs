module Value

open UntypedExpr

type Value =
    | UnitVal
    | Closure of VarId * UntypedExpr * Env
    | BoolVal of bool
    | IntVal of int
    | RefVal of Ref<Value>

and Env = List<VarId * Value>
