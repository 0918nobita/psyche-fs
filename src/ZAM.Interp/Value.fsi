module Value

open UntypedAst

type Value =
    | UnitVal
    | Closure of VarId * UntypedAst * Env
    | BoolVal of bool
    | IntVal of int
    | RefVal of Ref<Value>

and Env = List<VarId * Value>
