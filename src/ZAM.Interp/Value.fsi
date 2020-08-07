module Value

open FrontEnd.UntypedAst

type Value =
    | UnitVal
    | Closure of VarId * UntypedAst * Env
    | BoolVal of bool
    | IntVal of int
    | FloatVal of float
    | RefVal of Ref<Value>

and Env = List<VarId * Value>
