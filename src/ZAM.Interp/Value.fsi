module Value

type Value =
    | UnitVal
    | Closure of FrontEnd.UntypedAst.VarId * FrontEnd.UntypedAst.UntypedAst * Env
    | BoolVal of bool
    | IntVal of int
    | RefVal of Ref<Value>

and Env = List<FrontEnd.UntypedAst.VarId * Value>
