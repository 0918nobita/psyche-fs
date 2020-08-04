module Runtime

open UntypedExpr
open Value

val eval : Env -> UntypedExpr -> Result<Value, string>
