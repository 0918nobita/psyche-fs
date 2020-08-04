module Runtime

open UntypedAst
open Value

val eval : Env -> UntypedAst -> Result<Value, string>
