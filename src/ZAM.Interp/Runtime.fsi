module Runtime

open Value

val eval : Env -> FrontEnd.UntypedAst.UntypedAst -> Result<Value, string>
