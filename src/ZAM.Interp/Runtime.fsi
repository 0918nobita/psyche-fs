module Runtime

open Value
open FrontEnd.UntypedAst

val eval : Env -> UntypedAst -> Result<Value, string>
