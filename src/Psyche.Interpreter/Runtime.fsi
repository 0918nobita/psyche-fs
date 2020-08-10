module internal Runtime

open Value
open Psyche.UntypedAst

val eval : Env -> UntypedAst -> Result<Value, string>