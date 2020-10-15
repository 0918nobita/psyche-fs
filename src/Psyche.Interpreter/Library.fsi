namespace Psyche

module Interpreter =
    open Value
    open Psyche.AST.UntypedAst

    val eval : Env -> UntypedAst -> Result<Value, string>
