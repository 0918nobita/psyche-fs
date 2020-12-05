namespace Psyche

module Interpreter =
    open Psyche.AST.Value
    open Psyche.AST.UntypedAst

    val eval : Env -> UntypedAst -> Result<Value, string>
