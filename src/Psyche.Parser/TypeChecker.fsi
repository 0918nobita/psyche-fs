module internal TypeChecker

open Psyche.Types
open Psyche.UntypedAst
open TypedAst

val typeCheck: TypeEnv -> TypedAst -> Result<Type * UntypedAst, string>
