module internal TypeChecker

open Psyche.Types
open Psyche.AST.UntypedAst
open AnnotatedAst

val typeCheck: TypeEnv -> AnnotatedAst -> Result<Type * UntypedAst, string>
