module TypeChecker

open Type
open TypedAst
open UntypedAst

val typeCheck: TypeEnv -> TypedAst -> Result<Type * UntypedAst, string>
