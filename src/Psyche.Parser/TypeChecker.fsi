module internal TypeChecker

open FrontEnd.Type
open FrontEnd.UntypedAst
open TypedAst

val typeCheck: TypeEnv -> TypedAst -> Result<Type * UntypedAst, string>
