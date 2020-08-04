module TypeChecker

open Type
open TypedExpr
open UntypedExpr

val typeCheck: TypeEnv -> TypedExpr -> Result<Type * UntypedExpr, string>
