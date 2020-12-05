namespace Psyche

module TypeChecker =
    open Psyche.AST.Type
    open Psyche.AST.AnnotatedAst
    open Psyche.AST.UntypedAst

    val typeCheck: TypeEnv -> AnnotatedAst -> Result<Type * UntypedAst, string>
