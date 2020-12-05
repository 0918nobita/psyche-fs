namespace Psyche

module Parser =
    open Psyche.AST.AnnotatedAst

    val tryParse : string -> Result<AnnotatedAst, string>
