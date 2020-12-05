namespace Psyche

module Parser =
    module BResult = Psyche.Base.Result

    /// Parse string into type-annotated AST
    let tryParse src =
        BResult.result {
            let! sexp = Parser.program src
            return! Psyche.AST.SExpr.toAnnotatedAst sexp
        }
