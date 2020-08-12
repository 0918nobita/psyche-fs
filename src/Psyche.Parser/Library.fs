namespace Psyche

module Parser =
    module BResult = Psyche.Base.Result

    /// Parse string into untyped AST and type signature
    let tryParse src =
        BResult.result {
            let! sexp = Parser.program src
            let! annotatedAst = SExpr.toAnnotatedAst sexp
            return! TypeChecker.typeCheck
                (Primitive.primitives)
                annotatedAst
        }
