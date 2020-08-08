namespace FrontEnd

module Exposed =
    module BResult = Base.Result

    /// Parse string into untyped AST and type signature
    let tryParse src =
        BResult.result {
            let! sexp = Parser.program src
            let! typedAst = SExpr.toTypedAst sexp
            return! TypeChecker.typeCheck (Primitive.primitives) typedAst }
