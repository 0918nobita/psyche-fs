namespace FrontEnd

module Exposed =
    module BResult = Base.Result

    /// Parse string into untyped AST and type signature
    let tryParse src =
        BResult.result {
            let! sexp = Parser.program src
            let! typedAst = SExpr.toExpr sexp
            return! TypeChecker.typeCheck [] typedAst }
