namespace FrontEnd

module Parser =
    val program : string -> Result<SExpr.SExpr, string>
