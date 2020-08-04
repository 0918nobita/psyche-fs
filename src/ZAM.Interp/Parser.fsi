module Parser

open SExpr

val program : string -> Result<SExpr, string>
