module internal Parser

open SExpr

val program : string -> Result<SExpr, string>
