module internal Parser

open Psyche.AST.SExpr

val program : string -> Result<SExpr, string>
