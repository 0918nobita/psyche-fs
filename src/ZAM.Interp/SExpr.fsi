module SExpr

open TypedAst

type Atom =
    | SBool of bool
    | SInt of int
    | Symbol of string

type SExpr =
    | Atom of Atom
    | SList of SExpr list

val toExpr : SExpr -> Result<TypedAst, string>
