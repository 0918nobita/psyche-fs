namespace Psyche.AST

module UntypedAst =
    type VarId = string

    type BinOp =
        | AddI
        | AddF
        | SubI
        | SubF
        | MulI
        | MulF
        | DivI
        | DivF
        | Mod
        | Eq
        | Lt
        | Le

        static member StrMap =
            Map.ofArray
                [| ("+", AddI)
                   ("+.", AddF)
                   ("-", SubI)
                   ("-.", SubF)
                   ("*", MulI)
                   ("*.", MulF)
                   ("/", DivI)
                   ("/.", DivF)
                   ("%", Mod)
                   ("=", Eq)
                   ("<", Lt)
                   ("<=", Le) |]

        override this.ToString() =
            match this with
            | AddI -> "+"
            | AddF -> "+."
            | SubI -> "-"
            | SubF -> "-."
            | MulI -> "*"
            | MulF -> "*."
            | DivI -> "/"
            | DivF -> "/."
            | Mod -> "%"
            | Eq -> "="
            | Lt -> "<"
            | Le -> "<="

    type UntypedAst =
        | UUnit
        | UBool of value: bool
        | UInt of value: int
        | UFloat of value: float
        | UBinApp of op: BinOp * lhs: UntypedAst * rhs: UntypedAst
        | UVar of id: VarId
        | UFun of arg: VarId * body: UntypedAst
        | UIntOfFloat of UntypedAst
        | UFloatOfInt of UntypedAst
        | UApp of func: UntypedAst * actualArg: UntypedAst
        | UIf of cond: UntypedAst * _then: UntypedAst * _else: UntypedAst
        | ULet of VarId * UntypedAst * UntypedAst
        | UBegin of Psyche.Base.Nel<UntypedAst>
        | UMakeRef of UntypedAst
        | UDeref of UntypedAst
        | UMut of UntypedAst * UntypedAst

        override this.ToString() =
            match this with
            | UUnit -> "#unit"
            | UBool true -> "true"
            | UBool false -> "false"
            | UInt n -> string n
            | UFloat f -> string f
            | UBinApp(op, lhs, rhs) -> $"({op} {lhs} {rhs})"
            | UVar x -> x
            | UFun(arg, body) -> $"(λ {arg} {body})"
            | UIntOfFloat ast -> $"(int-of-float {ast})"
            | UFloatOfInt ast -> $"(float-of-int {ast})"
            | UApp(func, arg) -> $"({func} {arg})"
            | UIf(cond, _then, _else) -> $"(if {cond} {_then} {_else})"
            | ULet(ident, e1, e2) -> $"(let {ident} {e1} {e2})"
            | UBegin body ->
                let inner =
                    body
                    |> Seq.map string
                    |> Seq.reduce (sprintf "%O %O")
                $"(begin {inner})"
            | UMakeRef e -> $"(ref {e})"
            | UDeref e -> $"(deref {e})"
            | UMut(refExpr, expr) -> $"(mut {refExpr} {expr})"
