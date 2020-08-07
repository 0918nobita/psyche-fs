namespace FrontEnd

module UntypedAst =
    module BNel = Base.Nel

    type VarId = string

    type BinOp =
        | Add
        | Sub
        | Mul
        | Eq
        | Lt
        | Le

        static member StrMap =
            Map.ofArray
                [| ("+", Add)
                   ("-", Sub)
                   ("*", Mul)
                   ("=", Eq)
                   ("<", Lt)
                   ("<=", Le) |]

        override this.ToString() =
            match this with
            | Add -> "+"
            | Sub -> "-"
            | Mul -> "*"
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
        | UBegin of BNel.Nel<UntypedAst>
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
            | UBinApp(op, lhs, rhs) -> sprintf "(%O %O %O)" op lhs rhs
            | UVar x -> x
            | UFun(arg, body) -> sprintf "(λ %O %O)" arg body
            | UIntOfFloat ast -> sprintf "(int-of-float %O)" ast
            | UFloatOfInt ast -> sprintf "(float-of-int %O)" ast
            | UApp(func, arg) -> sprintf "(%O %O)" func arg
            | UIf(cond, _then, _else) -> sprintf "(if %O %O %O)" cond _then _else
            | ULet(ident, e1, e2) -> sprintf "(let %O %O %O)" ident e1 e2
            | UBegin(body) ->
                let inner =
                    body
                    |> Seq.map string
                    |> Seq.reduce (sprintf "%O %O")
                sprintf "(begin %s)" inner
            | UMakeRef e -> sprintf "(ref %O)" e
            | UDeref e -> sprintf "(deref %O)" e
            | UMut(refExpr, expr) -> sprintf "(mut %O %O)" refExpr expr
