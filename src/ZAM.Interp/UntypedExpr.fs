module UntypedExpr

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

type UntypedExpr =
    | UUnit
    | UBool of value: bool
    | UInt of value: int
    | UBinApp of op: BinOp * lhs: UntypedExpr * rhs: UntypedExpr
    | UVar of id: VarId
    | UFun of arg: VarId * body: UntypedExpr
    | UApp of func: UntypedExpr * actualArg: UntypedExpr
    | UIf of cond: UntypedExpr * _then: UntypedExpr * _else: UntypedExpr
    | ULet of VarId * UntypedExpr * UntypedExpr
    | UBegin of BNel.Nel<UntypedExpr>
    | UMakeRef of UntypedExpr
    | UDeref of UntypedExpr
    | UMut of UntypedExpr * UntypedExpr

    override this.ToString() =
        match this with
        | UUnit -> "#unit"
        | UBool true -> "true"
        | UBool false -> "false"
        | UInt n -> string n
        | UBinApp(op, lhs, rhs) -> sprintf "(%O %O %O)" op lhs rhs
        | UVar x -> x
        | UFun(arg, body) -> sprintf "(λ %O %O)" arg body
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
