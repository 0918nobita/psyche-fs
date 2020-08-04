module TypedExpr

module BNel = Base.Nel
module BOption = Base.Option
module BResult = Base.Result

open Type

type TEVarId = string

type TEBinOp =
    | TEAdd
    | TESub
    | TEMul
    | TEEq
    | TELt
    | TELe

    static member StrMap =
        Map.ofArray
            [| ("+", TEAdd)
               ("-", TESub)
               ("*", TEMul)
               ("=", TEEq)
               ("<", TELt)
               ("<=", TELe) |]

    override this.ToString() =
        match this with
        | TEAdd -> "+"
        | TESub -> "-"
        | TEMul -> "*"
        | TEEq -> "="
        | TELt -> "<"
        | TELe -> "<="

type TypedExpr =
    | TEUnit
    | TEBool of bool
    | TEInt of int
    | TEBinApp of op: TEBinOp * lhs: TypedExpr * rhs: TypedExpr
    | TEVar of TEVarId
    | TEFun of arg: TEVarId * argType: Type * body: TypedExpr
    | TEApp of func: TypedExpr * actualArg: TypedExpr
    | TEIf of cond: TypedExpr * _then: TypedExpr * _else: TypedExpr
    | TELet of name: TEVarId * typeOfName: Type * expr1: TypedExpr * expr2: TypedExpr
    | TEBegin of BNel.Nel<TypedExpr>
    | TEMakeRef of TypedExpr
    | TEDeref of TypedExpr
    | TEMut of TypedExpr * TypedExpr

    override this.ToString() =
        match this with
        | TEUnit -> "#unit"
        | TEBool true -> "true"
        | TEBool false -> "false"
        | TEInt n -> string n
        | TEBinApp(op, lhs, rhs) -> sprintf "(%O %O %O)" op lhs rhs
        | TEVar x -> x
        | TEFun(arg, argType, body) -> sprintf "(λ (: %s %O) %O)" arg argType body
        | TEApp(func, actualArg) -> sprintf "(%O %O)" func actualArg
        | TEIf(cond, _then, _else) -> sprintf "(if %O %O %O)" cond _then _else
        | TELet(name, typeOfName, expr1, expr2) ->
            sprintf "(let (: %s %O) %O %O)" name typeOfName expr1 expr2
        | TEBegin(body) ->
            let inner =
                body
                |> Seq.map string
                |> Seq.reduce (sprintf "%O %O")
            sprintf "(begin %s)" inner
        | TEMakeRef(expr) -> sprintf "(ref %O)" expr
        | TEDeref(expr) -> sprintf "(deref %O)" expr
        | TEMut(expr1, expr2) -> sprintf "(mut %O %O)" expr1 expr2
