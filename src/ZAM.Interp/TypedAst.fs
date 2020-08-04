module TypedAst

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

type TypedAst =
    | TEUnit
    | TEBool of bool
    | TEInt of int
    | TEBinApp of op: TEBinOp * lhs: TypedAst * rhs: TypedAst
    | TEVar of TEVarId
    | TEFun of arg: TEVarId * argType: Type * body: TypedAst
    | TEApp of func: TypedAst * actualArg: TypedAst
    | TEIf of cond: TypedAst * _then: TypedAst * _else: TypedAst
    | TELet of name: TEVarId * typeOfName: Type * expr1: TypedAst * expr2: TypedAst
    | TEBegin of BNel.Nel<TypedAst>
    | TEMakeRef of TypedAst
    | TEDeref of TypedAst
    | TEMut of TypedAst * TypedAst

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
