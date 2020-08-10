module internal TypedAst

module BNel = Psyche.Base.Nel
module BOption = Psyche.Base.Option
module BResult = Psyche.Base.Result

open Psyche.Types

type TEVarId = string

type TypedAst =
    | TEUnit
    | TEBool of bool
    | TEInt of int
    | TEFloat of float
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
        | TEFloat f -> string f
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
