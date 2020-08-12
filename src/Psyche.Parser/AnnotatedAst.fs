module internal AnnotatedAst

module Base = Psyche.Base
module BNel = Base.Nel
module BOption = Base.Option
module BResult = Base.Result

open Psyche.Types

type TEVarId = string

type AnnotatedAst =
    | TEUnit
    | TEBool of bool
    | TEInt of int
    | TEFloat of float
    | TEVar of TEVarId
    | TEFun of arg: TEVarId * argType: Type * body: AnnotatedAst
    | TEApp of func: AnnotatedAst * actualArg: AnnotatedAst
    | TEIf of cond: AnnotatedAst * _then: AnnotatedAst * _else: AnnotatedAst
    | TELet of name: TEVarId * typeOfName: Type * expr1: AnnotatedAst * expr2: AnnotatedAst
    | TEBegin of Base.Nel<AnnotatedAst>
    | TEMakeRef of AnnotatedAst
    | TEDeref of AnnotatedAst
    | TEMut of AnnotatedAst * AnnotatedAst

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
