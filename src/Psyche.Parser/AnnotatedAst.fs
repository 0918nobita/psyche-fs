module internal AnnotatedAst

open Psyche.Types

type AVarId = string

type AnnotatedAst =
    | AUnit
    | ABool of bool
    | AInt of int
    | AFloat of float
    | AVar of AVarId
    | AFun of arg: AVarId * argType: Type * body: AnnotatedAst
    | AApp of func: AnnotatedAst * actualArg: AnnotatedAst
    | AIf of cond: AnnotatedAst * _then: AnnotatedAst * _else: AnnotatedAst
    | ALet of name: AVarId * typeOfName: Type * expr1: AnnotatedAst * expr2: AnnotatedAst
    | ABegin of Psyche.Base.Nel<AnnotatedAst>
    | AMakeRef of AnnotatedAst
    | ADeref of AnnotatedAst
    | AMut of AnnotatedAst * AnnotatedAst

    override this.ToString() =
        match this with
        | AUnit -> "#unit"
        | ABool true -> "true"
        | ABool false -> "false"
        | AInt n -> string n
        | AFloat f -> string f
        | AVar x -> x
        | AFun(arg, argType, body) -> sprintf "(λ (: %s %O) %O)" arg argType body
        | AApp(func, actualArg) -> sprintf "(%O %O)" func actualArg
        | AIf(cond, _then, _else) -> sprintf "(if %O %O %O)" cond _then _else
        | ALet(name, typeOfName, expr1, expr2) ->
            sprintf "(let (: %s %O) %O %O)" name typeOfName expr1 expr2
        | ABegin body ->
            let inner =
                body
                |> Seq.map string
                |> Seq.reduce (sprintf "%O %O")
            sprintf "(begin %s)" inner
        | AMakeRef expr -> sprintf "(ref %O)" expr
        | ADeref expr -> sprintf "(deref %O)" expr
        | AMut (expr1, expr2) -> sprintf "(mut %O %O)" expr1 expr2
