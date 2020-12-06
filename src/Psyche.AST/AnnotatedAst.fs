module Psyche.AST.AnnotatedAst

open Type

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
    | ALet of name: AVarId * typeSig: Type * expr1: AnnotatedAst * expr2: AnnotatedAst
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
        | AFun(arg, argType, body) -> $"(λ (: {arg} {argType}) {body})"
        | AApp(func, actualArg) -> $"({func} {actualArg})"
        | AIf(cond, _then, _else) -> $"(if {cond} {_then} {_else})"
        | ALet(name, typeSig, expr1, expr2) ->
            $"(let (: {name} {typeSig}) {expr1} {expr2})"
        | ABegin body ->
            let inner =
                body
                |> Seq.map string
                |> Seq.reduce (sprintf "%O %O")
            $"(begin {inner})"
        | AMakeRef expr -> $"(ref {expr})"
        | ADeref expr -> $"(deref {expr})"
        | AMut (expr1, expr2) -> $"(mut {expr1} {expr2})"
