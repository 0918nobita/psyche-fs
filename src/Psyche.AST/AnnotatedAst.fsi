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
