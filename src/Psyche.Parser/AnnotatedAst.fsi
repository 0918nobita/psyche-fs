module internal AnnotatedAst

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
    | TEBegin of Psyche.Base.Nel<AnnotatedAst>
    | TEMakeRef of AnnotatedAst
    | TEDeref of AnnotatedAst
    | TEMut of AnnotatedAst * AnnotatedAst
