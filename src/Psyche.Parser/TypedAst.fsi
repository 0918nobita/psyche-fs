module internal TypedAst

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
    | TEBegin of Psyche.Base.Nel.Nel<TypedAst>
    | TEMakeRef of TypedAst
    | TEDeref of TypedAst
    | TEMut of TypedAst * TypedAst
