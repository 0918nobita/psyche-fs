module TypedExpr

open Type

type TEVarId = string

type TEBinOp =
    | TEAdd
    | TESub
    | TEMul
    | TEEq
    | TELt
    | TELe

    static member StrMap : Map<string, TEBinOp>

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
    | TEBegin of Base.Nel.Nel<TypedExpr>
    | TEMakeRef of TypedExpr
    | TEDeref of TypedExpr
    | TEMut of TypedExpr * TypedExpr
