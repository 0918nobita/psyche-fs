module UntypedExpr

type BinOp =
    | Add
    | Sub
    | Mul
    | Eq
    | Lt
    | Le

type VarId = string

type UntypedExpr =
    | UUnit
    | UBool of value: bool
    | UInt of value: int
    | UBinApp of op: BinOp * lhs: UntypedExpr * rhs: UntypedExpr
    | UVar of id: VarId
    | UFun of arg: VarId * body: UntypedExpr
    | UApp of func: UntypedExpr * actualArg: UntypedExpr
    | UIf of cond: UntypedExpr * _then: UntypedExpr * _else: UntypedExpr
    | ULet of VarId * UntypedExpr * UntypedExpr
    | UBegin of Base.Nel.Nel<UntypedExpr>
    | UMakeRef of UntypedExpr
    | UDeref of UntypedExpr
    | UMut of UntypedExpr * UntypedExpr
