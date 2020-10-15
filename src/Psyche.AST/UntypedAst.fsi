namespace Psyche.AST

module UntypedAst =
    type BinOp =
        | AddI
        | AddF
        | SubI
        | SubF
        | MulI
        | MulF
        | DivI
        | DivF
        | Mod
        | Eq
        | Lt
        | Le

    type VarId = string

    type UntypedAst =
        | UUnit
        | UBool of value: bool
        | UInt of value: int
        | UFloat of value: float
        | UBinApp of op: BinOp * lhs: UntypedAst * rhs: UntypedAst
        | UVar of id: VarId
        | UFun of arg: VarId * body: UntypedAst
        | UIntOfFloat of UntypedAst
        | UFloatOfInt of UntypedAst
        | UApp of func: UntypedAst * actualArg: UntypedAst
        | UIf of cond: UntypedAst * _then: UntypedAst * _else: UntypedAst
        | ULet of VarId * UntypedAst * UntypedAst
        | UBegin of Psyche.Base.Nel<UntypedAst>
        | UMakeRef of UntypedAst
        | UDeref of UntypedAst
        | UMut of UntypedAst * UntypedAst
