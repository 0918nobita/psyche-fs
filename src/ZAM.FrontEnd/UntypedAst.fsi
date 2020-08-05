namespace FrontEnd

module UntypedAst =
    type BinOp =
        | Add
        | Sub
        | Mul
        | Eq
        | Lt
        | Le

    type VarId = string

    type UntypedAst =
        | UUnit
        | UBool of value: bool
        | UInt of value: int
        | UBinApp of op: BinOp * lhs: UntypedAst * rhs: UntypedAst
        | UVar of id: VarId
        | UFun of arg: VarId * body: UntypedAst
        | UApp of func: UntypedAst * actualArg: UntypedAst
        | UIf of cond: UntypedAst * _then: UntypedAst * _else: UntypedAst
        | ULet of VarId * UntypedAst * UntypedAst
        | UBegin of Base.Nel.Nel<UntypedAst>
        | UMakeRef of UntypedAst
        | UDeref of UntypedAst
        | UMut of UntypedAst * UntypedAst
