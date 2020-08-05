namespace FrontEnd

module TypedAst =
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

    type TypedAst =
        | TEUnit
        | TEBool of bool
        | TEInt of int
        | TEBinApp of op: TEBinOp * lhs: TypedAst * rhs: TypedAst
        | TEVar of TEVarId
        | TEFun of arg: TEVarId * argType: Type * body: TypedAst
        | TEApp of func: TypedAst * actualArg: TypedAst
        | TEIf of cond: TypedAst * _then: TypedAst * _else: TypedAst
        | TELet of name: TEVarId * typeOfName: Type * expr1: TypedAst * expr2: TypedAst
        | TEBegin of Base.Nel.Nel<TypedAst>
        | TEMakeRef of TypedAst
        | TEDeref of TypedAst
        | TEMut of TypedAst * TypedAst
