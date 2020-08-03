module Type

type TypeVarId = string

type Type =
    | TUnit
    | TInt
    | TBool
    | TVar of TypeVarId
    | TFun of Type * Type
    | Ref of Type

type TypeEnv = List<TypeVarId * Type>
