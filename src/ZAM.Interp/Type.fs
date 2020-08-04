module Type

type TypeVarId = string

type Type =
    | TUnit
    | TInt
    | TBool
    | TFun of Type * Type
    | TRef of Type

type TypeEnv = List<TypeVarId * Type>
