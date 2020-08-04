module Type

type Type =
    | TUnit
    | TInt
    | TBool
    | TFun of Type * Type
    | TRef of Type

    static member StrMap : Map<string, Type>

type TypeEnv = List<UntypedExpr.VarId * Type>
