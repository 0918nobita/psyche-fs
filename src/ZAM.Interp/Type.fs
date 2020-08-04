module Type

type TypeVarId = string

type Type =
    | TUnit
    | TInt
    | TBool
    | TFun of Type * Type
    | TRef of Type

    static member StrMap =
        Map.ofArray
            [| ("Unit", TUnit)
               ("Int", TInt)
               ("Bool", TBool) |]

    override this.ToString() =
        match this with
        | TUnit -> "Unit"
        | TInt -> "Int"
        | TBool -> "Bool"
        | TFun(arg, ret) -> sprintf "(-> %O %O)" arg ret
        | TRef ty -> sprintf "(Ref %O)" ty

type TypeEnv = List<TypeVarId * Type>
