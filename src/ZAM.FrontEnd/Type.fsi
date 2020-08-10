namespace FrontEnd

module Type =
    open UntypedAst

    type Type =
        | TUnit
        | TInt
        | TFloat
        | TBool
        | TFun of Type * Type
        | TRef of Type

        static member StrMap : Map<string, Type>

    type TypeEnv

    module TypeEnv =
        val empty : TypeEnv

        val append : VarId -> Type -> TypeEnv -> TypeEnv

        val tryFind : VarId -> TypeEnv -> Option<Type>
