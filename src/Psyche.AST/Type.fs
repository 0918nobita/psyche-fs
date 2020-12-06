module Psyche.AST.Type

open UntypedAst

type Type =
    | TUnit
    | TInt
    | TFloat
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
        | TFloat -> "Float"
        | TBool -> "Bool"
        | TFun(arg, ret) -> $"(-> {arg} {ret})"
        | TRef ty -> $"(Ref {ty})"

type TypeEnv =
    | TyEnv of (VarId * Type) list

[<RequireQualifiedAccess>]
module TypeEnv =
    let empty = TyEnv []

    let append varId ty (TyEnv env) = TyEnv ((varId, ty) :: env)

    let tryFind varId (TyEnv env) =
        env
        |> List.tryFind (fst >> (=) varId)
        |> Option.map snd
