module Value

open Psyche.AST.UntypedAst

type Value =
    | UnitVal
    | Closure of VarId * UntypedAst * Env
    | BoolVal of bool
    | IntVal of int
    | FloatVal of float
    | RefVal of Value ref

and Env

module Env =
    val empty : Env

    val append : VarId -> Value -> Env -> Env

    val tryFind : VarId -> Env -> Value option
