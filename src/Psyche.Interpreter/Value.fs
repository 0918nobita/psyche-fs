module Value

module UntypedAst = Psyche.UntypedAst

type Value =
    | UnitVal
    | Closure of UntypedAst.VarId * UntypedAst.UntypedAst * Env
    | BoolVal of bool
    | IntVal of int
    | FloatVal of float
    | RefVal of Ref<Value>

    override this.ToString() =
        match this with
        | UnitVal -> "#unit"
        | BoolVal true -> "true"
        | BoolVal false -> "false"
        | IntVal n -> string n
        | FloatVal f -> string f
        | Closure _ -> "<Closure>"
        | RefVal r -> sprintf "<Ref %O>" !r

and Env =
    private
    | Env of List<UntypedAst.VarId * Value>

module Env =
    let empty = Env []

    let append varId value (Env env) = Env ((varId, value) :: env)

    let tryFind varId (Env env) =
        env
        |> List.tryFind (fst >> (=) varId)
        |> Option.map snd
