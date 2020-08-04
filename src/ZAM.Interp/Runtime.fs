module Runtime

module BOption = Base.Option
module BResult = Base.Result

open Base.Nel.ActivePattern
open UntypedAst
open Value

let evalBinExpr op lhs rhs =
    match op, lhs, rhs with
    | Add, IntVal n1, IntVal n2 -> Ok(IntVal(n1 + n2))
    | Sub, IntVal n1, IntVal n2 -> Ok(IntVal(n1 - n2))
    | Mul, IntVal n1, IntVal n2 -> Ok(IntVal(n1 * n2))
    | Eq, IntVal n1, IntVal n2 -> Ok(BoolVal((n1 = n2)))
    | Lt, IntVal n1, IntVal n2 -> Ok(BoolVal(n1 < n2))
    | Le, IntVal n1, IntVal n2 -> Ok(BoolVal(n1 <= n2))
    | _ ->
        Error
        <| sprintf "2項演算子が用いられた式の評価に失敗しました:\n\t演算子: %O\n\t左辺: %O\n\t右辺: %O" op lhs rhs

let rec eval env expr =
    match expr with
    | UUnit -> Ok UnitVal
    | UBool b -> Ok(BoolVal b)
    | UInt n -> Ok(IntVal n)
    | UBinApp(op, lhs, rhs) ->
        BResult.result {
            let! lhs = eval env lhs
            let! rhs = eval env rhs
            return! evalBinExpr op lhs rhs }
    | UVar x -> evalVar env x
    | UFun(x, e) -> Ok(Closure(x, e, env))
    | UApp(func, arg) ->
        BResult.result {
            let! func = eval env func
            let! arg = eval env arg
            return! evalApp func arg }
    | UIf(cond, _then, _else) ->
        BResult.result {
            let! cond = eval env cond
            return! evalIfExpr env cond _then _else }
    | ULet(x, e1, e2) ->
        BResult.result {
            let! e1 = eval env e1
            let env = (x, e1) :: env
            return! eval env e2
        }
    | UBegin(Nel(x, xs)) ->
        BResult.result {
            let! x = eval env x
            let folder (_: Value) (elem: UntypedAst) = eval env elem
            return! BResult.fold folder x xs
        }
    | UMakeRef e ->
        BResult.result {
            let! v = eval env e
            return RefVal(ref v) }
    | UDeref e ->
        BResult.result {
            let! value = eval env e
            match value with
            | RefVal r -> return !r
            | _ ->
                return! Error
                            (sprintf "参照型ではない値に対して deref が呼び出されました: (deref %O ..)" value)
        }
    | UMut(refExpr, expr) ->
        BResult.result {
            let! refVal = eval env refExpr
            let! value = eval env expr
            match refVal with
            | RefVal r ->
                r := value
                return value
            | _ ->
                return! Error(sprintf "参照型ではない値に対して mut が呼び出されました: (mut %O ..)" refVal)
        }

and evalVar env varId =
    List.tryFind (fst >> (=) varId) env
    |> Option.map snd
    |> BOption.toResult
    |> Result.mapError (fun () -> sprintf "未束縛の名前を参照しました: %s" varId)

and evalIfExpr env cond _then _else =
    match cond with
    | BoolVal true -> eval env _then
    | BoolVal false -> eval env _else
    | _ -> Error "if 式の条件が真偽値ではありません"

and evalApp f v =
    match f with
    | Closure(arg, body, fenv) -> eval ((arg, v) :: fenv) body
    | _ -> Error "関数適用に失敗しました: 適用しようとしているものが関数ではありません"
