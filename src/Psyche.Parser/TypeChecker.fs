module internal TypeChecker

module BNel = Psyche.Base.Nel
module BOption = Psyche.Base.Option
module BResult = Psyche.Base.Result

open BNel.ActivePattern
open Psyche.Types
open Psyche.AST.UntypedAst
open AnnotatedAst

let assertType (expected: Type) (actual: Type) =
    if expected = actual
    then Ok()
    else Error(sprintf "expected: %O, actual: %O" expected actual)

let rec typeCheck env =
    function
    | AUnit -> Ok(TUnit, UUnit)
    | ABool b -> Ok(TBool, UBool b)
    | AInt n -> Ok(TInt, UInt n)
    | AFloat f -> Ok(TFloat, UFloat f)
    | AVar x -> typeCheckVar env x
    | AFun(x, ty, body) -> typeCheckFun env x ty body
    | AApp(func, arg) -> typeCheckApp env func arg
    | AIf(cond, _then, _else) -> typeCheckIf env cond _then _else
    | ALet(x, ty, e1, e2) -> typeCheckLet env x ty e1 e2
    | ABegin body -> typeCheckBegin env body
    | AMakeRef expr -> typeCheckMakeRef env expr
    | ADeref expr -> typeCheckDeref env expr
    | AMut(refExpr, expr) -> typeCheckMut env refExpr expr

and typeCheckVar env x =
    BResult.result {
        let! ty = TypeEnv.tryFind x env
                  |> BOption.toResult
                  |> Result.mapError
                     (fun () -> sprintf "(TypeError) Unbound identifier: %s" x)
        return (ty, UVar x)
    }

and typeCheckFun env x ty body =
    BResult.result {
        let! (bodyType, body) = typeCheck (TypeEnv.append x ty env) body
        return (TFun(ty, bodyType), UFun(x, body)) }

and typeCheckApp env func arg =
    let mapError (r: Result<'a, string>) =
        r
        |> Result.mapError (sprintf "(TypeError) in function application:\n  %s")
    BResult.result {
        let! (funcType, func) = typeCheck env func
        let! (argType, arg) = typeCheck env arg
        match funcType with
        | TFun(a, b) ->
            do! mapError (assertType a argType)
            return (b, UApp(func, arg))
        | _ -> return! mapError (Error(sprintf "cannot call %O: %O" func funcType))
    }

and typeCheckLet env x ty e1 e2 =
    let mapError = Result.mapError (sprintf "(TypeError) in let expression:  %O")
    BResult.result {
        let! (e1Type, e1) = typeCheck env e1
        do! mapError (assertType ty e1Type)
        let! (e2Type, e2) = typeCheck (TypeEnv.append x ty env) e2
        return (e2Type, ULet(x, e1, e2))
    }

and typeCheckIf env cond _then _else =
    let mapError =
        Result.mapError (sprintf "(TypeError) in if expression\n  %O")
    BResult.result {
        let! (condType, cond) = typeCheck env cond
        do! mapError (assertType TBool condType)
        let! (_thenType, _then) = typeCheck env _then
        let! (_elseType, _else) = typeCheck env _else
        do! BResult.result {
            if _thenType = _elseType
            then return ()
            else return! Error
                    (sprintf
                        "type mismatch\n    then clause: %O\n    else clause: %O"
                        _thenType
                        _elseType)
            }
            |> mapError
        return (_thenType, UIf(cond, _then, _else))
    }

and typeCheckBegin env (Nel(head, tail)) =
    BResult.result {
        let! (ty, head) = typeCheck env head

        let folder (state: Type * Psyche.Base.Nel<UntypedAst>) (elem: AnnotatedAst) =
            let (_, Nel(x, xs)) = state
            BResult.result {
                let! (ty, expr) = typeCheck env elem
                return (ty, BNel.create x (xs @ [ expr ])) }
        let! (ty, body) = BResult.fold folder (ty, BNel.singleton head) tail

        return (ty, UBegin body)
    }

and typeCheckMakeRef env expr =
    BResult.result {
        let! (exprType, expr) = typeCheck env expr
        return (TRef exprType, UMakeRef expr) }

and typeCheckDeref env expr =
    BResult.result {
        let! (exprType, expr) = typeCheck env expr
        match exprType with
        | TRef ty -> return (ty, UDeref expr)
        | _ ->
            return! Error
                        (sprintf
                            "(TypeError) in deref expression:\n  type %O is not reference type"
                             exprType)
    }

and typeCheckMut env refExpr expr =
    let errMsgPrefix = "(TypeError) in mut expression:\n  "
    BResult.result {
        let! (refExprType, refExpr) = typeCheck env refExpr
        match refExprType with
        | TRef(ty) ->
            let! (exprType, expr) = typeCheck env expr
            do! Result.mapError (sprintf "%s%s" errMsgPrefix) (assertType ty exprType)
            return (ty, UMut(refExpr, expr))
        | _ ->
            return! Error
                        (sprintf "%stype %O is not refernece type" errMsgPrefix
                             refExprType)
    }
