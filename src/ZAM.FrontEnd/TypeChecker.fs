module internal TypeChecker

module BNel = Base.Nel
module BOption = Base.Option
module BResult = Base.Result

open BNel.ActivePattern
open FrontEnd.Type
open FrontEnd.UntypedAst
open TypedAst

let assertType (expected: Type) (actual: Type) =
    if expected = actual
    then Ok()
    else Error(sprintf "expected: %O, actual: %O" expected actual)

let rec typeCheck env =
    function
    | TEUnit -> Ok(TUnit, UUnit)
    | TEBool b -> Ok(TBool, UBool b)
    | TEInt n -> Ok(TInt, UInt n)
    | TEBinApp(op, lhs, rhs) -> typeCheckBinApp env op lhs rhs
    | TEVar x -> typeCheckVar env x
    | TEFun(x, ty, body) -> typeCheckFun env x ty body
    | TEApp(func, arg) -> typeCheckApp env func arg
    | TEIf(cond, _then, _else) -> typeCheckIf env cond _then _else
    | TELet(x, ty, e1, e2) -> typeCheckLet env x ty e1 e2
    | TEBegin(body) -> typeCheckBegin env body
    | TEMakeRef(expr) -> typeCheckMakeRef env expr
    | TEDeref(expr) -> typeCheckDeref env expr
    | TEMut(refExpr, expr) -> typeCheckMut env refExpr expr

and typeCheckBinApp env op lhs rhs =
    let mapError =
        Result.mapError (sprintf "(TypeError) in binary expression:\n  %O")
    BResult.result {
        let! (lhsType, lhs) = typeCheck env lhs
        let! (rhsType, rhs) = typeCheck env rhs
        match op with
        | TEAdd ->
            do! mapError (assertType TInt lhsType)
            do! mapError (assertType TInt rhsType)
            return (TInt, UBinApp(Add, lhs, rhs))
        | TESub ->
            do! mapError (assertType TInt lhsType)
            do! mapError (assertType TInt rhsType)
            return (TInt, UBinApp(Sub, lhs, rhs))
        | TEMul ->
            do! mapError (assertType TInt lhsType)
            do! mapError (assertType TInt rhsType)
            return (TInt, UBinApp(Mul, lhs, rhs))
        | TEEq ->
            do! mapError (assertType TInt lhsType)
            do! mapError (assertType TInt rhsType)
            return (TBool, UBinApp(Eq, lhs, rhs))
        | TELt ->
            do! mapError (assertType TInt lhsType)
            do! mapError (assertType TInt rhsType)
            return (TBool, UBinApp(Lt, lhs, rhs))
        | TELe ->
            do! mapError (assertType TInt lhsType)
            do! mapError (assertType TInt rhsType)
            return (TBool, UBinApp(Le, lhs, rhs))
    }

and typeCheckVar env x =
    BResult.result {
        let! ty = List.tryFind (fst >> (=) x) env
                  |> Option.map snd
                  |> BOption.toResult
                  |> Result.mapError
                     (fun () -> sprintf "(TypeError) Unbound identifier: %s" x)
        return (ty, UVar x)
    }

and typeCheckFun env x ty body =
    BResult.result {
        let! (bodyType, body) = typeCheck ((x, ty) :: env) body
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
        let! (e2Type, e2) = typeCheck ((x, ty) :: env) e2
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
        do! mapError
                (BResult.result {
                    if _thenType = _elseType then
                        return ()
                    else
                        return! Error
                                    (sprintf
                                        "type mismatch\n    then clause: %O\n    else clause: %O"
                                         _thenType _elseType)
                })
        return (_thenType, UIf(cond, _then, _else))
    }

and typeCheckBegin env (Nel(head, tail)) =
    BResult.result {
        let! (ty, head) = typeCheck env head

        let folder (state: Type * BNel.Nel<UntypedAst>) (elem: TypedAst) =
            let (_, Nel(x, xs)) = state
            BResult.result {
                let! (ty, expr) = typeCheck env elem
                return (ty, BNel.create x (xs @ [ expr ])) }
        let! (ty, body) = BResult.fold folder (ty, BNel.singleton head) tail

        return (ty, UBegin(body))
    }

and typeCheckMakeRef env expr =
    BResult.result {
        let! (exprType, expr) = typeCheck env expr
        return (TRef(exprType), UMakeRef(expr)) }

and typeCheckDeref env expr =
    BResult.result {
        let! (exprType, expr) = typeCheck env expr
        match exprType with
        | TRef(ty) -> return (ty, UDeref(expr))
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
