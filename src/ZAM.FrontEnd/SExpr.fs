namespace FrontEnd

module SExpr =
    module BNel = Base.Nel
    module BOption = Base.Option
    module BResult = Base.Result

    open BNel.ActivePattern
    open Type
    open TypedAst

    type Atom =
        | SBool of bool
        | SInt of int
        | Symbol of string

        override this.ToString() =
            match this with
            | SBool true -> "true"
            | SBool false -> "false"
            | SInt n -> string n
            | Symbol s -> s

    type SExpr =
        | Atom of Atom
        | SList of SExpr list

        override this.ToString() =
            match this with
            | Atom a -> string a
            | SList [] -> "()"
            | SList(x :: xs) ->
                let inner = List.map string xs |> List.fold (sprintf "%s %s") (string x)
                sprintf "(%s)" inner

    let (|BinOp|_|) str = Map.tryFind str TEBinOp.StrMap

    let rec (|TypeSig|_|) =
        function
        | Atom(Symbol str) -> Map.tryFind str Type.StrMap
        | SList [ Atom(Symbol "->"); (TypeSig arg); (TypeSig ret) ] -> Some(TFun(arg, ret))
        | SList [ Atom(Symbol "Ref"); (TypeSig ty) ] -> Some(TRef ty)
        | _ -> None

    let rec toExpr sexpr =
        match sexpr with
        | SList [ Atom(Symbol(BinOp op)); lhs; rhs ] ->
            BResult.result {
                let! lhs = toExpr lhs
                let! rhs = toExpr rhs
                return TEBinApp(op, lhs, rhs) }
        | SList [ Atom(Symbol "λ"); SList [ Atom(Symbol ":"); Atom(Symbol arg); (TypeSig ty) ];
                  body ] ->
            BResult.result {
                let! body = toExpr body
                return TEFun(arg, ty, body) }
        | SList [ Atom(Symbol "if"); cond; _then; _else ] ->
            BResult.result {
                let! cond = toExpr cond
                let! _then = toExpr _then
                let! _else = toExpr _else
                return TEIf(cond, _then, _else) }
        | SList [ Atom(Symbol "let");
                  SList [ Atom(Symbol ":"); Atom(Symbol name); (TypeSig ty) ]; value; body ] ->
            BResult.result {
                let! value = toExpr value
                let! body = toExpr body
                return TELet(name, ty, value, body) }
        | SList(Atom(Symbol "begin") :: x :: xs) ->
            BResult.result {
                let! x = toExpr x
                let folder (state: BNel.Nel<TypedAst>) (elem: SExpr) =
                    let (Nel(head, tail)) = state
                    BResult.result {
                        let! expr = toExpr elem
                        return BNel.create head (tail @ [ expr ]) }
                let! body = BResult.fold folder (BNel.singleton x) xs
                return TEBegin(body)
            }
        | SList [ Atom(Symbol "ref"); content ] ->
            BResult.result {
                let! content = toExpr content
                return TEMakeRef content }
        | SList [ Atom(Symbol "deref"); refExpr ] ->
            BResult.result {
                let! refExpr = toExpr refExpr
                return TEDeref(refExpr) }
        | SList [ Atom(Symbol "mut"); refSExp; sexp ] ->
            BResult.result {
                let! refExpr = toExpr refSExp
                let! expr = toExpr sexp
                return TEMut(refExpr, expr) }
        | SList [ func; arg ] ->
            BResult.result {
                let! func = toExpr func
                let! arg = toExpr arg
                return TEApp(func, arg) }
        | SList _ -> Error(sprintf "bad syntax: %O" sexpr)
        | Atom(SBool b) -> Ok(TEBool b)
        | Atom(SInt n) -> Ok(TEInt n)
        | Atom(Symbol "#unit") -> Ok TEUnit
        | Atom(Symbol x) -> Ok(TEVar x)
