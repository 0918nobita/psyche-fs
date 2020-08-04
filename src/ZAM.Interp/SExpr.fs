module SExpr

module BNel = Base.Nel

open BNel.ActivePattern

module BResult = Base.Result

open UntypedExpr

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

let (|BinOp|_|) str = Map.tryFind str BinOp.StrMap

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

    member this.ToExpr(): Result<UntypedExpr, string> =
        match this with
        | SList [ Atom(Symbol(BinOp op)); lhs; rhs ] ->
            BResult.result {
                let! lhs = lhs.ToExpr()
                let! rhs = rhs.ToExpr()
                return UBinApp(op, lhs, rhs) }
        | SList [ Atom(Symbol "λ"); Atom(Symbol arg); body ] ->
            BResult.result {
                let! body = body.ToExpr()
                return UFun(arg, body) }
        | SList [ Atom(Symbol "if"); cond; _then; _else ] ->
            BResult.result {
                let! cond = cond.ToExpr()
                let! _then = _then.ToExpr()
                let! _else = _else.ToExpr()
                return UIf(cond, _then, _else) }
        | SList [ Atom(Symbol "let"); Atom(Symbol name); value; body ] ->
            BResult.result {
                let! value = value.ToExpr()
                let! body = body.ToExpr()
                return ULet(name, value, body) }
        | SList(Atom(Symbol "begin") :: x :: xs) ->
            BResult.result {
                let! x = x.ToExpr()
                let folder (state: BNel.Nel<UntypedExpr>) (elem: SExpr) =
                    let (Nel(head, tail)) = state
                    BResult.result {
                        let! expr = elem.ToExpr()
                        return BNel.create head (tail @ [ expr ]) }
                let! body = BResult.fold folder (BNel.singleton x) xs
                return UBegin(body)
            }
        | SList [ Atom(Symbol "ref"); content ] ->
            BResult.result {
                let! content = content.ToExpr()
                return UMakeRef content }
        | SList [ Atom(Symbol "deref"); refExpr ] ->
            BResult.result {
                let! refExpr = refExpr.ToExpr()
                return UDeref(refExpr) }
        | SList [ Atom(Symbol "mut"); refSExp; sexp ] ->
            BResult.result {
                let! refExpr = refSExp.ToExpr()
                let! expr = sexp.ToExpr()
                return UMut(refExpr, expr) }
        | SList [ func; arg ] ->
            BResult.result {
                let! func = func.ToExpr()
                let! arg = arg.ToExpr()
                return UApp(func, arg) }
        | SList _ -> Error(sprintf "bad syntax: %O" this)
        | Atom(SBool b) -> Ok(UBool b)
        | Atom(SInt n) -> Ok(UInt n)
        | Atom(Symbol "#unit") -> Ok UUnit
        | Atom(Symbol x) -> Ok(UVar x)
