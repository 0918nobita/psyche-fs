module SExp

open Syntax

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

let binOpMap = Map.ofList [("+", Add); ("-", Sub); ("*", Mul); ("=", Eq); ("<", Lt); ("<=", Le)]

let (|BinOp|_|) str = Map.tryFind str binOpMap

open FSharpPlus.Builders

type SExp =
    | Atom of Atom
    | SList of SExp list

    override this.ToString() =
        match this with
        | Atom a -> string a
        | SList [] -> "()"
        | SList (x::xs) ->
            let inner =
                List.map string xs
                |> List.fold (sprintf "%s %s") (string x)
            sprintf "(%s)" inner

    member this.ToExpr(): Result<Expr, string> =
        match this with
        | SList [ Atom(Symbol(BinOp op)); lhs; rhs ] ->
            monad.fx' {
                let! lhs = lhs.ToExpr()
                let! rhs = rhs.ToExpr()
                return BinApp(op, lhs, rhs) }
        | SList [ Atom(Symbol "λ"); Atom(Symbol arg); body ] ->
            monad.fx' {
                let! body = body.ToExpr()
                return Fun(arg, body) }
        | SList [ Atom(Symbol "if"); cond; _then; _else ] ->
            monad.fx' {
                let! cond = cond.ToExpr()
                let! _then = _then.ToExpr()
                let! _else = _else.ToExpr()
                return If(cond, _then, _else) }
        | SList [ Atom(Symbol "let"); Atom(Symbol name); value; body ] ->
            monad.fx' {
                let! value = value.ToExpr()
                let! body = body.ToExpr()
                return Let(name, value, body) }
        | SList [ func; arg ] ->
            monad.fx' {
                let! func = func.ToExpr()
                let! arg = arg.ToExpr()
                return App(func, arg) }
        | SList _ -> Error(sprintf "bad syntax: %O" this)
        | Atom(SBool b) -> Ok(Bool(b))
        | Atom(SInt n) -> Ok(Int(n))
        | Atom(Symbol x) -> Ok(Var(x))
