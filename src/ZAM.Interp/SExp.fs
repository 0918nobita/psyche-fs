module SExp

module BResult = Base.Result
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

let (|BinOp|_|) str = Map.tryFind str BinOp.StrMap

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
            BResult.result {
                let! lhs = lhs.ToExpr()
                let! rhs = rhs.ToExpr()
                return BinApp(op, lhs, rhs) }
        | SList [ Atom(Symbol "λ"); Atom(Symbol arg); body ] ->
            BResult.result {
                let! body = body.ToExpr()
                return Fun(arg, body) }
        | SList [ Atom(Symbol "if"); cond; _then; _else ] ->
            BResult.result {
                let! cond = cond.ToExpr()
                let! _then = _then.ToExpr()
                let! _else = _else.ToExpr()
                return If(cond, _then, _else) }
        | SList [ Atom(Symbol "let"); Atom(Symbol name); value; body ] ->
            BResult.result {
                let! value = value.ToExpr()
                let! body = body.ToExpr()
                return Let(name, value, body) }
        | SList (Atom(Symbol "begin") :: x :: xs) ->
            List.fold
                (fun (acc: Result<Expr * List<Expr>, string>) (elem: SExp) ->
                    acc
                    |> Result.bind (fun (head, tail) ->
                        elem.ToExpr()
                        |> Result.bind (fun expr -> Ok (head, tail @ [expr]))))
                (Result.map (fun e -> (e, [])) (x.ToExpr()))
                xs
            |> Result.map (fun (head, tail) -> Begin (head, tail))
        | SList [Atom (Symbol "set!"); Atom(Symbol name); value] ->
            BResult.result {
                let! value = value.ToExpr()
                return Setf(name, value)
            }
        | SList [ func; arg ] ->
            BResult.result {
                let! func = func.ToExpr()
                let! arg = arg.ToExpr()
                return App(func, arg) }
        | SList _ -> Error(sprintf "bad syntax: %O" this)
        | Atom(SBool b) -> Ok(Bool(b))
        | Atom(SInt n) -> Ok(Int(n))
        | Atom(Symbol x) -> Ok(Var(x))
