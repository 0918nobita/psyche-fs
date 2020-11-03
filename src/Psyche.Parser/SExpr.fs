module internal SExpr

module BNel = Psyche.Base.Nel
module BOption = Psyche.Base.Option
module BResult = Psyche.Base.Result

open BNel.ActivePattern
open Psyche.Types
open AnnotatedAst

type Atom =
    | SBool of bool
    | SInt of int
    | SFloat of float
    | Symbol of string

    override this.ToString() =
        match this with
        | SBool true -> "true"
        | SBool false -> "false"
        | SInt n -> string n
        | SFloat f -> string f
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

let rec (|TypeSig|_|) =
    function
    | Atom(Symbol str) -> Map.tryFind str Type.StrMap
    | SList [ Atom(Symbol "->"); (TypeSig arg); (TypeSig ret) ] -> Some(TFun(arg, ret))
    | SList [ Atom(Symbol "Ref"); (TypeSig ty) ] -> Some(TRef ty)
    | _ -> None

let rec toAnnotatedAst sexpr =
    match sexpr with
    | SList [ Atom(Symbol "λ"); SList [ Atom(Symbol ":"); Atom(Symbol arg); (TypeSig ty) ];
              body ] ->
        BResult.result {
            let! body = toAnnotatedAst body
            return AFun(arg, ty, body)
        }
    | SList [ Atom(Symbol "if"); cond; _then; _else ] ->
        BResult.result {
            let! cond = toAnnotatedAst cond
            let! _then = toAnnotatedAst _then
            let! _else = toAnnotatedAst _else
            return AIf(cond, _then, _else)
        }
    | SList [ Atom(Symbol "let");
              SList [ Atom(Symbol ":"); Atom(Symbol name); (TypeSig ty) ]; value; body ] ->
        BResult.result {
            let! value = toAnnotatedAst value
            let! body = toAnnotatedAst body
            return ALet(name, ty, value, body)
        }
    | SList(Atom(Symbol "begin") :: x :: xs) ->
        BResult.result {
            let! x = toAnnotatedAst x
            let folder (state: Psyche.Base.Nel<AnnotatedAst>) (elem: SExpr) =
                let (Nel(head, tail)) = state
                BResult.result {
                    let! expr = toAnnotatedAst elem
                    return BNel.create head (tail @ [ expr ])
                }
            let! body = BResult.fold folder (BNel.singleton x) xs
            return ABegin body
        }
    | SList [ Atom(Symbol "ref"); content ] ->
        BResult.result {
            let! content = toAnnotatedAst content
            return AMakeRef content 
        }
    | SList [ Atom(Symbol "deref"); refExpr ] ->
        BResult.result {
            let! refExpr = toAnnotatedAst refExpr
            return ADeref refExpr
        }
    | SList [ Atom(Symbol "mut"); refSExp; sexp ] ->
        BResult.result {
            let! refExpr = toAnnotatedAst refSExp
            let! expr = toAnnotatedAst sexp
            return AMut(refExpr, expr)
        }
    | SList (x::xs) ->
        BResult.result {
            let! x = toAnnotatedAst x

            let folder innerAst sexpr = BResult.result {
                let! sexpr = toAnnotatedAst sexpr
                return AApp(innerAst, sexpr)
            }

            return! BResult.fold folder x xs
        }
    | SList [] -> Error(sprintf "bad syntax: %O" sexpr)
    | Atom(SBool b) -> Ok(ABool b)
    | Atom(SInt n) -> Ok(AInt n)
    | Atom(SFloat f) -> Ok(AFloat f)
    | Atom(Symbol "#unit") -> Ok AUnit
    | Atom(Symbol x) -> Ok(AVar x)
