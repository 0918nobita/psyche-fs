module internal SExpr

module BNel = Base.Nel
module BOption = Base.Option
module BResult = Base.Result

open BNel.ActivePattern
open FrontEnd.Type
open TypedAst

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

let (|BinOp|_|) str = Map.tryFind str TEBinOp.StrMap

let rec (|TypeSig|_|) =
    function
    | Atom(Symbol str) -> Map.tryFind str Type.StrMap
    | SList [ Atom(Symbol "->"); (TypeSig arg); (TypeSig ret) ] -> Some(TFun(arg, ret))
    | SList [ Atom(Symbol "Ref"); (TypeSig ty) ] -> Some(TRef ty)
    | _ -> None

let rec toTypedAst sexpr =
    match sexpr with
    | SList [ Atom(Symbol(BinOp op)); lhs; rhs ] ->
        BResult.result {
            let! lhs = toTypedAst lhs
            let! rhs = toTypedAst rhs
            return TEBinApp(op, lhs, rhs) }
    | SList [ Atom(Symbol "λ"); SList [ Atom(Symbol ":"); Atom(Symbol arg); (TypeSig ty) ];
              body ] ->
        BResult.result {
            let! body = toTypedAst body
            return TEFun(arg, ty, body) }
    | SList [ Atom(Symbol "if"); cond; _then; _else ] ->
        BResult.result {
            let! cond = toTypedAst cond
            let! _then = toTypedAst _then
            let! _else = toTypedAst _else
            return TEIf(cond, _then, _else) }
    | SList [ Atom(Symbol "let");
              SList [ Atom(Symbol ":"); Atom(Symbol name); (TypeSig ty) ]; value; body ] ->
        BResult.result {
            let! value = toTypedAst value
            let! body = toTypedAst body
            return TELet(name, ty, value, body) }
    | SList(Atom(Symbol "begin") :: x :: xs) ->
        BResult.result {
            let! x = toTypedAst x
            let folder (state: BNel.Nel<TypedAst>) (elem: SExpr) =
                let (Nel(head, tail)) = state
                BResult.result {
                    let! expr = toTypedAst elem
                    return BNel.create head (tail @ [ expr ]) }
            let! body = BResult.fold folder (BNel.singleton x) xs
            return TEBegin(body)
        }
    | SList [ Atom(Symbol "int-of-float"); f ] ->
        BResult.result {
            let! f = toTypedAst f
            return TEIntOfFloat f }
    | SList [ Atom(Symbol "float-of-int"); n ] ->
        BResult.result {
            let! n = toTypedAst n
            return TEFloatOfInt n }
    | SList [ Atom(Symbol "ref"); content ] ->
        BResult.result {
            let! content = toTypedAst content
            return TEMakeRef content }
    | SList [ Atom(Symbol "deref"); refExpr ] ->
        BResult.result {
            let! refExpr = toTypedAst refExpr
            return TEDeref(refExpr) }
    | SList [ Atom(Symbol "mut"); refSExp; sexp ] ->
        BResult.result {
            let! refExpr = toTypedAst refSExp
            let! expr = toTypedAst sexp
            return TEMut(refExpr, expr) }
    | SList [ func; arg ] ->
        BResult.result {
            let! func = toTypedAst func
            let! arg = toTypedAst arg
            return TEApp(func, arg) }
    | SList _ -> Error(sprintf "bad syntax: %O" sexpr)
    | Atom(SBool b) -> Ok(TEBool b)
    | Atom(SInt n) -> Ok(TEInt n)
    | Atom(SFloat f) -> Ok(TEFloat f)
    | Atom(Symbol "#unit") -> Ok TEUnit
    | Atom(Symbol x) -> Ok(TEVar x)
