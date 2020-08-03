module Parser

module BMap = Base.Map

open FParsec
open SExpr
open UntypedExpr

let binOp: Parser<SExpr, unit> =
    BMap.keys BinOp.StrMap
    |> Seq.map pstring
    |> Seq.reduce (<|>)
    |>> Symbol
    |>> Atom

let ident: Parser<SExpr, unit> =
    let isAsciiIdStart c = isAsciiLetter c || c = '_' || c = 'λ' || c = '#'
    let isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_' || c = '-' || c = '\''
    (identifier <| IdentifierOptions(isAsciiIdStart, isAsciiIdContinue)) |>> Symbol
    |>> Atom

let intLiteral: Parser<SExpr, unit> = pint32 |>> SInt |>> Atom

let boolLiteral: Parser<SExpr, unit> =
    let ptrue = stringReturn "true" <| Atom(SBool true)
    let pfalse = stringReturn "false" <| Atom(SBool false)
    ptrue <|> pfalse

let atom = intLiteral <|> boolLiteral <|> binOp <|> ident

let rec sList(): Parser<SExpr, unit> =
    parse {
        do! skipChar '('
        let! head = expr()
        let! tail = many (spaces1 >>. expr())
        do! skipChar ')'
        return SList(head :: tail)
    }

and expr(): Parser<SExpr, unit> = atom <|> sList()

let program src =
    match run (atom <|> expr()) src with
    | Success(v, _, _) -> Result.Ok(v)
    | Failure(msg, _, _) -> Result.Error(msg)
