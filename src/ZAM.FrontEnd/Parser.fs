module internal Parser

module BMap = Base.Map

open FParsec
open SExpr

let ident =
    let start = anyOf "+-*_λ<=>#:\'" <|> asciiLetter
    let cont = start <|> digit
    parse {
        let! c = start |>> string
        let! cs = manyChars cont
        return Atom(Symbol(c + cs)) }

let intLiteral = pint32 |>> SInt |>> Atom

let intOrFloatLiteral =
    let intLiteral = pint32 |>> SInt |>> Atom
    let floatLiteral =
        parse {
            let! f = pfloat
            do! skipChar 'f'
            return Atom(SFloat f)
        }
        |> attempt
    floatLiteral <|> intLiteral

let boolLiteral =
    let ptrue = stringReturn "true" <| Atom(SBool true)
    let pfalse = stringReturn "false" <| Atom(SBool false)
    ptrue <|> pfalse

let atom = intOrFloatLiteral <|> boolLiteral <|> ident

let rec sList() =
    parse {
        do! skipChar '('
        let! head = expr()
        let! tail = many (spaces1 >>. expr())
        do! skipChar ')'
        return SList(head :: tail)
    }

and expr() = atom <|> sList()

let program src =
    match run (atom <|> expr()) src with
    | Success(v, _, _) -> Result.Ok(v)
    | Failure(msg, _, _) -> Result.Error(msg)
