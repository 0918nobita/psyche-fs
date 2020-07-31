module Parser

open SExp

open FParsec

let binOp: Parser<SExp, unit> =
    (pstring "+" <|> pstring "-" <|> pstring "*" <|> pstring "<"
     <|> pstring "<=") |>> Symbol |>> Atom

let ident: Parser<SExp, unit> =
    let isAsciiIdStart c = isAsciiLetter c || c = '_' || c = 'λ'
    let isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_' || c = '\''
    (identifier <| IdentifierOptions(isAsciiIdStart, isAsciiIdContinue))
    |>> Symbol |>> Atom

let intLiteral: Parser<SExp, unit> =
    parse {
        let! n = pint32
        return Atom(SInt n) }

let boolLiteral: Parser<SExp, unit> =
    let ptrue = pstring "true" >>% Atom(SBool true)
    let pfalse = pstring "false" >>% Atom(SBool false)
    ptrue <|> pfalse

let atom = intLiteral <|> boolLiteral <|> binOp <|> ident

let rec sList(): Parser<SExp, unit> =
    parse {
        do! skipChar '('
        let! head = expr()
        let! tail = many (spaces1 >>. expr())
        do! skipChar ')'
        return SList(head :: tail)
    }

and expr(): Parser<SExp, unit> = atom <|> sList()

let program src =
    match run (atom <|> expr()) src with
    | Success(v, _, _) -> Result.Ok(v)
    | Failure(msg, _, _) -> Result.Error(msg)
