module Program

open System.IO

open FSharpPlus.Builders

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv then
        eprintfn "ファイル名が指定されていません"
        exit 1
    let filename = argv.[0]
    let src = File.ReadAllText filename

    let res =
        monad.fx' {
            let! sexp = Parser.program src
            printfn "SExp: %O" sexp
            let! ast = sexp.ToExpr()
            return! Runtime.eval [] ast }

    match res with
    | Ok v -> printfn "Result: %O" v
    | Error msg ->
        eprintfn "%s" msg
        exit 1

    0
