module Program

open System.IO

open ResultBuilder

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv then
        eprintfn "ファイル名が指定されていません"
        exit 1
    let filename = argv.[0]
    let src = File.ReadAllText filename

    let res =
        result {
            let! sexp = Parser.program src
            let! ast = SExp.sexpToExpr sexp
            return! Runtime.eval [] ast }

    match res with
    | Ok v -> printfn "Result: %O" v
    | Error msg ->
        eprintfn "%s" msg
        exit 1

    0
