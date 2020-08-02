module Program

module BResult = Base.Result
open FSharp.Json
open System.IO

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv then
        eprintfn "ファイル名が指定されていません"
        exit 1
    let srcPath = argv.[0]
    let src = File.ReadAllText srcPath

    let res =
        BResult.result {
            let! sexp = Parser.program src

            let sw = new StreamWriter(Path.ChangeExtension(srcPath, ".sexp.json"))
            sw.WriteLine(Json.serializeU sexp)
            sw.Close()

            let! ast = sexp.ToExpr()

            let sw = new StreamWriter(Path.ChangeExtension(srcPath, ".expr.json"))
            sw.WriteLine(Json.serializeU ast)
            sw.Close()

            return! Runtime.eval [] ast }

    match res with
    | Ok v -> printfn "Result: %O" v
    | Error msg ->
        eprintfn "%s" msg
        exit 1

    0
