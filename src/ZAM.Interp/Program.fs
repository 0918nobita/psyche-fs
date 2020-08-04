module Program

module BNel = Base.Nel
module BResult = Base.Result

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
            let! typedAst = SExpr.toExpr sexp
            let! (ty, untypedAst) = TypeChecker.typeCheck [] typedAst
            let! value = Runtime.eval [] untypedAst
            return (ty, value) }

    match res with
    | Ok(ty, value) ->
        printfn "Static type: %O\nResult: %O" ty value
    | Error msg ->
        eprintfn "%s" msg
        exit 1
    0
