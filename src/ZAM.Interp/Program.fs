module Program

module BNel = Base.Nel
module BResult = Base.Result
// open FSharp.Json
// open System.IO
open Type
open TypedExpr

[<EntryPoint>]
let main argv =
    let typedExpr =
        TELet
            ("double", TFun(TInt, TInt),
             TEFun("n", TInt, TEBinApp(TEMul, TEVar "n", TEInt 2)),
             TELet
                 ("x", TRef(TInt), TEMakeRef(TEInt 7),
                  TEBegin
                      (BNel.create (TEMut(TEVar "x", TEApp(TEVar "double", TEInt 4)))
                           [ TEDeref(TEVar "x") ])))
    printfn "TypedExpr: %O" typedExpr
    let res =
        BResult.result {
            let! (ty, untypedExpr) = typeCheck [] typedExpr
            printfn "UntypedExpr: %O" untypedExpr
            printfn "Type: %O" ty
            return! Runtime.eval [] untypedExpr
        }
    match res with
    | Ok v -> printfn "Result: %O" v
    | Error msg ->
        eprintfn "%s" msg
        exit 1

    (*
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

    *)
    0
