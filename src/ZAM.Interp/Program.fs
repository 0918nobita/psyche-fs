module Program

module BNel = Base.Nel
module BResult = Base.Result
module Parser = FrontEnd.Parser
module SExpr = FrontEnd.SExpr
module TypeChecker = FrontEnd.TypeChecker

open System.IO

let run src =
    BResult.result {
        let! sexp = Parser.program src
        let! typedAst = SExpr.toExpr sexp
        let! (ty, untypedAst) = TypeChecker.typeCheck [] typedAst
        let! value = Runtime.eval [] untypedAst
        return (ty, value) }

let interactive () =
    fun _ ->
        printf "> "
        System.Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.takeWhile (fun input -> not(isNull(input)) && input <> ":exit")
    |> Seq.iter (fun src ->
        match (run src) with
        | Ok(ty, value) ->
            printfn "Static type: %O\nResult: %O\n" ty value
        | Error msg ->
            eprintfn "%s\n" msg)

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv
    then
        interactive ()
    else
        let srcPath = argv.[0]
        let src = File.ReadAllText srcPath

        match (run src) with
        | Ok(ty, value) ->
            printfn "Static type: %O\nResult: %O" ty value
        | Error msg ->
            eprintfn "%s" msg
            exit 1
    0
