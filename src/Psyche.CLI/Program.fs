module Program

module BNel = Psyche.Base.Nel
module BResult = Psyche.Base.Result

open System.IO

let run src =
    BResult.result {
        let! annotatedAst = Psyche.Parser.tryParse src
        let! (ty, untypedAst) = Psyche.TypeChecker.typeCheck (Primitive.typeEnv) annotatedAst
        printfn "Static type: %O" ty
        let! value = Psyche.Interpreter.eval (Primitive.env) untypedAst
        return value
    }

let interactive () =
    fun _ ->
        printf "> "
        System.Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.takeWhile (fun input -> not(isNull input) && input <> ":exit")
    |> Seq.iter (fun src ->
        match run src with
        | Ok value ->
            printfn "Result: %O\n" value
        | Error msg ->
            eprintfn "%s\n" msg)

let help () =
    printfn "The Psyche programming language"
    printfn ""
    printfn "Usage:"
    printfn ""
    printfn "    Psyche.CLI <command> [arguments]"
    printfn ""
    printfn "The commands are:"
    printfn ""
    printfn "    compile    compile source code, then generate byte code"
    printfn "    interpret  interpret source code"
    printfn "    repl       launch a REPL process"
    printfn "    run        execute byte code"
    printfn "    version    show Psyche version"
    printfn ""

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv
    then
        help ()
    else
        match argv.[0] with
        | "repl" -> interactive ()
        | "compile" ->
            Psyche.Compiler.compile ()
        | "run" ->
            Psyche.CAM.startup ()
        | "interpret" ->
            if Array.length argv <> 2
                then
                    eprintfn "Invalid argument"
                    exit 1
            let srcPath = argv.[1]
            let src = File.ReadAllText srcPath

            match run src with
            | Ok value ->
                printfn "Result: %O" value
            | Error msg ->
                eprintfn "%s" msg
                exit 1
        | "version" ->
            printfn "Psyche version 0.0.3"
        | "help" ->
            help ()
        | _ ->
            eprintfn "Invalid command"
            exit 1
    0
