module Program

module BNel = Psyche.Base.Nel


module BResult = Psyche.Base.Result


open System.IO


let run src =
    BResult.result {
        let! (ty, untypedAst) = Psyche.Parser.tryParse src
        printfn "Static type: %O" ty
        let! value = Runtime.eval (Primitive.primitives) untypedAst
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

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv
    then
        interactive ()
    else
        let srcPath = argv.[0]
        let src = File.ReadAllText srcPath

        match run src with
        | Ok value ->
            printfn "Result: %O" value
        | Error msg ->
            eprintfn "%s" msg
            exit 1
    0
