module Program

open System

open FSharp.Compiler.SourceCodeServices

[<EntryPoint>]
let main argv =
    printfn "Module Dependencies Visualizer\n"

    // 1st arg: the list of defined symbols (required beacuse the tokenizer handles `#if` directives)
    // 2nd arg: the file name of the source code (it doesn't have to exist)
    let sourceTok = FSharpSourceTokenizer([], Some "Example.fs")

    let rec tokenizeLine (tokenizer: FSharpLineTokenizer) state =
        match tokenizer.ScanToken(state) with
        | Some tok, state ->
            // FSharpTokenInfo
            // TokenName: the name of the token (as defined in the F# lexer)
            // LeftColumn, RightColumn: the location of the token inside the line
            // ColorClass: information about the token category that can be used for colorizing F# code
            Console.ForegroundColor <-
                match tok.ColorClass with
                | FSharpTokenColorKind.Default ->
                    ConsoleColor.DarkGray
                | FSharpTokenColorKind.Keyword ->
                    ConsoleColor.Cyan
                | FSharpTokenColorKind.Operator ->
                    ConsoleColor.DarkCyan
                | FSharpTokenColorKind.Punctuation ->
                    ConsoleColor.Yellow
                | FSharpTokenColorKind.String ->
                    ConsoleColor.Blue
                | _ ->
                    ConsoleColor.White
            printfn "  %s (at %d ~ %d)" tok.TokenName tok.LeftColumn tok.RightColumn
            Console.ResetColor()
            tokenizeLine tokenizer state
        | None, state -> state

    let rec tokenizeLines state count =
        function
        | line :: lines ->
            printfn "Line %d:" count
            let tokenizer = sourceTok.CreateLineTokenizer(line)
            let state = tokenizeLine tokenizer state
            tokenizeLines state (count + 1) lines
        | [] -> ()

    "let hello () =\n    printfn \"Hello, world!\"".Split('\r', '\n')
    |> List.ofSeq
    |> tokenizeLines FSharpTokenizerLexState.Initial 1

    0
