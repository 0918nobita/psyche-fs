type CamInstr =
    | CAMLdi of int
    | CAMLdf of float
    | CAMLdb of bool
    | CAMAccess of int
    | CAMClosure of CamCode
    | CAMApply
    | CAMReturn
    | CAMLet
    | CAMEndLet
    | CAMTest of CamCode * CamCode
    | CAMAddI
    | CAMAddF
    | CAMEq

and CamCode = CamInstr list

type CamValue =
    | CAMIntVal of int
    | CAMFloatVal of float
    | CAMBoolVal of bool
    | CAMClosureVal of CamCode * CamEnv

and CamEnv = CamValue list

type CamStack = CamValue list

let rec run (code: CamCode) (env: CamEnv) (stack: CamStack) : (CamValue * CamEnv * CamStack) =
    match code with
    | [] -> (stack.[0], env, stack)
    | (CAMLdi i)::tail -> run tail env ((CAMIntVal i)::stack)
    | (CAMLdf f)::tail -> run tail env ((CAMFloatVal f)::stack)
    | (CAMLdb b)::tail -> run tail env ((CAMBoolVal b)::stack)
    | (CAMAccess i)::tail -> run tail env (env.[i]::stack)
    | (CAMClosure code)::tail ->
        run tail env ((CAMClosureVal (code, env))::stack)
    | CAMLet::tail ->
        run tail (stack.[0]::env) (stack.[1..])
    | CAMEndLet::tail ->
        run tail (env.[1..]) stack
    | CAMAddI::tail ->
        match (stack.[0], stack.[1]) with
        | (CAMIntVal rhs, CAMIntVal lhs) ->
            run tail env ((lhs + rhs |> CAMIntVal)::stack.[2..])
        | _ -> failwith "Invalid operation"
    | CAMAddF::tail ->
        match (stack.[0], stack.[1]) with
        | (CAMFloatVal rhs, CAMFloatVal lhs) ->
            run tail env ((lhs + rhs |> CAMFloatVal)::stack.[2..])
        | _ -> failwith "Invalid operation"
    | _ -> failwith "Not implemented"

[<EntryPoint>]
let main argv =
    let code =
        [ CAMLdi 2
        ; CAMLet
        ; CAMAccess 0
        ; CAMLdi 4
        ; CAMLdi 1
        ; CAMAddI
        ; CAMAddI
        ; CAMEndLet
        ]
    let env = []
    let stack = []
    let (res, env', stack') = run code env stack
    printfn $"Result: %A{res}"
    printfn $"Env: %A{env'}"
    printfn $"Stack: %A{stack'}"
    0
