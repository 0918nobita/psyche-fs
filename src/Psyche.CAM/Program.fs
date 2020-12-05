type CamInstr =
    | CAMLdi of int
    | CAMLdb of bool
    | CAMAccess of int
    | CAMClosure of CamCode
    | CAMApply
    | CAMReturn
    | CAMLet
    | CAMEndLet
    | CAMTest of CamCode * CamCode
    | CAMAdd
    | CAMSub
    | CAMMul
    | CAMDiv
    | CAMMod
    | CAMEq
    | CAMLt
    | CAMLe

and CamCode = CamInstr list

type CamValue =
    | CAMIntVal of int
    | CAMBoolVal of bool
    | CAMClosureVal of CamCode * CamEnv

and CamEnv = CamValue list

type CamStack = CamValue list

let rec run (code: CamCode) (env: CamEnv) (stack: CamStack) : (CamValue * CamEnv * CamStack) =
    match code with
    | [] -> (stack.[0], env, stack)
    | (CAMLdi i)::tail -> run tail env ((CAMIntVal i)::stack)
    | (CAMLdb b)::tail -> run tail env ((CAMBoolVal b)::stack)
    | (CAMAccess i)::tail -> run tail env (env.[i]::stack)
    | CAMApply::tail ->
        let closure = stack.[0]
        let argument = stack.[1]
        match closure with
        | CAMClosureVal (closureCode, closureEnv) ->
            run closureCode (argument::closure::closureEnv) (CAMClosureVal (tail, env)::stack.[2..])
        | _ -> failwith "Invalid operation"
    | CAMReturn::_ ->
        let ret = stack.[0]
        let cont = stack.[1]
        match cont with
        | CAMClosureVal (closureCode, closureEnv) ->
            run closureCode closureEnv (ret::stack.[2..])
        | _ -> failwith "Invalid operation"
    | (CAMClosure code)::tail ->
        run tail env ((CAMClosureVal (code, env))::stack)
    | CAMLet::tail ->
        run tail (stack.[0]::env) (stack.[1..])
    | CAMEndLet::tail ->
        run tail (env.[1..]) stack
    | (CAMTest (c1, c2))::tail ->
        match stack.[0] with
        | CAMBoolVal true ->
            run (c1 @ tail) env stack.[1..]
        | CAMBoolVal false ->
            run (c2 @ tail) env stack.[1..]
        | _ -> failwith "Invalid operation"
    | CAMAdd::tail ->
        match (stack.[0], stack.[1]) with
        | (CAMIntVal rhs, CAMIntVal lhs) ->
            run tail env ((lhs + rhs |> CAMIntVal)::stack.[2..])
        | _ -> failwith "Invalid operation"
    | CAMSub::tail ->
        match (stack.[0], stack.[1]) with
        | (CAMIntVal rhs, CAMIntVal lhs) ->
            run tail env ((lhs - rhs |> CAMIntVal)::stack.[2..])
        | _ -> failwith "Invalid operation"
    | CAMMul::tail ->
        match (stack.[0], stack.[1]) with
        | (CAMIntVal rhs, CAMIntVal lhs) ->
            run tail env ((lhs * rhs |> CAMIntVal)::stack.[2..])
        | _ -> failwith "Invalid operation"
    | CAMDiv::tail ->
        match (stack.[0], stack.[1]) with
        | (CAMIntVal rhs, CAMIntVal lhs) ->
            run tail env ((lhs / rhs |> CAMIntVal)::stack.[2..])
        | _ -> failwith "Invalid operation"
    | CAMMod::tail ->
        match (stack.[0], stack.[1]) with
        | (CAMIntVal rhs, CAMIntVal lhs) ->
            run tail env ((lhs % rhs |> CAMIntVal)::stack.[2..])
        | _ -> failwith "Invalid operation"
    | CAMEq::tail ->
        match (stack.[0], stack.[1]) with
        | (CAMIntVal rhs, CAMIntVal lhs) ->
            run tail env ((lhs = rhs |> CAMBoolVal)::stack.[2..])
        | _ -> failwith "Invalid operation"
    | CAMLt::tail ->
        match (stack.[0], stack.[1]) with
        | (CAMIntVal rhs, CAMIntVal lhs) ->
            run tail env ((lhs < rhs |> CAMBoolVal)::stack.[2..])
        | _ -> failwith "Invalid operation"
    | CAMLe::tail ->
        match (stack.[0], stack.[1]) with
        | (CAMIntVal rhs, CAMIntVal lhs) ->
            run tail env ((lhs <= rhs |> CAMBoolVal)::stack.[2..])
        | _ -> failwith "Invalid operation"

[<EntryPoint>]
let main argv =
    let code =
        [ CAMClosure
            [ CAMLdi 1
            ; CAMAccess 0
            ; CAMEq
            ; CAMTest
                ( [ CAMLdi 1 ]
                , [ CAMAccess 0; CAMLdi 1; CAMSub; CAMAccess 1; CAMApply; CAMAccess 0; CAMAdd ]
                )
            ; CAMReturn
            ]
        ; CAMLet
        ; CAMLdi 10
        ; CAMAccess 0
        ; CAMApply
        ; CAMEndLet
        ]
    let env = []
    let stack = []
    let (res, env', stack') = run code env stack
    printfn $"Result: %A{res}"
    printfn $"Env: %A{env'}"
    printfn $"Stack: %A{stack'}"
    0
