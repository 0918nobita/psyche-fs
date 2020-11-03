module Program

open Expecto
open Psyche.Base.State

[<Tests>]
let testState =
    test "state" {
        let state1 = State (fun x -> (string (x + 1), x * 2))
        Expect.equal (runState state1 6) ("7", 12) "runState"

        let state2 = state {
           let! v = state1
           return v + "!"
        }
        Expect.equal (runState state2 6) ("7!", 12) "Bind, Return"

        let state3 = state {
            let! v = state1
            return! State (fun s -> (Some (v + ":" + string s), 0))
        }
        Expect.equal (runState state3 6) (Some "7:12", 0) "ReturnFrom"
    }

[<Tests>]
let properties =
    testList "FsCheck"
        [ testProperty "Addition is commutative" <| fun a b -> a + b = b + a

          testProperty "reverse list" <| fun (xs: int list) ->
              let reversed = List.rev xs
              List.rev reversed = xs ]

[<EntryPoint>]
let main argv = runTestsInAssembly defaultConfig argv
