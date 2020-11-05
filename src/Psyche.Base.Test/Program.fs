module Program

open Expecto
open Psyche.Base.Monad
open Psyche.Base.State
open Psyche.Base.Functor

[<Tests>]
let testState =
    test "state" {
        let state1 = State (fun x -> (string (x + 1), x * 2))
        Expect.equal (State.runState state1 6) ("7", 12) "runState"

        let state2 = monad {
           let! v = state1
           return v + "!"
        }

        Expect.equal (State.runState state2 6) ("7!", 12) "Bind, Return"

        let state3 = monad {
            let! v = state1
            return! State (fun s -> (Some (v + ":" + string s), 0))
        }

        Expect.equal (State.runState state3 6) ((Some "7:12"), 0) "ReturnFrom"
    }

[<Tests>]
let testFunctor =
    test "functor" {
        let actual = snd <%> Some (1, 2)
        Expect.equal actual (Some 2) "Functor Option"

        let actual = (sprintf "%s!") <%> ["A"; "B"; "C"]
        Expect.equal actual ["A!"; "B!"; "C!"] "Functor List"

        let actual = (sprintf "(%s)") <%> [|"A"; "B"; "C"|]
        Expect.equal actual [|"(A)"; "(B)"; "(C)"|] "Functor Array"

        let actual = fst <%> Ok (3, 4)
        Expect.equal actual (Ok 3) "Functor Result"
    }

[<Tests>]
let testMonad =
    test "monad" {
        let actual = monad {
            let! a = Some 21
            let! b = Some 2
            return a * b
        }

        Expect.equal actual (Some 42) "Monad Option"

        // concat (map (fun a -> (concat (map (fun b -> [a * 2 + b]) [10; 20; 30]))) [1; 2; 3])
        // concat [(concat (map (fun b -> [2 + b]) [10; 20; 30])); (concat (map (fun b -> [4 + b]) [10; 20; 30])); (concat (map (fun b -> [6 + b]) [10; 20; 30]))]
        // concat [(concat [[12]; [22]; [32]]); (concat [[14]; [24]; [34]]); (concat [[16]; [26]; [36]])]
        // concat [[12; 22; 32]; [14; 24; 34]; [16; 26; 36]]
        // [12; 22; 32; 14; 24; 34; 16; 26; 36]
        let actual = monad {
            let! a = [1; 2; 3]
            let! b = [10; 20; 30]
            return a * 2 + b
        }

        let expected = [12; 22; 32; 14; 24; 34; 16; 26; 36]

        Expect.equal actual expected "Monad List"
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
