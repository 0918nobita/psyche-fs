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

open Psyche.Base.Functor

type FmapEx() =
    inherit Fmap()
    static member (?) (_: Fmap, source) =
        fun f -> Result.map f source

let inline (<%>) f x = FmapEx() ? (x) f

[<Tests>]
let testFunctor =
    test "functor" {
        let res = snd <%> Some((1, 2))
        Expect.equal res (Some(2)) "Functor Option"

        let res = (sprintf "%s!") <%> ["A"; "B"; "C"]
        Expect.equal res ["A!"; "B!"; "C!"] "Functor List"

        let res = (sprintf "(%s)") <%> [|"A"; "B"; "C"|]
        Expect.equal res [|"(A)"; "(B)"; "(C)"|] "Functor Array"

        let res = fst <%> Ok((3, 4))
        Expect.equal res (Ok(3)) "Functor Result"
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
