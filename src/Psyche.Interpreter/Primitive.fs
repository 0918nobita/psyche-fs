module internal Primitive

open Psyche.UntypedAst
open Value

let pureAddI = Closure("a", UFun("b", UBinApp(AddI, UVar "a", UVar "b")), Env.empty)

let pureSubI = Closure("a", UFun("b", UBinApp(SubI, UVar "a", UVar "b")), Env.empty)

let pureMulI = Closure("a", UFun("b", UBinApp(MulI, UVar "a", UVar "b")), Env.empty)

let pureDivI = Closure("a", UFun("b", UBinApp(DivI, UVar "a", UVar "b")), Env.empty)

let pureAddF = Closure("a", UFun("b", UBinApp(AddF, UVar "a", UVar "b")), Env.empty)

let pureSubF = Closure("a", UFun("b", UBinApp(SubF, UVar "a", UVar "b")), Env.empty)

let pureMulF = Closure("a", UFun("b", UBinApp(MulF, UVar "a", UVar "b")), Env.empty)

let pureDivF = Closure("a", UFun("b", UBinApp(DivF, UVar "a", UVar "b")), Env.empty)

let pureMod = Closure("a", UFun("b", UBinApp(Mod, UVar "a", UVar "b")), Env.empty)

let pureLt = Closure("a", UFun("b", UBinApp(Lt, UVar "a", UVar "b")), Env.empty)

let pureLe = Closure("a", UFun("b", UBinApp(Le, UVar "a", UVar "b")), Env.empty)

let pureIntOfFloat = Closure("f", UIntOfFloat(UVar "f"), Env.empty)

let pureFloatOfInt = Closure("n", UIntOfFloat(UVar "n"), Env.empty)

let primitives =
    Env.empty
    |> Env.append "+" pureAddI
    |> Env.append "-" pureSubI
    |> Env.append "*" pureMulI
    |> Env.append "/" pureDivI
    |> Env.append "+." pureAddF
    |> Env.append "-." pureSubF
    |> Env.append "*." pureMulF
    |> Env.append "/." pureDivF
    |> Env.append "%" pureMod
    |> Env.append "<" pureLt
    |> Env.append "<=" pureLe
    |> Env.append "int-of-float" pureIntOfFloat
    |> Env.append "float-of-int" pureFloatOfInt
