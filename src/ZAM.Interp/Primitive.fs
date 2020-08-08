module internal Primitive

open FrontEnd.UntypedAst
open Value

let pureAddI = Closure("a", UFun("b", UBinApp(AddI, UVar "a", UVar "b")), [])

let pureSubI = Closure("a", UFun("b", UBinApp(SubI, UVar "a", UVar "b")), [])

let pureMulI = Closure("a", UFun("b", UBinApp(MulI, UVar "a", UVar "b")), [])

let pureDivI = Closure("a", UFun("b", UBinApp(DivI, UVar "a", UVar "b")), [])

let pureAddF = Closure("a", UFun("b", UBinApp(AddF, UVar "a", UVar "b")), [])

let pureSubF = Closure("a", UFun("b", UBinApp(SubF, UVar "a", UVar "b")), [])

let pureMulF = Closure("a", UFun("b", UBinApp(MulF, UVar "a", UVar "b")), [])

let pureDivF = Closure("a", UFun("b", UBinApp(DivF, UVar "a", UVar "b")), [])

let pureMod = Closure("a", UFun("b", UBinApp(Mod, UVar "a", UVar "b")), [])

let pureLt = Closure("a", UFun("b", UBinApp(Lt, UVar "a", UVar "b")), [])

let pureLe = Closure("a", UFun("b", UBinApp(Le, UVar "a", UVar "b")), [])

let pureIntOfFloat = Closure("f", UIntOfFloat(UVar "f"), [])

let pureFloatOfInt = Closure("n", UIntOfFloat(UVar "n"), [])


let primitives = [
    ("+", pureAddI)
    ("-", pureSubI)
    ("*", pureMulI)
    ("/", pureDivI)
    ("+.", pureAddF)
    ("-.", pureSubF)
    ("*.", pureMulF)
    ("/.", pureDivF)
    ("%", pureMod)
    ("<", pureLt)
    ("<=", pureLe)
    ("int-of-float", pureIntOfFloat)
    ("float-of-int", pureFloatOfInt)
]
