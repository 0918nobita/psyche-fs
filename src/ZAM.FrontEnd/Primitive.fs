module internal Primitive

open FrontEnd.Type

let pureAddI = TFun(TInt, TFun(TInt, TInt))

let pureSubI = TFun(TInt, TFun(TInt, TInt))

let pureMulI = TFun(TInt, TFun(TInt, TInt))

let pureDivI = TFun(TInt, TFun(TInt, TInt))

let pureAddF = TFun(TFloat, TFun(TFloat, TFloat))

let pureSubF = TFun(TFloat, TFun(TFloat, TFloat))

let pureMulF = TFun(TFloat, TFun(TFloat, TFloat))

let pureDivF = TFun(TFloat, TFun(TFloat, TFloat))

let pureMod = TFun(TInt, TFun(TInt, TInt))

let pureLt = TFun(TInt, TFun(TInt, TBool))

let pureLe = TFun(TInt, TFun(TInt, TBool))

let pureIntOfFloat = TFun(TFloat, TInt)

let pureFloatOfInt = TFun(TInt, TFloat)

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
