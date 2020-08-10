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

let primitives =
    TypeEnv.empty
    |> TypeEnv.append "+" pureAddI
    |> TypeEnv.append "-" pureSubI
    |> TypeEnv.append "*" pureMulI
    |> TypeEnv.append "/" pureDivI
    |> TypeEnv.append "+." pureAddF
    |> TypeEnv.append "-." pureSubF
    |> TypeEnv.append "*." pureMulF
    |> TypeEnv.append "/." pureDivF
    |> TypeEnv.append "%" pureMod
    |> TypeEnv.append "<" pureLt
    |> TypeEnv.append "<=" pureLe
    |> TypeEnv.append "int-of-float" pureIntOfFloat
    |> TypeEnv.append "float-of-int" pureFloatOfInt
