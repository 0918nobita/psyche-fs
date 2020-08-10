#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators

Target.create "Clean" (fun _ -> Shell.cleanDirs (!!"src/**/bin" ++ "src/**/obj"))

Target.create "Debug" (fun _ ->
    !! "src/Psyche.Interpreter/Psyche.Interpreter.fsproj"
    |> Seq.iter
        (DotNet.build (fun options -> { options with Configuration = DotNet.Debug })))

"Clean" ==> "Debug"

Target.create "Release" (fun _ ->
    !! "src/Psyche.Interpreter/Psyche.Interpreter.fsproj"
    |> Seq.iter
        (DotNet.build (fun options -> { options with Configuration = DotNet.Release })))

"Clean" ==> "Release"

let dotnet cmd arg =
    let processResult = DotNet.exec id cmd arg
    if not processResult.OK then failwith "dotnet command failed"

Target.create "Test" (fun _ -> dotnet "run" "--project src/Psyche.Base.Test")

"Clean" ==> "Test"

Target.runOrDefault "Debug"
