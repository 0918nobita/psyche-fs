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

Target.create "Publish" (fun _ ->
    match (Environment.isWindows, Environment.isMacOS, Environment.isLinux) with
    | true, false, false -> dotnet "publish" "-c Release --self-contained --runtime win-x64 --nologo"
    | false, true, false -> dotnet "publish" "-c Release --self-contained --runtime osx-x64 --nologo"
    | false, false, true -> dotnet "publish" "-c Release --self-contained --runtime linux-x64 --nologo"
    | _, _, _ -> failwith "Publishing project in this platform is not supported")

"Clean" ==> "Publish"

Target.runOrDefault "Debug"
