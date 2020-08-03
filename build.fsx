#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators

Target.create "Clean" (fun _ -> Shell.cleanDirs (!!"src/**/bin" ++ "src/**/obj"))

Target.create "Debug" (fun _ ->
    !!"src/**/*.fsproj"
    |> Seq.iter
        (DotNet.build (fun options -> { options with Configuration = DotNet.Debug })))

"Clean" ==> "Debug"

Target.create "Release" (fun _ ->
    !!"src/**/*.fsproj"
    |> Seq.iter
        (DotNet.build (fun options -> { options with Configuration = DotNet.Release })))

"Clean" ==> "Release"

let dotnet cmd arg =
    let processResult = DotNet.exec id cmd arg
    if not processResult.OK then failwith "dotnet command failed"

Target.create "Test" (fun _ -> dotnet "run" "--project src/ZAM.Interp.Tests")

"Clean" ==> "Test"

Target.create "Lint" (fun _ ->
    dotnet "fantomas" "--check --recurse --pageWidth 90 ./src"
    dotnet "fantomas" "--check --recurse --pageWidth 90 build.fsx")

Target.create "Format" (fun _ ->
    dotnet "fantomas" "--recurse --pageWidth 90 ./src"
    dotnet "fantomas" "--recurse --pageWidth 90 build.fsx")

Target.runOrDefault "Debug"
