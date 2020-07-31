# ZAM

ZAM (ZINC Abstract Machine) implementation in F#

## Setup dotnet tools

```bash
$ dotnet tool restore
```

## Run

```bash
$ dotnet run --project src/ZAM.Interp -- examples/branch.txt
Result: 0

$ dotnet run --project src/ZAM.Interp -- examples/let-lambda.txt
Result: 14
```
