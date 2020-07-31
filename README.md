# ZAM

ZAM (ZINC Abstract Machine) implementation in F#

## Setup dotnet tools

```bash
$ dotnet tool restore
$ dotnet paket restore
```

## Run

```bash
$ dotnet run --project src/ZAM.Interp -- examples/branch.txt
Result: 0

$ dotnet run --project src/ZAM.Interp -- examples/let-lambda.txt
Result: 14
```

## Create self-contained executable

### Ubuntu

```bash
$ dotnet publish -c Release --self-contained --runtime linux-x64 --nologo
```

executable path: `src/ZAM.Interp/bin/Release/netcoreapp3.1/linux-x64/zam`
