# ZAM

ZAM (ZINC Abstract Machine) implementation in F#

(under construction)

## Setup dotnet tools

```bash
$ dotnet tool restore
$ dotnet paket restore
```

## Build

### Debug Build

```bash
$ dotnet fake build
```

### Release Build

```bash
$ dotnet fake build -t release
```

## Run interpreter

```bash
$ dotnet run --project src/ZAM.Interp -- examples/branch.txt
Static type: Int
Result: 0

$ dotnet run --project src/ZAM.Interp -- examples/let-lambda.txt
Static type: Int
Result: 14

$ dotnet run --project src/ZAM.Interp -- examples/counter.txt
Static type: Int
Result: 6

$ dotnet run --project src/ZAM.Interp -- examples/counter2.txt
Static type: Int
Result: 3
```

## Run tests

```bash
$ dotnet fake build -t test
```

## Create self-contained executable

### Ubuntu

```bash
$ dotnet publish -c Release --self-contained --runtime linux-x64 --nologo
```

executable path: `src/ZAM.Interp/bin/Release/netcoreapp3.1/linux-x64/zam`
