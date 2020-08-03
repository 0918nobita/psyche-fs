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

## Run tests

```bash
$ dotnet fake build -t test
```

## Lint

```bash
$ dotnet fake build -t lint
```

## Format

```bash
$ dotnet fake build -t format
```

## Create self-contained executable

### Ubuntu

```bash
$ dotnet publish -c Release --self-contained --runtime linux-x64 --nologo
```

executable path: `src/ZAM.Interp/bin/Release/netcoreapp3.1/linux-x64/zam`
