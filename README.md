# Psyche

![Lint](https://github.com/0918nobita/psyche/workflows/Lint/badge.svg)  ![Test](https://github.com/0918nobita/psyche/workflows/Test/badge.svg)

Programming language

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
$ dotnet run -p src/Psyche.CLI -- examples/branch.txt
Static type: Int
Result: 0

$ dotnet run -p src/Psyche.CLI -- examples/let-lambda.txt
Static type: Int
Result: 14

$ dotnet run -p src/Psyche.CLI -- examples/counter.txt
Static type: Int
Result: 6

$ dotnet run -p src/Psyche.CLI -- examples/counter2.txt
Static type: Int
Result: 3
```

## Run tests

```bash
$ dotnet fake build -t test
```

## Create self-contained executable

```bash
$ dotnet fake build -t publish
```

resulting publish folder : ``src/Psyche.Interpreter/bin/Release/netcoreapp3.1/**/publish``

executable file : ``src/Psyche.Interpreter/bin/Release/netcoreapp3.1/**/publish/Psyche.Interpreter``
