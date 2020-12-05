# Psyche

![Lint](https://github.com/0918nobita/psyche/workflows/Lint/badge.svg)  ![Test](https://github.com/0918nobita/psyche/workflows/Test/badge.svg)

Programming language

## Requirements

- .NET 5.0 SDK

## Setup dotnet tools

```
$ dotnet tool restore
$ dotnet paket restore
```

## Build

### Debug Build

```
$ dotnet fake build
```

### Release Build

```
$ dotnet fake build -t release
```

## Launch a REPL

- Type `:exit` to exit the REPL

```
$ dotnet run -p src/Psyche.CLI
> (+ 1)
Static type: (-> Int Int)
Result: <Closure>

> (+ 3 4)
Static type: Int
Result: 7

> :exit
```

## Run programs

```
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

```
$ dotnet fake build -t test
```

## Create self-contained executable

```
$ dotnet fake build -t publish
```

resulting publish folder : ``src/Psyche.CLI/bin/Release/net5.0/{runtime}/publish``

executable file : ``src/Psyche.CLI/bin/Release/net5.0/{runtime}/publish/Psyche.CLI``
