# Syntax

```
program = expr*;

expr = atom | list;

atom = bool | int | ident;

list = '(' expr* ')';

bool = "true" | "false";

int = ["+" | "-"] non_zero_digit digit*;

digit = 0 | non_zero_digit;

non_zero_digit = 1 | 2 | 3 | .. | 7 | 8 | 9;

ident = ? true, false でない半角英字の列 (2文字目以降では半角数字を使用可能) ?;
```
