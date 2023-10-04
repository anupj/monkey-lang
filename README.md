# Monkey Language Interpreter in Rust

## About The Project

Monkey is a small, C-like programming language. This repository contains a
Rust-based interpreter for the Monkey language.

**It is based on the interpreter described in
[Writing an Interpreter in Go](https://interpreterbook.com/) book by Thorsten
Ball.**

### Work in Progress

- [x] Lexer
- [ ] Parser (In progress)
- [ ] Evaluation
- [ ] Extending the Interpreter
- [ ] Finishing touches

### Language Features

Monkey comes with several features commonly found in modern programming
languages:

#### Syntax

- C-like syntax for ease of understanding and usage.

#### Variable Bindings

- Ability to bind values to names using the `let` statement.

```monkey
let age = 1;
let name = "Monkey";
let result = 10 * (20 / 2);
```

#### Data Types

- Supports integers, booleans, strings, arrays, and hash data structures.

```monkey
let myArray = [1, 2, 3, 4, 5];
let thorsten = {"name": "Thorsten", "age": 28};
```

#### Operators

- Arithmetic expressions are supported.

#### Functions

- First-class and higher-order functions, closures are supported.

```monkey
let add = fn(a, b) { return a + b; };
let twice = fn(f, x) { return f(f(x)); };
```

#### Control Structures

If-else statements, loops are supported.

#### Access Expressions

Access array and hash elements using index expressions.

```monkey
myArray[0] // => 1
thorsten["name"] // => "Thorsten"
```

### Built with

[Rust Programming Language](https://www.rust-lang.org/)
