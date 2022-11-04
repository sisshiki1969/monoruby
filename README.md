# monoruby

[![Rust](https://github.com/sisshiki1969/monoruby/actions/workflows/rust.yml/badge.svg?branch=master)](https://github.com/sisshiki1969/monoruby/actions/workflows/rust.yml)
[![codecov](https://codecov.io/gh/sisshiki1969/monoruby/branch/master/graph/badge.svg?token=vAvpafdKER)](https://codecov.io/gh/sisshiki1969/monoruby)

another toy Ruby implementation with a fast JIT compiler written in Rust.

## Features

- register-based bytecode.
- bytecode executer (virtual machine) written in x86-64 assembly (yes, we currently support only x86-64!).
- a compact and fast just-in-time compiler. (internally using self-made dynamic assembler [monoasm](https://github.com/sisshiki1969/monoasm))

## Status of this project

This project still remains in early-alpha stage. Currently, only the functionalities described below are implemented.

- classes: Integer, Float, String, Symbol, Class
- superclass and singleton class and methods
- local variables
- instance variables & accessor
- if-then-elsif-end statement
- for-in statement
- while statement
- method definition
- class definition

## Benchmark

- measured by [benchmark-driver](https://github.com/benchmark-driver/benchmark-driver) with '--repeat-count 3' option.
- measurements are shown in iteration/sec (the higher, the better).

|  bench \ impl   |  3.2.0-dev |   --yjit   | monoruby  |
| :-------------: | :--------: | :--------: | :-------: |
|      qsort      |   74.016k  |   113.075k |  361.426k |
|     app_fib     |     3.615  |     13.938 |    17.951 |
|  so_mandelbrot  |     0.615  |      0.832 |    13.571 |
|    so_nbody     |     1.033  |      1.560 |     4.751 |

|    bench \ impl     | 3.2.0-dev |  --yjit   | monoruby  |
| :-----------------: | :-------: | :-------: | :-------: |
|             vm_ivar |  159.172M |  160.985M |  593.191M |
|         vm_ivar_get |    12.251 |    28.422 |    58.717 |
|         vm_ivar_set |  134.974M |  137.563M |  533.527M |
| vm_ivar_generic_get |   16.402M |   16.332M |  112.353M |
| vm_ivar_generic_set |    8.152M |   10.313M |   75.855M |
|        vm_attr_ivar |   58.936M |   58.063M |  301.761M |
|    vm_attr_ivar_set |   64.160M |   62.901M |  300.453M |

## How to run

To build monoruby, You'll need installation of Rust.
Please be aware that **only nightly version of Rust works** for monoruby.

To run ruby program file on monoruby,

```sh
% cargo run app_fib.rb
```

or

```sh
% cargo run --release -- app_fib.rb
```

one liner

```sh
% cargo run -- -e "puts 100"
```

You can launch REPL, omitting file name.

```sh
% cargo run
```
