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

|  impl \ bench   | app_fib | so_mandelbrot |  tarai | quicksort |
| :-------------: | ------: | ------------: | -----: | :-------: |
|    monoruby     |  23.137 |        14.695 | 15.195 | 408.580k  |
| ruby(3.2.0-dev) |   3.305 |         0.528 |  2.742 |  65.608k  |
|   ruby --yjit   |  13.250 |         0.702 | 11.398 | 108.308k  |

|    bench \ impl     | 3.2.0-dev |  --yjit  | monoruby |
| :-----------------: | :-------: | :------: | :------: |
|       vm_ivar       | 209.106M  | 143.486M | 860.795M |
|     vm_ivar_get     |  12.703   |  25.456  |  65.388  |
|     vm_ivar_set     | 145.923M  | 139.298M | 401.092M |
| vm_ivar_generic_get |  16.950M  | 15.658M  | 191.712M |
| vm_ivar_generic_set |  7.496M   |  9.636M  | 180.772M |
|    vm_attr_ivar     |  60.527M  | 58.644M  | 277.252M |
|  vm_attr_ivar_set   |  53.804M  | 57.584M  | 183.506M |

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
