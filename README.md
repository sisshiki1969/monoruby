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

- classes: Object, Integer, Float, String, Symbol, Class, Array
- superclass and singleton class and methods
- local variables
- instance variables & accessor
- block and dynamic local variables
- if-then-elsif-end statement
- for-in statement
- while statement
- method definition
- class definition

## Benchmark

- measured by [benchmark-driver](https://github.com/benchmark-driver/benchmark-driver) with '--repeat-count 3' option.
- benchmark codes are [in the official repo](https://github.com/ruby/ruby/tree/master/benchmark) except qsort and tarai.
- measurements are shown in iteration/sec (the higher, the better).

|                |3.2.0-preview3| 3.2.0-preview3 --yjit| 3.2.0-preview3 --mjit|   monoruby|
|:---------------|-------------:|---------------------:|---------------------:|----------:|
|loop_whileloop  |         2.892|                 2.887|                 2.858|     23.685|
|qsort*          |      125.036k|              315.657k|              119.578k|   384.874k|
|app_fib         |         3.637|                14.396|                 4.569|     17.185|
|tarai*          |         2.809|                12.851|                 3.915|     14.762|
|so_mandelbrot   |         0.623|                 0.976|                 0.824|     13.832|
|so_nbody        |         0.920|                 1.538|                 0.878|      5.132|

|                     |3.2.0-preview3| 3.2.0-preview3 --yjit|     monoruby|
|:--------------------|-------------:|---------------------:|------------:|
|vm_ivar              |      170.332M|              170.449M|     732.590M|
|vm_ivar_get          |        12.067|                26.865|       74.040|
|vm_ivar_set          |      109.025M|              115.217M|     707.712M|
|vm_ivar_generic_get  |       15.167M|               16.079M|     118.955M|
|vm_ivar_generic_set  |       12.556M|               16.322M|      79.166M|
|vm_attr_ivar         |       58.778M|               58.759M|     269.301M|
|vm_attr_ivar_set     |       52.270M|               52.345M|     307.156M|

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
