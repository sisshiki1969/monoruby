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
- while/until statement and postfix while/until modifier
- method definition
  - required parameter
  - optional parameter
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
|so_mandelbrot   |         0.629|                 0.985|                 0.810|     15.359|
|so_nbody        |         0.917|                 1.541|                 0.876|      5.636|
|app_aobench     |         0.028|                 0.045|                 0.035|      0.104|

|                     |3.2.0-preview3| 3.2.0-preview3 --yjit|     monoruby|
|:--------------------|-------------:|---------------------:|------------:|
|vm_ivar              |      172.132M|              195.274M|       2.399G|
|vm_ivar_get          |        12.226|                26.882|       68.584|
|vm_ivar_set          |      121.974M|              123.096M|       1.519G|
|vm_ivar_generic_get  |       15.793M|               15.693M|     125.449M|
|vm_ivar_generic_set  |       12.508M|               16.265M|      92.820M|
|vm_attr_ivar         |       58.379M|               58.935M|     332.861M|
|vm_attr_ivar_set     |       52.319M|               52.612M|     309.355M|

## How to run

To build monoruby, You'll need installation of Rust.
Please be aware that **only nightly version of Rust works** for monoruby.

To run ruby program file on monoruby,

```sh
% cargo run test.rb
```

or

```sh
% cargo run --release -- test.rb
```

one liner

```sh
% cargo run -- -e "puts 100"
```

You can launch REPL.

```sh
% ./irm.sh
```
