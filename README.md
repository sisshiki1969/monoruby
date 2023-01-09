# monoruby

[![Rust](https://github.com/sisshiki1969/monoruby/actions/workflows/rust.yml/badge.svg?branch=master)](https://github.com/sisshiki1969/monoruby/actions/workflows/rust.yml)
[![codecov](https://codecov.io/gh/sisshiki1969/monoruby/branch/master/graph/badge.svg?token=vAvpafdKER)](https://codecov.io/gh/sisshiki1969/monoruby)

another toy Ruby implementation with a fast JIT compiler written in Rust.

## Features

- register-based bytecode.
- bytecode executer (virtual machine) written in x86-64 assembly (yes, we currently support only x86-64!).
- a compact and fast just-in-time compiler. (internally using self-made dynamic assembler [monoasm](https://github.com/sisshiki1969/monoasm))

## Status of this project

This project still remains in alpha stage. Currently, functionalities described below are implemented.

- local variables
- instance variables & accessor
- global variables
- block and dynamic local variables
- if-then-elsif-end statement
- for-in statement
- while/until statement and postfix while/until modifier
- Object-oriented features
  - classes: Object, Integer, Float, String, Symbol, Class, Array, Hash, Proc, ..
  - superclass and singleton class and methods
- class definition
- method definition
  - required parameter
  - optional parameter
  - rest parameter
  - block parameter
- garbage collection

## Benchmark

- measured by [benchmark-driver](https://github.com/benchmark-driver/benchmark-driver) with '--repeat-count 3' option.
- benchmark codes are [in the official repo](https://github.com/ruby/ruby/tree/master/benchmark), or in the benchmark directory (`qsort.rb` and `tarai.rb` etc, shown with *).
- measurements are shown in iteration/sec (the higher, the better).

|                     |   3.2.0| 3.2.0 --yjit|     monoruby|
|:--------------------|-------:|------------:|------------:|
|loop_whileloop       |   3.035|        3.034|       23.642|
|qsort*               |124.288k|     331.889k|     520.898k|
|app_fib              |   3.672|       14.480|       17.158|
|tarai*               |   2.969|       13.138|       14.219|
|so_mandelbrot        |   0.609|        0.959|       16.530|
|so_nbody             |   1.075|        1.719|        6.183|
|app_aobench          |   0.027|        0.047|        0.134|

|                     |   3.2.0| 3.2.0 --yjit|     monoruby|
|:--------------------|-------:|------------:|------------:|
|vm_ivar              |135.293M|     132.692M|       1.098G|
|vm_ivar_get          |  11.542|       20.882|       68.378|
|vm_ivar_set          |179.545M|     180.243M|     855.925M|
|vm_ivar_generic_get  | 15.578M|      15.860M|     127.333M|
|vm_ivar_generic_set  | 12.817M|      16.633M|      88.632M|
|vm_attr_ivar         | 58.971M|      57.871M|     331.872M|
|vm_attr_ivar_set     | 53.193M|      53.351M|     332.233M|

|                     |   3.2.0| 3.2.0 --yjit|     monoruby|
|:--------------------|-------:|------------:|------------:|
|integer_times*       |  41.014|       42.531|      113.344|
|vm_const             |135.574M|     133.186M|       1.076G|
|vm_const_many        | 10.765M|      60.566M|      63.591M|
|vm_method_with_block |  8.399M|       8.239M|      37.176M|
|vm_block             | 32.759M|      61.373M|      93.229M|
|vm_yield             |   1.179|        1.158|        4.037|

|                     |   3.2.0| 3.2.0 --yjit|     monoruby|
|:--------------------|-------:|------------:|------------:|
|to_f*                | 14.402M|      14.480M|     281.587M|
|math_sqrt*           |  9.878M|       9.955M|      76.104M|
|math_sin*            |  6.031M|       6.156M|      16.321M|
|math_cos*            |  6.132M|       6.288M|      15.967M|

## How to run

To build monoruby, You'll need installation of Rust.
Please be aware that **only nightly version of Rust works** for monoruby.

To run ruby program file on monoruby,

```sh
> cargo run test.rb
```

or

```sh
> cargo run --release -- test.rb
```

one liner

```sh
> cargo run -- -e "puts 100"
```

You can launch REPL.

```sh
> cargo run --bin irm
```

or

```sh
> ./irm.sh
```
