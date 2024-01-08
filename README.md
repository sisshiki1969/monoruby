# monoruby

[![Rust](https://github.com/sisshiki1969/monoruby/actions/workflows/rust.yml/badge.svg?branch=master)](https://github.com/sisshiki1969/monoruby/actions/workflows/rust.yml)
[![codecov](https://codecov.io/gh/sisshiki1969/monoruby/branch/master/graph/badge.svg?token=vAvpafdKER)](https://codecov.io/gh/sisshiki1969/monoruby)

Ruby implementation with yet another JIT compiler written in Rust.

## Features

- Written in Rust from scratch. No dependencies on any other Ruby implementations.
- Fast. Currently, monoruby is comparable to ruby3.3.0+YJIT in the optcarrot benchmark.
- Hand-written original parser.
- Register-based bytecode.
- Bytecode executor (virtual machine) written in x86-64 assembly (yes, we currently support only x86-64!).
- A compact and fast just-in-time compiler. (internally using self-made dynamic assembler [monoasm](https://github.com/sisshiki1969/monoasm))

## Prerequisites

### Platform

Currently, only x86-64/linux is supported.

### Build

To build monoruby, You'll need installation of Rust.
Please be aware that **only nightly Rust works** for monoruby.

[Check here to install Rust](https://www.rust-lang.org/ja/tools/install),
and [see here to work with nightly Rust](https://rust-lang.github.io/rustup/concepts/channels.html#working-with-nightly-rust).

## How to run

To run ruby program file on monoruby,

```sh
> cargo run test.rb
```

or

```sh
> cargo run --release -- test.rb
```

one liner.

```sh
> cargo run -- -e "puts 100"
```

And also, you can launch REPL.

```sh
> cargo run --bin irm
```

or

```sh
> ./irm.sh
```

## How to install

To install monoruby,

```sh
> cargo install --path monoruby
```

Now, you can play with monoruby,

```sh
> monoruby test.rb
```

and its REPL.

```sh
> irm
```

## Status of this project

This project still remains in alpha stage. Currently, functionalities described below are implemented.

- local variables
- instance variables & accessor
- global variables
- block and dynamic local variables
- if-then-elsif-end statement
- for-in statement
- while/until statement and postfix while/until modifier
- begin-rescue-ensure statement
- Object-oriented features
  - classes: Object, Integer, Float, String, Symbol, Class, Array, Hash, Proc, ..
  - superclass and singleton class and methods
- class definition
- method definition
  - required parameter
  - optional parameter
  - rest parameter
  - block parameter
  - keyword parameter
- coroutine (Fiber class)
- garbage collector

## Benchmark

### optcarrot

Currently, monoruby is about 20% faster than CRuby3.2.2preview1+YJIT in the optcarrot benchmark.

```sh
❯ ./optcarrot.sh

ruby 3.3.0 (2023-12-25 revision 5124f9ac75) +YJIT [x86_64-linux]
fps: 126.70500588624708
checksum: 59662

monoruby 0.2.0
fps: 352.1105555393293
checksum: 59662

❯ lscpu
Architecture:                    x86_64
CPU op-mode(s):                  32-bit, 64-bit
CPU(s):                          12
Thread(s) per core:              2
Core(s) per socket:              6
Socket(s):                       1
Model name:                      Intel(R) Core(TM) i7-8700K CPU @ 3.70GHz
CPU MHz:                         3696.000
L1d cache:                       192 KiB
L1i cache:                       192 KiB
L2 cache:                        1.5 MiB
L3 cache:                        12 MiB
<partially omitted>
```

### micro benchmark

- measured by [benchmark-driver](https://github.com/benchmark-driver/benchmark-driver) with '--repeat-count 3' option.
- benchmark codes are [in the official repo](https://github.com/ruby/ruby/tree/master/benchmark), and in the benchmark directory (`qsort.rb` and `tarai.rb` etc, shown with *).
- measurements are shown in iteration/sec (the higher, the better).

|                     |   3.3.0| 3.3.0 --yjit|     monoruby|
|:--------------------|-------:|------------:|------------:|
|loop_whileloop       |   3.030|        3.004|       28.734|
|qsort*               |  1.371k|       3.855k|       7.150k|
|app_fib              |   3.356|       18.950|       20.327|
|tarai*               |   2.917|       14.289|       16.293|
|so_mandelbrot        |   0.550|        0.944|       20.026|
|so_nbody             |   0.984|        1.964|        6.925|
|binarytrees          |   2.669|        4.842|        4.777|
|app_aobench          |   0.025|        0.048|        0.148|

|                     |   3.3.0| 3.3.0 --yjit|    monoruby|
|:--------------------|-------:|------------:|-----------:|
|vm_ivar              |135.987M|     140.434M|    877.408M|
|vm_ivar_get          |  10.971|       27.283|      78.162|
|vm_ivar_set          |163.651M|     200.768M|      1.109G|
|vm_ivar_generic_get  | 15.051M|      15.051M|    188.579M|
|vm_ivar_generic_set  |  8.475M|       9.827M|    167.269M|
|vm_attr_ivar         | 47.534M|      48.266M|    398.173M|
|vm_attr_ivar_set     | 42.305M|      42.606M|    482.824M|

|                             |   3.3.0| 3.3.0 --yjit|      monoruby|
|:----------------------------|-------:|------------:|-------------:|
|vm_array_index_small         |  7.662M|       7.705M|       52.798M|
|vm_array_index_assign_small  |  3.195M|       3.195M|       42.554M|
|vm_array_index               |  7.975M|       7.973M|       37.424M|
|vm_array_index_assign        |  3.930M|       3.882M|       30.736M|

|                             |   3.3.0| 3.3.0 --yjit|    monoruby|
|:----------------------------|-------:|------------:|-----------:|
|vm_const                     | 89.278M|      86.284M|      1.105G|
|vm_const_many                |  7.311M|      69.626M|     67.432M|
|vm_method_with_block         |  8.000M|       8.136M|     44.042M|
