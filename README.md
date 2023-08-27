# monoruby

[![Rust](https://github.com/sisshiki1969/monoruby/actions/workflows/rust.yml/badge.svg?branch=master)](https://github.com/sisshiki1969/monoruby/actions/workflows/rust.yml)
[![codecov](https://codecov.io/gh/sisshiki1969/monoruby/branch/master/graph/badge.svg?token=vAvpafdKER)](https://codecov.io/gh/sisshiki1969/monoruby)

Ruby implementation with yet another JIT compiler written in Rust.

## Features

- register-based bytecode.
- bytecode executor (virtual machine) written in x86-64 assembly (yes, we currently support only x86-64!).
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

monoruby was about 20% faster than CRuby + YJIT in the optcarrot benchmark.

```sh
❯ ./optcarrot.sh

ruby 3.3.0preview1 (2023-05-12 master a1b01e7701) [x86_64-linux]
fps: 44.79677739994306
checksum: 59662

ruby 3.3.0preview1 (2023-05-12 master a1b01e7701) +YJIT [x86_64-linux]
fps: 115.83473434296302
checksum: 59662

monoruby 0.2.0
fps: 141.93747364788806
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

|                     |   3.2.2| 3.2.2 --yjit|     monoruby|
|:--------------------|-------:|------------:|------------:|
|loop_whileloop       |   3.151|        3.159|       18.044|
|qsort*               |  1.435k|       3.342k|       6.287k|
|app_fib              |   3.771|       14.614|       19.603|
|tarai*               |   3.056|       13.392|       14.618|
|so_mandelbrot        |   0.600|        0.984|       20.453|
|so_nbody             |   1.066|        1.815|        6.866|
|app_aobench          |   0.027|        0.048|        0.147|

|                     |   3.2.2| 3.2.2 --yjit|     monoruby|
|:--------------------|-------:|------------:|------------:|
|vm_ivar              |129.866M|     130.487M|       1.547G|
|vm_ivar_get          |  12.187|       21.640|       76.887|
|vm_ivar_set          |158.653M|     174.550M|       5.459G|
|vm_ivar_generic_get  | 14.025M|      14.028M|     185.909M|
|vm_ivar_generic_set  | 11.315M|      14.576M|     149.477M|
|vm_attr_ivar         | 59.776M|      57.298M|     632.528M|
|vm_attr_ivar_set     | 53.396M|      51.188M|     616.784M|

|                             |   3.2.2| 3.2.2 --yjit|   monoruby|
|:----------------------------|-------:|------------:|----------:|
|vm_array_index_small         |  8.634M|       8.514M|    42.198M|
|vm_array_index_assign_small  |  3.350M|       3.348M|    36.469M|
|vm_array_index               |  8.476M|       8.483M|    31.332M|
|vm_array_index_assign        |  4.002M|       4.034M|    30.559M|

|                       |      3.2.2| 3.2.2 --yjit|     monoruby|
|:----------------------|----------:|------------:|------------:|
|vm_const               |   133.905M|     132.822M|       2.535G|
|vm_const_many          |    11.021M|      62.729M|      66.044M|
|vm_method_with_block   |     8.846M|       9.322M|      44.352M|

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

Now, you can play with monoruby.

```sh
> monoruby test.rb
```
