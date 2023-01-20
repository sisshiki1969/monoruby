#!/bin/bash

cargo run --release --features emit-asm -- benchmark/app_fib.rb 2> benchmark/fib.disas > /dev/null
cargo run --release --features emit-bc -- benchmark/app_fib.rb 2> benchmark/fib.bytecode > /dev/null

cargo run --release --features emit-asm -- benchmark/app_aobench.rb 2> benchmark/aobench.disas > /dev/null
cargo run --release --features emit-bc -- benchmark/app_aobench.rb 2> benchmark/aobench.bytecode > /dev/null

cargo build --release
ruby benchmark/app_aobench.rb > benchmark/ruby.ppm
target/release/monoruby benchmark/app_aobench.rb > benchmark/monoruby.ppm
convert benchmark/monoruby.ppm benchmark/monoruby.jpg
convert benchmark/ruby.ppm benchmark/ruby.jpg

