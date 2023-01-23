#!/bin/bash

cargo run --release --features emit-asm -- benchmark/app_fib.rb 2> benchmark/fib.disas
cargo run --release --features emit-bc -- benchmark/app_fib.rb 2> benchmark/fib.bytecode

cargo run --release --features emit-asm -- benchmark/tarai.rb 2> benchmark/tarai.disas
cargo run --release --features emit-bc -- benchmark/tarai.rb 2> benchmark/tarai.bytecode

cargo run --release --features emit-asm -- benchmark/app_aobench.rb 2> benchmark/aobench.disas > /dev/null
cargo run --release --features emit-bc -- benchmark/app_aobench.rb 2> benchmark/aobench.bytecode > /dev/null

cargo run --release --features emit-asm -- benchmark/so_mandelbrot.rb 2> benchmark/mandel.disas > /dev/null
cargo run --release --features emit-bc -- benchmark/so_mandelbrot.rb 2> benchmark/mandel.bytecode > /dev/null

cargo build --release
ruby benchmark/app_aobench.rb > benchmark/ruby.ppm
target/release/monoruby benchmark/app_aobench.rb > benchmark/monoruby.ppm
convert benchmark/monoruby.ppm benchmark/monoruby.jpg
convert benchmark/ruby.ppm benchmark/ruby.jpg

ruby benchmark/so_mandelbrot.rb > benchmark/mandel.ppm
target/release/monoruby benchmark/so_mandelbrot.rb > benchmark/mandel1.ppm
target/release/monoruby --no-jit benchmark/so_mandelbrot.rb > benchmark/mandel2.ppm
diff -s benchmark/mandel.ppm benchmark/mandel1.ppm
diff -s benchmark/mandel.ppm benchmark/mandel2.ppm
rm benchmark/*.ppm