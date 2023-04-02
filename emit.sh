#!/bin/bash

cargo build --release --features emit-asm
target/release/monoruby benchmark/app_fib.rb 2> benchmark/fib.disas
target/release/monoruby benchmark/tarai.rb 2> benchmark/tarai.disas
target/release/monoruby benchmark/app_aobench.rb 2> benchmark/aobench.disas > /dev/null
target/release/monoruby benchmark/so_mandelbrot.rb 2> benchmark/mandel.disas > /dev/null
target/release/monoruby benchmark/binarytrees.rb 2> benchmark/binarytrees.disas

cargo build --release --features emit-bc
target/release/monoruby benchmark/app_fib.rb 2> benchmark/fib.bytecode
target/release/monoruby benchmark/tarai.rb 2> benchmark/tarai.bytecode
target/release/monoruby benchmark/app_aobench.rb 2> benchmark/aobench.bytecode > /dev/null
target/release/monoruby benchmark/so_mandelbrot.rb 2> benchmark/mandel.bytecode > /dev/null
target/release/monoruby benchmark/binarytrees.rb 2> benchmark/binarytrees.bytecode

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
