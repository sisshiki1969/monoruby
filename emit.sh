#!/bin/bash

cargo build --release --features emit-asm
target/release/monoruby benchmark/app_fib.rb 2> benchmark/fib.disas > /dev/null
target/release/monoruby benchmark/tarai.rb 2> benchmark/tarai.disas > /dev/null
target/release/monoruby benchmark/app_aobench.rb 2> benchmark/aobench.disas > /dev/null
target/release/monoruby benchmark/so_mandelbrot.rb 2> benchmark/mandel.disas > /dev/null
target/release/monoruby benchmark/binarytrees.rb 2> benchmark/binarytrees.disas
target/release/monoruby benchmark/quick_sort.rb 2> benchmark/quick_sort.disas > /dev/null
target/release/monoruby benchmark/so_nbody.rb 2> benchmark/so_nbody.disas > /dev/null

target/release/monoruby benchmark/plb2/nqueen.rb 2> benchmark/plb2/nqueen.disas > /dev/null
target/release/monoruby benchmark/plb2/sudoku.rb 2> benchmark/plb2/sudoku.disas > /dev/null
target/release/monoruby benchmark/plb2/matmul.rb 2> benchmark/plb2/matmul.disas > /dev/null
target/release/monoruby benchmark/plb2/bedcov.rb 2> benchmark/plb2/bedcov.disas > /dev/null

target/release/monoruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes 2> benchmark/optcarrot.disas > /dev/null

cargo build --release --features emit-bc
target/release/monoruby benchmark/app_fib.rb 2> benchmark/fib.bytecode > /dev/null
target/release/monoruby benchmark/tarai.rb 2> benchmark/tarai.bytecode > /dev/null
target/release/monoruby benchmark/app_aobench.rb 2> benchmark/aobench.bytecode > /dev/null
target/release/monoruby benchmark/so_mandelbrot.rb 2> benchmark/mandel.bytecode > /dev/null
target/release/monoruby benchmark/binarytrees.rb 2> benchmark/binarytrees.bytecode > /dev/null
target/release/monoruby benchmark/quick_sort.rb 2> benchmark/quick_sort.bytecode > /dev/null
target/release/monoruby benchmark/so_nbody.rb 2> benchmark/so_nbody.bytecode > /dev/null
target/release/monoruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes 2> benchmark/optcarrot.bytecode > /dev/null

cargo build --release && target/release/monoruby benchmark/app_aobench.rb > benchmark/monoruby.ppm && convert benchmark/monoruby.ppm benchmark/monoruby.jpg
ruby benchmark/app_aobench.rb > benchmark/ruby.ppm && convert benchmark/ruby.ppm benchmark/ruby.jpg
