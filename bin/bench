#!/bin/bash

cargo build --release
cd benchmark && benchmark-driver -o markdown quick_sort.yaml app_fib.rb tarai.rb so_mandelbrot.rb so_nbody.rb binarytrees.rb app_aobench.rb plb2/nqueen.rb plb2/sudoku.rb plb2/matmul.rb plb2/bedcov.rb --rbenv '3.3.0; 3.3.0 --yjit; truffleruby-23.1.1' -e ../target/release/monoruby -e "../target/release/monoruby --no-jit" --repeat-count 3