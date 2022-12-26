#!/bin/bash

cargo build --release
cd benchmark && benchmark-driver -o markdown class_new.yaml loop_whileloop.rb quick_sort.yaml app_fib.rb tarai.rb so_mandelbrot.rb so_nbody.rb app_aobench.rb --rbenv '3.2.0; 3.2.0 --yjit; 3.2.0 --mjit;' -e ../target/release/monoruby -e "../target/release/monoruby --no-jit" --repeat-count 3