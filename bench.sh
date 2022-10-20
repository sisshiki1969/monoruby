#!/bin/bash

cargo build --release
cd benchmark && benchmark-driver class_new.yaml loop_whileloop.rb quick_sort.yaml app_fib.rb tarai.rb so_mandelbrot.rb so_nbody.rb --rbenv '3.2.0-dev; 3.2.0-dev --yjit; 3.2.0-dev --mjit' -e ../target/release/monoruby -e "../target/release/monoruby --no-jit" --repeat-count 3