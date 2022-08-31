#!/bin/bash

cargo build --release
ruby so_mandelbrot.rb > mandel.ppm
target/release/monoruby so_mandelbrot.rb > mandel1.ppm
target/release/monoruby --no-jit so_mandelbrot.rb > mandel2.ppm
diff -s mandel.ppm mandel1.ppm
diff -s mandel.ppm mandel2.ppm
rm mandel.ppm mandel1.ppm mandel2.ppm
benchmark-driver so_mandelbrot.rb --rbenv '3.2.0-dev; 3.2.0-dev --yjit' -e 'target/release/monoruby' -e 'target/release/monoruby --no-jit' --repeat-count 3
