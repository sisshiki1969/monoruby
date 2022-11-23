#!/bin/bash

cargo build --release
ruby benchmark/so_mandelbrot.rb > mandel.ppm
target/release/monoruby benchmark/so_mandelbrot.rb > mandel1.ppm
target/release/monoruby --no-jit benchmark/so_mandelbrot.rb > mandel2.ppm
diff -s mandel.ppm mandel1.ppm
diff -s mandel.ppm mandel2.ppm
rm mandel.ppm mandel1.ppm mandel2.ppm
benchmark-driver benchmark/so_mandelbrot.rb --rbenv '3.2.0-preview3; 3.2.0-preview3 --yjit' -e 'target/release/monoruby' -e 'target/release/monoruby --no-jit' --repeat-count 3
