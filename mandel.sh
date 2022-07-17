#!/bin/bash

cargo build --release
ruby mandel.rb > mandel.ppm
target/release/monoruby mandel.rb > mandel1.ppm
target/release/monoruby --jit mandel.rb > mandel2.ppm
diff -s mandel.ppm mandel1.ppm
diff -s mandel.ppm mandel2.ppm
rm mandel.ppm mandel1.ppm mandel2.ppm
benchmark-driver mandel.rb --rbenv '3.2.0-dev; 3.2.0-dev --yjit; 3.2.0-dev --mjit' -e 'target/release/monoruby' -e 'target/release/monoruby --aot' --repeat-count 3
