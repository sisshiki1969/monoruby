#!/bin/bash

cargo build --release --features emit-asm
target/release/monoruby benchmark/app_aobench.rb 2> aobench.disas > /dev/null
cargo build --release
#ruby benchmark/app_aobench.rb > ruby.ppm
target/release/monoruby benchmark/app_aobench.rb > monoruby.ppm
convert monoruby.ppm monoruby.jpg
benchmark-driver benchmark/app_aobench.rb --rbenv '3.2.0-dev; 3.2.0-dev --yjit' -e 'target/release/monoruby' -e 'target/release/monoruby --no-jit' --repeat-count 1
