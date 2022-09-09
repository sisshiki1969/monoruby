#!/bin/bash

cargo build --release
benchmark-driver tarai.rb tarai_float.rb --rbenv '3.2.0-dev; 3.2.0-dev --yjit' -e target/release/monoruby -e 'target/release/monoruby --no-jit' --repeat-count 3