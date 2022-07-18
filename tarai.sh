#!/bin/bash

cargo build --release
benchmark-driver tarai.rb tarai_float.rb --rbenv '3.2.0-dev; 3.2.0-dev --yjit; 3.2.0-dev --mjit' -e target/release/monoruby -e 'target/release/monoruby --aot' --repeat-count 5