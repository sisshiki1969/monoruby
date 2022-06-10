#!/bin/bash

cargo build --release
benchmark-driver ../ruby/benchmark/app_fib.rb --rbenv '3.2.0-dev; 3.2.0-dev --yjit; 3.2.0-dev --mjit' -e target/release/monoruby