#!/bin/bash

cargo build --release
benchmark-driver benchmark/class_new.yaml --rbenv '3.2.0-dev; 3.2.0-dev --yjit; 3.2.0-dev --mjit' -e target/release/monoruby -e 'target/release/monoruby --no-jit' --repeat-count 5