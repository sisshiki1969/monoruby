#!/bin/bash

cargo build --release
cd benchmark && benchmark-driver class_new.yaml vm_ivar.yaml vm_attr_ivar.yaml vm_attr_ivar_set.yaml loop_whileloop.rb quick_sort.yaml app_fib.rb tarai.rb  --rbenv '3.2.0-dev; 3.2.0-dev --yjit; 3.2.0-dev --mjit' -e ../target/release/monoruby -e "../target/release/monoruby --no-jit" --repeat-count 3