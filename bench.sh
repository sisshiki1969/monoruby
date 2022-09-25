#!/bin/bash

cargo build --release
cd benchmark && benchmark-driver quick_sort.yaml vm_ivar.yaml vm_attr_ivar.yaml vm_attr_ivar_set.yaml app_fib.rb tarai.rb  --rbenv '3.2.0-dev; 3.2.0-dev --yjit; 3.2.0-dev --mjit' -e ../target/release/monoruby --repeat-count 3