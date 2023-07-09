cargo build --release
benchmark-driver -o markdown benchmark/vm_fiber_allocate.yml benchmark/array.yaml benchmark/integer_times.yaml benchmark/vm_const.yml benchmark/vm_method_with_block.yml benchmark/vm_block.yml benchmark/vm_yield.yml benchmark/vm_super.yml --rbenv '3.2.0; 3.2.0 --yjit' -e 'target/release/monoruby' -e 'target/release/monoruby --no-jit' --repeat-count 3
