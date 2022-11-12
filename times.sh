cargo build --release
benchmark-driver benchmark/integer_times.yaml benchmark/vm_block.yml benchmark/vm_yield.yml --rbenv '3.2.0-preview3; 3.2.0-preview3 --yjit' -e 'target/release/monoruby' -e 'target/release/monoruby --no-jit' --repeat-count 3
