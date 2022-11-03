cargo build --release
benchmark-driver benchmark/integer_times.yaml --rbenv '3.2.0-dev; 3.2.0-dev --yjit' -e 'target/release/monoruby' -e 'target/release/monoruby --no-jit' --repeat-count 3
