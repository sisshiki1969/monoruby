cargo build --release
benchmark-driver -o markdown benchmark/integer.yml benchmark/math.yml --rbenv '3.2.2; 3.2.2 --yjit' -e 'target/release/monoruby' -e 'target/release/monoruby --no-jit' --repeat-count 3
