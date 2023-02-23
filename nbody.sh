cargo build --release
benchmark-driver -o markdown benchmark/so_nbody.rb benchmark/so_nbody2.rb --rbenv '3.2.0; 3.2.0 --yjit' -e 'target/release/monoruby' -e 'target/release/monoruby --no-jit' --repeat-count 3
