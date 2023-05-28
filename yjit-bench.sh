cargo build --release
cargo install --path monoruby
cd ../yjit-bench
./run_benchmarks.rb fib -e "monoruby"
cd ../monoruby