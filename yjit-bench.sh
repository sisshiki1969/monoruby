cargo build --release
cargo install --path .
cd ../yjit-bench
./run_benchmarks.rb fib -e "monoruby"
cd ../monoruby