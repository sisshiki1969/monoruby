cargo build
cargo install --debug --path monoruby
cd ../yjit-bench
./run_benchmarks.rb fib -e "monoruby"
cd ../monoruby