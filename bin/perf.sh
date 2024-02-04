RUSTFLAGS=-Cforce-frame-pointers cargo install --features perf --path monoruby
perf record -F 2000 -g -- monoruby $@
perf script | ../FlameGraph/stackcollapse-perf.pl | ../FlameGraph/flamegraph.pl > out.svg
rm perf.data*
