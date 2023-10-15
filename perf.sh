cargo install --features perf --path monoruby
perf record -F 1000 -g -- monoruby $@
perf script | ../FlameGraph/stackcollapse-perf.pl | ../FlameGraph/flamegraph.pl > out.svg
rm perf.data*

