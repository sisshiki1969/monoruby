cargo install --features perf --path monoruby
perf record -F 200 -g -- monoruby $@
perf script | ../FlameGraph/stackcollapse-perf.pl | ../FlameGraph/flamegraph.pl > out.svg
rm perf.data*

