cargo install --path monoruby
perf record -F 1000 -g -- monoruby $@
perf script > out.perf
../FlameGraph/stackcollapse-perf.pl out.perf > out.folded
../FlameGraph/flamegraph.pl out.folded > out.svg
rm out.perf perf.data*

