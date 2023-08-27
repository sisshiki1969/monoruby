cd monoruby && cargo install --path . && cd ../
perf record -F 1000 -g -- monoruby benchmark/binarytrees.rb
perf script > out.perf
../FlameGraph/stackcollapse-perf.pl out.perf > out.folded
../FlameGraph/flamegraph.pl out.folded > out.svg
rm out.perf perf.data*

