cd monoruby && cargo install --path . && cd ../
perf record -F 99 -g -- monoruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
perf script > out.perf
../FlameGraph/stackcollapse-perf.pl out.perf > out.folded
../FlameGraph/flamegraph.pl out.folded > out.svg
rm out.perf perf.data*

