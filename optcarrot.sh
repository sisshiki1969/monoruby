echo 'Ruby'
ruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
echo 'monoruby'
cargo run --release -- ../optcarrot/bin/optcarrot -- -b ../optcarrot/examples/Lan_Master.nes