cd monoruby && cargo install --path .
cd ../
ruby -v
ruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
echo 'with --yjit'
ruby --yjit ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
echo 'monoruby'
monoruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes