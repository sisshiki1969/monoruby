cd monoruby && cargo install --path .
cd ../
ruby -v
ruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
echo
ruby -v --yjit
ruby --yjit ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
echo
monoruby -v
monoruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes