cargo install --path monoruby
rbenv local 3.3.0-preview3
ruby -v
ruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
ruby ../optcarrot/bin/optcarrot -b --opt ../optcarrot/examples/Lan_Master.nes
echo

ruby -v --yjit
ruby --yjit ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
ruby --yjit ../optcarrot/bin/optcarrot -b --opt ../optcarrot/examples/Lan_Master.nes
echo

monoruby -v
monoruby --no-jit ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
monoruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
monoruby --no-jit ../optcarrot/bin/optcarrot -b --opt ../optcarrot/examples/Lan_Master.nes
monoruby ../optcarrot/bin/optcarrot -b --opt ../optcarrot/examples/Lan_Master.nes
