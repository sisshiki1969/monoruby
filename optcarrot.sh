cargo install --path monoruby
rbenv local 3.2.2
ruby -v --yjit
ruby --yjit ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
echo

rbenv local 3.3.0-preview1
ruby -v --yjit
ruby --yjit ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
echo

rbenv local 3.3.0-dev
ruby -v --yjit
ruby --yjit ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
echo

monoruby -v
monoruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes