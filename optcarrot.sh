cargo install --path monoruby
rbenv local 3.3.0-preview2
ruby -v
ruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
echo

ruby -v --yjit
ruby --yjit ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
echo

#rbenv local truffleruby+graalvm-22.3.1
#ruby -v
#ruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
#echo

monoruby -v
monoruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
