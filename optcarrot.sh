#cd monoruby && cargo install --path .
#cd ../
echo 'Ruby'
ruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
echo 'Ruby --yjit'
ruby --yjit ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes
echo 'monoruby'
monoruby ../optcarrot/bin/optcarrot -b ../optcarrot/examples/Lan_Master.nes