cargo build --release
benchmark-driver benchmark/vm_ivar.yaml benchmark/vm_ivar_get.yml benchmark/vm_ivar_set.yml benchmark/vm_attr_ivar.yaml benchmark/vm_attr_ivar_set.yaml --rbenv '3.2.0-dev; 3.2.0-dev --yjit' -e 'target/release/monoruby' -e 'target/release/monoruby --no-jit' --repeat-count 3
