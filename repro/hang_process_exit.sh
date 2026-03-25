#!/bin/bash
# Process.exit spec hangs - uses Thread + sleep expecting SystemExit propagation,
# also ruby_exe subprocess for exit! tests
cd /home/user
timeout 15 /home/user/monoruby/target/release/monoruby \
  /home/user/mspec/bin/mspec-run \
  /home/user/spec/core/process/exit_spec.rb 2>&1
echo "EXIT: $?"
# Expected: exit 124 (timeout)
